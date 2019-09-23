{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.GasModel where

import Control.Exception              (bracket)
import Control.Monad                  (void)
import Statistics.Types               (Estimate(..))
import Data.List                      (foldl')

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Criterion.Main             as C
import qualified Criterion                  as C
import qualified Criterion.Types            as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Csv                   as Csv
import qualified Options.Applicative        as O


-- Internal exports
--
import Pact.Eval                  (eval)
import Pact.GasModel.Utils
import Pact.GasModel.GasTests
import Pact.Types.GasModel
import Pact.Types.Lang
import Pact.Types.Runtime


data Option = Option
  { _oFilter :: Maybe T.Text
  , _oBenchOnly :: Bool
  }
  deriving (Eq,Show)

options :: O.ParserInfo Option
options = O.info (O.helper <*> parser)
          (O.fullDesc <> O.header "Gas Model Benchmarks")
  where
    parser =
      Option
      <$> ((Just <$> O.strOption
            (O.short 'f' <> O.long "filter" <> O.metavar "NATIVE" <>
             O.help "Pipe-delimited list of natives to run")) O.<|>
            pure Nothing)
      <*> (O.flag False True
           (O.short 'b' <> O.long "bench" <> O.help "Just bench"))

optionToGasTests :: Option -> [(NativeDefName, GasUnitTests)]
optionToGasTests opt = HM.toList $
  case _oFilter opt of
    Nothing -> unitTests
    Just ts -> HM.filterWithKey matchingFuns unitTests
      where tlist = NativeDefName <$> T.split (== '|') ts
            matchingFuns k _ = k `elem` tlist


type Mean = Double
newtype Average = Average {_averagens :: Double }
instance Csv.ToField Average where
  toField (Average avg) = Csv.toField avg

average :: [Mean] -> Average
average nums = Average avgNanoSeconds
  where s = foldl' (+) 0.0 nums
        n = fromIntegral (length nums)
        avgSeconds = s / n
        avgNanoSeconds = avgSeconds * 1000000000


exec
  :: EvalState
  -> EvalEnv e
  -> [Term Name]
  -> IO (Either PactError [Term Name], EvalState)
exec s e terms = runEval' s e $ mapM eval terms


setupEnv :: GasSetup e -> IO (EvalEnv e, EvalState)
setupEnv (GasSetup e s _ _) = do
  env <- e
  state <- s
  return (env, state)


mockRun
  :: PactExpression
  -> (EvalEnv e, EvalState)
  -> IO (Either PactError [Term Name], EvalState)
mockRun expr (env, state) = do
  terms <- compileCode (_pactExpressionFull expr)
  exec state env terms


mockRuns
  :: GasUnitTests
  -> IO ()
mockRuns tests = do
  void $ mapOverGasUnitTests tests run run
  where
    run expr dbSetup = do
      putStrLn $ "Dry run for " ++ T.unpack (getDescription expr dbSetup)

      (res,_) <- bracket (setupEnv dbSetup)
                         (gasSetupCleanup dbSetup)
                         (mockRun expr)
      eitherDie (getDescription expr dbSetup) res


-- | For debugging purposes
mockRuns'
  :: GasUnitTests
  -> IO [(Either PactError [Term Name], EvalState)]
mockRuns' tests = do
  mapOverGasUnitTests tests run run
  where
    run expr dbSetup = do
      putStrLn $ "Pact results for: " ++ T.unpack (getDescription expr dbSetup)
      (res,state) <- bracket (setupEnv dbSetup)
                         (gasSetupCleanup dbSetup)
                         (mockRun expr)
      printResult res
      return (res,state)


bench
  :: PactExpression
  -> GasSetup e
  -> IO Mean
bench expr dbSetup = do
  terms <- compileCode (_pactExpressionFull expr)
  putStrLn $ T.unpack (getDescription expr dbSetup)
  report <- bracket setup teardown $ C.benchmark' . (run terms)
  return $ estPoint $
           C.anMean $
           C.reportAnalysis report
  where
    setup = do
      s <- setupEnv dbSetup
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (gasSetupCleanup dbSetup) env
    run terms ~(NoopNFData (env, state)) =
          C.nfIO (exec state env terms)


benchesOnce
  :: GasUnitTests
  -> IO ()
benchesOnce tests =
  void $ mapOverGasUnitTests tests bench bench


-- | Benchmarks each native function's tests three times
data BenchResult = BenchResult
  { _benchResultFunName :: NativeDefName
  , _benchResultBackendType :: T.Text
  , _benchResultAbridgedExpr :: T.Text
  , _benchResultMeans :: Average
  }
instance Csv.ToRecord BenchResult where
  toRecord (BenchResult funName backend abridgedExpr avg) =
    Csv.record [Csv.toField (asString funName),
                Csv.toField backend,
                Csv.toField abridgedExpr,
                Csv.toField avg
               ]
csvHeader
  :: (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text)
csvHeader =
  ("function", "backendType", "description", "mean1", "mean2", "mean3")


benchesMultiple
  :: (NativeDefName, GasUnitTests)
  -> IO [BenchResult]
benchesMultiple (funName, tests) =
  mapOverGasUnitTests tests run run
  where
    run expr setup = do
      mean1 <- bench expr setup
      mean2 <- bench expr setup
      mean3 <- bench expr setup
      return $ BenchResult
               funName
               (gasSetupBackendType setup)
               (getDescription expr setup)
               (average [mean1, mean2, mean3])


writeCSV :: [BenchResult] -> IO ()
writeCSV results = do
  let content = Csv.encode results
      header = Csv.encode [csvHeader]
  BSL8.writeFile "gas-model-raw-data.csv" (header <> content)


coverageReport :: IO ()
coverageReport = do
  print $ "Missing benchmark tests for "
           ++ show (length untestedNatives)
           ++ " out of "
           ++ show (length allNatives)
           ++ " native functions."
  mapM_ (print . show) untestedNatives


main :: IO ()
main = do
  Option{..} <- O.execParser options
  let tests = HM.toList $ case _oFilter of
        Nothing -> unitTests
        Just ts -> HM.filterWithKey matching unitTests
          where tlist = NativeDefName <$> T.split (== '|') ts
                matching k _ = k `elem` tlist

  -- Enforces that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  mapM_ (\(_,t) -> mockRuns t) tests

  putStrLn "Running benchmark(s)"

  if _oBenchOnly then
    mapM_ (\(_,t) -> benchesOnce t) tests
  else do
    allBenches <- mapM benchesMultiple tests

    putStrLn "Exporting benchmarks"
    writeCSV (concat allBenches)

    putStrLn "Reporting coverage"
    coverageReport


-- | For debugging
runGasTestByName :: T.Text -> IO ()
runGasTestByName nname = do
  case (unitTestFromDef (NativeDefName nname)) of
    Nothing -> print $ show nname ++ "'s gas tests not found."
    Just g -> do
      print $ show nname
      _ <- mockRuns' g
      return ()

runAllGasTests :: IO ()
runAllGasTests = do
  mapM_ (\(_,t) -> mockRuns t)
        (HM.toList unitTests)
