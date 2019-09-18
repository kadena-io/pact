{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.GasModel where

import Control.Exception              (bracket)
import Control.Monad                  (void)
import Statistics.Types               (Estimate(..))

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Criterion.Main             as C
import qualified Criterion                  as C
import qualified Criterion.Types            as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NEL
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

parseNativeFunList :: Option -> [NativeDefName]
parseNativeFunList opt =
  case _oFilter opt of
    Nothing -> []
    Just t -> NativeDefName <$> T.split (== '|') t


type Mean = Double

data BenchResult = BenchResult
  { _benchResultFunName :: NativeDefName
  , _benchResultBackendType :: T.Text
  , _benchResultAbridgedExpr :: T.Text
  , _benchResultMeans :: (Mean, Mean, Mean)
  }
instance Csv.ToRecord BenchResult where
  toRecord (BenchResult funName backend abridgedExpr (m1, m2, m3)) =
    Csv.record [Csv.toField (asString funName),
                Csv.toField backend,
                Csv.toField abridgedExpr,
                Csv.toField m1,
                Csv.toField m2,
                Csv.toField m3
               ]
csvHeader
  :: (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text)
csvHeader =
  ("function", "backendType", "description", "mean1", "mean2", "mean3")


exec
  :: EvalState
  -> EvalEnv e
  -> [Term Name]
  -> IO (Either PactError [Term Name], EvalState)
exec s e terms = runEval' s e $ mapM eval terms


mockRun
  :: GasTest e
  -> (EvalEnv e, EvalState)
  -> IO (Either PactError [Term Name], EvalState)
mockRun (GasTest expr _ _ _ _) (env, state) = do
  terms <- compileCode expr
  exec state env terms


mockRuns
  :: GasUnitTests
  -> IO ()
mockRuns tests = do
  mapM_ run (NEL.toList $ _gasUnitTestsSqlite tests)
  mapM_ run (NEL.toList $ _gasUnitTestsMock tests)
  where
    run test = do
      putStrLn $ "Dry run for " ++ T.unpack (getDescription test)
      (res,_) <- bracket (_gasTestSetup test)
                         (_gasTestSetupCleanup test)
                         (mockRun test)
      eitherDie (getDescription test) res


-- | For debugging purposes
mockRuns'
  :: GasUnitTests
  -> IO [(Either PactError [Term Name], EvalState)]
mockRuns' tests = do
  sqliteRes <- mapM run (NEL.toList $ _gasUnitTestsSqlite tests)
  mockRes <- mapM run (NEL.toList $ _gasUnitTestsMock tests)
  return (sqliteRes <> mockRes)
  where
    run test = do
      print $ "Pact results for: " ++ T.unpack (getDescription test)
      (res,state) <- bracket (_gasTestSetup test)
                             (_gasTestSetupCleanup test)
                             (mockRun test)
      printResult res
      return (res,state)


bench
  :: GasTest e
  -> IO Mean
bench test = do
  terms <- compileCode (_gasTestExpression test)
  putStrLn $ T.unpack (getDescription test)

  report <- bracket setup teardown $ \s ->
    C.benchmark' (run terms s)

  return $ estPoint $
           C.anMean $
           C.reportAnalysis report
  where
    setup = do
      s <- _gasTestSetup test
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (_gasTestSetupCleanup test) env
    run terms ~(NoopNFData (env, state)) =
          C.nfIO (exec state env terms)


benchesOnce
  :: GasUnitTests
  -> IO ()
benchesOnce tests =
  void $ mapOverGasUnitTests tests bench bench


-- | Benchmarks each native function's tests three times
benchesMultiple
  :: (NativeDefName, GasUnitTests)
  -> IO [BenchResult]
benchesMultiple (funName, tests) =
  mapOverGasUnitTests tests run run
  where
    run test = do
      mean1 <- bench test
      mean2 <- bench test
      mean3 <- bench test
      return $ BenchResult
               funName
               (_gasTestBackendType test)
               (_gasTestAbridgedExpr test)
               (mean1, mean2, mean3)



writeCSV :: [BenchResult] -> IO ()
writeCSV results = do
  let content = Csv.encode results
      header = Csv.encode [csvHeader]
  BSL8.writeFile "gas-model.csv" (header <> content)


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
  mapM_ (\(_,t) -> mockRuns' t)
        (HM.toList unitTests)
