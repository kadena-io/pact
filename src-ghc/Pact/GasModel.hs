{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.GasModel where

import Control.Exception              (bracket)
import Control.Monad                  (unless)
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
mockRun (GasTest expr _ _ _) (env, state) = do
  terms <- compileCode expr
  exec state env terms


mockRuns
  :: GasUnitTests
  -> IO ()
mockRuns tests = do
  putStrLn "SQLITE DATABASE"
  mapM_ run (NEL.toList $ _gasUnitTestsSqlite tests)
  putStrLn "MOCK DATABASE"
  mapM_ run (NEL.toList $ _gasUnitTestsMock tests)
  where
    run test = do
      putStrLn $ "MockRun for " ++ T.unpack (_gasTestDescription test)
      (res,_) <- bracket (_gasTestSetup test) (_gasTestSetupCleanup test) (mockRun test)
      eitherDie (_gasTestDescription test) res


mockRuns'
  :: GasUnitTests
  -> IO [(Either PactError [Term Name], EvalState)]
mockRuns' tests = do
  putStrLn "SQLITE DATABASE"
  sqliteRes <- mapM run (NEL.toList $ _gasUnitTestsSqlite tests)
  putStrLn "MOCK DATABASE"
  mockRes <- mapM run (NEL.toList $ _gasUnitTestsMock tests)
  return (sqliteRes <> mockRes)
  where
    run test = do
      print $ "Results for: " ++ T.unpack (_gasTestDescription test)
      (res,state) <- bracket (_gasTestSetup test) (_gasTestSetupCleanup test) (mockRun test)
      printResult res
      return (res,state)


type BenchResults = (T.Text, T.Text, T.Text, Double, Double, Double)
csvHeader :: (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text)
csvHeader = ("function", "backendType", "description", "mean1", "mean2", "mean3")

bench
  :: Bool
  -> T.Text
  -> T.Text
  -> GasTest e
  -> IO BenchResults
bench justBench funName backendType test = do
  terms <- compileCode (_gasTestExpression test)
  bracket setup teardown $ \s -> do
    putStrLn $ T.unpack funName ++ "/" ++
               T.unpack backendType ++ "/" ++
               T.unpack (_gasTestDescription test)
    r1 <- C.benchmark' (run terms s)
    if justBench
      then do
        return (funName,backendType,(_gasTestDescription test),0,0,0)
      else do
        r2 <- C.benchmark' (run terms s)
        r3 <- C.benchmark' (run terms s)
        return (funName,
                backendType,
                (_gasTestDescription test),
                (estPoint $ C.anMean $ C.reportAnalysis r1),
                (estPoint $ C.anMean $ C.reportAnalysis r2),
                (estPoint $ C.anMean $ C.reportAnalysis r3))

  where
    setup = do
      s <- _gasTestSetup test
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (_gasTestSetupCleanup test) env
    run terms ~(NoopNFData (env, state)) =
          C.nfIO (exec state env terms)


benches
  :: Bool
  -> T.Text
  -> GasUnitTests
  -> IO [BenchResults]
benches justBench funName tests = do
  sqliteBenches <- mapM (bench justBench funName "SQLiteDb")
                   (NEL.toList $ _gasUnitTestsSqlite tests)
  mockBenches <- mapM (bench justBench funName "MockDb")
                 (NEL.toList $ _gasUnitTestsSqlite tests)
  return $ sqliteBenches <> mockBenches


writeCSV :: [BenchResults] -> IO ()
writeCSV results = do
  let content = Csv.encode results
      header = Csv.encode [csvHeader]
  BSL8.writeFile "gas-model.csv" (header <> content)


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


main :: IO ()
main = do
  Option{..} <- O.execParser options
  let tests = HM.toList $ case _oFilter of
        Nothing -> unitTests
        Just ts -> HM.filterWithKey matching unitTests
          where tlist = NativeDefName <$> T.split (== '|') ts
                matching k _ = k `elem` tlist

  -- Checks that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  mapM_ (\(_,t) -> mockRuns t) tests

  -- Run benchmarks
  putStrLn "Running benchmarks"
  allBenches <- mapM (\(n,t) -> benches _oBenchOnly (asString n) t) tests

  unless _oBenchOnly $ do

      putStrLn "Exporting benchmarks"
      writeCSV (concat allBenches)

      -- Report gas testing coverage
      putStrLn "Reporting coverage"
      print $ "Missing benchmark tests for "
              ++ show (length untestedNatives)
              ++ " out of "
              ++ show (length allNatives)
              ++ " native functions."
      mapM_ (print . show) untestedNatives


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
