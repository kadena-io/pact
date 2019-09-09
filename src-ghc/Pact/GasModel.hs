{-# LANGUAGE OverloadedStrings #-}

module Pact.GasModel where


import Control.Exception              (bracket)
import Statistics.Types               (Estimate(..))

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Criterion.Main             as C
import qualified Criterion                  as C
import qualified Criterion.Types            as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NEL
import qualified Data.Text                  as T
import qualified Data.Csv                   as Csv


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
  :: T.Text
  -> T.Text
  -> GasTest e
  -> IO BenchResults
bench funName backendType test = do
  terms <- compileCode (_gasTestExpression test)
  bracket setup teardown $ \s -> do
    putStrLn $ T.unpack funName ++ "/" ++
               T.unpack backendType ++ "/" ++
               T.unpack (_gasTestDescription test)
    r1 <- C.benchmark' (run terms s)
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
  :: T.Text
  -> GasUnitTests
  -> IO [BenchResults]
benches funName tests = do
  sqliteBenches <- mapM (bench funName "SQLiteDb")
                   (NEL.toList $ _gasUnitTestsSqlite tests)
  mockBenches <- mapM (bench funName "MockDb")
                 (NEL.toList $ _gasUnitTestsSqlite tests)
  return $ sqliteBenches <> mockBenches


writeCSV :: [BenchResults] -> IO ()
writeCSV results = do
  let content = Csv.encode results
      header = Csv.encode [csvHeader]
  BSL8.writeFile "gas-model.csv" (header <> content)


main :: IO ()
main = do
  -- Checks that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  mapM_ (\(_,t) -> mockRuns t)
        (HM.toList unitTests)

  -- Run benchmarks
  putStrLn "Running benchmarks"
  allBenches <- mapM (\(n,t) -> benches (asString n) t)
                (HM.toList unitTests)

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
