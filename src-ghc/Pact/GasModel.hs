
module Pact.GasModel where


import Control.Exception              (bracket)

import qualified Criterion.Main       as C
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NEL
import qualified Data.Text            as T


-- Internal exports
--
import Pact.Eval                  (eval)
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
  mapM_ run (NEL.toList $ _gasUnitTestsSqlite tests)
  mapM_ run (NEL.toList $ _gasUnitTestsMock tests)
  where
    run test = do
      (res,_) <- bracket (_gasTestSetup test) (_gasTestSetupCleanup test) (mockRun test)
      eitherDie (_gasTestDescription test) res


mockRuns'
  :: GasUnitTests
  -> IO [(Either PactError [Term Name], EvalState)]
mockRuns' tests = do
  print "-- SQLiteDb Tests --"
  sqliteRes <- mapM run (NEL.toList $ _gasUnitTestsSqlite tests)
  print "-- MockDb Tests --"
  mockRes <- mapM run (NEL.toList $ _gasUnitTestsMock tests)
  return (sqliteRes <> mockRes)
  where
    run test = do
      print $ "Results for: " ++ T.unpack (_gasTestDescription test)
      (res,state) <- bracket (_gasTestSetup test) (_gasTestSetupCleanup test) (mockRun test)
      printResult res
      return (res,state)


benchRun
  :: GasTest e
  -> (EvalEnv e, EvalState)
  -> C.Benchmark
benchRun (GasTest expr desc _ _) (env, state) =
  C.env (compileCode expr) execBench
  where
    execBench terms =
      C.bench (T.unpack desc) $
              C.nfIO (exec state env terms)

bench
  :: GasTest e
  -> C.Benchmark
bench test = C.envWithCleanup setup teardown run
  where
    setup = do
      s <- _gasTestSetup test
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (_gasTestSetupCleanup test) env
    run ~(NoopNFData (env,state)) =
      benchRun test (env,state)

benches
  :: T.Text
  -> GasUnitTests
  -> C.Benchmark
benches groupName allTests = C.bgroup (T.unpack groupName)
     [ C.bgroup "SQLiteDb" (runBenches $ _gasUnitTestsSqlite allTests)
     , C.bgroup "MockDb" (runBenches $ _gasUnitTestsMock allTests)
     ]
  where
    runBenches tests
      = map bench $ NEL.toList tests

main :: IO ()
main = do
  -- | Checks that unit tests succeed
  mapM_ (\(_,t) -> mockRuns t)
        (HM.toList unitTests)
{--
   -- | Run benchmarks 
  C.defaultMain $
    map (\(n,t) -> benches (asString n) t)
        (HM.toList unitTests) 
--}
  -- | Report gas testing coverage
  mapM_ (print . show) untestedNatives
  print $ "Missing benchmark tests for "
          ++ (show $ length untestedNatives)
          ++ " out of "
          ++ (show $ length allNatives)
          ++ " native functions."



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
