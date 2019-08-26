
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
  terms <- parseCode expr
  exec state env terms


mockRuns
  :: GasUnitTests
  -> IO ()
mockRuns (GasUnitTests tests) = mapM_ run (NEL.toList tests)
  where
    run (SomeGasTest test) = do
      (res,_) <- bracket (_gasTestMockEnv test) (_gasTestCleanup test) (mockRun test)
      eitherDie (_gasTestDescription test) res


mockRuns'
  :: GasUnitTests
  -> IO [(Either PactError [Term Name], EvalState)]
mockRuns' (GasUnitTests tests) = mapM run (NEL.toList tests)
  where
    run (SomeGasTest test) = do
      print $ "Results for: " ++ T.unpack (_gasTestDescription test)
      (res,state) <- bracket (_gasTestMockEnv test) (_gasTestCleanup test) (mockRun test)
      printResult res
      return (res,state)


benchRun
  :: GasTest e
  -> (EvalEnv e, EvalState)
  -> C.Benchmark
benchRun (GasTest expr desc _ _) (env, state) =
  C.env (parseCode expr) execBench
  where
    execBench terms =
      C.bench (T.unpack desc) $
              C.nfIO (exec state env terms)

bench
  :: SomeGasTest
  -> C.Benchmark
bench (SomeGasTest test) = C.envWithCleanup setup teardown run
  where
    setup = do
      s <- _gasTestMockEnv test
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (_gasTestCleanup test) env
    run (NoopNFData env) =
      benchRun test env

benches
  :: T.Text
  -> GasUnitTests
  -> C.Benchmark
benches groupName (GasUnitTests tests)
  = C.bgroup (T.unpack groupName) (map bench $ NEL.toList tests)


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
  print $ (show $ length untestedNatives)
          ++ " out of "
          ++ (show $ length allNatives)
          ++ " natives still need to be benchmarked."
  
  
{--  let test = GasTest "(+ 1 2)" "(+ 1 2) 1" defEvalState createDefMockSetup (const $ return ())
  (res,_) <- bracket (_gasTestMockEnv test) (_gasTestCleanup test) (mockRun test)
  mapM_ (print . show) untestedNatives
  --C.defaultMain [benchmark (SomeGasTest test)]

  printResult res
--}
