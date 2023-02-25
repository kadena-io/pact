module CoverageSpec (spec) where


import Control.Lens
import Control.Monad
import Test.Hspec
import Data.Default

import Pact.Coverage
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime


spec :: Spec
spec = do
  testCover


testCover :: Spec
testCover = runIO $ do
  let fn = "tests/lcov/lcov.repl"
  (ref,adv) <- mkCoverageAdvice
  s <- set (rEnv . eeAdvice) adv <$> initReplState (Script False fn) Nothing
  void $! execScriptState' fn s (set (rEnv . eeAdvice) def)
  writeCovReport ref
