module CoverageSpec (spec) where


import Control.Lens
import Control.Monad
import Test.Hspec

import Pact.Coverage
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime

spec :: Spec
spec = do
  testCover


testCover :: Spec
testCover = do
  runIO $ do
    let fn = "tests/lcov/lcov.repl"
    (ref,adv) <- mkCoverageAdvice
    s <- initReplState (Script False fn) Nothing
    void $! execScriptState' fn s (set (rEnv . eeAdvice) adv)
    writeCovReportInDir "tests/lcov" ref
  golden <- runIO $ readFile "tests/lcov/coverage/lcov.info.golden"
  actual <- runIO $ readFile "tests/lcov/coverage/lcov.info"
  it "should match golden" $ actual `shouldBe` golden
