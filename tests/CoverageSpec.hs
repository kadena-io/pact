module CoverageSpec (spec) where


import Control.Lens
import Control.Monad
import Data.IORef
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.Golden

import Pact.Coverage
import Pact.Coverage.Report
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime

spec :: Spec
spec = do
  testCover


testCover :: Spec
testCover = do
  rpt <- runIO $ do
    let fn = "tests/lcov/lcov.repl"
    (ref,adv) <- mkCoverageAdvice
    s <- initReplState (Script False fn) Nothing
    void $! execScriptState' fn s (set (rEnv . eeAdvice) adv)
    readIORef ref
  it "matches golden coverage" $ Golden
      { output = T.unpack (showReport rpt)
      , encodePretty = id
      , writeToFile = writeFile
      , readFromFile = readFile
      , testName = "lcov"
      , directory = "golden"
      , failFirstTime = False
      }
