module ParserSpec where

import Test.Hspec
import Pact.Repl
import Data.Either

spec :: Spec
spec = loadBadParens

loadBadParens :: Spec
loadBadParens = do
  (r,_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-parens.repl"
  it "should fail due to extra close-parens" $ r `shouldSatisfy` isLeft
