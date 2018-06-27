module ParserSpec (spec) where

import Test.Hspec
import Pact.Repl
import Pact.Repl.Types
import Data.Either

spec :: Spec
spec = loadBadParens

loadBadParens :: Spec
loadBadParens = do
  (r,_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-parens.repl"
  it "should fail due to extra close-parens" $ r `shouldSatisfy` isLeft

  (r',_s) <- runIO $ execScript' Quiet "tests/pact/parsing.repl"
  it "should parse correctly" $ r' `shouldSatisfy` isRight
