module ParserSpec (spec) where

import Test.Hspec
import Pact.Repl
import Pact.Repl.Types
import Data.Either
import Control.Monad.State.Strict
import Pact.Types.Term

spec :: Spec
spec = do
  loadBadParens
  runBadTests

evalString' :: String -> IO (Either String (Term Name), ReplState)
evalString' cmd = initReplState StringEval Nothing >>= runStateT (evalRepl' cmd)

loadBadParens :: Spec
loadBadParens = do
  (r,_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-parens.repl"
  it "should fail due to extra close-parens" $ r `shouldSatisfy` isLeft

  (r',_s) <- runIO $ execScript' Quiet "tests/pact/parsing.repl"
  it "should parse correctly" $ r' `shouldSatisfy` isRight

runBadTests :: Spec
runBadTests = do
  (r'',_s) <- runIO $ evalString' "{ 'a: 1 'b: 2}"
  it "should not parse bad object" $ r'' `shouldSatisfy` isLeft
