{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Pact.Repl
import Pact.Repl.Types
import Data.Either
import Control.Monad.State.Strict
import Pact.Types.Term
import Pact.Types.Exp
import Pact.Types.Pretty

spec :: Spec
spec = do
  describe "loadBadParens" $ loadBadParens
  describe "runBadTests" $ runBadTests
  describe "prettyLit" $ prettyLit

evalString' :: String -> IO (Either String (Term Name), ReplState)
evalString' cmd = initReplState StringEval Nothing >>= runStateT (evalRepl' cmd)

loadBadParens :: Spec
loadBadParens = do
  (r,_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-parens.repl"
  it "should fail due to extra close-parens" $ r `shouldSatisfy` isLeft

  (r',_s) <- runIO $ execScript' Quiet "tests/pact/parsing.repl"
  it "should parse correctly" $ r' `shouldSatisfy` isRight

  (r'',_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-pact.repl"
  it "should fail due to a rollback on the last step" $ r'' `shouldSatisfy` isLeft

runBadTests :: Spec
runBadTests = do
  (r,_s) <- runIO $ evalString' "{ 'a: 1 'b: 2}"
  it "should not parse bad object" $ r `shouldSatisfy` isLeft

  (r',_s) <- runIO $ evalString' "2.5000000000000082438366423843257667709673523588188657691908576057841129660246632518344756443840469044035563870195392340918714818321610099200359876739795884554030084775850222523998548261336376757270608452858970446458373412193311600907493698300484416227447910"
  it "should not parse overly-long decimal literals" $ r' `shouldSatisfy` isLeft

prettyLit :: Spec
prettyLit = do

  it "123 prints properly" $ pretty (LInteger 123) `shouldBe` "123"
  it "-123 prints properly" $ pretty (LInteger (-123)) `shouldBe` "-123"
  it "123.0 prints properly" $ pretty (LDecimal 123.0) `shouldBe` "123.0"
  it "-123.0 prints properly" $ pretty (LDecimal (- 123.0)) `shouldBe` "-123.0"
  it "123.45 prints properly" $ pretty (LDecimal 123.45) `shouldBe` "123.45"
  it "-123.45 prints properly" $ pretty (LDecimal (- 123.45)) `shouldBe` "-123.45"
  it "true prints properly" $ pretty (LBool True) `shouldBe` "true"
  it "false prints properly" $ pretty (LBool False) `shouldBe` "false"
  it "string prints properly" $ pretty (LString "hello world") `shouldBe` "\"hello world\""
