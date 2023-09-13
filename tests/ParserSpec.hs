{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec (spec) where

import Data.Default
import NeatInterpolation (text)
import Test.Hspec
import Pact.Repl
import Pact.Repl.Types
import Data.Either
import Data.Text (Text)
import Control.Monad.State.Strict
import Pact.Compile
import Pact.Parse
import Pact.Types.PactError
import Pact.Types.Exp
import Pact.Types.ExpParser
import Pact.Types.Names
import Pact.Types.Pretty
import Pact.Types.Term

spec :: Spec
spec = do
  describe "loadBadParens" loadBadParens
  describe "runBadTests" runBadTests
  describe "prettyLit" prettyLit
  describe "narrowTryBackCompat" narrowTryBackCompat
  describe "condParsing" condParsing
  parseNames

evalString' :: String -> IO (Either String (Term Name), ReplState)
evalString' cmd = initReplState StringEval Nothing >>= runStateT (evalRepl' cmd)

loadBadParens :: Spec
loadBadParens = do
  it "should fail due to extra close-parens" $ do
    (r,_s) <- execScript' Quiet "tests/pact/bad/bad-parens.repl"
    r `shouldSatisfy` isLeft

  it "should parse correctly" $ do
    (r',_s) <- execScript' Quiet "tests/pact/parsing.repl"
    r' `shouldSatisfy` isRight

  it "should fail due to a rollback on the last step" $ do
    (r'',_s) <- execScript' Quiet "tests/pact/bad/bad-pact.repl"
    r'' `shouldSatisfy` isLeft

runBadTests :: Spec
runBadTests = do
  it "should not parse bad object" $ do
    (r,_s) <- evalString' "{ 'a: 1 'b: 2}"
    r `shouldSatisfy` isLeft

  it "should not parse overly-long decimal literals" $ do
    (r',_s) <- evalString' "2.5000000000000082438366423843257667709673523588188657691908576057841129660246632518344756443840469044035563870195392340918714818321610099200359876739795884554030084775850222523998548261336376757270608452858970446458373412193311600907493698300484416227447910"
    r' `shouldSatisfy` isLeft

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

parseNames :: Spec
parseNames = do
  let
    positive name test = it ("should parse " <> name) test
    negative name parse = it ("should not parse " <> name) $ parse `shouldSatisfy` isLeft

  describe "parseModuleName" $ do
    positive "a module name" $ parseModuleName "m" `shouldBe` Right (ModuleName "m" Nothing)
    positive "a namespaced module name" $ parseModuleName "n.m" `shouldBe` Right (ModuleName "m" (Just (NamespaceName "n")))

    negative "the empty string" $ parseModuleName ""
    negative "a single dot" $ parseModuleName "."
    negative "extra dots" $ parseModuleName "a..b"
    negative "three components" $ parseModuleName "a.b.c"

  describe "parseQualifiedName" $ do
    positive "a qualified name" $ parseQualifiedName def "m.r" `shouldBe` Right (QualifiedName "m" "r" def)
    positive "a namespaced qualified name" $ parseQualifiedName def "n.m.r" `shouldBe` Right (QualifiedName "n.m" "r" def)

    negative "the empty string" $ parseQualifiedName def ""
    negative "a single dot" $ parseQualifiedName def "."
    negative "extra dots" $ parseQualifiedName def "a..b"
    negative "four components" $ parseQualifiedName def "a.b.c.d"

parseCompile :: ParseEnv -> Text -> Either String (Either PactError [Term Name])
parseCompile pe m = compileExps pe mkEmptyInfo <$> parseExprs m

rightRight :: Either l1 (Either l2 r) -> Bool
rightRight (Right Right {}) = True
rightRight _ = False

narrowTryBackCompat :: Spec
narrowTryBackCompat = do
  -- code from bad-term-in-list.repl
  it "preserves old try bug" $
    parseCompile (ParseEnv False False) "[(module m g (defcap g () 1))]" `shouldSatisfy` rightRight

condParsing :: Spec
condParsing = do
  it "parses single-token fallbacks" $
    parses [text|
      (module m g (defcap g() true)
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            ((= name "bob")   "Boy")
            name
          )
        )
      )
      |]
  it "parses S-expr fallbacks" $
    parses [text|
      (module m g (defcap g() true)
        (defun unknown:string (name:string param:string) (format "I don't know {}" [name]))
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            ((= name "bob")   "Boy")
            (unknown name name)
          )
        )
      )
      |]
  it "parses S-expr fallbacks of two terms" $
    parses [text|
      (module m g (defcap g() true)
        (defun unknown:string (name:string) (format "I don't know {}" [name]))
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            ((= name "bob")   "Boy")
            (unknown name)
          )
        )
      )
      |]
  it "doesn't parse single-token branches in the middle" $
    doesn'tParse [text|
      (module m g (defcap g() true)
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            name
            ((= name "bob")   "Boy")
            name
          )
        )
      )
      |]
  it "doesn't parse single-token branches in the middle followed by another single-token" $
    doesn'tParse [text|
      (module m g (defcap g() true)
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            name
            name
            ((= name "bob")   "Boy")
            name
          )
        )
      )
      |]
  it "doesn't parse S-exprs in the middle" $
    doesn'tParse [text|
      (module m g (defcap g() true)
        (defun unknown:string (name:string param:string) (format "I don't know {}" [name]))
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            (unknown name name)
            ((= name "bob")   "Boy")
            name
          )
        )
      )
      |]
  it "doesn't parse S-exprs in the middle followed by another S-expr" $
    doesn'tParse [text|
      (module m g (defcap g() true)
        (defun unknown:string (name:string param:string) (format "I don't know {}" [name]))
        (defun is-boy-or-girl:string (name:string)
          (cond
            ((= name "alice") "Girl")
            (unknown name name)
            (unknown name name)
            ((= name "bob")   "Boy")
            name
          )
        )
      )
      |]
  where
  parses txt = parseCompile (ParseEnv False True) txt `shouldSatisfy` rightRight
  doesn'tParse txt = parseCompile (ParseEnv False True) txt `shouldNotSatisfy` rightRight
