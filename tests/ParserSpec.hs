{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import           Data.Either
import qualified Data.Text           as T
import           GHC.Int             (Int64)
import           Pact.PactExpParser
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.SExp
import           Test.Hspec
import           Text.Trifecta
import           Text.Trifecta.Delta

spec :: Spec
spec = loadBadParens

loadBadParens :: Spec
loadBadParens = do
  (r,_s) <- runIO $ execScript' Quiet "tests/pact/bad/bad-parens.repl"
  it "should fail due to extra close-parens" $ r `shouldSatisfy` isLeft

  (r',_s) <- runIO $ execScript' Quiet "tests/pact/parsing.repl"
  it "should parse correctly" $ r' `shouldSatisfy` isRight

  it "should parse numbers" $ do
    parseNumber "-2"   `shouldBe` Right (Left (-2))
    parseNumber "2"    `shouldBe` Right (Left 2)
    parseNumber "-2.0" `shouldBe` Right (Right (-2))
    parseNumber "2.0"  `shouldBe` Right (Right 2)

  it "should parse sexps" $ do
    parseSExp "1"       `shouldBe`
      Right (Token (Number (Left 1)) :~ Span (c 0) (c 1) "")
    parseSExp "-1"      `shouldBe`
      Right (Token (Number (Left (-1))) :~ Span (c 0) (c 2) "")
    parseSExp "+"       `shouldBe`
      Right (Token (Ident "+" NoTrailingSpace) :~ Span (c 0) (c 1) "")
    parseSExp "\"foo\"" `shouldBe`
      Right (Token (String "foo") :~ Span (c 0) (c 5) "")
    parseSExp "[1 2]"   `shouldBe` Right
      (List Square
        [ Token (Number (Left 1)) :~ Span (c 1) (c 3) ""
        , Token (Number (Left 2)) :~ Span (c 3) (c 4) ""
        ] :~ Span (c 0) (c 5) "")

    parseSExp "[1, 2]"  `shouldBe` Right
      (List Square
        [ Token (Number (Left 1))               :~ Span (c 1) (c 2) ""
        , Token (Punctuation "," TrailingSpace) :~ Span (c 2) (c 4) ""
        , Token (Number (Left 2))               :~ Span (c 4) (c 5) ""
        ] :~ Span (c 0) (c 6) "")

    parseSExp "{ foo: bar }" `shouldBe` Right
      (List Curly
        [ Token (Ident "foo" NoTrailingSpace)   :~ Span (c 2) (c 5) ""
        , Token (Punctuation ":" TrailingSpace) :~ Span (c 5) (c 7) ""
        , Token (Ident "bar" TrailingSpace)     :~ Span (c 7) (c 11) ""
        ] :~ Span (c 0) (c 12) "")

    parseSExp "(\"foo\" \"bar\")" `shouldBe`  Right
      (List Paren
        [ Token (String "foo") :~ Span (c 1) (c 7) ""
        , Token (String "bar") :~ Span (c 7) (c 12) ""
        ] :~ Span (c 0) (c 13) "")

    parseSExp "(+ 1 2)" `shouldBe` Right
      (List Paren
        [ Token (Ident "+" TrailingSpace) :~ Span (c 1) (c 3) ""
        , Token (Number (Left 1))         :~ Span (c 3) (c 5) ""
        , Token (Number (Left 2))         :~ Span (c 5) (c 6) ""
        ] :~ Span (c 0) (c 7) "")

    parseSExp "(- 1 2)" `shouldBe` Right
      (List Paren
        [ Token (Ident "-" TrailingSpace) :~ Span (c 1) (c 3) ""
        , Token (Number (Left 1))         :~ Span (c 3) (c 5) ""
        , Token (Number (Left 2))         :~ Span (c 5) (c 6) ""
        ] :~ Span (c 0) (c 7) "")

    parseSExp "(define-keyset 'k (sig-keyset))" `shouldBe` Right
      (List Paren
        [Token (Ident "define-keyset" TrailingSpace) :~ Span (c 1) (c 15) ""
        ,Token (Punctuation "'" NoTrailingSpace)       :~ Span (c 15) (c 16) ""
        ,Token (Ident "k" TrailingSpace)             :~ Span (c 16) (c 18) ""
        ,List Paren
          [ Token (Ident "sig-keyset" NoTrailingSpace) :~ Span (c 19) (c 29) ""
          ] :~ Span (c 18) (c 30) ""
        ] :~ Span (c 0) (c 31) "")

    parseSExp "[1 \"2\" true (+ 3 4)]" `shouldBe` Right
      (List Square
        [ Token (Number (Left 1)) :~ Span (c 1) (c 3) ""
        , Token (String "2") :~ Span (c 3) (c 7) ""
        , Token (Ident "true" TrailingSpace) :~ Span (c 7) (c 12) ""
        , List Paren
           [ Token (Ident "+" TrailingSpace) :~ Span (c 13) (c 15) ""
           , Token (Number (Left 3)) :~ Span (c 15) (c 17) ""
           , Token (Number (Left 4)) :~ Span (c 17) (c 18) ""
           ] :~ Span (c 12) (c 19) ""
        ] :~ Span (c 0) (c 20) "")

    parseSExps "1 2" `shouldBe` Right
      [ Token (Number (Left 1))
      , Token (Number (Left 2))
      ]

    parseSExps "(1 2) (3 4)" `shouldBe` Right
      [ List Paren
        [ Token (Number (Left 1)) :~ Span (c 1) (c 3) ""
        , Token (Number (Left 2)) :~ Span (c 3) (c 4) ""
        ]
      , List Paren
        [ Token (Number (Left 3)) :~ Span (c 7) (c 9) ""
        , Token (Number (Left 4)) :~ Span (c 9) (c 10) ""
        ]
      ]

    parseSExps "(defschema person dob:time)" `shouldBe` Right
      [ List Paren
        [ Token (Ident "defschema" TrailingSpace) :~ Span (c 1)  (c 11) ""
        , Token (Ident "person"    TrailingSpace) :~ Span (c 11) (c 18) ""
        , Token (Ident "dob"     NoTrailingSpace) :~ Span (c 18) (c 21) ""
        , Token (Punctuation ":" NoTrailingSpace) :~ Span (c 21) (c 22) ""
        , Token (Ident "time"    NoTrailingSpace) :~ Span (c 22) (c 26) ""
        ]
      ]


    let s = T.unlines
          [ "(defschema token-row balance:integer)"
          , "(deftable tokens:{token-row})"
          , ""
          , "(defun test:string ()"
          , "  (update tokens \"stu\" {}))"
          ]
    parseSExps s `shouldBe` Right
      [List Paren
        [Token (Ident "defschema" TrailingSpace) :~ Span (c 1) (c 11) ""
        ,Token (Ident "token-row" TrailingSpace) :~ Span (c 11) (c 21) ""
        ,Token (Ident "balance" NoTrailingSpace) :~ Span (c 21) (c 28) ""
        ,Token (Punctuation ":" NoTrailingSpace) :~ Span (c 28) (c 29) ""
        ,Token (Ident "integer" NoTrailingSpace) :~ Span (c 29) (c 36) ""
        ]
      ,List Paren
        [Token (Ident "deftable"  TrailingSpace) :~ Span (c 39) (c 48) ""
        ,Token (Ident "tokens"  NoTrailingSpace) :~ Span (c 48) (c 54) ""
        ,Token (Punctuation ":" NoTrailingSpace) :~ Span (c 54) (c 55) ""
        ,List Curly
          [Token (Ident "token-row" NoTrailingSpace) :~ Span (c 56) (c 65) ""
          ] :~ Span (c 55) (c 66) ""
        ]
      ,List Paren
        [Token (Ident "defun"     TrailingSpace) :~ Span (c 70) (c 76) ""
        ,Token (Ident "test"    NoTrailingSpace) :~ Span (c 76) (c 80) ""
        ,Token (Punctuation ":" NoTrailingSpace) :~ Span (c 80) (c 81) ""
        ,Token (Ident "string"    TrailingSpace) :~ Span (c 81) (c 88) ""
        ,List Paren []                           :~ Span (c 88) (c 93) ""
        ,List Paren
          [Token (Ident "update"  TrailingSpace) :~ Span (c 94) (c 101) ""
          ,Token (Ident "tokens"  TrailingSpace) :~ Span (c 101) (c 108) ""
          ,Token (String "stu")                  :~ Span (c 108) (c 114) ""
          ,List Curly []                         :~ Span (c 114) (c 116) ""
          ]                                      :~ Span (c 93) (c 117) ""
        ]
      ]

    parseSExps "(nestedPact.tester) (nestedPact.tester)"
      `shouldBe` Right
      [List Paren
        [Token (Ident "nestedPact" NoTrailingSpace) :~ Span (c 1) (c 11) ""
        ,Token (Punctuation "."    NoTrailingSpace) :~ Span (c 11) (c 12) ""
        ,Token (Ident "tester"     NoTrailingSpace) :~ Span (c 12) (c 18) ""
        ]
      ,List Paren
        [Token (Ident "nestedPact" NoTrailingSpace) :~ Span (c 21) (c 31) ""
        ,Token (Punctuation "."    NoTrailingSpace) :~ Span (c 31) (c 32) ""
        ,Token (Ident "tester"     NoTrailingSpace) :~ Span (c 32) (c 38) ""
        ]
      ]

    parseSExps "(deftable systbl:{sysdata})" `shouldBe` Right
      [List Paren
        [Token (Ident "deftable"  TrailingSpace) :~ Span (c 1) (c 10) ""
        ,Token (Ident "systbl"  NoTrailingSpace) :~ Span (c 10) (c 16) ""
        ,Token (Punctuation ":" NoTrailingSpace) :~ Span (c 16) (c 17) ""
        ,List Curly
          [Token (Ident "sysdata" NoTrailingSpace) :~ Span (c 18) (c 25) ""
          ] :~ Span (c 17) (c 26) ""
        ]
      ]

    parseSExps "\"foo\\\n\\bar\\\n\\baz\"" `shouldBe`
      Right [ Token (String "foobarbaz") ]

c :: Int64 -> Delta
c x = Columns x x
