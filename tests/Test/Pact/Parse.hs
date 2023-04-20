{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Test.Pact.Parse
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Pact.Parse
( spec
) where

import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Text.Trifecta as D
import qualified Text.Trifecta.Delta as D

-- internal modules

import Pact.Parse
import Pact.Types.Parser
import Pact.Types.Exp
import Pact.Types.Info

-- -------------------------------------------------------------------------- --
-- Spec

spec :: Spec
spec = do
    spec_position
    spec_parsePact
    spec_parseModule

-- -------------------------------------------------------------------------- --
-- Tests

-- | This tests the implementation of the `DeltaParsing` instance for
-- "Data.Attoparsec.Text" in "Pact.Types.Orphans".
--
spec_position :: Spec
spec_position = describe "parsing deltas" $ do
    describe "3-bytes utf-8" $ do
        check "\"\x200f\""
        check "\"\x200f\x200f\""
        check "\"a\x200f\x200fz\""

    describe "4-byte utf-8" $ do
        check "\"\x1038D\""
        check "\"\x1038D\x10385\""

    prop "parse delta" $ \x ->
        let s = T.pack (show @T.Text x)
        in getCols s `shouldBe` Right (cols s)

    prop "parse delta 2" $ \(Lit x) ->
       getCols (quoted x) === Right (cols (quoted x))
  where

    -- Check that `pactAttoParseOnly` parses a `Text` as a string literal
    -- and produces the Delta that we would expect (it has the number of
    -- characters and the number of bytes that `text` and `bytestring`
    -- report).
    check :: T.Text -> Spec
    check s = it (show s) $ getCols s `shouldBe` Right (cols s)

    getCols :: T.Text -> Either String D.Delta
    getCols = pactAttoParseOnly (D.stringLiteral @_ @T.Text *> D.position <* D.eof)

spec_parsePact :: Spec
spec_parsePact = describe "parsePact string literal" $ do
    it "U-f002" $ check "\x200f"
    it "U-f002U-f002" $ check "\x200f\x200f"
    it "U-f002mU-f002" $ check "\x200fm\x200f"
    it "aU-f002U-f002z" $ check "a\x200f\x200fz"
    it "aU-f002mU-f002z" $ check "a\x200fm\x200fz"
    prop "parse string literal" $ \(Lit s) -> check s
  where
    check s =
        parsePact (quoted s) `shouldBe` Right (expected s (fromIntegral $ D.column $ cols (quoted s)))
    expected s l = ParsedCode
        { _pcCode = quoted s
        , _pcExps =
            [ ELiteral $ LiteralExp
                { _litLiteral = LString {_lString = s}
                , _litInfo = Parsed {_pDelta = D.Columns 0 0, _pLength = l}
                }
            ]
        }

spec_parseModule :: Spec
spec_parseModule = do
    it "parses a module" $ do
        pm `shouldBe` m
        i `shouldBe` Parsed (D.Columns 0 0) (T.length m)
  where
    (pm, i) = case parsePact m of
        Right (ParsedCode y [EList ListExp { _listInfo = x } ]) -> (y, x)
        _ -> error "spec_parseModule: unexpected parse result"
    m = T.unlines
        [ "(module m G"
        , "  (defcap G () true)"
        , "  (defun f () true)"
        , ")"
        ]

-- -------------------------------------------------------------------------- --
-- Utils

newtype Lit = Lit { unLit :: T.Text }
    deriving (Show, Eq, Ord)

instance Arbitrary Lit where
    arbitrary = Lit
        . T.filter (/= '\\')
        . T.filter (/= '"')
        . T.filter (not . isControl)
        <$> arbitrary

quoted :: T.Text -> T.Text
quoted s = "\"" <> s <> "\""

-- | Produce a trifecta Delta from a given `Text` string.
--   `Columns` arguments are the number of characters and the number of bytes.
cols :: T.Text -> D.Delta
#if MIN_VERSION_text(2,0,0)
cols s = D.Columns (fromIntegral $ T.length s) (fromIntegral $ B.length (T.encodeUtf8 s))
#else
cols s = D.Columns (fromIntegral $ T.length s) (fromIntegral $ B.length (T.encodeUtf16LE s))
#endif

