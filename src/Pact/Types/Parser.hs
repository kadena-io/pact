{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module      :  Pact.Types.Parser
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Base types for parsers in Pact.
--

module Pact.Types.Parser
  ( -- * Abstract Pact Parser
    PactParser(..)
  , symbols
  , style

  -- * Pact Attoparsec Parser
  , PactAttoparsec(..)
  , pactAttoParseOnly
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Attoparsec.Internal.Types as APT
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta
import Text.Trifecta.Delta as TF

-- -------------------------------------------------------------------------- --
-- | Abstract Pact Parser
--
-- On-chain this is use for Attoparsec as parser backend. In the repl trifecta
-- is used.
--
newtype PactParser p a = PactParser { unPactParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, DeltaParsing)

instance TokenParsing p => TokenParsing (PactParser p) where
  someSpace   = PactParser $ buildSomeSpaceParser someSpace $ CommentStyle "" "" ";" False
  nesting     = PactParser . nesting . unPactParser
  semi        = token $ char ';' <?> ";"
  highlight h = PactParser . highlight h . unPactParser
  token p     = p <* whiteSpace

symbols :: CharParsing m => m Char
symbols = oneOf "%#+-_&$@<>=^?*!|/~"

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "atom"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        (HS.fromList ["true","false"])
        Symbol
        ReservedIdentifier

-- -------------------------------------------------------------------------- --
-- Pact Attoparsec backend parser

-- | A wrapper around Attoparsec that adds DeltaParsing
--
newtype PactAttoparsec a = PactAttoparsec
    { runPactAttoparsec :: StateT Int AP.Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState Int)
    deriving (Parsing, TokenParsing)

pactAttoParseOnly :: PactAttoparsec a -> T.Text -> Either String a
pactAttoParseOnly = AP.parseOnly . flip evalStateT 0 . runPactAttoparsec

-- | Atto DeltaParsing instance provides 'position' only (with no support for
-- hidden chars like Trifecta).
--
instance DeltaParsing PactAttoparsec where
    line = return mempty

    -- Returns @Column NUMBER_OF_CHARS NUMBER_OF_BYTES@
    --
    -- Notes about legacy behavior:
    --
    -- For text <2 it used to return for both values the number of 16-bit code
    -- units words. With the UTF-16 encoding used by text <2 this is the total
    -- number of characters plus number of characters that are encoded using two
    -- 16-bit code units. For instance the Ugaritic letter U+1038D would result
    -- in a positional length of two (two 16-bit code units). The string
    -- "a\0x263A\0x1038D" ("a☺𐎍") would have positional length 4 (3 characters
    -- plus one 2 16-bit character).
    --
    -- In practice the old behavior was close enough to the number of characters
    -- that it went mostly unnoticed and didn't cause harm on chain. The code
    -- just assumed that it represented the number text characters. Those
    -- numbers appear on chain (up to some block height) within info objects and
    -- later still in failure messages. It is also relevant for extracting the
    -- module text from the pact transaction before storing it in the pact db.
    -- The presence of unicode characters can result in modules containing
    -- dangling data because there are less characters in the module than what
    -- is assumed based on the position information.
    --
    -- For text >=2 the attoparsic position tracks just bytes and the internal
    -- representation of UTF-8. For instance the Ugaritic letter U+1038D results
    -- in a byte length of 4. The string "a\0x263A\0x1038D" ("a☺𐎍") has 8 bytes
    -- (1 code unit plus 3 code unit plus 4 code units).
    --
    position = do
#if MIN_VERSION_text(2,0,0) && LEGACY_PARSER == 1
        -- This will almost surely cause trouble because for utf-8 neither the
        -- number of bytes nor the number of characters correspond with the
        -- number of 16bit code units which the legacy parser returns.
        APT.Pos !tmp <- parserPos
        let !bytePos = tmp `div` 2
        let !charPos = fromIntegral bytePos

#elif MIN_VERSION_text(2,0,0)
        -- This is probably our best bet, because for most text the number
        -- of bytes matches the number of characters. However, this correspondence
        -- is much more flaky than for utf-16 and 16bit code units.
        --
        -- We would need special cases for on chain data that includes unicode
        -- code points that require more than a single byte and affect on-chain
        -- representation.
        --
        -- Some code logic may require both values for bytes and characters
        -- to be equal as was the case with the legacy parser. We should try to
        -- fix that logic, if possible.
        APT.Pos !bytePos <- parserPos
        !charPos <- gets fromIntegral

#elif LEGACY_PARSER == 1
        -- This is our legacy parser
        APT.Pos !bytePos <- parserPos
        let !charPos = fromIntegral bytePos
#else
        APT.Pos !bytePos <- (* 2) <$> parserPos
        !charPos <- gets fromIntegral
#endif
        return $ TF.Columns charPos (fromIntegral bytePos)
    {-# INLINE position #-}

    slicedWith f a = (`f` mempty) <$> a
    rend = return mempty
    restOfLine = return mempty

-- | retrieve pos from Attoparsec.
--
-- For text <2 this is the offset in utf-16 code units, which are
-- of size 16 bits.
--
-- For text >=2 this is the offset in bytes.
--
-- The first parameter to the parser is the remaining available input.
--
parserPos :: PactAttoparsec APT.Pos
parserPos = PactAttoparsec $ StateT $ \x ->
    APT.Parser $ \t !pos more _lose win -> win t pos more (pos, x)

instance CharParsing PactAttoparsec where
    satisfy p = PactAttoparsec (satisfy p) <* modify' (+ 1)
    {-# INLINE satisfy #-}

    string s = PactAttoparsec (string s) <* modify' (+ length s)
    {-# INLINE string #-}

    text s = PactAttoparsec (text s) <* modify' (+ T.length s)
    {-# INLINE text #-}

