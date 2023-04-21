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
import Control.Monad.State.Strict

#if ! (MIN_VERSION_text(2,0,0) && LEGACY_PARSER == 1)
import qualified Data.Attoparsec.Internal.Types as APT
#endif
import qualified Data.Attoparsec.Text as AP
import Data.Char
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
    deriving (Parsing)

-- The TokenParsing instance of StateT is buggy, so we can't derive from it

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
    -- For text <2 the parser used to return for both delta values the number of
    -- 16-bit code unit words. With the UTF-16 encoding used by text <2 this is
    -- the total number of characters plus number of characters that are encoded
    -- using two 16-bit code units. For instance the Ugaritic letter U+1038D
    -- would result in a positional length of two (two 16-bit code units). The
    -- string "a\0x263A\0x1038D" ("a☺𐎍") would have positional length 4 (3
    -- characters plus one 2 16-bit character).
    --
    -- In practice the old behavior was close enough to the number of characters
    -- that it went unnoticed and didn't cause harm on chain. The code just
    -- assumed that it represented the number text characters. Those numbers
    -- appear on chain (up to some block height) within info objects and later
    -- still in failure messages. It is also relevant for extracting the module
    -- text from a pact transaction before storing it in the pact db. The
    -- presence of unicode characters can result in modules containing dangling
    -- data because there are less characters in the module than what is assumed
    -- based on the position information. It could also result in corrupted
    -- module if unicode characters for code points > U+FFFF would appear in
    -- in a transaction in a pact expression before the code of a module.
    --
    -- For text >=2 the attoparsic position tracks just bytes and the internal
    -- representation is UTF-8. For instance the Ugaritic letter U+1038D results
    -- in a byte length of 4. The string "a\0x263A\0x1038D" ("a☺𐎍") has 8 bytes
    -- (1 code unit plus 3 code unit plus 4 code units).
    --
    position = do
#if MIN_VERSION_text(2,0,0) && LEGACY_PARSER == 1

        -- For the Utf-8 legacy parser we do the following: count numbers of
        -- chars and leave the rest of the code unchanged. Most of the time
        -- (including characters that require more than 8 bits in Utf-8) the
        -- result is the same as for the utf-16 legacy parser. We have to apply
        -- fixes for those cases when unicode characters are parsed that require
        -- more than 16bit in UTF-16, which is done below in the 'CharParsing'
        -- instance for 'PactAttoParsec'.

        !charPos <- gets fromIntegral
        let !bytePos = charPos

        -- This works for almost all blocks:
        -- APT.Pos !bytePos <- parserPos
        -- let !charPos = fromIntegral bytePos

#elif MIN_VERSION_text(2,0,0)
        -- This parser produces semantically correct deltas, but is not
        -- backward compatible.
        APT.Pos !bytePos <- parserPos
        !charPos <- gets fromIntegral

#elif LEGACY_PARSER == 1
        -- The Utf-16 legacy parser counts 16bit code units, which is almost the
        -- number of chars. The code then assumes that it is the number of
        -- chars. The parser does not distiguish between bytes and character
        -- counts.
        APT.Pos !bytePos <- parserPos
        let !charPos = fromIntegral bytePos
#else
        -- This parser produces semantically correct deltas, but is not
        -- backward compatible.
        APT.Pos !bytePos <- (* 2) <$> parserPos
        !charPos <- gets fromIntegral
#endif
        return $ TF.Columns charPos (fromIntegral bytePos)
    {-# INLINE position #-}

    slicedWith f a = (`f` mempty) <$> a
    rend = return mempty
    restOfLine = return mempty

#if ! (MIN_VERSION_text(2,0,0) && LEGACY_PARSER == 1)
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
#endif

instance CharParsing PactAttoparsec where
#if MIN_VERSION_text(2,0,0) && LEGACY_PARSER == 1
    satisfy p = do
        c <- PactAttoparsec (satisfy p)
        -- This fixes the case when the legacy parser counts two
        -- utf-16 code units for a single character.
        modify' (+ if fromEnum c > 0xffff then 2 else 1)
        return c
#else
    satisfy p = PactAttoparsec (satisfy p) <* do
        modify' (+ 1)
#endif
    {-# INLINE satisfy #-}

-- | Work around buggy TokenParsing instance of StateT.
--
-- The 'TokenParsing' instance for 'StateT' overwrites the default implementations
-- fo 'someSpace' and 'semi' as follows:
--
-- @
--   someSpace = lift someSpace
--   semi      = lift semi
-- @
--
-- This is problematic since it bypasses the 'Parsing' and 'CharParsing'
-- instances for 'PactAttoParsec'.
--
instance TokenParsing PactAttoparsec where
    someSpace = skipSome (satisfy isSpace)
    {-# INLINE someSpace #-}
    semi = token (satisfy (';'==) <?> ";")
    {-# INLINE semi #-}

