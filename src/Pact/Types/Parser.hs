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
  (
    PactParser(..)
  , symbols
  , style
  )
  where


import Control.Applicative
import Control.Monad
import Text.Trifecta
import qualified Data.HashSet as HS
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style

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
