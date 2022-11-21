{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Pact.Core.Errors where

import Control.Exception
import Data.Text(Text)
import Data.Dynamic (Typeable)
import qualified Data.Text as T

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Info

type PactErrorI = PactError LineInfo

data LexerError info
  = LexicalError Char Char info
  -- ^ Lexical error: encountered character, last seen character
  | InvalidIndentation Int Int info
  -- ^ Invalid indentation: ^ current indentation, expected indentation
  | InvalidInitialIndent Int info
  -- ^ Invalid initial indentation: got ^, expected 2 or 4
  | StringLiteralError Text info
  -- ^ Error lexing string literal
  | OutOfInputError info
  deriving Show

instance (Show info, Typeable info) => Exception (LexerError info)

data ParseError info
  = ParsingError [Text] [Text] info
  -- ^ Parsing error: [remaining] [expected]
  | PrecisionOverflowError Int info
  deriving Show

instance (Show info, Typeable info) => Exception (ParseError info)

data DesugarError info
  = UnboundTermVariable Text info
  | UnboundTypeVariable Text info
  | UnresolvedQualName QualifiedName info
  deriving Show

instance (Show info, Typeable info) => Exception (DesugarError info)

renderDesugarError :: (info -> Text) -> DesugarError info -> Text
renderDesugarError render = \case
  UnboundTermVariable t i ->
    T.concat ["Unbound variable ", t, " at", render i]
  UnboundTypeVariable t i ->
    T.concat ["Unbound type variable ", t, " at", render i]
  UnresolvedQualName qual i ->
    T.concat ["No such name", renderQualName qual, " at", render i]

data TypecheckError info
  = UnificationError (Type Text) (Type Text) info
  deriving Show

instance (Show info, Typeable info) => Exception (TypecheckError info)

data ExecutionError
  = ArrayOutOfBoundsException
  | EnforceException Text
  | ReadException Text
  | ArithmeticException
  | EnumeratationError Text
  | DecodeError Text
  deriving Show

instance Exception ExecutionError

data FatalPactError
  = FatalExecutionError Text
  | FatalOverloadError Text
  | FatalParserError Text
  deriving Show

instance Exception FatalPactError

data PactError info
  = PELexerError (LexerError info)
  | PEParseError (ParseError info)
  | PEDesugarError (DesugarError info)
  | PETypecheckError (TypecheckError info)
  | PEExecutionError ExecutionError
  | PEFatalError FatalPactError
  deriving Show

instance (Show info, Typeable info) => Exception (PactError info)
