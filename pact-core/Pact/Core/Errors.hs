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

data LexerError
  = LexicalError Char Char
  -- ^ Lexical error: encountered character, last seen character
  | InvalidIndentation Int Int
  -- ^ Invalid indentation: ^ current indentation, expected indentation
  | InvalidInitialIndent Int
  -- ^ Invalid initial indentation: got ^, expected 2 or 4
  | StringLiteralError Text
  -- ^ Error lexing string literal
  | OutOfInputError
  deriving Show

instance Exception LexerError

data ParseError
  = ParsingError [Text] [Text]
  -- ^ Parsing error: [remaining] [expected]
  | PrecisionOverflowError Int
  deriving Show

instance Exception ParseError

data DesugarError
  = UnboundTermVariable Text
  | UnboundTypeVariable Text
  | UnresolvedQualName QualifiedName
  deriving Show

instance Exception DesugarError

renderDesugarError :: DesugarError -> Text
renderDesugarError = \case
  UnboundTermVariable t ->
    T.concat ["Unbound variable ", t]
  UnboundTypeVariable t ->
    T.concat ["Unbound type variable ", t]
  UnresolvedQualName qual ->
    T.concat ["No such name", renderQualName qual]

data TypecheckError
  = UnificationError (Type Text) (Type Text)
  | ContextReductionError (Pred Text)
  deriving Show

instance Exception TypecheckError

data ExecutionError
  = ArrayOutOfBoundsException
  | ArithmeticException
  | EnumeratationError Text
  | DecodeError Text
  | GasExceeded Text
  deriving Show

instance Exception ExecutionError

data FatalPactError
  = FatalExecutionError Text
  | FatalOverloadError Text
  | FatalParserError Text
  deriving Show

instance Exception FatalPactError

data PactError info
  = PELexerError LexerError info
  | PEParseError ParseError info
  | PEDesugarError DesugarError info
  | PETypecheckError TypecheckError info
  | PEExecutionError ExecutionError info
  | PEFatalError FatalPactError info
  deriving Show

instance (Show info, Typeable info) => Exception (PactError info)
