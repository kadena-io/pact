{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.IR.LexUtils where

import Control.Lens hiding (uncons)
import Control.Monad.State.Strict
import Data.Word (Word8)
import Data.Text(Text)
import Data.List(uncons)
import Data.ByteString.Internal(w2c)
import Data.ByteString.Lazy(ByteString)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as B


data Token
  -- Keywords
  = TokenLet
  | TokenIn
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenLambda
  | TokenLambdaArrow
  | TokenModule
  | TokenInterface
  | TokenDefun
  | TokenDefConst
  | TokenDefCap
  | TokenDefPact
  | TokenDefSchema
  | TokenDefTable
  | TokenBless
  | TokenImplements
  -- Delimiters
  | TokenOpenBrace -- {
  | TokenCloseBrace -- }
  | TokenOpenParens -- (
  | TokenCloseParens -- )
  | TokenOpenBracket
  | TokenCloseBracket
  | TokenComma
  -- Types
  | TokenTyList
  | TokenTyTable
  | TokenTyInteger
  | TokenTyDecimal
  | TokenTyString
  | TokenTyBool
  | TokenTyUnit
  | TokenTyArrow
  | TokenTyVar Text
  -- Operators
  | TokenAssign
  | TokenEq
  | TokenNeq
  | TokenGT
  | TokenGEQ
  | TokenLT
  | TokenLEQ
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenObjAccess
  | TokenObjRemove
  | TokenBitAnd
  | TokenBitOr
  | TokenAnd
  | TokenOr
  | TokenIdent !Text
  | TokenInteger Text
  | TokenDecimal Text
  -- Layout
  | TokenVOpen
  | TokenVSemi
  | TokenVClose
  | TokenEOF
  deriving (Eq, Show)

data AlexInput
 = AlexInput
 { _inpLine :: {-# UNPACK #-} !Int
 , _inpColumn :: {-# UNPACK #-} !Int
 , _inpLast :: {-# UNPACK #-} !Char
 , _inpStream :: ByteString
 }
 deriving (Eq, Show)

makeLenses ''AlexInput

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = _inpLast

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput line col _ stream) =
  advance <$> B.uncons stream
  where
  advance (c, rest) | w2c c  == '\n' =
    (c
    , AlexInput
    { _inpLine  = line + 1
    , _inpColumn = 0
    , _inpLast = '\n'
    , _inpStream = rest})
  advance (c, rest) =
    (c
    , AlexInput
    { _inpLine  = line
    , _inpColumn = col + 1
    , _inpLast = w2c c
    , _inpStream = rest})

newtype Layout
  = Layout Int
  deriving (Eq, Show)

data LexState
  = LexState
  { _lexInput :: !AlexInput
  , _lexStartCodes :: !(NonEmpty Int)
  , _lexLayout :: [Layout]
  , _lexIndentSize :: Int }
  deriving (Eq, Show)

makeLenses ''LexState

type LexerT = StateT LexState (Either String)

column :: LexerT Int
column = gets (_inpColumn . _lexInput)

startCode :: LexerT Int
startCode = gets (NE.head . _lexStartCodes)

pushStartCode :: Int -> LexerT ()
pushStartCode i = modify' \case
  LexState inp sc ly is -> LexState inp (NE.cons i sc) ly is

popStartCode :: LexerT ()
popStartCode = modify' \case
  LexState inp sc ly is -> case sc of
    _ :| [] -> LexState inp (0 :| []) ly is
    _ :| (x:xs) -> LexState inp (x :| xs) ly is

layout :: LexerT (Maybe Layout)
layout = gets (fmap fst . uncons . _lexLayout)

pushLayout :: Layout -> LexerT ()
pushLayout i = modify' \case
  LexState inp sc ly is -> LexState inp sc (i:ly) is

popLayout :: LexerT ()
popLayout = modify' \case
  LexState inp sc ly is -> case ly of
    _:xs -> LexState inp sc xs is
    [] -> LexState inp sc [] is

initState :: ByteString -> LexState
initState s =
  LexState (AlexInput 0 1 '\n' s) (0 :| []) [] (-1)

emit :: (Text -> Token) -> Text -> LexerT Token
emit = (pure .)

token :: Token -> Text -> LexerT Token
token = const . pure

runLexerT :: LexerT a -> ByteString -> Either String a
runLexerT act s = evalStateT act (initState s)
