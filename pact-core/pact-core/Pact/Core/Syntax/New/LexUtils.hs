{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.Syntax.New.LexUtils where

import Control.Lens hiding (uncons)
import Control.Monad.State.Strict
import Data.Word (Word8)
import Data.Text(Text)
import Data.List(uncons)
import Data.ByteString.Internal(w2c)
import Data.ByteString(ByteString)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as B

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Pretty (Pretty(..))
import Pact.Core.Syntax.New.ParseTree

type ParserT = Either String
type ParsedExpr = Expr ParsedName LineInfo
type ParsedDefun = Defun ParsedName LineInfo
type ParsedDef = Def ParsedName LineInfo
type ParsedDefConst = DefConst ParsedName LineInfo
type ParsedModule = Module ParsedName LineInfo
type ParsedTopLevel = TopLevel ParsedName LineInfo
type ParsedReplTopLevel = ReplTopLevel ParsedName LineInfo

data PosToken =
  PosToken
  { _ptToken :: Token
  , _ptInfo :: LineInfo }
  deriving Show

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
  | TokenKeyGov
  | TokenCapGov
  | TokenInterface
  | TokenImport
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
  | TokenColon
  | TokenDot
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
  | TokenNumber !Text
  | TokenString !Text
  | TokenTrue
  | TokenFalse
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

getLineInfo :: LexerT LineInfo
getLineInfo = do
  input <- gets _lexInput
  pure (LineInfo (_inpLine input) (_inpColumn input) 1)

withLineInfo :: Token -> LexerT PosToken
withLineInfo tok = PosToken tok <$> getLineInfo

emit :: (Text -> Token) -> Text -> LexerT PosToken
emit f e = withLineInfo (f e)

token :: Token -> Text -> LexerT PosToken
token tok = const (withLineInfo tok)

runLexerT :: LexerT a -> ByteString -> Either String a
runLexerT act s = evalStateT act (initState s)

instance Pretty Token where
  pretty = \case
    TokenLet -> "let"
    TokenIn -> "in"
    TokenIf -> "if"
    TokenThen -> "then"
    TokenElse -> "else"
    TokenLambda -> "fn"
    TokenLambdaArrow -> "=>"
    TokenModule -> "module"
    TokenKeyGov -> "keyGov"
    TokenCapGov -> "capGov"
    TokenInterface -> "interface"
    TokenImport -> "import"
    TokenDefun -> "defun"
    TokenDefConst -> "defconst"
    TokenDefCap -> "defcap"
    TokenDefPact -> "defpact"
    TokenDefSchema -> "defschema"
    TokenDefTable -> "deftable"
    TokenBless -> "bless"
    TokenImplements -> "implements"
    TokenOpenBrace -> "{"
    TokenCloseBrace -> "}"
    TokenOpenParens -> "("
    TokenCloseParens -> ")"
    TokenOpenBracket -> "["
    TokenCloseBracket -> "]"
    TokenComma -> ","
    TokenColon -> ":"
    TokenDot -> "."
    TokenTyList -> "list"
    TokenTyTable -> "table"
    TokenTyInteger -> "integer"
    TokenTyDecimal -> "decimal"
    TokenTyString -> "string"
    TokenTyBool -> "bool"
    TokenTyUnit -> "unit"
    TokenTyArrow -> "->"
    TokenTyVar b -> "'" <> pretty b
    TokenAssign -> "="
    TokenEq -> "=="
    TokenNeq -> "!="
    TokenGT -> ">"
    TokenGEQ -> ">="
    TokenLT -> "<"
    TokenLEQ -> "<="
    TokenPlus -> "+"
    TokenMinus -> "-"
    TokenMult -> "*"
    TokenDiv -> "/"
    TokenObjAccess -> "@"
    TokenObjRemove -> "#"
    TokenBitAnd -> "&"
    TokenBitOr -> "|"
    TokenAnd -> "&&"
    TokenOr -> "||"
    TokenIdent t -> "ident<" <> pretty t <> ">"
    TokenNumber n -> "number<" <> pretty n <> ">"
    TokenString s -> "\"" <> pretty s <> "\""
    TokenTrue -> "true"
    TokenFalse -> "false"
    TokenVOpen -> "VOPEN"
    TokenVSemi -> "VSEMI"
    TokenVClose -> "VCLOSE"
    TokenEOF -> "EOF"
