{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Pact.Core.Syntax.Lisp.Lexer(lexer, runLexerIO) where

import Control.Monad.State.Strict
import Control.Exception(throwIO)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.ByteString.Internal(w2c)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Syntax.Lisp.LexUtils

}
%encoding "latin1"

$lower = [ a-z ]
$digit = [ 0-9 ]
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@ident = [$alpha][$alpha $digit \-]*
@integer = [\-]?[$digit]+
@comment = [\;][\;][.]*[\n]


tokens :-
    @comment;
    $white+;
    -- Keywords
    let          { token TokenLet }
    in           { token TokenIn }
    if           { token TokenIf }
    else         { token TokenElse }
    then         { token TokenThen }
    defun        { token TokenDefun }
    defcap       { token TokenDefCap }
    defconst     { token TokenDefConst }
    defschema    { token TokenDefSchema }
    deftable     { token TokenDefTable }
    defcap       { token TokenDefCap }
    interface    { token TokenInterface }
    module       { token TokenModule }
    bless        { token TokenBless }
    implements   { token TokenImplements }
    use          { token TokenImport }
    list         { token TokenTyList }
    true         { token TokenTrue }
    false        { token TokenFalse }
    keyGov       { token TokenKeyGov }
    capGov       { token TokenCapGov }
    bool         { token TokenTyBool }
    lambda       { token TokenLambda }
    integer      { token TokenTyInteger }
    bool         { token TokenTyBool }
    table        { token TokenTyTable }
    decimal      { token TokenTyDecimal }
    string       { token TokenTyString }
    unit         { token TokenTyUnit }
    and          { token TokenAnd }
    or           { token TokenOr }
    at           { token TokenObjAccess }
    remove       { token TokenObjRemove }
    try          { token TokenTry }
    error        { token TokenError }
    progn        { token TokenBlockIntro }
    suspend      { token TokenSuspend }

    @integer     { emit TokenNumber }
    @ident       { emit TokenIdent }
    \(           { token TokenOpenParens }
    \)           { token TokenCloseParens }
    \{           { token TokenOpenBrace }
    \}           { token TokenCloseBrace }
    \[           { token TokenOpenBracket }
    \]           { token TokenCloseBracket }
    \,           { token TokenComma }
    \.           { token TokenDot }
    \:           { token TokenColon }
    \=\>         { token TokenLambdaArrow}
    \=\=         { token TokenEq }
    \!\=         { token TokenNeq }
    -- \=           { token TokenAssign }
    \>\=         { token TokenGEQ }
    \>           { token TokenGT }
    \<\=         { token TokenLEQ }
    \<           { token TokenLT }
    \+           { token TokenPlus }
    \-           { token TokenMinus }
    \*           { token TokenMult }
    \/           { token TokenDiv }
    \&           { token TokenBitAnd }
    \|           { token TokenBitAnd }
    \"           { stringLiteral }
    \-\>         { token TokenTyArrow }

{
-- TODO: non-horrible errors
scan :: LexerM PosToken
scan = do
  input@(AlexInput _ _ _ bs) <- get
  case alexScan input 0 of
    AlexEOF -> withLineInfo TokenEOF
    AlexError (AlexInput line col _last inp) ->
      let li = LineInfo line col 1
      in case B.uncons inp of
        Just (h, _) ->
          throwLexerError (LexicalError (w2c h) _last) li
        Nothing -> throwLexerError (OutOfInputError _last) li
    AlexSkip input' _ -> do
      put input'
      scan
    AlexToken input' tokl action -> do
      put input'
      let t = T.decodeLatin1 (B.take (fromIntegral tokl) bs)
      action t

stringLiteral :: Text -> LexerM PosToken
stringLiteral _ = do
  inp <- get
  info <- getLineInfo
  body <- loop [] inp
  pure (PosToken (TokenString (T.pack body)) info)
  where
  loop acc inp =
    case alexGetByte inp of
      Just (c, rest) ->
        handleChar acc (w2c c) rest
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"
  handleChar acc c rest
    | c == '\\' = escape acc rest
    | c == '\n' = throwLexerError' $ StringLiteralError "newline in string literal"
    | c == '\r' = throwLexerError' $ StringLiteralError "carriage return in string literal"
    | c == '\"' = reverse acc <$ put rest
    | otherwise = loop (c:acc) rest
  escape acc inp =
    case alexGetByte inp of
      Just (w2c -> c, rest)
        | c == 'n' -> loop ('\n':acc) rest
        | c == 't' -> loop ('\t':acc) rest
        | c == '\\' -> loop ('\\':acc) rest
        | c == '\"' -> loop ('\"':acc) rest
        | c == 'r' -> throwLexerError' $ StringLiteralError "carriage return is not supported in strings literals"
        | otherwise -> throwLexerError' $ StringLiteralError "Invalid escape sequence"
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"

-- A colon _may_ indicate the start of a block,
-- so we emit the token and push the start code.
scanTokens :: LexerM [PosToken]
scanTokens = scan' []
  where
  scan' acc =
    scan >>= \case
      PosToken TokenEOF _ -> pure (reverse acc)
      tok -> scan' (tok:acc)

lexer :: ByteString -> Either PactErrorI [PosToken]
lexer bs = runLexerT scanTokens bs

runLexerIO :: ByteString -> IO [PosToken]
runLexerIO bs = either throwIO pure (lexer bs)
}
