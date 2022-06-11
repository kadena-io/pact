{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Pact.Core.Syntax.New.Lexer where

import Data.Text(Text)
import Control.Lens hiding (uncons)
import Control.Monad.State.Strict
import Data.ByteString(ByteString)
import Data.ByteString.Internal(w2c)
import Control.Monad.Except

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Syntax.New.LexUtils

}
%encoding "latin1"

$lower = [ a-z ]
$digit = [ 0-9 ]
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@ident = [$alpha][$alpha $digit \-]*
@decimal = [$digit]+
@integer = [$digit]+
@tyvar = [\'][$lower]+


tokens :-
    [\ \t]+ ;
-- Keywords
<0> let          { token TokenLet }
<0> in           { token TokenIn }
<0> if           { token TokenIf }
<0> else         { token TokenElse }
<0> then         { token TokenThen }
<0> defun        { token TokenDefun }
<0> defcap       { token TokenDefCap }
<0> defconst     { token TokenDefConst }
<0> defschema    { token TokenDefSchema }
<0> deftable     { token TokenDefTable }
<0> defcap       { token TokenDefCap }
<0> interface    { token TokenInterface }
<0> module       { token TokenModule }
<0> bless        { token TokenBless }
<0> implements   { token TokenImplements }
<0> import       { token TokenImport }
<0> list         { token TokenTyList }
<0> true         { token TokenTrue }
<0> false        { token TokenFalse }
<0> keyGov       { token TokenKeyGov }
<0> capGov       { token TokenCapGov }
<0> bool         { token TokenTyBool }
<0> fn           { token TokenLambda }
<0> integer      { token TokenTyInteger }
<0> bool         { token TokenTyBool }
<0> table        { token TokenTyTable }
<0> decimal      { token TokenTyDecimal }
<0> string       { token TokenTyString }

<0> unit         { token TokenTyUnit }
<0> \(           { token TokenOpenParens }
<0> \)           { token TokenCloseParens }
<0> \{           { handleOpenBrace }
<0> \}           { token TokenCloseBrace }
<0> \[           { token TokenOpenBracket }
<0> \]           { token TokenCloseBracket }
<0> \,           { token TokenComma }
<0> \.           { token TokenDot }
<0> \:           { token TokenSemiColon }
<0> \=\>         { token TokenLambdaArrow}
<0> \=\=         { token TokenEq }
<0> \!\=         { token TokenEq }
<0> \=           { token TokenAssign }
<0> \>\=         { token TokenGEQ }
<0> \>           { token TokenGT }
<0> \<\=         { token TokenLEQ }
<0> \<           { token TokenLT }
<0> \+           { token TokenPlus }
<0> \-           { token TokenMinus }
<0> \*           { token TokenMult }
<0> \/           { token TokenDiv }
<0> \&\&         { token TokenAnd }
<0> \|\|         { token TokenOr }
<0> \&           { token TokenBitAnd }
<0> \|           { token TokenBitAnd }
<0> \@           { token TokenObjAccess }
<0> \#           { token TokenObjRemove }
<0> \"           { stringLiteral }
<0> @integer     { emit TokenNumber }
<0> @ident       { emit TokenIdent }
<0> @tyvar       { emit TokenTyVar }
<0> \-\>         { token TokenTyArrow }
<0> \r\n         { handleNewline }
<0> \n           { handleNewline }

<openBrace> {
  "--".*;
  [\n]+[\ \t]* { beginLayout }
  () { \_ -> popStartCode *> scan }
}

<newline> {
  \n       ;
  "--".*\n ;

  () { offsideRule }
}

<eof> () { doEOF }

{
-- Todo: non-horrible errors
scan :: LexerT PosToken
scan = do
  input@(AlexInput _ _ _ bs) <- gets _lexInput
  startcode <- startCode
  case alexScan input startcode of
    AlexEOF -> handleEOF
    AlexError (AlexInput _ _ _ inp) ->
      throwError $ "Lexical error: " ++ show (B.head inp)
    AlexSkip input' _ -> do
      modify' $ \s -> s { _lexInput = input' }
      scan
    AlexToken input' tokl action -> do
      modify' $ \s -> s { _lexInput = input' }
      let t = T.decodeLatin1 (B.take (fromIntegral tokl) bs)
      action t


-- Starting a layout inserts the "virtual open"
-- following a brace.
-- VOpen is how we distinguish something like
-- defun f(a:integer, b:integer) = {a:1, b:1}
-- from the alternative which expects a function body
beginLayout :: Text -> LexerT PosToken
beginLayout _ = do
  popStartCode
  col <- column
  indentSize <- gets _lexIndentSize
  layout >>= \case
    -- We're in a layout block, so we'll perform some checks that we've
    -- incremented the right quantity, etc.
    Just (Layout lz) -> do
      let currIndent = col - lz
      when (currIndent <= 0 || currIndent /= indentSize) $ throwError "Lexical error, invalid indentation"
      pushLayout (Layout (lz + indentSize))
      withLineInfo TokenVOpen
    -- We're _not_ in a layout, so we'll check whether indent size was set at all
    -- Given we're _not_ in an indent block at all, we assume we're @ indent 0
    Nothing -> handleIndent indentSize col
      -- essentially, it's part of the same line and they're doing some funny things
  where
  handleIndent oldIndent currIndent
    -- We hit a case where we haven't indented _any_ blocks,
    -- so we establish what our general indenting is going to be
    -- for the rest of parsing.
    -- If we decided to begin a layout, we _ensure_ that
    -- the indent is exactly 2 or 4 spaces
    | oldIndent < 0 = do
      if currIndent /= 2 && currIndent /= 4
        then throwError ("Lexical error, invalid indentation " ++ show currIndent)
        else do
          lexIndentSize .= currIndent
          pushLayout (Layout currIndent)
          withLineInfo TokenVOpen
    -- Indent has been set before, so there's two cases:
    -- The current indentation greater so it's part of the previous line
    | currIndent == oldIndent = do
        pushLayout (Layout (currIndent+oldIndent))
        withLineInfo TokenVOpen
    | otherwise = throwError "Lexical error: indentation is "

handleNewline :: Text -> LexerT PosToken
handleNewline _ = pushStartCode newline *> scan

handleEOF :: LexerT PosToken
handleEOF = pushStartCode eof *> scan

doEOF :: Text -> LexerT PosToken
doEOF _ = do
  layout >>= \case
    Nothing -> popStartCode *> withLineInfo TokenEOF
    _ ->
      popLayout *> withLineInfo TokenVClose

offsideRule :: Text -> LexerT PosToken
offsideRule _ = do
  col <- column
  layout >>= \case
    Just (Layout lcol) -> case compare col lcol of
      EQ ->
        popStartCode *> withLineInfo TokenVSemi
      GT -> continue
      LT -> do
        popStartCode
        popLayout
        withLineInfo TokenVClose
    Nothing -> continue
  where
  continue = popStartCode *> scan

stringLiteral :: Text -> LexerT PosToken
stringLiteral _ = do
  inp <- gets _lexInput
  info <- getLineInfo
  body <- loop [] inp
  pure (PosToken (TokenString (T.pack body)) info)
  where
  loop acc inp =
    case alexGetByte inp of
      Just (c, rest) ->
        handleChar acc (w2c c) rest
      Nothing -> throwError "Lexical error: Did not close string literal"
  handleChar acc c rest
    | c == '\\' = escape acc rest
    | c == '\n' = throwError "Lexical error: newline in string literal"
    | c == '\r' = throwError "Lexical error: carriage return in string literal"
    | c == '\"' = reverse acc <$ modify' (\s -> s { _lexInput = rest })
    | otherwise = loop (c:acc) rest
  escape acc inp =
    case alexGetByte inp of
      Just (w2c -> c, rest)
        | c == 'n' -> loop ('\n':acc) rest
        | c == 't' -> loop ('\t':acc) rest
        | c == '\\' -> loop ('\\':acc) rest
        | c == '\"' -> loop ('\"':acc) rest
        | c == 'r' -> throwError "Lexical error: carriage return is not supported in strings literals"
        | otherwise -> throwError "Lexical error: Invalid escape sequence"
      Nothing -> throwError "Lexical error: Did not close string literal"



-- Opening a brace lets us know we're beginning an indented block,
-- so we push a layout which is
handleOpenBrace :: Text -> LexerT PosToken
handleOpenBrace _ = do
  pushStartCode openBrace
  withLineInfo TokenOpenBrace

scanTokens :: LexerT [PosToken]
scanTokens = scan' []
  where
  scan' acc =
    scan >>= \case
      PosToken TokenEOF _ -> pure (reverse acc)
      tok -> scan' (tok:acc)

lexer :: ByteString -> Either String [PosToken]
lexer bs = runLexerT scanTokens bs
}
