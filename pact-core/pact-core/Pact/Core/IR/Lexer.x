{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


module Pact.Core.IR.Lexer where

import Pact.Core.IR.LexUtils

import Data.Text(Text)
import Control.Lens hiding (uncons)
import Control.Monad.State.Strict
import Control.Monad.Except

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding as T

}
%encoding "latin1"

$lower = [ a-z ]
$digit = [ 0-9 ]
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@ident = [$alpha][$alpha $digit \-]*
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
<0> list         { token TokenTyList }

<0> \(           { token TokenOpenParens }
<0> \)           { token TokenCloseParens }
<0> \{           { handleOpenBrace }
<0> \}           { token TokenCloseBrace }
<0> \[           { token TokenOpenBracket }
<0> \]           { token TokenCloseBracket }
<0> \,           { token TokenComma }
<0> \\           { token TokenLambda }
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
<0> \\           { token TokenDiv }
<0> \&\&         { token TokenAnd }
<0> \|\|         { token TokenOr }
<0> \&           { token TokenBitAnd }
<0> \|           { token TokenBitAnd }
<0> \@           { token TokenObjAccess }
<0> \#           { token TokenObjRemove }
<0> @ident       { emit  TokenIdent }
<0> \-\>         { token TokenTyArrow }
<0> \n           { handleNewline }

<openBrace> {
  "--".*;
  [\n]+[\ \t]* { beginLayout }
  ();
}

<closeBrace> {
  "--".*;
  [\n][\ \t] { closeLayout }
  ();
}

<newline> {
  \n         ;
  "--".*\n ;

  () { offsideRule }
}

<eof> () { doEOF }

{
-- handleEOF = do
--   -- TODO: handle layout
--   pure TkEOF
-- Todo: non-horrible errors
scan :: LexerT Token
scan = do
  input@(AlexInput _ _ _ bs) <- gets _lexInput
  startcode <- startCode
  case alexScan input startcode of
    AlexEOF -> pure TokenEOF
    AlexError (AlexInput _ _ _ inp) ->
      throwError $ "Lexical error: " ++ show (B.head inp)
    AlexSkip input' _ -> do
      modify' $ \s -> s { _lexInput = input' }
      scan
    AlexToken input' tokl action -> do
      modify' $ \s -> s { _lexInput = input' }
      action (T.decodeLatin1 (B.toStrict (B.take (fromIntegral tokl) bs)))


-- Starting a layout inserts the "virtual open"
-- following a brace.
-- VOpen is how we distinguish something like
-- defun f(a:integer, b:integer) = {a:1, b:1}
-- from the alternative which expects a function body
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
      pure TokenVOpen
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
          pure TokenVOpen
    -- Indent has been set before, so there's two cases:
    -- The current indentation greater so it's part of the previous line
    | currIndent == oldIndent = pure TokenVOpen
    | otherwise = throwError "Lexical error: indentation is "

handleNewline _ = pushStartCode newline *> scan

handleEOF = pushStartCode eof *> scan

doEOF _ = do
  layout >>= \case
    Nothing -> popStartCode *> pure TokenEOF
    _ -> popLayout *> pure TokenVClose

closeLayout _ = do
  popLayout
  pure TokenVClose

offsideRule _ = do
  col <- column
  layout >>= \case
    Just (Layout lcol) -> case compare col lcol of
      EQ -> popStartCode *> pure TokenVSemi
      GT -> continue
      LT -> do
        popLayout
        pure TokenVClose
    Nothing -> continue
  where
  continue = popStartCode *> scan

-- Opening a brace lets us know we're beginning an indented block,
-- so we push a layout which is
handleOpenBrace :: Text -> LexerT Token
handleOpenBrace _ = do
  pushStartCode openBrace
  pure TokenOpenBrace

}
