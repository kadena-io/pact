{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Module      :  Pact.PactExpParser
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Parser to 'PactExp's.
--

module Pact.PactExpParser
    (exprs, exprsOnly
    ,parseExprs
    ,parseNumber
    ,parseSExp
    ,parseSExps
    ) where

import           Control.Applicative
import           Control.Comonad      (extract)
import           Control.Monad
import qualified Data.Attoparsec.Text as AP
import           Data.Decimal
import           Data.List            hiding (span)
import           Data.Semigroup
import           Data.String
import           Text.Trifecta        (Spanned (..))
import qualified Text.Trifecta        as TF
import           Text.Trifecta.Delta  (bytes)

import           Pact.SExpParser
import           Pact.Types.Lang
import           Pact.Types.SExp

import           Prelude              hiding (span)


toBool :: Text -> Maybe Bool
toBool = \case
  "true"  -> Just True
  "false" -> Just False
  _       -> Nothing

-- | Marker for where we allow / disallow annotations to an expression. This
-- affects whether we try to pick up a trailing colon after an identifier. We
-- usually allow annotations, but not immediately inside curly braces, because
-- of objects.
data AllowAnnot = AllowAnnot | DontAllowAnnot
  deriving Eq

expr :: SExpProcessor PactExp
expr = expr' AllowAnnot

-- | Main parser for Pact expressions.
expr' :: AllowAnnot -> SExpProcessor PactExp
expr' allowAnnot = SExpProcessor $ \case

  -- lists
  List listTy inner :~ span : input -> retL span input inner $ case listTy of
    Paren  -> EList <$> many expr <*> pure IsntLiteralList
    Square -> EList
      <$> (expr `sepBy1` comma <|> many expr)
      <*> pure IsLiteralList

    Curly -> do
      ps <- pairs
      let (ops, kvs) = unzip ps
      if | all (== ":")  ops -> pure $ EObject  kvs
         | all (== ":=") ops -> pure $ EBinding kvs
         | otherwise -> fail $ "Mixed binding/object operators: " ++ show ops

  -- token patterns
  Token (Punctuation "'" NoTrailingSpace) :~ span1
    : Token (Ident ident' _) :~ span2
    : input
    -> ret (span1 <> span2) input $ ESymbol ident'

  Token (String str) :~ span : input
    -> ret span input $ ELiteral $ LString str
  Token (Ident bStr _) :~ span : input
    | Just b <- toBool bStr
    -> ret span input $ ELiteral $ LBool b
  Token (Number num) :~ span : input -> ret span input $ case num of
    Left i  -> ELiteral $ LInteger i
    Right d -> ELiteral $ LDecimal d

  Token (Ident a NoTrailingSpace) :~ span1
    : Token (Punctuation "." NoTrailingSpace) :~ span2
    : Token (Ident q _) :~ span3
    : input
    -> ret (span1 <> span2 <> span3) input $ EAtom a (Just q) Nothing

  Token (Ident a _) :~ span : input -> case input of
    Token (Punctuation ":" _) :~ _
      : (viewTy -> Just (sexps, input'))
      | allowAnnot == AllowAnnot
      -> ret span input' $ EAtom a Nothing (Just sexps)
    _ -> ret span input $ EAtom a Nothing Nothing

  _ -> Nothing

  where

    retL span input inner action = do
      ([], _span, result) <- unP action inner
      ret span input result

    ret span input exp = pure (input, Just span, exp (spanToParsed span))

    spanToParsed :: TF.Span -> Parsed
    spanToParsed (TF.Span start end _bs)
      = Parsed start $ fromIntegral $ bytes end - bytes start

    viewTy = \case
      t1@(Token (Ident objTab NoTrailingSpace) :~ _)
        : t2@(List Curly _ :~ _)
        : input
        | objTab == "object" || objTab == "table"
        -> Just ([t1, t2], input)
      t@(List _ _ :~ _)          : input -> Just ([t], input)
      t@(Token (Ident _ _) :~ _) : input -> Just ([t], input)
      _ -> Nothing


pairs :: SExpProcessor [(Text, (PactExp, PactExp))]
pairs =
  let p = do
        k  <- expr' DontAllowAnnot
        op <- punctuation ":=" <|> colon
        v  <- expr' DontAllowAnnot
        return (op, (k, v))
  in p `sepBy` comma

-- | Parse one or more Pact expressions.
exprs :: SExpProcessor [PactExp]
exprs = some expr

-- | Parse one or more Pact expressions and EOF.
-- Unnecessary with Atto's 'parseOnly'.
exprsOnly :: TF.Parser [PactExp]
exprsOnly = do
  sexps' <- sexps
  case runP exprs sexps' of
    ParsedOne pactExps [] -> pure $ extract pactExps
    ParsedOne _        xs -> TF.raiseErr $ TF.failed $ "leftover sexps from parse " ++ show xs
    NoParse               -> TF.raiseErr $ TF.failed "no parse"

-- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [PactExp]
parseExprs text = do
  sexps' <- AP.parseOnly sexps text
  case runP exprs sexps' of
    ParsedOne pactExps [] -> pure $ extract pactExps
    ParsedOne _        xs -> fail $ "leftover sexps from parse " ++ show xs
    NoParse               -> fail "no parse"

parseNumber :: Text -> Either String (Either Integer Decimal)
parseNumber text = AP.parseOnly (unSExpParser number) text

parseSExp :: Text -> Either String (Spanned SExp)
parseSExp txt =
  AP.parseOnly (unSExpParser $ TF.whiteSpace *> sexp <* TF.eof) txt

parseSExps :: Text -> Either String [SExp]
parseSExps txt = fmap extract <$>
  AP.parseOnly (unSExpParser $ TF.whiteSpace *> some sexp <* TF.eof) txt

_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp

_parseAccounts :: IO (TF.Result ([PactExp],String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
