{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
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
import           Data.Foldable        (asum)
import           Data.List            hiding (span)
import           Data.String
import qualified Data.Text            as T
import           Text.Trifecta        (Spanned (..))
import qualified Text.Trifecta        as TF

import           Pact.SExpParser
import           Pact.Types.Lang
import           Pact.Types.SExp

import           Prelude              hiding (span)


-- | Main parser for Pact expressions.
expr :: SExpProcessor PactExp
expr =
  let doSymbol = liftParsed $
        punctuationNoTrailing "'" >> ESymbol <$> prism _Ident
      doStr = liftParsed $ ELiteral . LString <$> prism _String
      doTyped = liftParsed $ do
        a <- prism _Ident
        EAtom a Nothing . Just <$> typed
      doQualified = liftParsed $ do
        a <- prism _IdentNoTrailing
        _ <- punctuationNoTrailing "."
        q <- prism _Ident
        pure $ EAtom a (Just q) Nothing
      doBareAtom = liftParsed $ do
        a <- prism _Ident
        pure $ EAtom a Nothing Nothing
      doList lit exps = liftParsed $ do
        exps' <- exps
        let lty = case nub (map expPrimTy exps') of
              [Just ty] -> ty
              _         -> TyAny
        pure $ EList exps' (lit lty)
      doPairs = liftParsed $ do
        ps <- pairs
        let (ops, kvs) = unzip ps
        if | all ((== Just ":")  . puncText) ops -> pure $ EObject  kvs
           | all ((== Just ":=") . puncText) ops -> pure $ EBinding kvs
           | otherwise -> fail $ "Mixed binding/object operators: " ++ show ops
      doBool = liftParsed $ do
        Ident tok _ <- ident "true" <|> ident "false"
        let b = case tok of
              "true"  -> True
              "false" -> False
              _       -> error "this case is vacuous"
        pure $ ELiteral (LBool b)
      doNum = liftParsed $ do
        num <- prism _Number
        pure $ case num of
          Left i  -> ELiteral (LInteger i)
          Right d -> ELiteral (LDecimal d)
  in asum
     [ doSymbol
     , doStr
     , doBool
     , doNum
     , doTyped
     , doQualified
     , doBareAtom
     , doList (const IsntLiteralList) $ list Paren  $ many expr
     , doList IsLiteralList           $ list Square $ many expr
     , doList IsLiteralList           $ list Square $ expr `sepBy` comma
     , doPairs
     ]
{-# INLINE expr #-}

pairs :: SExpProcessor [(Token, (PactExp, PactExp))]
pairs = list Curly $
  let p = do
        k  <- expr
        op <- punctuation ":=" <|> colon
        v  <- expr
        return (op, (k, v))
  in p `sepBy` comma

expPrimTy :: PactExp -> Maybe (Type TypeName)
expPrimTy ELiteral {..} = Just $ TyPrim $ litToPrim _eLiteral
expPrimTy ESymbol {}    = Just $ TyPrim TyString
expPrimTy _             = Nothing

typed :: SExpProcessor (Type TypeName)
typed = colon *> parseType

parseType :: SExpProcessor (Type TypeName)
parseType = asum
  [ do
       ls <- list Square $ many parseType
       case ls of
         [l] -> pure $ TyList l
         _   -> fail "expected type"
  , parseUserSchema
  , parseSchemaType tyObject TyObject
  , parseSchemaType tyTable TyTable
  , TyPrim TyInteger <$ ident tyInteger
  , TyPrim TyDecimal <$ ident tyDecimal
  , TyPrim TyTime    <$ ident tyTime
  , TyPrim TyBool    <$ ident tyBool
  , TyPrim TyString  <$ ident tyString
  , TyList TyAny     <$ ident tyList
  , TyPrim TyValue   <$ ident tyValue
  , TyPrim TyKeySet  <$ ident tyKeySet
  ]

parseUserSchema :: SExpProcessor (Type TypeName)
parseUserSchema = do
  -- TODO: one?
  ts <- list Curly $ some $ prism _Ident
  case ts of
    [t] -> return $ TyUser $ fromString $ T.unpack t
    _   -> fail "expected user type"

parseSchemaType :: Text -> SchemaType -> SExpProcessor (Type TypeName)
parseSchemaType tyRep sty = asum
  [ ident tyRep >> TySchema sty <$> parseUserSchema
  , TySchema sty TyAny <$ ident tyRep
  ]

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
