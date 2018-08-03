{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module      :  Pact.Compile
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Parser to 'Exp's.
--

module Pact.Parse
    (
     exprs, exprsOnly
    ,parseExprs
    ,number
    ,PactParser(unPactParser)
    )

where

import Text.Trifecta as TF
import Text.Trifecta.Delta as TF
import Control.Applicative
import Data.List
import Control.Monad
import Prelude
import Data.String
import Data.Decimal
import qualified Data.Attoparsec.Text as AP
import Data.Char (digitToInt)

import Pact.Types.Lang
import Pact.Types.Parser

type P a = forall m. (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => PactParser m a


-- | Main parser for Pact expressions.
expr :: P Exp
expr = do
  delt <- position
  let inf = do
        end <- position
        let len = bytes end - bytes delt
        return $ Parsed delt (fromIntegral len)
  msum
    [ TF.try (ELiteral <$> token number <*> inf <?> "number")
    , ELiteral . LString <$> stringLiteral <*> inf <?> "string"
    , ELiteral <$> bool <*> inf <?> "bool"
    , ESymbol <$> (symbolic '\'' >> ident style) <*> inf <?> "symbol"
    , do
      a <- ident style
      msum
        [ TF.try $ do
            t <- typed
            i <- inf
            return (EAtom a Nothing (Just t) i) <?> "typed atom"
        , TF.try $ do
            q <- qualified
            i <- inf
            return (EAtom a (Just q) Nothing i) <?> "qual atom"
        , do
             i <- inf
             return (EAtom a Nothing Nothing i) <?> "bare atom"
        ]
    , EList <$> parens (many expr) <*> pure Nothing <*> inf <?> "sexp"
    , do
      is <- TF.try (brackets (many expr) <?> "space-delimited list literal") <|>
                   (brackets (expr `sepBy` comma) <?> "comma-delimited list literal")
      let lty = case nub (map expPrimTy is) of
                  [Just ty] -> ty
                  _ -> TyAny
      i <- inf
      return $ EList is (Just lty) i
    , do
      ps <- pairs
      let ops = map fst ps
          kvs = map snd ps
      if all (== ":") ops then EObject kvs <$> inf
      else if all (== ":=") ops then EBinding kvs <$> inf
           else unexpected $ "Mixed binding/object operators: " ++ show ops
    ]
{-# INLINE expr #-}

number :: P Literal
number = do
  -- Tricky: note that we use `char :: CharParsing m => Char -> m Char` rather
  -- than `symbolic :: TokenParsing m => Char -> m Char` here. We use the char
  -- parser because we want to disallow whitespace following the negative sign
  -- (token parsers apply `whiteSpace` after every token). With a whitespace we
  -- consider this an expression rather than a literal.
  neg <- maybe id (const negate) <$> optional (char '-')
  num <- some digit
  dec <- optional (dot *> some digit)
  let strToNum start = foldl' (\x d -> 10*x + toInteger (digitToInt d)) start
  case dec of
    Nothing -> return $ LInteger (neg (strToNum 0 num))
    Just d -> return $ LDecimal $ Decimal
              (fromIntegral (length d))
              (neg (strToNum (strToNum 0 num) d))
{-# INLINE number #-}

bool :: P Literal
bool = msum
  [ LBool True  <$ symbol "true"
  , LBool False <$ symbol "false"
  ]
{-# INLINE bool #-}

expPrimTy :: Exp -> Maybe (Type TypeName)
expPrimTy ELiteral {..} = Just $ TyPrim $ litToPrim _eLiteral
expPrimTy ESymbol {} = Just $ TyPrim TyString
expPrimTy _ = Nothing

typed :: P (Type TypeName)
typed = colon *> parseType

parseType :: P (Type TypeName)
parseType = msum
  [ brackets (TyList <$> parseType) <?> "typed list"
  , parseUserSchema
  , parseSchemaType tyObject TyObject
  , parseSchemaType tyTable TyTable
  , TyPrim TyInteger <$ tsymbol tyInteger
  , TyPrim TyDecimal <$ tsymbol tyDecimal
  , TyPrim TyTime    <$ tsymbol tyTime
  , TyPrim TyBool    <$ tsymbol tyBool
  , TyPrim TyString  <$ tsymbol tyString
  , TyList TyAny     <$ tsymbol tyList
  , TyPrim TyValue   <$ tsymbol tyValue
  , TyPrim TyKeySet  <$ tsymbol tyKeySet
  ]

tsymbol :: Text -> P String
tsymbol = symbol . unpack

parseUserSchema :: P (Type TypeName)
parseUserSchema = braces $ do
  t <- ident style
  return (TyUser (fromString t)) <?> "user type"

parseSchemaType :: Text -> SchemaType -> P (Type TypeName)
parseSchemaType tyRep sty =
  TF.try (TySchema sty <$> (tsymbol tyRep *> parseUserSchema)) <|>
  (tsymbol tyRep *> return (TySchema sty TyAny))

-- | Parse one or more Pact expressions.
exprs :: P [Exp]
exprs = some expr

-- | Parse one or more Pact expressions and EOF.
-- Unnecessary with Atto's 'parseOnly'.
exprsOnly :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m [Exp]
exprsOnly = unPactParser $ whiteSpace *> exprs <* TF.eof

pairs :: P [(String,(Exp,Exp))]
pairs =
    braces ((`sepBy` comma)
    (do
       k <- expr
       op <- symbol ":=" <|> symbol ":"
       v <- expr
       return (op,(k,v))
    )) <?> "object"

-- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [Exp]
parseExprs = AP.parseOnly (unPactParser (whiteSpace *> exprs))

_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp

_parseAccounts :: IO (Result ([Exp],String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
