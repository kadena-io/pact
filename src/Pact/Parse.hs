{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
     expr,exprs,exprsOnly
    ,parseExprs
    ,number
    )

where

import Text.Trifecta as TF hiding (spaces)
import Text.Trifecta.Delta as TF
import Control.Applicative
import Data.List
import Control.Monad
import Prelude hiding (exp)
import Data.String
import qualified Data.HashSet as HS
import Text.Parser.Token.Highlight
import Data.Decimal
import qualified Data.Attoparsec.Text as AP
import Data.Char (digitToInt)

import Pact.Types.Lang


symbols :: CharParsing m => m Char
symbols = oneOf "%#+-_&$@<>=^?*!|/"

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "atom"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        (HS.fromList ["true","false"])
        Symbol
        ReservedIdentifier


-- | Main parser for Pact expressions.
expr :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m Exp
expr = do
  delt <- position
  let inf = do
        end <- position
        let len = bytes end - bytes delt
        return $ Parsed delt (fromIntegral len)
  TF.try (ELiteral <$> token number <*> inf <?> "number")
   <|>
   (ELiteral . LString <$> stringLiteral <*> inf <?> "string")
   <|>
   (ELiteral <$> bool <*> inf <?> "bool")
   <|>
   (ESymbol <$> (char '\'' >> ident style) <*> inf <?> "symbol")
   <|>
   do
     a <- ident style
     TF.try (typed >>= \t -> inf >>= \i -> return (EAtom a Nothing (Just t) i) <?> "typed atom") <|>
       TF.try (qualified >>= \q -> inf >>= \i -> return (EAtom a (Just q) Nothing i) <?> "qual atom") <|>
       (inf >>= \i -> return (EAtom a Nothing Nothing i) <?> "bare atom")
   <|>
   (EList <$> parens (sepBy expr spaces) <*> pure Nothing <*> inf <?> "sexp")
   <|>
   do
     is <- brackets (sepBy expr spaces) <?> "list literal"
     let lty = case nub (map expPrimTy is) of
                 [Just ty] -> ty
                 _ -> TyAny
     i <- inf
     return $ EList is (Just lty) i
   <|> do
     ps <- pairs
     let ops = map fst ps
         kvs = map snd ps
     if all (== ":") ops then EObject kvs <$> inf
     else if all (== ":=") ops then EBinding kvs <$> inf
          else unexpected $ "Mixed binding/object operators: " ++ show ops

number :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m Literal
number = do
  neg <- maybe id (const negate) <$> optional (char '-')
  num <- some digit
  dec <- optional (char '.' *> some digit)
  let strToNum start = foldl' (\x d -> 10*x + toInteger (digitToInt d)) start
  case dec of
    Nothing -> return $ LInteger (neg (strToNum 0 num))
    Just d -> return $ LDecimal $ Decimal
              (fromIntegral (length d))
              (neg (strToNum (strToNum 0 num) d))
{-# INLINE number #-}

bool :: TokenParsing m => m Literal
bool = symbol "true" *> pure (LBool True) <|> symbol "false" *> pure (LBool False)
{-# INLINE bool #-}

expPrimTy :: Exp -> Maybe (Type TypeName)
expPrimTy ELiteral {..} = Just $ TyPrim $ litToPrim _eLiteral
expPrimTy ESymbol {} = Just $ TyPrim TyString
expPrimTy _ = Nothing

qualified :: (Monad m,TokenParsing m) => m Text
qualified = char '.' *> ident style

typed :: (Monad m,TokenParsing m) => m (Type TypeName)
typed = do
  _ <- char ':'
  spaces
  parseType

parseType :: (Monad m,TokenParsing m) => m (Type TypeName)
parseType =
  (char '[' >> parseType >>= \t -> char ']' >> return (TyList t) <?> "typed list") <|>
  parseUserSchema <|>
  tsymbol tyInteger *> return (TyPrim TyInteger) <|>
  tsymbol tyDecimal *> return (TyPrim TyDecimal) <|>
  tsymbol tyTime *> return (TyPrim TyTime) <|>
  tsymbol tyBool *> return (TyPrim TyBool) <|>
  tsymbol tyString *> return (TyPrim TyString) <|>
  tsymbol tyList *> return (TyList TyAny) <|>
  parseSchemaType tyObject TyObject <|>
  tsymbol tyValue *> return (TyPrim TyValue) <|>
  tsymbol tyKeySet *> return (TyPrim TyKeySet) <|>
  parseSchemaType tyTable TyTable

tsymbol :: TokenParsing m => Text -> m String
tsymbol = symbol . unpack

parseUserSchema :: (Monad m,TokenParsing m) => m (Type TypeName)
parseUserSchema = char '{' >> ident style >>= \t -> char '}' >> return (TyUser (fromString t)) <?> "user type"

parseSchemaType :: (Monad m,TokenParsing m) => Text -> SchemaType -> m (Type TypeName)
parseSchemaType tyRep sty =
  TF.try (TySchema sty <$> (tsymbol tyRep *> parseUserSchema)) <|>
  (tsymbol tyRep *> return (TySchema sty TyAny))



-- | Skip spaces or one-line comments
spaces :: CharParsing m => m ()
spaces = skipMany (skipSome space <|> oneLineComment)
    where oneLineComment = TF.try (string ";") *> skipMany (satisfy (/= '\n')) <?> "comment"
{-# INLINE spaces #-}
--space <?> "white space"


{-# INLINE expr #-}

-- | Parse one or more Pact expressions.
exprs :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m [Exp]
exprs = some (spaces *> expr <* spaces)

-- | Parse one or more Pact expressions and EOF.
-- Unnecessary with Atto's 'parseOnly'.
exprsOnly :: Parser [Exp]
exprsOnly = exprs <* TF.eof

pairs :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) =>
         m [(String,(Exp,Exp))]
pairs =
    braces ((`sepBy` char ',')
    (do
       spaces
       k <- expr
       spaces
       op <- symbol ":=" <|> symbol ":"
       spaces
       v <- expr
       spaces
       return (op,(k,v))
    )) <?> "object"


-- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [Exp]
parseExprs = AP.parseOnly exprs


_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp

_parseAccounts :: IO (Result ([Exp],String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
