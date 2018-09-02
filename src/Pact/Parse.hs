{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
    ,parseExprs,parseExprsMP
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
import Data.Decimal
import qualified Data.Attoparsec.Text as AP
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Parsers as MPP
import Data.Char (digitToInt)
import Data.Text (Text)

import Pact.Types.Exp
import Pact.Types.Parser
import Pact.Types.Info

type P a = forall m. (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => PactParser m a


-- | Main parser for Pact expressions.
expr :: P (Exp Parsed)
expr = do
  delt <- position
  let inf = do
        end <- position
        let len = bytes end - bytes delt
        return $ Parsed delt (fromIntegral len)
      separator t s = symbol t >> ((ESeparator . SeparatorExp s) <$> inf)
  msum
    [ TF.try (ELiteral <$> (LiteralExp <$> token number <*> inf)) <?> "number"
    , ELiteral <$> (LiteralExp . LString <$> stringLiteral <*> inf) <?> "string"
    , ELiteral <$> (LiteralExp . LString <$> (symbolic '\'' >> ident style) <*> inf) <?> "symbol"
    , ELiteral <$> (LiteralExp <$> bool <*> inf) <?> "bool"
    , (qualifiedAtom >>= \(a,qs) -> (EAtom . AtomExp a qs) <$> inf) <?> "atom"
    , EList <$> (ListExp <$> parens (many expr) <*> pure Parens <*> inf) <?> "(list)"
    , EList <$> (ListExp <$> braces (many expr) <*> pure Braces <*> inf) <?> "[list]"
    , EList <$> (ListExp <$> brackets (many expr) <*> pure Brackets <*> inf) <?> "{list}"
    , separator ":=" ColonEquals
    , separator ":" Colon
    , separator "," Comma
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


qualifiedAtom :: (Monad p, TokenParsing p) => p (Text,[Text])
qualifiedAtom = ident style `sepBy` dot >>= \as -> case reverse as of
  [] -> fail "qualifiedAtom"
  (a:qs) -> return (a,reverse qs)

bool :: P Literal
bool = msum
  [ LBool True  <$ symbol "true"
  , LBool False <$ symbol "false"
  ]
{-# INLINE bool #-}


-- | Parse one or more Pact expressions.
exprs :: P [(Exp Parsed)]
exprs = some expr

-- | Parse one or more Pact expressions and EOF.
-- Unnecessary with Atto's 'parseOnly'.
exprsOnly :: (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => m [(Exp Parsed)]
exprsOnly = unPactParser $ whiteSpace *> exprs <* TF.eof

-- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [(Exp Parsed)]
parseExprs = AP.parseOnly (unPactParser (whiteSpace *> exprs))

instance Ord e => DeltaParsing (MPP.ParsecT e Text m) where
    line = return mempty
    position = return $ Columns 0 0
    slicedWith f a = (`f` mempty) <$> a
    rend = return mempty
    restOfLine = return mempty
-- | megaparsec version
parseExprsMP :: Monad m => Text -> m (Either (MP.ParseError (MP.Token Text) ()) [Exp Parsed])
parseExprsMP = MP.runParserT (MPP.unParsecT (unPactParser (whiteSpace *> exprs))) ""


_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp


_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty


_parseAccounts :: IO (Result ([(Exp Parsed)],String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
