{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    ,ParsedInteger(..),ParsedDecimal(..)
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
import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)

import Pact.Types.Exp
import Pact.Types.Parser
import Pact.Types.Info



-- | Main parser for Pact expressions.
expr :: (Monad m, TokenParsing m, DeltaParsing m) => PactParser m (Exp Parsed)
expr = do
  delt <- position
  let inf = do
        end <- position
        let len = bytes end - bytes delt
        return $ Parsed delt (fromIntegral len)
      separator t s = symbol t >> (ESeparator . SeparatorExp s <$> inf)
  msum
    [ TF.try (ELiteral <$> (LiteralExp <$> token number <*> inf)) <?> "number"
    , ELiteral <$> (LiteralExp . LString <$> stringLiteral <*> inf) <?> "string"
    , ELiteral <$> (LiteralExp . LString <$> (symbolic '\'' >> ident style) <*> inf) <?> "symbol"
    , ELiteral <$> (LiteralExp <$> bool <*> inf) <?> "bool"
    , (qualifiedAtom >>= \(a,qs) -> EAtom . AtomExp a qs <$> inf) <?> "atom"
    , EList <$> (ListExp <$> parens (many expr) <*> pure Parens <*> inf) <?> "(list)"
    , EList <$> (ListExp <$> braces (many expr) <*> pure Braces <*> inf) <?> "[list]"
    , EList <$> (ListExp <$> brackets (many expr) <*> pure Brackets <*> inf) <?> "{list}"
    , separator ":=" ColonEquals
    , separator ":" Colon
    , separator "," Comma
    ]
{-# INLINE expr #-}

-- TODO As number parsing is a solved problem, consider something like:
-- http://hackage.haskell.org/package/megaparsec-7.0.0/docs/Text-Megaparsec-Char-Lexer.html#v:float
number :: (Monad m, TokenParsing m, DeltaParsing m) => PactParser m Literal
number = do
  -- Tricky: note that we use `char :: CharParsing m => Char -> m Char` rather
  -- than `symbolic :: TokenParsing m => Char -> m Char` here. We use the char
  -- parser because we want to disallow whitespace following the negative sign
  -- (token parsers apply `whiteSpace` after every token). With a whitespace we
  -- consider this an expression rather than a literal.
  neg <- maybe id (const negate) <$> optional (char '-')
  num <- some digit
  dec <- optional (dot *> some digit)
  let strToNum = foldl' (\x d -> 10*x + toInteger (digitToInt d))
  case dec of
    Nothing -> return $ LInteger (neg (strToNum 0 num))
    Just d ->
      let precision = length d
      in if precision > 255
         then fail $ "decimal precision overflow (255 max): " ++ show num ++ "." ++ show d
         else return $ LDecimal $ Decimal
           (fromIntegral precision)
           (neg (strToNum (strToNum 0 num) d))
{-# INLINE number #-}


qualifiedAtom :: (Monad p, TokenParsing p) => p (Text,[Text])
qualifiedAtom = ident style `sepBy` dot >>= \as -> case reverse as of
  [] -> fail "qualifiedAtom"
  (a:qs) -> return (a,reverse qs)

bool :: (Monad m, DeltaParsing m) => PactParser m Literal
bool = msum
  [ LBool True  <$ symbol "true"
  , LBool False <$ symbol "false"
  ]
{-# INLINE bool #-}


-- | Parse one or more Pact expressions.
exprs :: (TokenParsing m, DeltaParsing m) => PactParser m [Exp Parsed]
exprs = some expr

-- | Parse one or more Pact expressions and EOF.
-- Unnecessary with Atto's 'parseOnly'.
exprsOnly :: (Monad m, TokenParsing m, DeltaParsing m) => m [Exp Parsed]
exprsOnly = unPactParser $ whiteSpace *> exprs <* TF.eof



-- | JSON serialization for 'readDecimal' and public meta info;
-- accepts both a String version (parsed as a Pact decimal) or
-- a Number.
newtype ParsedDecimal = ParsedDecimal Decimal
  deriving (Eq,Show,Ord,Num,Real,Fractional,Generic,NFData,Serialize)
instance A.FromJSON ParsedDecimal where
  parseJSON (A.String s) =
    ParsedDecimal <$> case AP.parseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v
instance A.ToJSON ParsedDecimal where
  toJSON (ParsedDecimal d) = A.Number $ fromRational $ toRational d


-- | JSON serialization for 'readInteger' and public meta info;
-- accepts both a String version (parsed as a Pact integer) or
-- a Number.
newtype ParsedInteger = ParsedInteger Integer
  deriving (Eq,Show,Ord,Num,Real,Enum,Integral,Generic,NFData,Serialize)
instance A.FromJSON ParsedInteger where
  parseJSON (A.String s) =
    ParsedInteger <$> case AP.parseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedInteger (round n)
  parseJSON v = fail $ "Failure parsing integer: " ++ show v
instance A.ToJSON ParsedInteger where
  toJSON (ParsedInteger i) = A.Number (fromIntegral i)


-- | "Production" parser: atto, parse multiple exps.
parseExprs :: Text -> Either String [Exp Parsed]
parseExprs = AP.parseOnly (unPactParser (whiteSpace *> exprs))

_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx p fp


_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty

_parseAccounts :: IO (Result ([Exp Parsed], String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
