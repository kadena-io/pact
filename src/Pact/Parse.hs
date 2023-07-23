{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
    ,parsePact
    ,legacyParsePact
    )

where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Lens (Wrapped(..))
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Decimal
import Data.List
import Data.Serialize (Serialize)
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Data.Scientific
import GHC.Generics (Generic)
import Prelude
import Text.Trifecta as TF
import Text.Trifecta.Delta as TF

import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Parser
import Pact.Types.Pretty (Pretty(..),viaShow)
import Pact.Types.Info
import Pact.Types.Term (ToTerm)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- Expression Parser

-- | Main parser for Pact expressions.
expr :: DeltaParsing m => PactParser m (Exp Parsed)
expr = do
  delt <- position
  let inf = do
        end <- position
#ifdef LEGACY_PARSER
        let len = bytes end - bytes delt
#else
        let len = column end - column delt
#endif
        return $! Parsed delt (fromIntegral len)
      separator t s = symbol t >> (ESeparator . SeparatorExp s <$> inf)
  msum
    [ TF.try (ELiteral <$> (LiteralExp <$> token number <*> inf)) <?> "number"
    , ELiteral <$> (LiteralExp . LString <$> stringLiteral <*> inf) <?> "string"
    , ELiteral <$> (LiteralExp . LString <$> (symbolic '\'' >> ident style) <*> inf) <?> "symbol"
    , ELiteral <$> (LiteralExp <$> bool <*> inf) <?> "bool"
    , TF.try (dynamicAtom >>= \(a,qs) -> EAtom . AtomExp a qs True <$> inf) <?> "dyn-atom"
    , (qualifiedAtom >>= \(a,qs) -> EAtom . AtomExp a qs False <$> inf) <?> "atom"
    , EList <$> (ListExp <$> parens (many expr) <*> pure Parens <*> inf) <?> "(list)"
    , EList <$> (ListExp <$> braces (many expr) <*> pure Braces <*> inf) <?> "[list]"
    , EList <$> (ListExp <$> brackets (many expr) <*> pure Brackets <*> inf) <?> "{list}"
    , separator ":=" ColonEquals
    , separator ":" Colon
    , separator "," Comma
    ]
{-# INLINE expr #-}

number :: DeltaParsing m => PactParser m Literal
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
         then unexpected $ "decimal precision overflow (255 max): " ++ show num ++ "." ++ show d
         else return $ LDecimal $ Decimal
           (fromIntegral precision)
           (neg (strToNum (strToNum 0 num) d))
{-# INLINE number #-}


qualifiedAtom :: (Monad p, TokenParsing p) => p (Text,[Text])
qualifiedAtom = ident style `sepBy` dot >>= \as -> case reverse as of
  [] -> unexpected "qualifiedAtom"
  (a:qs) -> return (a,reverse qs)

dynamicAtom :: (Monad p, TokenParsing p) => p (Text,[Text])
dynamicAtom = do
  ref <- ident style
  void $ colon >> colon
  var <- ident style
  return (var,[ref])

bool :: DeltaParsing m => PactParser m Literal
bool = msum
  [ LBool True  <$ symbol "true"
  , LBool False <$ symbol "false"
  ]
{-# INLINE bool #-}


-- | Parse one or more Pact expressions.
exprs :: DeltaParsing m => PactParser m [Exp Parsed]
exprs = some expr

-- | Parse one or more Pact expressions and EOF.
exprsOnly :: DeltaParsing m => m [Exp Parsed]
exprsOnly = unPactParser $ whiteSpace *> exprs <* TF.eof

-- | JSON serialization for 'readDecimal' and public meta info;
-- accepts both a String version (parsed as a Pact decimal) or
-- a Number.
newtype ParsedDecimal = ParsedDecimal Decimal
  deriving (Eq,Ord,Num,Real,RealFrac,Fractional,Generic,NFData,Serialize,ToTerm)

instance A.FromJSON ParsedDecimal where
  parseJSON (A.String s) =
    ParsedDecimal <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v

instance J.Encode ParsedDecimal where
  build (ParsedDecimal d) = J.build $ J.Aeson @Scientific $ fromRational $ toRational d
  {-# INLINE build #-}

instance Show ParsedDecimal where
  show (ParsedDecimal d) = show d

instance Pretty ParsedDecimal where
  pretty (ParsedDecimal d) = viaShow d

instance Arbitrary ParsedDecimal where
  arbitrary = ParsedDecimal <$> arbitrary

instance Wrapped ParsedDecimal

-- | JSON serialization for 'readInteger' and public meta info;
-- accepts both a String version (parsed as a Pact integer),
-- a Number, or a PactValue { "int": ... } integer
newtype ParsedInteger = ParsedInteger Integer
  deriving (Eq,Show,Ord,Num,Real,Enum,Integral,Generic,NFData,Serialize,ToTerm,Pretty)

instance A.FromJSON ParsedInteger where
  parseJSON (A.String s) =
    ParsedInteger <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedInteger (round n)
  parseJSON v@A.Object{} = A.parseJSON v >>= \i -> case i of
    PLiteral (LInteger li) -> return $ ParsedInteger li
    _ -> fail $ "Failure parsing integer PactValue object: " ++ show i
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

instance J.Encode ParsedInteger where
  build (ParsedInteger i) = J.build $ J.Aeson i
  {-# INLINE build #-}

instance Arbitrary ParsedInteger where
  arbitrary = ParsedInteger <$> arbitrary

instance Wrapped ParsedInteger

-- -------------------------------------------------------------------------- --
-- Top Level Parsers

-- | "Production" parser: atto, parse multiple exprs.
parseExprs :: Text -> Either String [Exp Parsed]
parseExprs = pactAttoParseOnly (unPactParser (whiteSpace *> exprs <* TF.eof))
{-# INLINABLE parseExprs #-}

-- | Legacy version of "production" parser: atto, parse multiple exprs. This
-- parser does not force EOF and thus accepts trailing inputs that are not valid
-- pact code.
legacyParseExprs :: Text -> Either String [Exp Parsed]
legacyParseExprs = pactAttoParseOnly (unPactParser (whiteSpace *> exprs))
{-# INLINABLE legacyParseExprs #-}

-- | ParsedCode version of 'parseExprs'
parsePact :: Text -> Either String ParsedCode
parsePact code = ParsedCode code <$> parseExprs code
{-# INLINABLE parsePact #-}

-- | Legacy version of the production parser. This parser does not force EOF and
-- thus accepts trailing inputs that are not valid pact code.
legacyParsePact :: Text -> Either String ParsedCode
legacyParsePact code = ParsedCode code <$> legacyParseExprs code
{-# INLINABLE legacyParsePact #-}

_parseF :: TF.Parser a -> FilePath -> IO (TF.Result (a,String))
_parseF p fp = do
  bs <- BS.readFile fp
  let s = unpack $ decodeUtf8 bs
  fmap (,s) <$> TF.parseFromFileEx p fp

_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty

_parseAccounts :: IO (Result ([Exp Parsed], String))
_parseAccounts = _parseF exprsOnly "examples/accounts/accounts.pact"
