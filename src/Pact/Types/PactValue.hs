{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.Term
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- 'PactValue' is a type for marshalling term values in Pact's
-- "front end", to address the issue where 'Term Name' cannot
-- be safely or meaningfully represented outside of the interpreter.
-- 'PactValue' is needed for reliable reproduction of receipt
-- results and continuation arguments in the SPV process.
--

module Pact.Types.PactValue
  ( PactValue(..)
  , toPactValue
  , toPactValueLenient
  , fromPactValue
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.Term (ObjectMap(..),Term(..),Name(..),Object(..),Guard(..), PList(..))
import Pact.Types.Type (Type(TyAny))


data PactValue
  = PactLiteral Literal
  | PactList (Vector PactValue)
  | PactObject (ObjectMap PactValue)
  | PactGuard Guard
  deriving (Eq,Show,Generic)

instance NFData PactValue

instance ToJSON PactValue where
  toJSON (PactLiteral l) = toJSON l
  toJSON (PactObject o) = toJSON o
  toJSON (PactList v) = toJSON v
  toJSON (PactGuard x) = toJSON x


instance FromJSON PactValue where
  parseJSON v = (PactLiteral <$> parseJSON v)
    <|> (PactList <$> parseJSON v)
    <|> (PactGuard <$> parseJSON v)
    <|> (PactObject <$> parseJSON v)

instance Pretty PactValue where
  pretty (PactLiteral l) = pretty l
  pretty (PactObject l) = pretty l
  pretty (PactList l) = pretty (V.toList l)
  pretty (PactGuard l) = pretty l

-- | Strict conversion.
toPactValue :: Term Name -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ PactLiteral l
toPactValue (TObject (Object o _ _ _)) = PactObject <$> traverse toPactValue o
toPactValue (TList (PList l _ _)) = PactList <$> V.mapM toPactValue l
toPactValue (TGuard g _) = pure (PactGuard g)
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (PactLiteral l) = TLiteral l def
fromPactValue (PactObject o) = TObject $ Object (fmap fromPactValue o) TyAny def def
fromPactValue (PactList l) = TList $ PList (fmap fromPactValue l) TyAny def
fromPactValue (PactGuard x) = TGuard x def

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (PactLiteral (LInteger l)) -> PactLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> PactLiteral $ LString $ renderCompactText t
