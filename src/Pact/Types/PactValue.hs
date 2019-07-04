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
  = VLiteral Literal
  | VList (Vector PactValue)
  | VObject (ObjectMap PactValue)
  | VGuard Guard
  deriving (Eq,Show,Generic)

instance NFData PactValue

instance ToJSON PactValue where
  toJSON (VLiteral l) = toJSON l
  toJSON (VObject o) = toJSON o
  toJSON (VList v) = toJSON v
  toJSON (VGuard x) = toJSON x


instance FromJSON PactValue where
  parseJSON v = (VLiteral <$> parseJSON v)
    <|> (VList <$> parseJSON v)
    <|> (VGuard <$> parseJSON v)
    <|> (VObject <$> parseJSON v)

instance Pretty PactValue where
  pretty (VLiteral l) = pretty l
  pretty (VObject l) = pretty l
  pretty (VList l) = pretty (V.toList l)
  pretty (VGuard l) = pretty l

-- | Strict conversion.
toPactValue :: Term Name -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ VLiteral l
toPactValue (TObject (Object o _ _ _)) = VObject <$> traverse toPactValue o
toPactValue (TList (PList l _ _)) = VList <$> V.mapM toPactValue l
toPactValue (TGuard g _) = pure (VGuard g)
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (VLiteral l) = TLiteral l def
fromPactValue (VObject o) = TObject $ Object (fmap fromPactValue o) TyAny def def
fromPactValue (VList l) = TList $ PList (fmap fromPactValue l) TyAny def
fromPactValue (VGuard x) = TGuard x def

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (VLiteral (LInteger l)) -> VLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> VLiteral $ LString $ renderCompactText t
