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
import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.Term (ObjectMap(..),Term(..),Name(..),Object(..),Guard(..))
import Pact.Types.Type (Type(TyAny))


data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard Guard
  deriving (Eq,Show)


instance ToJSON PactValue where
  toJSON (PLiteral l) = toJSON l
  toJSON (PObject o) = toJSON o
  toJSON (PList v) = toJSON v
  toJSON (PGuard x) = toJSON x


instance FromJSON PactValue where
  parseJSON v =
    (PLiteral <$> parseJSON v) <|>
    (PList <$> parseJSON v) <|>
    (PGuard <$> parseJSON v) <|>
    (PObject <$> parseJSON v)

instance Pretty PactValue where
  pretty (PLiteral l) = pretty l
  pretty (PObject l) = pretty l
  pretty (PList l) = pretty (V.toList l)
  pretty (PGuard l) = pretty l

-- | Strict conversion.
toPactValue :: Term Name -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ PLiteral l
toPactValue (TObject (Object o _ _ _) _) = PObject <$> traverse toPactValue o
toPactValue (TList l _ _) = PList <$> V.mapM toPactValue l
toPactValue (TGuard x _) = pure (PGuard x)
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (PLiteral l) = TLiteral l def
fromPactValue (PObject o) = TObject (Object (fmap fromPactValue o) TyAny def def) def
fromPactValue (PList l) = TList (fmap fromPactValue l) TyAny def
fromPactValue (PGuard x) = TGuard x def

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (PLiteral (LInteger l)) -> PLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> PLiteral $ LString $ renderCompactText t
