{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


-- |
-- Module      :  Pact.Types.PactValue
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
  , elideModRefInfo
  , _PLiteral
  , _PList
  , _PGuard
  , _PObject
  , _PModRef
  , stripPactValueInfo
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Lens (makePrisms,set)
import Data.Aeson hiding (Value(..))
import qualified Data.Aeson as A
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics hiding (Meta)

import Pact.Types.Exp
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.SizeOf
import Pact.Types.Term
import Pact.Types.Type (Type(TyAny))
import Pact.Types.Util (enableToJSON)

import qualified Pact.JSON.Encode as J

data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard (Guard PactValue)
  | PModRef ModRef
  deriving (Eq,Show,Generic,Ord)

instance NFData PactValue

instance ToJSON PactValue where
  toJSON = enableToJSON "Pact.Types.PactValue.PactValue" . \case
    (PLiteral l) -> toJSON l
    (PObject o) -> toJSON o
    (PList v) -> toJSON v
    (PGuard x) -> toJSON x
    (PModRef m) -> A.Object $ modRefProperties_ m
      -- this uses a non-standard alternative JSON encoding for 'ModRef'

  toEncoding (PLiteral l) = toEncoding l
  toEncoding (PObject o) = toEncoding o
  toEncoding (PList v) = toEncoding v
  toEncoding (PGuard x) = toEncoding x
  toEncoding (PModRef m) = pairs $ modRefProperties_ m
    -- this uses a non-standard alternative JSON encoding for 'ModRef'

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode PactValue where
  build (PLiteral l) = J.build l
  build (PObject o) = J.build o
  build (PList v) = J.build (J.Array v)
  build (PGuard x) = J.build x
  build (PModRef m) = J.object $ modRefKeyValues_ m
    -- this uses a non-standard alternative JSON encoding for 'ModRef'
  {-# INLINE build #-}

instance FromJSON PactValue where
  parseJSON v =
    (PLiteral <$> parseJSON v) <|>
    (PList <$> parseJSON v) <|>
    (PGuard <$> parseJSON v) <|>
    (PModRef <$> (parseNoInfo v <|> parseJSON v)) <|>
    (PObject <$> parseJSON v)
    where
      parseNoInfo = withObject "ModRef" $ \o -> ModRef
        <$> o .: "refName"
        <*> o .: "refSpec"
        <*> (fromMaybe def <$> o .:? "refInfo")

instance Pretty PactValue where
  pretty (PLiteral l) = pretty l
  pretty (PObject l) = pretty l
  pretty (PList l) = pretty (V.toList l)
  pretty (PGuard l) = pretty l
  pretty (PModRef m) = pretty m

instance SizeOf PactValue where
  sizeOf ver (PLiteral l) = (constructorCost 1) + (sizeOf ver l)
  sizeOf ver (PList v) = (constructorCost 1) + (sizeOf ver v)
  sizeOf ver (PObject o) = (constructorCost 1) + (sizeOf ver o)
  sizeOf ver (PGuard g) = (constructorCost 1) + (sizeOf ver g)
  sizeOf ver (PModRef m) = (constructorCost 1) + (sizeOf ver m)

-- | Strict conversion.
toPactValue :: Pretty n => Term n -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ PLiteral l
toPactValue (TObject (Object o _ _ _) _) = PObject <$> traverse toPactValue o
toPactValue (TList l _ _) = PList <$> V.mapM toPactValue l
toPactValue (TGuard x _) = PGuard <$> traverse toPactValue x
toPactValue (TModRef m _) = pure $ PModRef m
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (PLiteral l) = TLiteral l def
fromPactValue (PObject o) = TObject (Object (fmap fromPactValue o) TyAny def def) def
fromPactValue (PList l) = TList (fmap fromPactValue l) TyAny def
fromPactValue (PGuard x) = TGuard (fmap fromPactValue x) def
fromPactValue (PModRef r) = TModRef r def

elideModRefInfo :: PactValue -> PactValue
elideModRefInfo (PModRef m) = PModRef (set modRefInfo def m)
elideModRefInfo p = p


stripPactValueInfo :: PactValue -> PactValue
stripPactValueInfo = \case
  PLiteral lit -> PLiteral lit
  PList vec -> PList (stripPactValueInfo <$> vec)
  PObject om -> PObject (stripPactValueInfo <$> om)
  PGuard gu -> PGuard gu
  PModRef mr -> PModRef mr{_modRefInfo = def }

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (PLiteral (LInteger l)) -> PLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> PLiteral $ LString $ renderCompactText t

makePrisms ''PactValue
