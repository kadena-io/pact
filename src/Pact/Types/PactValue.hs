{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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

  -- | Helper functions for generating arbitrary pact values
  , PactValueGeneratorSize(..)
  , decreaseGenSize
  , genSomePactValue
  , genTerminatingPactValueGuard
  , genPactValueObjectMap
  , genPactValueList
  , genPactValueGuard
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics hiding (Meta)
import Test.QuickCheck

import Pact.Types.Exp
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.SizeOf
import Pact.Types.Term
import Pact.Types.Type (Type(TyAny))
import Pact.Types.Util (satisfiesRoundtripJSON)


-- | Determines how deep a generated PactValue _could_ be.
-- Restricts how many times a recursive PactValue constructor (i.e. PObject, PList, and PGuard)
-- can be called when creating a PactValue.
genSomePactValue :: PactValueGeneratorSize -> Gen PactValue
genSomePactValue TerminateFast = oneof
  [ PLiteral <$> arbitrary
  , PGuard <$> (oneof genTerminatingPactValueGuard) ]
genSomePactValue s = oneof
  [ PLiteral <$> arbitrary
  , PList <$> genPactValueList (decreaseGenSize s)
  , PObject <$> genPactValueObjectMap (decreaseGenSize s)
  , PGuard <$> genPactValueGuard (decreaseGenSize s) ]

data PactValueGeneratorSize = TerminateFast | RecurseOnce | RecurseTwice

decreaseGenSize :: PactValueGeneratorSize -> PactValueGeneratorSize
decreaseGenSize TerminateFast = TerminateFast -- lowest value
decreaseGenSize RecurseOnce = TerminateFast
decreaseGenSize RecurseTwice = RecurseOnce

genTerminatingPactValueGuard :: [Gen (Guard PactValue)]
genTerminatingPactValueGuard =
  [ GPact <$> arbitrary
  , GKeySet <$> arbitrary
  , GKeySetRef <$> arbitrary
  , GModule <$> arbitrary ]

genPactValueObjectMap :: PactValueGeneratorSize -> Gen (ObjectMap PactValue)
genPactValueObjectMap genSize = ObjectMap <$> M.fromList <$> genMap
  where genOneKeyValue = do
          f <- arbitrary :: Gen FieldKey
          pv <- genSomePactValue genSize
          pure (f, pv)
        genMap = listOf1 genOneKeyValue

genPactValueList :: PactValueGeneratorSize -> Gen (Vector PactValue)
genPactValueList genSize = V.fromList <$> listOf1 (genSomePactValue genSize)

genPactValueGuard :: PactValueGeneratorSize -> Gen (Guard PactValue)
genPactValueGuard genSize = oneof (genTerminatingPactValueGuard <> [GUser <$> genUserGuard])
  where genUserGuard = do
          args <- listOf1 (genSomePactValue genSize)
          fun <- arbitrary  -- TODO enforce that it's a non-native Name
          pure $ UserGuard fun args


data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard (Guard PactValue)
  deriving (Eq,Show,Generic,Ord)

instance Arbitrary PactValue where
  arbitrary = suchThat (genSomePactValue RecurseTwice) satisfiesRoundtripJSON

instance NFData PactValue

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

instance SizeOf PactValue where
  sizeOf (PLiteral l) = (constructorCost 1) + (sizeOf l)
  sizeOf (PList v) = (constructorCost 1) + (sizeOf v)
  sizeOf (PObject o) = (constructorCost 1) + (sizeOf o)
  sizeOf (PGuard g) = (constructorCost 1) + (sizeOf g)


-- | Strict conversion.
toPactValue :: Term Name -> Either Text PactValue
toPactValue (TLiteral l _) = pure $ PLiteral l
toPactValue (TObject (Object o _ _ _) _) = PObject <$> traverse toPactValue o
toPactValue (TList l _ _) = PList <$> V.mapM toPactValue l
toPactValue (TGuard x _) = PGuard <$> traverse toPactValue x
toPactValue t = Left $ "Unable to convert Term: " <> renderCompactText t

fromPactValue :: PactValue -> Term Name
fromPactValue (PLiteral l) = TLiteral l def
fromPactValue (PObject o) = TObject (Object (fmap fromPactValue o) TyAny def def) def
fromPactValue (PList l) = TList (fmap fromPactValue l) TyAny def
fromPactValue (PGuard x) = TGuard (fmap fromPactValue x) def

-- | Lenient conversion, implying that conversion back won't necc. succeed.
-- Integers are coerced to Decimal for simple representation.
-- Non-value types are turned into their String representation.
toPactValueLenient :: Term Name -> PactValue
toPactValueLenient t = case toPactValue t of
  Right (PLiteral (LInteger l)) -> PLiteral (LDecimal (fromIntegral l))
  Right v -> v
  Left _ -> PLiteral $ LString $ renderCompactText t
