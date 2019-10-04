{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

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
  , SizeOf(..)
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import Data.Aeson hiding (Value(..))
import Data.Default (def)
import Data.Text (Text)
import Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import qualified Data.Map.Strict as M
import Data.Decimal
import Data.Thyme hiding (Vector)
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as BS
import Text.Trifecta.Delta (Delta(..))
import GHC.Integer.GMP.Internals (Integer(..),BigNat(..))
import qualified Data.Primitive.ByteArray as BA
import GHC.Exts (Int(..))
import Data.Word (Word8)
import Data.Int (Int64)
import Control.Lens (view)

import Pact.Types.Exp (Literal(..))
import Pact.Types.Pretty (Pretty(..),pretty,renderCompactText)
import Pact.Types.Term
import Pact.Types.Type (Type(TyAny))
import Pact.Types.Info (Info(..),Code(..),Parsed(..))


data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard (Guard PactValue)
  deriving (Eq,Show,Generic,Ord)

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



-- |  Estimate of number of bytes needed to represent data type
--
-- Assumptions: GHC, 64-bit machine
-- General approach:
--   Memory Consumption = Constructor Header Size + Cost of Constructor Field(s)
--   Cost of Constructor Field(s)* = 1 word per field + cost of each field's value
-- (*) See Resource 2 for exceptions to these rules
-- Resources:
-- 1. http://wiki.haskell.org/GHC/Memory_Footprint
-- 2. https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types

class SizeOf t where
  sizeOf :: t -> Bytes


type Bytes = Int64

-- TOOD Figure out how to detect machine arch
-- | "word" is 4 bytes on 32-bit arch, but 8 bytes on 64-bit
wordSize32, wordSize64, wordSize :: Bytes
wordSize32 = 4
wordSize64 = 8
wordSize = wordSize64

-- | Constructor header is 1 word
headerCost :: Bytes
headerCost = 1 * wordSize

-- | In general, each constructor field costs 1 word
constructorFieldCost :: Int64 -> Bytes
constructorFieldCost numFields = numFields * wordSize

-- | Total cost for constructor
constructorCost :: Int64 -> Bytes
constructorCost numFields = headerCost + (constructorFieldCost numFields)


instance SizeOf PactValue where
  sizeOf (PLiteral l) = (constructorCost 1) + (sizeOf l)
  sizeOf (PList v) = (constructorCost 1) + (sizeOf v)
  sizeOf (PObject o) = (constructorCost 1) + (sizeOf o)
  sizeOf (PGuard g) = (constructorCost 1) + (sizeOf g)

instance SizeOf Literal where
  sizeOf (LString t) = (constructorCost 1) + (sizeOf t)
  sizeOf (LInteger i) = (constructorCost 1) + (sizeOf i)
  sizeOf (LDecimal d) = (constructorCost 1) + (sizeOf d)
  sizeOf (LBool _) = (constructorCost 1) + 0
  sizeOf (LTime ti) = (constructorCost 1) + (sizeOf ti)

instance (SizeOf v) => SizeOf (Vector v) where
  sizeOf v = vectorSize
    where
      vectorSize =
        ((7 + vectorLength) * wordSize) + sizeOfContents
      vectorLength = fromIntegral (V.length v)
      sizeOfContents = V.foldl' (\acc pv -> acc + (sizeOf pv)) 0 v

instance (SizeOf m) => SizeOf (ObjectMap m) where
  -- newtype is free
  sizeOf (ObjectMap m) = sizeOf m    

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf m = mapSize
    where
      mapSize = (6 * mapLength * wordSize) + sizeOfKeys + sizeOfValues
      mapLength = fromIntegral (M.size m)
      sizeOfValues = M.foldl' (\acc pv -> acc + (sizeOf pv)) 0 m
      sizeOfKeys = M.foldlWithKey' (\acc fk _ -> acc + (sizeOf fk)) 0 m

instance (SizeOf p) => SizeOf (Guard p) where
  sizeOf (GPact pg) = (constructorCost 1) + (sizeOf pg)
  sizeOf (GKeySet ks) = (constructorCost 1) + (sizeOf ks)
  sizeOf (GKeySetRef ksr) = (constructorCost 1) + (sizeOf ksr)
  sizeOf (GModule mg) = (constructorCost 1) + (sizeOf mg)
  sizeOf (GUser ug) = (constructorCost 1) + (sizeOf ug)


instance SizeOf PactGuard where
  sizeOf (PactGuard pid pn) =
    (constructorCost 2) + (sizeOf pid) + (sizeOf pn)

instance SizeOf KeySet where
  sizeOf (KeySet pkArr ksPred) =
    (constructorCost 2) + (sizeOf pkArr) + (sizeOf ksPred)

instance SizeOf KeySetName where
  -- newtype is free
  sizeOf (KeySetName n) = sizeOf n

instance SizeOf ModuleGuard where
  sizeOf (ModuleGuard md n) =
    (constructorCost 2) + (sizeOf md) + (sizeOf n)

instance (SizeOf p) => SizeOf (UserGuard p) where
  sizeOf (UserGuard n arr) =
    (constructorCost 2) + (sizeOf n) + (sizeOf arr)

instance SizeOf Name where
  sizeOf (QName qn) = (constructorCost 1) + sizeOf qn
  sizeOf (Name bn) = (constructorCost 1) + sizeOf bn

instance SizeOf QualifiedName where
  sizeOf (QualifiedName modName n i) =
    (constructorCost 3) + (sizeOf modName) + (sizeOf n) + (sizeOf i)

instance SizeOf BareName where
  sizeOf (BareName n i) =
    (constructorCost 2) + (sizeOf n) + (sizeOf i)

instance SizeOf ModuleName where
  sizeOf (ModuleName mn namespace) =
    (constructorCost 2) + (sizeOf mn) + (sizeOf namespace)

instance SizeOf Info where
  sizeOf (Info may) = sizeOf may

instance SizeOf Parsed where
  sizeOf (Parsed d len) =
    (constructorCost 2) + (sizeOf d) + (sizeOf len)

instance SizeOf Delta where
  -- Most of delta's arguments are unpacked (unboxed) Int64
  -- Most unboxed types just take one word
  -- Exceptions being Int64#, Word64#, and Double# which take 2 words on a 32-bit machine.
  -- TODO - Assumes 64-bit machine  
  sizeOf (Columns _ _) = 2 * wordSize
  sizeOf (Tab _ _ _) = 3 * wordSize
  sizeOf (Lines _ _ _ _) = 4 * wordSize
  -- bs is the only packed argument
  sizeOf (Directed bs _ _ _ _) =
    (constructorCost 1) + (4 * wordSize) + (sizeOf bs)   

instance SizeOf Code where
  -- newtype is free
  sizeOf (Code c) = sizeOf c

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf (a,b) = (constructorCost 3) + (sizeOf a) + (sizeOf b)

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf (Just e) = (constructorCost 1) + (sizeOf e)
  sizeOf Nothing = constructorCost 0

instance SizeOf NamespaceName where
  -- newtype is free
  sizeOf (NamespaceName n) = sizeOf n

instance (SizeOf a) => SizeOf [a] where
  sizeOf arr = arrSize
    where
      arrSize = ((1 + (3 * arrLength)) * wordSize) + sizeOfContents
      arrLength = fromIntegral (L.length arr)
      sizeOfContents = L.foldl' (\acc e -> acc + (sizeOf e)) 0 arr

instance SizeOf PublicKey where
  -- newtype is free
  sizeOf (PublicKey bs) = sizeOf bs

instance SizeOf BS.ByteString where
  sizeOf bs = byteStringSize
    where
      byteStringSize = (9 * wordSize) + byteStringLength
      byteStringLength = fromIntegral (BS.length bs)

instance SizeOf PactId where
  -- newtype is free
  sizeOf (PactId t) = sizeOf t

instance SizeOf FieldKey where
  -- newtype is free
  sizeOf (FieldKey t) = sizeOf t

instance SizeOf Text where
  sizeOf t = (6 * wordSize) + (2 * (fromIntegral (T.length t)))

instance SizeOf BigNat where
  sizeOf (BN# barr) =
    (constructorCost 1) + (fromIntegral (BA.sizeofByteArray (BA.ByteArray barr)))

instance SizeOf Integer where
  -- S's argument is an unboxed Int
  -- TODO double check unboxed
  sizeOf (S# _) = 1 * wordSize
  sizeOf (Jp# bn) = (constructorCost 1) + sizeOf bn
  sizeOf (Jn# bn) = (constructorCost 1) + sizeOf bn

instance SizeOf Int where
  sizeOf _ = 2 * wordSize

instance SizeOf Word8 where
  sizeOf _ = 2 * wordSize

instance (SizeOf i) => SizeOf (DecimalRaw i) where
  sizeOf (Decimal p m) = (constructorCost 2) + (sizeOf p) + (sizeOf m)

instance SizeOf Int64 where
  -- Assumes 64-bit machine
  sizeOf _ = 2 * wordSize

instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit count of 'microseconds'
  sizeOf ti =
    (constructorCost 1) + (sizeOf (view (_utctDayTime . microseconds) ti))
