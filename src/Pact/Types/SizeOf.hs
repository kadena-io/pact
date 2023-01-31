{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}
#if !defined(ghcjs_HOST_OS)
{-# LANGUAGE TypeApplications #-}
#endif



-- |
-- Module      :  Pact.Types.SizeOf
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Types.SizeOf
  ( SizeOf(..)
  , SizeOf1(..)
  , constructorCost
  , Bytes
  , wordSize
  , SizeOfVersion(..)
  ) where

import Bound
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Short as SBS
import Data.Decimal
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Pact.Types.Orphans()
import Pact.Types.Pretty

#if !defined(ghcjs_HOST_OS)
import qualified GHC.Integer.Logarithms as IntLog
import GHC.Int(Int(..))
#endif


-- |  Estimate of number of bytes needed to represent data type
--
-- Assumptions: GHC, 64-bit machine
-- General approach:
--   Memory Consumption = Constructor Header Size + Cost of Constructor Field(s)
--   Cost of Constructor Field(s)* = 1 word per field + cost of each field's value
-- (*) See Resource 2 for exceptions to these rules (i.e. newtypes are free)
-- Resources:
-- 1. http://wiki.haskell.org/GHC/Memory_Footprint
-- 2. https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types

data SizeOfVersion
  = SizeOfV0
  | SizeOfV1
  deriving Show

instance Pretty SizeOfVersion where
  pretty = viaShow

class SizeOf t where
  sizeOf :: SizeOfVersion -> t -> Bytes
  default sizeOf :: (Generic t, GSizeOf (Rep t)) => SizeOfVersion -> t -> Bytes
  sizeOf v a = gsizeOf v (from a)


type Bytes = Int64

-- | "word" is 8 bytes on 64-bit
wordSize64, wordSize :: Bytes
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


instance (SizeOf v) => SizeOf (Vector v) where
  sizeOf ver v = vectorSize
    where
      vectorSize =
        ((7 + vectorLength) * wordSize) + sizeOfContents
      vectorLength = fromIntegral (V.length v)
      sizeOfContents = V.foldl' (\acc pv -> acc + (sizeOf ver pv)) 0 v

instance (SizeOf a) => SizeOf (Set a) where
  sizeOf ver s = setSize
    where
      setSize = ((1 + 3 * setLength) * wordSize) + sizeOfSet
      setLength = fromIntegral $ S.size s
      sizeOfSet = S.foldl' (\acc a -> acc + sizeOf ver a) 0 s

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf ver m = mapSize
    where
      mapSize = (6 * mapLength * wordSize) + sizeOfKeys + sizeOfValues
      mapLength = fromIntegral (M.size m)
      sizeOfValues = M.foldl' (\acc pv -> acc + (sizeOf ver pv)) 0 m
      sizeOfKeys = M.foldlWithKey' (\acc fk _ -> acc + (sizeOf ver fk)) 0 m

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf ver (a,b) = constructorCost 3 + sizeOf ver a + sizeOf ver b

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf ver (Just e) = constructorCost 1 + sizeOf ver e
  sizeOf _ Nothing = constructorCost 0


instance (SizeOf a) => SizeOf [a] where
  sizeOf ver arr = arrSize
    where
      arrSize = ((1 + (3 * arrLength)) * wordSize) + sizeOfContents
      arrLength = fromIntegral (L.length arr)
      sizeOfContents = L.foldl' (\acc e -> acc + (sizeOf ver e)) 0 arr

instance SizeOf BS.ByteString where
  sizeOf _ bs = byteStringSize
    where
      byteStringSize = (9 * wordSize) + byteStringLength

      -- 'UTF8.length' returns different values than 'BS.length'. Moreoever,
      -- 'BS.length' is \(O(1)\) but 'UTF8.length' is \(O(n)\), with a pretty
      -- bad constant factor.
      --
      byteStringLength = fromIntegral (UTF8.length bs)

instance SizeOf SBS.ShortByteString where
  sizeOf ver = sizeOf ver . SBS.fromShort

instance SizeOf Text where
  sizeOf _ t = (6 * wordSize) + (2 * (fromIntegral (T.length t)))

instance SizeOf Integer where
  sizeOf ver i = case ver of
    SizeOfV0 ->
      if i <= 0 then 0 else ceiling ((logBase 100000 (realToFrac i)) :: Double)
    SizeOfV1 ->
#if !defined(ghcjs_HOST_OS)
      fromIntegral (max 64 (I# (IntLog.integerLog2# (abs i)) + 1))  `quot` 8
#else
      (max 64 (ceiling (logBase @Double 2 (fromIntegral (abs i))) + 1)) `quot` 8
#endif


instance SizeOf Int where
  sizeOf _ _ = 2 * wordSize

instance SizeOf Word8 where
  sizeOf _ _ = 2 * wordSize

instance (SizeOf i) => SizeOf (DecimalRaw i) where
  sizeOf ver (Decimal p m) = (constructorCost 2) + (sizeOf ver p) + (sizeOf ver m)

instance SizeOf Int64 where
  -- Assumes 64-bit machine
  sizeOf _ _ = 2 * wordSize

instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit count of 'microseconds'
  sizeOf ver ti =
    constructorCost 1 + sizeOf ver (toPosixTimestampMicros ti)

instance SizeOf Bool where
  sizeOf _ _ = wordSize

instance SizeOf () where
  sizeOf _ _ = 0

-- See ghc memory note above
-- as well as https://blog.johantibell.com/2011/06/memory-footprints-of-some-common-data.html
-- for both hash sets and hashmaps.
instance (SizeOf k, SizeOf v) => SizeOf (HM.HashMap k v) where
  sizeOf ver m = hmSize
    where
    hmSize = (5 * hmLength + 4 * (hmLength - 1)) * wordSize + contentLength
    contentLength = HM.foldlWithKey' (\a k v -> a + sizeOf ver k + sizeOf ver v) 0 m
    hmLength = fromIntegral (HM.size m)

-- Note: Atm hashset is only a newtype wrapper over hashmap with unit as the element
-- member of every entry. This means you don't pay at all for the cost of (),
-- but you do pay for the extra constructor field of holding it, hence the `hsSize` bit
-- stays roughly the same.
instance (SizeOf k) => SizeOf (HS.HashSet k) where
  sizeOf ver hs = hsSize
    where
    hsSize = (5 * hsLength + 4 * (hsLength - 1)) * wordSize + contentLength
    contentLength = HS.foldl' (\a v -> a + sizeOf ver v) 0 hs
    hsLength = fromIntegral (HS.size hs)

instance (SizeOf a, SizeOf b) => SizeOf (Either a b)

instance  (SizeOf a) => SizeOf (NE.NonEmpty a) where
  sizeOf ver (a NE.:| rest) =
    constructorCost 2 + sizeOf ver a + sizeOf ver rest


class SizeOf1 f where
  sizeOf1 :: SizeOf a => SizeOfVersion -> f a -> Bytes

instance (SizeOf a, SizeOf1 f, SizeOf b) => SizeOf (Var a (f b)) where
  sizeOf ver = \case
    B a -> constructorCost 1 + sizeOf ver a
    F a -> constructorCost 1 + sizeOf1 ver a

instance (SizeOf b, SizeOf a, SizeOf1 f) => SizeOf (Scope b f a) where
  sizeOf ver = sizeOf1 ver . unscope

-- Generic deriving
class GSizeOf f where
  gsizeOf :: SizeOfVersion -> f a -> Bytes

-- For sizes of products, we'll calculate the size at the leaves,
-- and simply add 1 extra word for every leaf.
instance (GSizeOf f, GSizeOf g) => GSizeOf (f :*: g) where
  gsizeOf ver (a :*: b) = gsizeOf ver a + gsizeOf ver b

-- Sums we can just branch recursively as usual
-- Ctor information is one level lower.
instance (GSizeOf a, GSizeOf b) => GSizeOf (a :+: b) where
  gsizeOf ver = \case
    L1 a -> gsizeOf ver a
    R1 b -> gsizeOf ver b


-- No fields ctors are shared.
-- We are ok charging a bit extra here.
instance {-# OVERLAPS #-} GSizeOf (C1 c U1) where
  gsizeOf _ (M1 _) = wordSize

-- Regular constructors pay the header cost
-- and 1 word for each field, which is added @ the leaves.
instance (GSizeOf f) => GSizeOf (C1 c f) where
  gsizeOf ver (M1 p) = headerCost + gsizeOf ver p

-- Metainfo about selectors
instance (GSizeOf f) => GSizeOf (S1 c f) where
  gsizeOf ver (M1 p) = gsizeOf ver p

-- Metainfo about the whole data type.
instance (GSizeOf f) => GSizeOf (D1 c f) where
  gsizeOf ver (M1 p) = gsizeOf ver p

-- Single field, means cost of field + 1 word.
instance (SizeOf c) => GSizeOf (K1 i c) where
  gsizeOf ver (K1 c) = sizeOf ver c + wordSize

-- No-argument constructors are always shared by ghc
-- so they don't really allocate.
-- However, this instance is used whenever we hit a leaf
-- that _does_ in fact allocate 1 word for the field itself.
-- 0-cost constructor `SizeOf` is caught by the `GSizeOf (C1 c U1)`
-- instance
instance GSizeOf U1 where
  gsizeOf _ U1 = wordSize
