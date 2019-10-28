{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      :  Pact.Types.SizeOf
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Types.SizeOf
  ( SizeOf(..)
  , constructorCost
  ) where

import Control.Lens (view)
import qualified Data.ByteString.UTF8 as BS
import Data.Decimal
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme hiding (Vector)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Exts (Int(..))



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

class SizeOf t where
  sizeOf :: t -> Bytes


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
  sizeOf v = vectorSize
    where
      vectorSize =
        ((7 + vectorLength) * wordSize) + sizeOfContents
      vectorLength = fromIntegral (V.length v)
      sizeOfContents = V.foldl' (\acc pv -> acc + (sizeOf pv)) 0 v

instance SizeOf a => SizeOf (Set a) where
  sizeOf s = setSize
    where
      setSize = ((1 + 3 * setLength) * wordSize) + sizeOfSet
      setLength = fromIntegral $ S.size s
      sizeOfSet = S.foldl' (\acc a -> acc + sizeOf a) 0 s

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf m = mapSize
    where
      mapSize = (6 * mapLength * wordSize) + sizeOfKeys + sizeOfValues
      mapLength = fromIntegral (M.size m)
      sizeOfValues = M.foldl' (\acc pv -> acc + (sizeOf pv)) 0 m
      sizeOfKeys = M.foldlWithKey' (\acc fk _ -> acc + (sizeOf fk)) 0 m

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf (a,b) = (constructorCost 3) + (sizeOf a) + (sizeOf b)

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf (Just e) = (constructorCost 1) + (sizeOf e)
  sizeOf Nothing = constructorCost 0


instance (SizeOf a) => SizeOf [a] where
  sizeOf arr = arrSize
    where
      arrSize = ((1 + (3 * arrLength)) * wordSize) + sizeOfContents
      arrLength = fromIntegral (L.length arr)
      sizeOfContents = L.foldl' (\acc e -> acc + (sizeOf e)) 0 arr

instance SizeOf BS.ByteString where
  sizeOf bs = byteStringSize
    where
      byteStringSize = (9 * wordSize) + byteStringLength
      byteStringLength = fromIntegral (BS.length bs)

instance SizeOf Text where
  sizeOf t = (6 * wordSize) + (2 * (fromIntegral (T.length t)))

instance SizeOf Integer where
  sizeOf i = ceiling ((logBase 100000 (realToFrac i)) :: Double)

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
