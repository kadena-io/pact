{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Pact.Utils.Json
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Pact.Utils.Json
( legacyHash
, legacyHashWithSalt
, legacyHashSort
, legacyJsonPropertySort
, legacyHashMapToEncoding
, legacyMapToEncoding
, LegacyHashed(..)

-- * Testing
, prop_legacyHashCompat
, prop_legacyHashInstance
) where

import Data.Aeson
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- -------------------------------------------------------------------------- --
-- Aeson 1.* backward compatible hashing

newtype LegacyHashed a = LegacyHashed { getLegacyHashed :: a }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

instance Hashable (LegacyHashed T.Text) where
  hash = legacyHash . getLegacyHashed
  hashWithSalt salt = legacyHashWithSalt salt . getLegacyHashed
  {-# INLINE hash #-}
  {-# INLINE hashWithSalt #-}

legacyHashSort :: [T.Text] -> [T.Text]
legacyHashSort = L.sortOn legacyHash
{-# INLINE legacyHashSort #-}

legacyJsonPropertySort :: [T.Text] -> [T.Text]
legacyJsonPropertySort = fmap getLegacyHashed
  . HM.keys
  . HM.fromList
  . fmap ((,()) . LegacyHashed)
{-# INLINE legacyJsonPropertySort #-}

legacyHashMapToEncoding :: ToJSON a => HM.HashMap T.Text a -> Encoding
legacyHashMapToEncoding = toEncoding . HM.mapKeys LegacyHashed
{-# INLINE legacyHashMapToEncoding #-}

legacyMapToEncoding :: ToJSON a => M.Map T.Text a -> Encoding
legacyMapToEncoding = toEncoding
  . HM.fromList
  . fmap (first LegacyHashed)
  . M.toList
{-# INLINE legacyMapToEncoding #-}

-- -------------------------------------------------------------------------- --
-- Hashing of 'T.Text' for `text <2` and `hashable <1.3.1`

-- | Legacy hash function for 'T.Text', that preserves the semantics of
-- text-1.5 and hashable-1.3.0 on 64bit POSIX systems.
--
legacyHash :: T.Text -> Int
legacyHash = fromIntegral . hashable_fnv1 . T.encodeUtf16LE
{-# INLINE legacyHash #-}

legacyHashWithSalt :: Int -> T.Text -> Int
legacyHashWithSalt salt =
  fromIntegral . hashable_fnv1_withSalt (fromIntegral salt) . T.encodeUtf16LE
{-# INLINE legacyHashWithSalt #-}

-- | Non-optimized implementation of FNV1 as used in hashable <1.3.1.
-- Only use for short inputs!
--
-- This implemetation deviates from the standard as follows
--
-- * It uses the 32 prime with 64bit arithmetic (on architectures where long has 8 bytes)
-- * It initializes the hash with a custom salt instead of FNV_offset_basis
--
hashable_fnv1_withSalt :: Int64 -> B.ByteString -> Int64
hashable_fnv1_withSalt = B.foldl' go
 where
  go :: Int64 -> Word8 -> Int64
  go h c = (h * fnv_prime_32) `xor` fromIntegral c

  -- 32 bit fnv1 prime
  fnv_prime_32 :: Int64
  fnv_prime_32 = 16777619

hashable_fnv1 :: B.ByteString -> Int64
hashable_fnv1 = hashable_fnv1_withSalt hashable_default_salt
 where
  hashable_default_salt :: Int64
  hashable_default_salt = -2578643520546668380
{-# INLINE hashable_fnv1 #-}

-- -------------------------------------------------------------------------- --
-- Properties

instance Arbitrary t => Arbitrary (LegacyHashed t) where
  arbitrary = LegacyHashed <$> arbitrary

#if !MIN_VERSION_hashable(1,3,1)
prop_legacyHashCompat :: T.Text -> Property
prop_legacyHashCompat t = legacyHash t === hash t

prop_legacyHashInstance :: LegacyHashed T.Text -> Property
prop_legacyHashInstance t = hash (getLegacyHashed t) === hash t
#endif
