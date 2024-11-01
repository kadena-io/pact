{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Pact.Utils.StableHashMap
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.Utils.StableHashMap
( StableHashable(..)
, StableHashMap
, unstable
, empty
, singleton
, null
, size
, member
, lookup
, (!?)
, (!)
, insert
, insertWith
, delete
, union
, unionWith
, unions
, difference
, map
, mapWithKey
, traverseWithKey
, mapKeys
, intersection
, filter
, filterWithKey
, foldMapWithKey
, foldrWithKey'
, foldlWithKey'
, foldrWithKey
, foldlWithKey
, keys
, elems
, toList
, fromList

-- * Internal
, hashInt
) where

import Control.DeepSeq
import Control.Monad
import Control.Lens

import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bits
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Hashable qualified as H
import Data.Int
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word

import Data.Hash.FNV1 (fnv1_64_)
import Data.Hash.FNV1.Salted

import GHC.Stack

import Prelude hiding (lookup, null, filter, map)

import Test.QuickCheck qualified as Q

-- -------------------------------------------------------------------------- --
-- Non-standard Hashable 1.4.4.0 Variant of Fnv1 64 bit

newtype HFnv164Context = HFnv164Context Word64

newtype HFnv164Hash = HFnv164Hash Word64
    deriving (Show, Eq, Ord)

instance IncrementalHash HFnv164Hash where
    type Context HFnv164Hash = HFnv164Context
    update (HFnv164Context !ctx) !ptr !n = HFnv164Context <$!> fnv1_64_ ptr n ctx
    finalize (HFnv164Context !ctx) = HFnv164Hash ctx
    {-# INLINE update #-}
    {-# INLINE finalize #-}

instance Hash HFnv164Hash where
    type Salt HFnv164Hash = Word64
    -- this initialization of a salted FNV1 hash is no-standard. The standard
    -- version seed the context first the value of fnvOffsetBasis64 and mixes
    -- the salted into that. The version in hashable-1.4.4.0 uses the salt as
    -- the seed for the initial context.
    initialize = HFnv164Context
    {-# INLINE initialize #-}

-- -------------------------------------------------------------------------- --
-- StableHashable

-- | Noncryptographic hashvalues for use with hash tables and similar data
-- structures.
--
-- This class shares its types signatures with `Hashable` from the
-- `hashable` package. However, it provides different guarantees:
--
-- * Instances of this class promise the the computed hash value is stable
--   accross all implementations including future versions, platforms,
--   compilers, etc.
--
class StableHashable k where
    stableHash :: k -> Int64
    stableHashWithSalt :: Int64 -> k -> Int64

    stableHash = stableHashWithSalt defaultSalt
    stableHashWithSalt s = hashInt s . stableHash
    {-# MINIMAL stableHash | stableHashWithSalt #-}

-- | This instance is compatible with the hashable-1.4.4.0 on 64-bit platforms.
--
-- (Generally, this instance should be avoided for software that may be used
-- on platforms with word-sizes other tha n 64 bits.)
--
instance StableHashable Int where
    stableHash = id . fromIntegral
    stableHashWithSalt s = hashInt s . fromIntegral

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable Int64 where
    stableHash = id
    stableHashWithSalt s = hashInt s

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable Word8 where
    stableHash = fromIntegral
    stableHashWithSalt s = hashInt s . stableHash

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable () where
    stableHash () = 0
    stableHashWithSalt s = hashInt s . stableHash

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable Bool where
    stableHash False = 0
    stableHash True = 1
    stableHashWithSalt s = hashInt s . stableHash

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable B.ByteString where
    stableHashWithSalt s b = fromIntegral h
      where
        s' = fromIntegral $ stableHashWithSalt @Int64 s (fromIntegral $ B.length b)
        HFnv164Hash h = hashByteString (s') b

-- | This instance is compatible with the hashable-1.4.4.0.
--
-- The implementation is NOT lazy, because of the use of 'BL.length'. If lazy
-- performance is required this implemenatation can be adjusted without changing
-- the semantics.
--
instance StableHashable BL.ByteString where
    stableHashWithSalt s b = stableHashWithSalt (fromIntegral h) (BL.length b)
      where
        s' = fromIntegral $ stableHash s
        HFnv164Hash h = hashByteStringLazy (s') b

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable BS.ShortByteString where
    stableHashWithSalt s b = fromIntegral h
      where
        s' = fromIntegral $ stableHashWithSalt s (BS.length b)
        HFnv164Hash h = hashShortByteString (s') b

-- | This instance is compatible with the hashable-1.4.4.0
--
instance StableHashable T.Text where
    stableHashWithSalt s t = stableHashWithSalt s $ T.encodeUtf8 t

-- -------------------------------------------------------------------------- --

defaultSalt :: Int64
defaultSalt = fromIntegral @Word64 @Int64 14695981039346656037
{-# INLINE defaultSalt #-}

-- Variant of fnv1 that is used in the `hashable` package version 1.4.4.0
-- Copied from https://hackage.haskell.org/package/hashable-1.4.4.0/docs/src/Data.Hashable.LowLevel.html#hashInt
-- Copyright Milan Straka 2010 (BSD-3-Clause)
--
hashInt :: Int64 -> Int64 -> Int64
hashInt s x = s `rnd` x1 `rnd` x2 `rnd` x3 `rnd` x4
  where
    {-# INLINE rnd #-}
    {-# INLINE x1 #-}
    {-# INLINE x2 #-}
    {-# INLINE x3 #-}
    {-# INLINE x4 #-}
    rnd a b = (a * 1099511628211) `xor` b
    x1 = shiftR x 48 .&. 0xffff
    x2 = shiftR x 32 .&. 0xffff
    x3 = shiftR x 16 .&. 0xffff
    x4 =           x .&. 0xffff

-- -------------------------------------------------------------------------- --

-- | Internal type for implenting StableHashMap
--
newtype StableHash k = StableHash k
    deriving (Show, Read)
    deriving newtype (Eq, Ord, NFData, FromJSONKey, Q.Arbitrary)
    deriving (Functor)

instance (Eq k, StableHashable k) => H.Hashable (StableHash k) where
    hashWithSalt s (StableHash k) = fromIntegral $ stableHashWithSalt (fromIntegral s) k
    {-# INLINEABLE hashWithSalt #-}

-- -------------------------------------------------------------------------- --
-- StableHashMap

newtype StableHashMap k v = StableHashMap (HM.HashMap (StableHash k) v)
    deriving (Show, Read)
    deriving newtype (Eq, Ord, Semigroup, Monoid, Foldable, Functor, NFData)

type role StableHashMap nominal representational

unstable :: H.Hashable k => StableHashMap k v -> HM.HashMap k v
unstable (StableHashMap m) = HM.mapKeys (\(StableHash k) -> k) m

instance Bifoldable StableHashMap where
    bifoldMap f g (StableHashMap m) = bifoldMap (\(StableHash k) -> f k) g m

instance Traversable (StableHashMap k) where
    traverse f (StableHashMap m) = StableHashMap <$> traverse f m

instance (Eq k, StableHashable k, FromJSONKey k, FromJSON v) => FromJSON (StableHashMap k v) where
    parseJSON = fmap StableHashMap . parseJSON

instance (Eq k, StableHashable k, Q.Arbitrary k, Q.Arbitrary v) => Q.Arbitrary (StableHashMap k v) where
    arbitrary = fromList <$> Q.arbitrary

type instance Index (StableHashMap k v) = k
type instance IxValue (StableHashMap k v) = v

instance (Eq k, StableHashable k) => At (StableHashMap k v) where
    at k f (StableHashMap m) = StableHashMap <$> at (StableHash k) f m

instance (Eq k, StableHashable k) => Ixed (StableHashMap k v) where

instance FunctorWithIndex k (StableHashMap k) where
    imap = mapWithKey

instance FoldableWithIndex k (StableHashMap k) where
    ifoldMap = foldMapWithKey
    ifoldr = foldrWithKey
    ifoldl' = foldlWithKey' . flip

instance TraversableWithIndex k (StableHashMap k) where
    itraverse = traverseWithKey

empty :: StableHashMap k v
empty = StableHashMap HM.empty

singleton :: Eq k => StableHashable k => k -> v -> StableHashMap k v
singleton k = StableHashMap . HM.singleton (StableHash k)

null :: StableHashMap k v -> Bool
null (StableHashMap m) = HM.null m

size :: StableHashMap k v -> Int
size (StableHashMap m) = HM.size m

member :: StableHashable k => Eq k => k -> StableHashMap k v -> Bool
member k (StableHashMap m) = HM.member (StableHash k) m

lookup :: Eq k => StableHashable k => k -> StableHashMap k v -> Maybe v
lookup  k (StableHashMap m) = HM.lookup (StableHash k) m

(!?) :: Eq k => StableHashable k => StableHashMap k v -> k -> Maybe v
(!?) = flip lookup

(!) :: HasCallStack => Eq k => StableHashable k => StableHashMap k v -> k -> v
(!) (StableHashMap m) k = (HM.!) m (StableHash k)

insert :: Eq k => StableHashable k => k -> v -> StableHashMap k v -> StableHashMap k v
insert k v (StableHashMap m) = StableHashMap $ HM.insert (StableHash k) v m

insertWith :: Eq k => StableHashable k => (v -> v -> v) -> k -> v -> StableHashMap k v -> StableHashMap k v
insertWith f k v (StableHashMap m) = StableHashMap $ HM.insertWith f (StableHash k) v m

delete :: Eq k => StableHashable k => k -> StableHashMap k v -> StableHashMap k v
delete k (StableHashMap m) = StableHashMap $ HM.delete (StableHash k) m

union :: Eq k => StableHashMap k v -> StableHashMap k v -> StableHashMap k v
union (StableHashMap m0) (StableHashMap m1) = StableHashMap $ HM.union m0 m1

unionWith :: Eq k => (v -> v -> v) -> StableHashMap k v -> StableHashMap k v -> StableHashMap k v
unionWith f (StableHashMap m0) (StableHashMap m1) = StableHashMap $ HM.unionWith f m0 m1

unions :: Eq k => [StableHashMap k v] -> StableHashMap k v
unions l = StableHashMap $ HM.unions ((\(StableHashMap m) -> m) <$> l)

difference :: Eq k => StableHashable k => StableHashMap k v -> StableHashMap k w -> StableHashMap k v
difference (StableHashMap m0) (StableHashMap m1) = StableHashMap $ HM.difference m0 m1

map :: (v1 -> v2) -> StableHashMap k v1 -> StableHashMap k v2
map f (StableHashMap m) = StableHashMap $ HM.map f m

mapWithKey :: (k -> v1 -> v2) -> StableHashMap k v1 -> StableHashMap k v2
mapWithKey f (StableHashMap m) = StableHashMap $ HM.mapWithKey f' m
  where
    f' (StableHash k) v = f k v

traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> StableHashMap k v1 -> f (StableHashMap k v2)
traverseWithKey f (StableHashMap m) = StableHashMap <$> HM.traverseWithKey f' m
  where
    f' (StableHash k) v = f k v

mapKeys :: Eq k2 => StableHashable k2 => (k1 -> k2) -> StableHashMap k1 v -> StableHashMap k2 v
mapKeys f (StableHashMap m) = StableHashMap $ HM.mapKeys (fmap f) m

intersection :: Eq k => StableHashMap k v -> StableHashMap k w -> StableHashMap k v
intersection (StableHashMap m0) (StableHashMap m1) = StableHashMap $ HM.intersection m0 m1

filter :: (v -> Bool) -> StableHashMap k v -> StableHashMap k v
filter f (StableHashMap m) = StableHashMap $ HM.filter f m

filterWithKey :: (k -> v -> Bool) -> StableHashMap k v -> StableHashMap k v
filterWithKey f (StableHashMap m) = StableHashMap $ HM.filterWithKey f' m
  where
    f' (StableHash k) v = f k v

foldMapWithKey :: Monoid m => (k -> v -> m) -> StableHashMap k v -> m
foldMapWithKey f (StableHashMap m) = HM.foldMapWithKey f' m
  where
    f' (StableHash k) = f k

foldlWithKey' :: (a -> k -> v -> a) -> a -> StableHashMap k v -> a
foldlWithKey' f a (StableHashMap m) = HM.foldlWithKey' f' a m
  where
    f' b (StableHash k) = f b k

foldrWithKey' :: (k -> v -> a -> a) -> a -> StableHashMap k v -> a
foldrWithKey' f a (StableHashMap m) = HM.foldrWithKey' f' a m
  where
    f' (StableHash k) = f k

foldlWithKey :: (a -> k -> v -> a) -> a -> StableHashMap k v -> a
foldlWithKey f a (StableHashMap m) = HM.foldlWithKey' f' a m
  where
    f' b (StableHash k) = f b k

foldrWithKey :: (k -> v -> a -> a) -> a -> StableHashMap k v -> a
foldrWithKey f a (StableHashMap m) = HM.foldrWithKey f' a m
  where
    f' (StableHash k) = f k

keys :: StableHashMap k v -> [k]
keys (StableHashMap m) = (\(StableHash k) -> k) <$> HM.keys m

elems :: StableHashMap k v -> [v]
elems (StableHashMap m) = HM.elems m

toList :: StableHashMap k v -> [(k, v)]
toList (StableHashMap m) = first (\(StableHash k) -> k) <$!> HM.toList m

fromList :: Eq k => StableHashable k => [(k, v)] -> StableHashMap k v
fromList l = StableHashMap $! HM.fromList $ first StableHash <$!> l

