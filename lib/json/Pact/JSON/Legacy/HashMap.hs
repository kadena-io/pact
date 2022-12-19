{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Pact.JSON.Legacy.HashMap
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: BSD-3
--
-- Lightweight, less efficient, adoption of the HashMap from the
-- unordered-containers package. The ordering behavior of the 'fromList'
-- function matches the behavior of unordered-containers-0.2.15.0 and
-- hashable-1.3.0.
--
-- The main purpose of this module is to provide backward compatible ordering of
-- JSON properties. Hence, values of this type are should be emphemeral and not
-- used to store long term data. Internally, data is stored in lazy lists. Only
-- a limited set of operations is provided.
--
-- The performance is expected to be fine for small numbers of items in the hash
-- map.
--
-- The code in this module is based on unordered-containers-0.2.15.0 which is
-- released under the BSD-3 license and copyright of 2010-2014 Johan Tibell,
-- 2010 Edward Z. Yang.
--
module Pact.JSON.Legacy.HashMap
( HashMap

-- * Creation
, empty
, singleton
, singletonWithHash
, insert
, insertWithHash
, fromList
, fromListWithHash

-- * Folds
, null
, toList
, keys
, mapWithKey
, foldMapWithKey
, foldrWithKey
, foldlWithKey'

-- * Use as sorting algorithm
, sort
, sortByKey
) where

import Data.Aeson
import qualified Data.Aeson.Encoding as AE
import Data.Aeson.Types
import Data.Bifoldable
import Data.Bits
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import qualified Data.Text as T
import Data.Word

import GHC.Stack

import Prelude hiding (null)

-- internal modules

import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Hashable

-- -------------------------------------------------------------------------- --
-- Backward Compat

-- | For unordered-containers <0.2.16.0 this value is 4. In latter versions
-- this value got increased to 5.
--
bitsPerSubkey :: Int
bitsPerSubkey = 4

-- -------------------------------------------------------------------------- --
-- HashMap

type Hash = Word64
type Bitmap = Word64
type Shift  = Int

data Leaf k v = L !k v
  deriving (Show, Eq)

-- | A 'HashMap' that preserves the behavior of `Data.HashMap.Strict' from
-- unordered-containers-0.2.15.0.
--
data HashMap k v
  = Empty
  | BitmapIndexed !Bitmap ![HashMap k v]
  | Leaf !Hash !(Leaf k v)
  | Full ![HashMap k v]
  | Collision !Hash ![Leaf k v]
  deriving (Show, Eq)

instance ToJSONKey k => ToJSON1 (HashMap k) where
  liftToJSON _ _ = error "Pact.Utils.LegacyHashMap: Using toJSON on Legacy HashMaps is not supported"
  liftToEncoding g _ = case toJSONKey of
      ToJSONKeyText _ f -> AE.dict f g foldrWithKey
      ToJSONKeyValue _ f -> listEncoding (pairEncoding f) . toList
    where
      pairEncoding f (a, b) = AE.list id [f a, g b]

instance (ToJSON v, ToJSONKey k) => ToJSON (HashMap k v) where
  toJSON = toJSON1
  toEncoding = toEncoding1
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode v => J.Encode (HashMap T.Text v) where
  build = J.object . fmap (uncurry (J..=)) . toList
  {-# INLINE build #-}

instance Functor (HashMap k) where
  fmap = mapWithKey . const
  {-# INLINE fmap #-}

instance F.Foldable (HashMap k) where
  foldMap = foldMapWithKey . const
  {-# INLINE foldMap #-}
  foldr = foldrWithKey . const
  {-# INLINE foldr #-}
  foldl' = foldlWithKey' . flip . const
  {-# INLINE foldl' #-}
  null = null
  {-# INLINE null #-}

instance Bifoldable HashMap where
  bifoldMap f g = foldMapWithKey (\ k v -> f k `mappend` g v)
  {-# INLINE bifoldMap #-}
  bifoldr f g = foldrWithKey (\ k v acc -> k `f` (v `g` acc))
  {-# INLINE bifoldr #-}
  bifoldl f g = foldlWithKey (\ acc k v -> (acc `f` k) `g` v)
  {-# INLINE bifoldl #-}

-- -------------------------------------------------------------------------- --
-- Creation

empty :: HashMap k v
empty = Empty
{-# INLINE empty #-}

singleton :: LegacyHashable k => k -> v -> HashMap k v
singleton = singletonWithHash legacyHash

singletonWithHash :: (k -> Int) -> k -> v -> HashMap k v
singletonWithHash h k !v = Leaf (fromIntegral $ h k) (L k v)

fromList :: Foldable f => LegacyHashable k => f (k, v) -> HashMap k v
fromList = foldl' (\ m (k, !v) -> insert k v m) Empty
{-# INLINABLE fromList #-}

-- | Use a custom hash function (main used for testing)
--
fromListWithHash :: Eq k => Foldable f => (k -> Int) -> f (k, v) -> HashMap k v
fromListWithHash h = foldl' (\ m (k, !v) -> insertWithHash (fromIntegral $ h k) k v m) Empty
{-# INLINABLE fromListWithHash #-}

-- -------------------------------------------------------------------------- --
-- Insertion

insert :: LegacyHashable k => k -> v -> HashMap k v -> HashMap k v
insert k = insertWithHash (fromIntegral $ legacyHash k) k
{-# INLINE insert #-}

insertWithHash :: Eq k => Hash -> k -> v -> HashMap k v -> HashMap k v
insertWithHash h0 k0 !v0 = go h0 k0 v0 0
 where
  go !h !k !x !_ Empty = Leaf h $ L k x

  go h k !x s t@(Leaf hy l@(L ky _))
    | hy == h = if ky == k then Leaf h (L k x) else Collision h [l, L k x]
    | otherwise = two s h k x hy t

  go h k !x s (BitmapIndexed b ary)
    | b .&. m == 0 = bitmapIndexedOrFull (b .|. m)
      $! listInsert ary i $! Leaf h (L k x)
    | otherwise = BitmapIndexed b
      $! listUpdate ary i
      $! go h k x (s+bitsPerSubkey) (ary !! i)
   where
    m = mask h s
    i = sparseIndex b m

  go h k x s (Full ary) = Full
    $ listUpdate ary i
    $! go h k x (s+bitsPerSubkey) (ary !! i)
   where
    i = index h s

  go h k x s t@(Collision hy v)
    | h == hy   = Collision h (updateOrSnocWith const k x v)
    | otherwise = go h k x s $ BitmapIndexed (mask hy s) [t]
{-# INLINABLE insertWithHash #-}

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey
{-# INLINE maxChildren #-}

subkeyMask :: Word64
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1
{-# INLINE subkeyMask #-}

index :: Hash -> Shift -> Int
index w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask
{-# INLINE index #-}

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

mask :: Hash -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

two :: Shift -> Hash -> k -> v -> Hash -> HashMap k v -> HashMap k v
two = go
 where
  go s h1 k1 !v1 h2 t2
    | bp1 == bp2 = BitmapIndexed bp1 [go (s+bitsPerSubkey) h1 k1 v1 h2 t2]
    | otherwise = BitmapIndexed (bp1 .|. bp2) $ if index h1 s < index h2 s
      then [Leaf h1 (L k1 v1), t2]
      else [t2, Leaf h1 (L k1 v1)]
   where
    bp1  = mask h1 s
    bp2  = mask h2 s

listInsert :: HasCallStack => [a] -> Int -> a -> [a]
listInsert l 0 !a = a : l
listInsert (h:t) i a = h : listInsert t (i-1) a
listInsert [] _ _ = error "listInsert: list to short"

listUpdate :: HasCallStack => [a] -> Int -> a -> [a]
listUpdate (_:t) 0 !a = a : t
listUpdate (h:t) i a = h : listUpdate t (i-1) a
listUpdate [] _ _ = error "listUpdate: list to short"

bitmapIndexedOrFull :: Bitmap -> [HashMap k v] -> HashMap k v
bitmapIndexedOrFull b !ary
  | b == fullNodeMask = Full ary
  | otherwise = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `shiftL` maxChildren)
{-# INLINE fullNodeMask #-}

updateOrSnocWith :: Eq k => (v -> v -> v) -> k -> v -> [Leaf k v] -> [Leaf k v]
updateOrSnocWith f k0 !v0 ary0 = go k0 v0 ary0 0 (length ary0)
 where
  go !k !v !ary !i !n
    | i >= n = ary <> [L k v]
    | L kx y <- ary !! i, k == kx, !v2 <- f v y = listUpdate ary i (L k v2)
    | otherwise = go k v ary (i+1) n
{-# INLINABLE updateOrSnocWith #-}

-- -------------------------------------------------------------------------- --
-- Folds

toList :: HashMap k v -> [(k, v)]
toList = foldrWithKey (\k v l -> (k,v):l) []
{-# INLINE toList #-}

keys :: HashMap k v -> [k]
keys = foldrWithKey (\k _ l -> k:l) []
{-# INLINE keys #-}

null :: HashMap k v -> Bool
null Empty = True
null _ = False
{-# INLINE null #-}

foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey f = go
  where
    go Empty = mempty
    go (Leaf _ (L k v)) = f k v
    go (BitmapIndexed _ ary) = foldMap go ary
    go (Full ary) = foldMap go ary
    go (Collision _ ary) = foldMap (\ (L k v) -> f k v) ary
{-# INLINE foldMapWithKey #-}

-- | Lazy right fold.
--
foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = flip go
 where
  go Empty z                 = z
  go (Leaf _ (L k v)) z      = f k v z
  go (BitmapIndexed _ ary) z = foldr go z ary
  go (Full ary) z            = foldr go z ary
  go (Collision _ ary) z     = foldr (\(L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey #-}

-- | Strict left fold
--
foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = foldl' go z ary
    go z (Full ary)            = foldl' go z ary
    go z (Collision _ ary)     = foldl' (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey' #-}

foldlWithKey :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = foldl go z ary
    go z (Full ary)            = foldl go z ary
    go z (Collision _ ary)     = foldl (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey #-}

-- -------------------------------------------------------------------------- --
-- Mapping

mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
 where
  go Empty = Empty
  go (Leaf h (L k v)) = Leaf h $ L k $! f k v
  go (BitmapIndexed b ary) = BitmapIndexed b $ go <$> ary
  go (Full ary) = Full $ go <$> ary
  go (Collision h ary) = Collision h $ (\(L k v) -> L k $! f k v) <$> ary
{-# INLINE mapWithKey #-}

-- -------------------------------------------------------------------------- --
-- Sorting

sortByKey :: LegacyHashable k => [(k,v)] -> [(k,v)]
sortByKey = toList . fromList
{-# INLINABLE sortByKey #-}

sort :: LegacyHashable k => [k] -> [k]
sort = keys . fromList . fmap (,())
{-# INLINABLE sort #-}

