{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,20,0)
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module: Data.List.Unsafe
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Pact Team
-- Stability: experimental
--
-- This module provides unsafe versions for all functions in "Data.List" that
-- are either partial or return a 'Maybe' value.
--
module Data.List.Unsafe
(
-- * Unsafe versions of partial functions
  unsafeHead
, unsafeLast
, unsafeTail
, unsafeInit
, unsafeIndex
, unsafeGenericIndex

-- * Unsafe versions of functions that return 'Maybe' values
, unsafeUncons
#if MIN_VERSION_base(4,19,0)
, unsafeUnsnoc
#endif
, unsafeLookup
, unsafeElemIndex
, unsafeFindIndex
, unsafeStripPrefix
) where

import Data.List

import GHC.Stack

-- -------------------------------------------------------------------------- --
-- Unsafe versions of partial functions

unsafeHead :: HasCallStack => [a] -> a
unsafeHead = head

unsafeLast :: HasCallStack => [a] -> a
unsafeLast = last

unsafeTail :: HasCallStack => [a] -> [a]
unsafeTail = tail

unsafeInit :: HasCallStack => [a] -> [a]
unsafeInit = init

unsafeIndex :: HasCallStack => [a] -> Int -> a
unsafeIndex = (!!)

unsafeGenericIndex :: Integral i => [a] -> i -> a
unsafeGenericIndex = genericIndex

-- -------------------------------------------------------------------------- --
-- Unsafe versions of functions that return Maybe

unsafeUncons :: HasCallStack => [a] -> (a, [a])
unsafeUncons a = case uncons a of
    Nothing -> error "Data.List.Unsafe.unsafeUncons: empty list"
    Just x -> x

#if MIN_VERSION_base(4,19,0)
unsafeUnsnoc :: [a] -> ([a], a)
unsafeUnsnoc a = case unsnoc a of
    Nothing -> error "Data.List.Unsafe.unsafeUnsnoc: empty list"
    Just x -> x
#endif

unsafeLookup :: HasCallStack => Eq a => a -> [(a,b)] -> b
unsafeLookup a b = case lookup a b of
    Nothing -> error "Data.List.Unsafe.unsafeLookup: not found"
    Just x -> x

unsafeElemIndex :: Eq a => a -> [a] -> Int
unsafeElemIndex a b = case elemIndex a b of
    Nothing -> error "Data.List.Unsafe.unsafeElemIndex: not found"
    Just x -> x

unsafeFindIndex :: (a -> Bool) -> [a] -> Int
unsafeFindIndex a b = case findIndex a b of
    Nothing -> error "Data.List.Unsafe.unsafeFindIndex: not found"
    Just x -> x

unsafeStripPrefix :: Eq a => [a] -> [a] -> [a]
unsafeStripPrefix a b = case stripPrefix a b of
    Nothing -> error "Data.List.Unsafe.unsafeStripPrefix: not found"
    Just x -> x

