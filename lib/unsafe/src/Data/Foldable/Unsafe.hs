{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,20,0)
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif


-- |
-- Module: unsafe.Data.Foldable.Unsafe
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Pact Team
-- Stability: experimental
--
-- This module provides unsafe versions for all functions in "Data.Foldable"
-- that are either partial or return a 'Maybe' value.
--
module Data.Foldable.Unsafe
(
-- * Unsafe versions of partial functions
  unsafeMaximum
, unsafeMaximumBy
, unsafeMinimum
, unsafeMinimumBy

-- * Unsafe versions of functions that return 'Maybe' values
, unsafeFind
) where

import Data.Foldable

import GHC.Stack

-- -------------------------------------------------------------------------- --
-- Unsafe versions of partial functions

unsafeMaximum :: HasCallStack => Foldable t => Ord a => t a -> a
unsafeMaximum = maximum

unsafeMaximumBy :: HasCallStack => Foldable t => (a -> a -> Ordering) -> t a -> a
unsafeMaximumBy = maximumBy

unsafeMinimum :: HasCallStack => (Foldable t, Ord a) => t a -> a
unsafeMinimum = minimum

unsafeMinimumBy :: HasCallStack => Foldable t => (a -> a -> Ordering) -> t a -> a
unsafeMinimumBy = minimumBy

-- -------------------------------------------------------------------------- --
-- Unsafe versions of functions that return Maybe

unsafeFind :: HasCallStack => Foldable t => (a -> Bool) -> t a -> a
unsafeFind a b = case find a b of
    Nothing -> error "Data.List.Unsafe.unsafeFind: not found"
    Just x -> x

