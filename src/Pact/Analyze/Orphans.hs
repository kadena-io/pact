{-# options_ghc -fno-warn-orphans #-}
module Pact.Analyze.Orphans where

import           Control.Applicative (ZipList (..))
import           Data.SBV            (Mergeable (symbolicMerge))
import           Pact.Types.Info

instance Mergeable a => Mergeable (ZipList a) where
  symbolicMerge force test (ZipList xs) (ZipList ys)
    = ZipList (symbolicMerge force test xs ys)

instance Mergeable Info where
  -- Because Info values have no effect on execution we just take the max
  -- (which could possibly have more info)
  symbolicMerge _ _ a b = max a b
