{-# options_ghc -fno-warn-orphans #-}
module Pact.Analyze.Orphans where

import           Control.Applicative        (ZipList (..))
import           Data.SBV                   (Mergeable(symbolicMerge))

instance Mergeable a => Mergeable (ZipList a) where
  symbolicMerge force test (ZipList xs) (ZipList ys)
    = ZipList (symbolicMerge force test xs ys)
