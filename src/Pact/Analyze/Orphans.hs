{-# options_ghc -fno-warn-orphans #-}

-- | Orphan typeclass instances specific to symbolic analysis.
module Pact.Analyze.Orphans where

import           Data.SBV        (Mergeable (symbolicMerge), Int64)
import           Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text)
import           Pact.Types.Info

instance Mergeable Info where
  -- Because Info values have no effect on execution we just take the max
  -- (which could possibly have more info)
  symbolicMerge _ _ a b = max a b

instance Pretty Int64 where
  pretty = text . show
