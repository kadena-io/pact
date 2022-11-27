{-# LANGUAGE DerivingVia #-}

module Pact.Core.Gas
 ( Gas(..)
 ) where

import Data.Word(Word64)
import Data.Monoid(Sum(..))
import Data.Semiring(Semiring)

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas Word64
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring) via Word64

  -- deriving (Num, Real, Integral, Enum, Show) via Word64
