{-# LANGUAGE DerivingVia #-}

module Pact.Core.Gas
 ( Gas(..)
 ) where

import Data.Word(Word64)

newtype Gas
  = Gas Word64
  deriving (Eq, Ord)
  deriving (Num, Real, Integral, Enum, Show) via Word64

