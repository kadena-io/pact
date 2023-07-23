{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Pact.Native.Pairing.GaloisField
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.Native.Pairing.GaloisField
( GaloisField(..)
, Fq(..)
) where

import Data.Euclidean (Euclidean, GcdDomain)
import Data.Semiring (Semiring, Ring)
import Data.Field (Field)
import qualified Data.Euclidean as E
import Data.Mod
import GHC.Natural(naturalToInteger)

import Control.DeepSeq (NFData)
import Numeric.Natural(Natural)

-----------------------------------------------------
-- Galois fields and field extensions
------------------------------------------------------
class (Field k, Fractional k, Ord k, Show k) => GaloisField k where
  -- | The characteristic of the field
  characteristic :: k -> Natural

  -- | The degree of the finite field
  degree :: k -> Word

  frobenius :: k -> k

  -- | order of a field p^k
  order :: k -> Natural
  order k = characteristic k ^ degree k
  {-# INLINABLE order #-}

type Q = 21888242871839275222246405745257275088696311157297823662689037894645226208583

newtype Fq = Fq (Mod Q)
  deriving
    ( Eq
    , Show
    , Ord
    , Num
    , Fractional
    , Euclidean
    , Field
    , GcdDomain
    , Ring
    , Semiring
    , Bounded
    , Enum
    , NFData
    )

instance Real Fq where
  toRational = fromIntegral

instance Integral Fq where
  quotRem = E.quotRem
  toInteger (Fq m) = naturalToInteger (unMod m)

instance GaloisField Fq where
  characteristic _ = 21888242871839275222246405745257275088696311157297823662689037894645226208583

  degree _ = 1

  frobenius = id

