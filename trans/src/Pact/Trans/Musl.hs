{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- |
-- Module      :  Pact.Trans.Musl
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins based on the Musl C library.
--

module Pact.Trans.Musl
  ( musl_exp
  , musl_ln
  , musl_log
  , musl_pow
  , musl_sqrt
  ) where

import Pact.Trans.Types

foreign import ccall unsafe "musl_pow"
  c'musl_pow :: Double -> Double -> Double

foreign import ccall unsafe "musl_exp"
  c'musl_exp :: Double -> Double

foreign import ccall unsafe "musl_log"
  c'musl_log :: Double -> Double

foreign import ccall unsafe "musl_sqrt"
  c'musl_sqrt :: Double -> Double

musl_exp :: Double -> TransResult Double
musl_exp = doubleToTransResult . c'musl_exp

musl_log :: Double -> Double -> TransResult Double
musl_log b x = doubleToTransResult (c'musl_log x / c'musl_log b)

musl_ln :: Double -> TransResult Double
musl_ln = doubleToTransResult . c'musl_log

musl_pow :: Double -> Double -> TransResult Double
musl_pow = (doubleToTransResult .) . c'musl_pow

musl_sqrt :: Double -> TransResult Double
musl_sqrt = doubleToTransResult . c'musl_sqrt
