-- |
-- Module      :  Pact.Native.Trans.Types
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Types
  ( trans_pow
  , trans_exp
  , trans_log
  , trans_ln
  , trans_sqrt
  )
  where

foreign import ccall unsafe "musl_pow"
  musl_pow :: Double -> Double -> Double

foreign import ccall unsafe "musl_exp"
  musl_exp :: Double -> Double

foreign import ccall unsafe "musl_log"
  musl_log :: Double -> Double

foreign import ccall unsafe "musl_sqrt"
  musl_sqrt :: Double -> Double

trans_pow :: Double -> Double -> Double
trans_pow = musl_pow

trans_exp :: Double -> Double
trans_exp = musl_exp

trans_log :: Double -> Double -> Double
trans_log b x = musl_log x / musl_log b

trans_ln :: Double -> Double
trans_ln = musl_log

trans_sqrt :: Double -> Double
trans_sqrt = musl_sqrt


