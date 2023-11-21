{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- |
-- Module      :  Pact.Trans.Dbl
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>, Jose Cardona <jose@kadena.io>
--
-- Operators and math built-ins based on Haskell Doubles.
--

module Pact.Trans.Dbl
  ( dbl_exp
  , dbl_ln
  , dbl_log
  , dbl_pow
  , dbl_sqrt
  ) where

import Pact.Trans.Types

dbl_exp :: Double -> TransResult Double
dbl_exp = doubleToTransResult . exp

dbl_ln :: Double -> TransResult Double
dbl_ln = doubleToTransResult . log

dbl_log :: Double -> Double -> TransResult Double
dbl_log = (doubleToTransResult .) . logBase

dbl_pow :: Double -> Double -> TransResult Double
dbl_pow = (doubleToTransResult .) . (**)

dbl_sqrt :: Double -> TransResult Double
dbl_sqrt = doubleToTransResult . sqrt
