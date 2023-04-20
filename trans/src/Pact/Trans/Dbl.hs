{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Pact.Trans.Dbl
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
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

decode :: Double -> TransResult Double
decode !r | isNaN r = TransNaN r
          | isInfinite r && r < 0 = TransNegInf r
          | isInfinite r = TransInf r
          | otherwise = TransNumber r

dbl_exp :: Double -> TransResult Double
dbl_exp = decode . exp

dbl_ln :: Double -> TransResult Double
dbl_ln = decode . log

dbl_log :: Double -> Double -> TransResult Double
dbl_log = (decode .) . logBase

dbl_pow :: Double -> Double -> TransResult Double
dbl_pow = (decode .) . (**)

dbl_sqrt :: Double -> TransResult Double
dbl_sqrt = decode . sqrt
