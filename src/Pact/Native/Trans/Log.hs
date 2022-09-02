-- |
-- Module      :  Pact.Native.Trans.Log
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Log
    ( trans_ln
    , trans_log2
    , trans_log10
    , trans_logBase
    ) where

import Pact.Native.Trans.Types
  ( c'MPFR_RNDN
  , c'mpfr_div
  , c'mpfr_log
  , c'mpfr_log2
  , c'mpfr_log10
  , dec2Mpfr
  , mpfr2Dec
  , withTemp
  , TransResult
  , trans_arity1
  )
import Data.Decimal (Decimal)
import System.IO.Unsafe (unsafePerformIO)

trans_ln :: Decimal -> TransResult Decimal
trans_ln = trans_arity1 c'mpfr_log

trans_log2 :: Decimal -> TransResult Decimal
trans_log2 = trans_arity1 c'mpfr_log2

trans_log10 :: Decimal -> TransResult Decimal
trans_log10 = trans_arity1 c'mpfr_log10

trans_logBase :: Decimal -> Decimal -> TransResult Decimal
trans_logBase x y = unsafePerformIO $
  dec2Mpfr x $ \x' ->
  dec2Mpfr y $ \y' ->
  withTemp $ \x'' ->
  withTemp $ \y'' ->
  withTemp $ \z' -> do
    c'mpfr_log x'' x' c'MPFR_RNDN
    c'mpfr_log y'' y' c'MPFR_RNDN
    c'mpfr_div z' y'' x'' c'MPFR_RNDN
    mpfr2Dec z'
