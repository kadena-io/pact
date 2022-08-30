-- |
-- Module      :  Pact.Native.Trans.Pow
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Pow
    ( trans_pow
    ) where

import Pact.Native.Trans.Types
  ( c'MPFR_RNDN
  , c'mpfr_init
  , c'mpfr_clear
  , c'mpfr_set_str
  , c'mpfr_pow
  , c'mpfr_snprintf
  , withFormattedNumber
  , TransResult
  , readResultNumber
  )
import Data.Decimal (Decimal, normalizeDecimal)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import System.IO.Unsafe (unsafePerformIO)

trans_pow :: Decimal -> Decimal -> TransResult Decimal
trans_pow x y = unsafePerformIO $ withFormattedNumber $ \out fmt ->
  withCString (show (normalizeDecimal x)) $ \xstr ->
  withCString (show (normalizeDecimal y)) $ \ystr ->
  alloca $ \x' ->
  alloca $ \y' ->
  alloca $ \z' -> do
    c'mpfr_init x'
    c'mpfr_set_str x' xstr 10 c'MPFR_RNDN
    c'mpfr_init y'
    c'mpfr_set_str y' ystr 10 c'MPFR_RNDN
    c'mpfr_init z'
    c'mpfr_pow z' x' y'
    c'mpfr_snprintf out 1024 fmt c'MPFR_RNDN z'
    c'mpfr_clear x'
    c'mpfr_clear y'
    c'mpfr_clear z'
    readResultNumber <$> peekCString out
