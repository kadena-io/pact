-- |
-- Module      :  Pact.Native.Trans.Sqrt
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Sqrt
    ( trans_sqrt
    ) where

import Pact.Native.Trans.Types
  ( c'MPFR_RNDN
  , c'mpfr_init
  , c'mpfr_set_str
  , c'mpfr_sqrt
  , c'mpfr_snprintf
  , withFormattedNumber
  , TransResult
  , readResultNumber
  )
import Data.Decimal (Decimal, normalizeDecimal)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import System.IO.Unsafe (unsafePerformIO)

trans_sqrt :: Decimal -> TransResult Decimal
trans_sqrt x = unsafePerformIO $ withFormattedNumber $ \out fmt ->
  withCString (show (normalizeDecimal x)) $ \xstr ->
  alloca $ \x' ->
  alloca $ \y' -> do
    c'mpfr_init x'
    c'mpfr_set_str x' xstr 10 c'MPFR_RNDN
    c'mpfr_init y'
    c'mpfr_sqrt y' x' c'MPFR_RNDN
    c'mpfr_snprintf out 1024 fmt c'MPFR_RNDN y'
    readResultNumber <$> peekCString out
