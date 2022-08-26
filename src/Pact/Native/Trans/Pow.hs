-- |
-- Module      :  Pact.Native.Trans.Pow
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Pow
    ( trans_pow
    ) where

import Pact.Native.Trans.Types
  ( c'MPFR_RNDN
  , c'mpfr_init
  , c'mpfr_set_str
  , c'mpfr_pow
  , c'mpfr_sprintf
  )
import Data.Decimal (Decimal, normalizeDecimal)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (dropWhileEnd)

trans_pow :: Decimal -> Decimal -> Decimal
trans_pow x y = read $ dropWhileEnd (== '0') $ unsafePerformIO $
  withCString (show (normalizeDecimal x)) $ \xstr ->
  withCString (show (normalizeDecimal y)) $ \ystr ->
  alloca $ \x' ->
  alloca $ \y' ->
  alloca $ \z' ->
  allocaBytes 2048 $ \out ->
  withCString "%.128R*f" $ \fmt -> do
    c'mpfr_init x'
    c'mpfr_set_str x' xstr 10 c'MPFR_RNDN
    c'mpfr_init y'
    c'mpfr_set_str y' ystr 10 c'MPFR_RNDN
    c'mpfr_init z'
    c'mpfr_pow z' x' y'
    c'mpfr_sprintf out fmt c'MPFR_RNDN z'
    peekCString out
