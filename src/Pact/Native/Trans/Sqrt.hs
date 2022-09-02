-- |
-- Module      :  Pact.Native.Trans.Sqrt
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Sqrt
    ( trans_sqrt
    ) where

import Pact.Native.Trans.Types
  ( c'MPFR_RNDN
  , c'mpfr_sqrt
  , dec2Mpfr
  , mpfr2Dec
  , withTemp
  , TransResult
  )
import Data.Decimal (Decimal)
import System.IO.Unsafe (unsafePerformIO)

trans_sqrt :: Decimal -> TransResult Decimal
trans_sqrt x = unsafePerformIO $
  dec2Mpfr x $ \x' ->
  withTemp $ \y' -> do
    c'mpfr_sqrt y' x' c'MPFR_RNDN
    mpfr2Dec y'
