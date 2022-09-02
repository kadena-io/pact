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
  ( c'mpfr_pow
  , dec2Mpfr
  , mpfr2Dec
  , withTemp
  , TransResult
  )
import Data.Decimal (Decimal)
import System.IO.Unsafe (unsafePerformIO)

trans_pow :: Decimal -> Decimal -> TransResult Decimal
trans_pow x y = unsafePerformIO $
  dec2Mpfr x $ \x' ->
  dec2Mpfr y $ \y' ->
  withTemp $ \z' -> do
    c'mpfr_pow z' x' y'
    mpfr2Dec z'
