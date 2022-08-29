-- |
-- Module      :  Pact.Native.Trans.Exp
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Exp
    ( trans_exp
    , trans_exp2
    , trans_exp10
    ) where

import Pact.Native.Trans.Types
  ( c'mpfr_exp
  , c'mpfr_exp2
  , c'mpfr_exp10
  , TransResult
  , trans_arity1
  )
import Data.Decimal (Decimal)

trans_exp :: Decimal -> TransResult Decimal
trans_exp = trans_arity1 c'mpfr_exp

trans_exp2 :: Decimal -> TransResult Decimal
trans_exp2 = trans_arity1 c'mpfr_exp2

trans_exp10 :: Decimal -> TransResult Decimal
trans_exp10 = trans_arity1 c'mpfr_exp10
