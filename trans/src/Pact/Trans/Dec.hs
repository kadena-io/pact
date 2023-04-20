{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      :  Pact.Trans.Dec
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins based on the MPFR library.
--

module Pact.Trans.Dec
  ( dec_exp
  , dec_ln
  , dec_log
  , dec_pow
  , dec_sqrt
  ) where

import qualified Data.Decimal as Decimal
import Numeric.Decimal hiding (Precision)
import Numeric.Decimal.Arithmetic
import qualified Numeric.Decimal.Operation as Op
import Pact.Trans.Types

type Precision = P43

dec_exp :: Decimal.Decimal -> TransResult Decimal.Decimal
dec_exp x = TransNumber $ fromRational $ toRational $
  case evalArith (Op.reduce =<< Op.exp (fromRational (toRational x) :: ExtendedDecimal Precision)
                  :: Arith Precision RoundHalfEven (ExtendedDecimal Precision))
       extendedDefaultContext of
    Left err -> error $ "exp error: " ++ show err
    Right n -> n

dec_ln :: Decimal.Decimal -> TransResult Decimal.Decimal
dec_ln x = TransNumber $ fromRational $ toRational $
  case evalArith (Op.reduce =<< Op.ln (fromRational (toRational x) :: ExtendedDecimal Precision)
                  :: Arith Precision RoundHalfEven (ExtendedDecimal Precision))
       extendedDefaultContext of
    Left err -> error $ "ln error: " ++ show err
    Right n -> n

dec_log :: Decimal.Decimal -> Decimal.Decimal -> TransResult Decimal.Decimal
dec_log b x =
  case dec_ln x of
    TransNumber x' ->
      case dec_ln b of
        TransNumber b' -> TransNumber (x' / b')
        b' -> b'
    x' -> x'

dec_pow :: Decimal.Decimal -> Decimal.Decimal -> TransResult Decimal.Decimal
dec_pow x y = TransNumber $ fromRational $ toRational $
  case evalArith (Op.reduce =<< Op.power (fromRational (toRational x) :: ExtendedDecimal Precision)
                   (fromRational (toRational y) :: ExtendedDecimal Precision)
                  :: Arith Precision RoundHalfEven (ExtendedDecimal Precision))
       extendedDefaultContext of
    Left err -> error $ "pow error: " ++ show err
    Right n -> n

dec_sqrt :: Decimal.Decimal -> TransResult Decimal.Decimal
dec_sqrt x = TransNumber $ fromRational $ toRational $
  case evalArith (Op.reduce =<< Op.squareRoot (fromRational (toRational x) :: ExtendedDecimal Precision)
                  :: Arith Precision RoundHalfEven (ExtendedDecimal Precision))
       extendedDefaultContext of
    Left err -> error $ "sqrt error: " ++ show err
    Right n -> n
