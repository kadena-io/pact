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
  , dec_reduce
  ) where

import qualified Data.Decimal as D
import Numeric.Decimal
import qualified Numeric.Decimal.Number as N
import Numeric.Decimal.Arithmetic
import qualified Numeric.Decimal.Operation as Op
import Pact.Trans.Types

type Prec = P100

toDecimal :: Decimal p r -> TransResult D.Decimal
toDecimal N.Num { N.sign = s, N.coefficient = c, N.exponent = e } =
  TransNumber $
    D.Decimal { D.decimalPlaces = negate (fromIntegral e)
              , D.decimalMantissa =
                (case s of
                   Op.Neg -> negate
                   Op.Pos -> id) (fromIntegral c)
              }
toDecimal (N.Inf Op.Neg) = TransNegInf 0
toDecimal (N.Inf Op.Pos) = TransInf 0
toDecimal N.NaN {} = TransNaN 0

fromDecimal :: D.Decimal -> Decimal p r
fromDecimal D.Decimal { D.decimalPlaces = e, D.decimalMantissa = c } =
  N.Num { N.sign = if c < 0
                   then Op.Neg
                   else Op.Pos
        , N.coefficient = fromIntegral c
        , N.exponent = negate (toInteger e)
        }

dec_reduce :: D.Decimal -> D.Decimal
dec_reduce x =
  case evalArith (Op.reduce (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "reduce error: " ++ show err
    Right n -> case toDecimal $ n of
      TransNumber n' -> n'
      e -> error $ "reduce error: " ++ show e

dec_exp :: D.Decimal -> TransResult D.Decimal
dec_exp x = toDecimal $
  case evalArith (Op.reduce =<< Op.exp (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "exp error: " ++ show err
    Right n -> n

dec_ln :: D.Decimal -> TransResult D.Decimal
dec_ln x = toDecimal $
  case evalArith (Op.reduce =<< Op.ln (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "ln error: " ++ show err
    Right n -> n

dec_log :: D.Decimal -> D.Decimal -> TransResult D.Decimal
dec_log b x =
  case dec_ln x of
    TransNumber x' ->
      case dec_ln b of
        TransNumber b' -> TransNumber (x' / b')
        b' -> b'
    x' -> x'

dec_pow :: D.Decimal -> D.Decimal -> TransResult D.Decimal
dec_pow x y = toDecimal $
  case evalArith (Op.reduce =<< Op.power (fromDecimal x :: ExtendedDecimal Prec)
                   (fromDecimal y :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "pow error: " ++ show err
    Right n -> n

dec_sqrt :: D.Decimal -> TransResult D.Decimal
dec_sqrt x = toDecimal $
  case evalArith (Op.reduce =<< Op.squareRoot (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "sqrt error: " ++ show err
    Right n -> n
