{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- |
-- Module      :  Pact.Trans.Dec
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins based on the MPFR library.
--

module Pact.Trans.Dec
  ( Decimal
  , dec_exp
  , dec_ln
  , dec_log
  , dec_pow
  , dec_sqrt
  , dec_reduce
  , decIsNaN
  , decIsInfinite
  , decShow
  , numShow
  , toDecimal
  , fromDecimal
  ) where

import qualified Data.Decimal as D
import qualified Numeric.Decimal as N
import qualified Numeric.Decimal.Number as N
import Numeric.Decimal.Arithmetic
import qualified Numeric.Decimal.Operation as Op
import Pact.Trans.Types

type Prec = N.PPlus1 (N.PPlus1 (N.PPlus1 (N.PPlus1 (N.PPlus1 N.P250))))
type Decimal = N.ExtendedDecimal Prec

decShow :: D.Decimal -> String
decShow D.Decimal { D.decimalPlaces = e, D.decimalMantissa = c } =
  "Dec { places = "
    ++ show e ++ ", mantissa = "
    ++ show c ++ " }"

numShow :: Decimal -> String
numShow N.Num { N.sign = s, N.coefficient = c, N.exponent = e } =
  "Num { sign = "
    ++ (case s of Op.Neg -> "Neg"; Op.Pos -> "Pos") ++ ", coefficient = "
    ++ show c ++ ", exponent = "
    ++ show e ++ " }"
numShow _ = error "unexpected"

toDecimal :: Decimal -> TransResult D.Decimal
toDecimal N.Num { N.sign = s, N.coefficient = c, N.exponent = e }
  -- If the number has more digits than the finite precision being used,
  -- truncate the number.
  | e < -255 =
    TransNumber $
      D.Decimal { D.decimalPlaces = 255
                , D.decimalMantissa =
                  (case s of
                     Op.Neg -> negate
                     Op.Pos -> id) (fromIntegral c)
                    `div` (10^(-(e - (-255))))
                }
  | e < 0 =
    TransNumber $
      D.Decimal { D.decimalPlaces = negate (fromIntegral e)
                , D.decimalMantissa =
                  (case s of
                     Op.Neg -> negate
                     Op.Pos -> id) (fromIntegral c)
                }
  | otherwise =
    TransNumber $
      D.Decimal { D.decimalPlaces = 0
                , D.decimalMantissa =
                  (case s of
                     Op.Neg -> negate
                     Op.Pos -> id) (fromIntegral c) * (10^e)
                }
toDecimal (N.Inf Op.Neg) = TransNegInf 0
toDecimal (N.Inf Op.Pos) = TransInf 0
toDecimal N.NaN {} = TransNaN 0

fromDecimal :: D.Decimal -> Decimal
fromDecimal D.Decimal { D.decimalPlaces = e, D.decimalMantissa = c } =
  N.Num { N.sign = if c < 0
                   then Op.Neg
                   else Op.Pos
        , N.coefficient = fromIntegral (abs c)
        , N.exponent = negate (toInteger e)
        }

decIsNaN :: Decimal -> Bool
decIsNaN n = case n of
  N.NaN{} -> True
  _num    -> False

decIsInfinite :: Decimal -> Bool
decIsInfinite n = case n of
  N.Inf{} -> True
  _num    -> False

reduce_ :: (N.Precision p, N.Rounding r) => N.Decimal p r -> N.Decimal p r
reduce_ n@N.Num { N.coefficient = c, N.exponent = e }
  | c == 0 =         n {                    N.exponent = 0     }
  | r == 0 = reduce_ n { N.coefficient = q, N.exponent = e + 1 }
  where
    (q, r) = c `quotRem` 10
reduce_ n = n

dec_reduce :: D.Decimal -> D.Decimal
dec_reduce x =
  case toDecimal (reduce_ (fromDecimal x :: Decimal)) of
    TransNumber n -> n
    e -> error $ "reduce error: " ++ show e

dec_exp :: D.Decimal -> TransResult D.Decimal
dec_exp x =
  case evalArith (Op.exp (fromDecimal x :: Decimal)
                  :: Arith Prec N.RoundHalfEven Decimal)
       extendedDefaultContext of
    Left err -> error $ "exp error: " ++ show err
    Right n -> toDecimal n

dec_ln :: D.Decimal -> TransResult D.Decimal
dec_ln x =
  case evalArith (Op.ln (fromDecimal x :: Decimal)
                  :: Arith Prec N.RoundHalfEven Decimal)
       extendedDefaultContext of
    Left err -> error $ "ln error: " ++ show err
    Right n -> toDecimal n

dec_log :: D.Decimal -> D.Decimal -> TransResult D.Decimal
dec_log D.Decimal{ D.decimalPlaces = _, D.decimalMantissa = 0 } _ =
  TransNumber 0.0
dec_log b x =
  case dec_ln x of
    TransNumber x' ->
      case dec_ln b of
        TransNumber b' -> TransNumber (x' / b')
        b' -> b'
    x' -> x'

dec_pow :: D.Decimal -> D.Decimal -> TransResult D.Decimal
dec_pow D.Decimal{ D.decimalPlaces = _, D.decimalMantissa = 0 }
        D.Decimal{ D.decimalPlaces = _, D.decimalMantissa = 0 } =
  TransNumber 1.0
dec_pow x y =
  case evalArith (Op.power (fromDecimal x :: Decimal)
                   (fromDecimal y :: Decimal)
                  :: Arith Prec N.RoundHalfEven Decimal)
       extendedDefaultContext of
    Left err -> error $ "pow error: " ++ show err
    Right n -> toDecimal n

dec_sqrt :: D.Decimal -> TransResult D.Decimal
dec_sqrt x =
  case evalArith (Op.squareRoot (fromDecimal x :: Decimal)
                  :: Arith Prec N.RoundHalfEven Decimal)
       extendedDefaultContext of
    Left err -> error $ "sqrt error: " ++ show err
    Right n -> toDecimal n
