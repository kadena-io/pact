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

{-
decShow :: D.Decimal -> String
decShow D.Decimal { D.decimalPlaces = e, D.decimalMantissa = c } =
  "Dec { places = "
    ++ show e ++ ", mantissa = "
    ++ show c ++ " }"

numShow :: Decimal p r -> String
numShow N.Num { N.sign = s, N.coefficient = c, N.exponent = e } =
  "Num { sign = "
    ++ (case s of Op.Neg -> "Neg"; Op.Pos -> "Pos") ++ ", coefficient = "
    ++ show c ++ ", exponent = "
    ++ show e ++ " }"
numShow _ = error "unexpected"
-}

toDecimal :: Decimal p r -> TransResult D.Decimal
toDecimal N.Num { N.sign = s, N.coefficient = c, N.exponent = e }
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

fromDecimal :: D.Decimal -> Decimal p r
fromDecimal D.Decimal { D.decimalPlaces = e, D.decimalMantissa = c } =
  N.Num { N.sign = if c < 0
                   then Op.Neg
                   else Op.Pos
        , N.coefficient = fromIntegral (abs c)
        , N.exponent = negate (toInteger e)
        }

reduce_ :: (Precision p, Rounding r) => Decimal p r -> Decimal p r
reduce_ n@N.Num { N.coefficient = c, N.exponent = e }
  | c == 0 =         n {                    N.exponent = 0     }
  | r == 0 = reduce_ n { N.coefficient = q, N.exponent = e + 1 }
  where
    (q, r) = c `quotRem` 10
reduce_ n = n

dec_reduce :: D.Decimal -> D.Decimal
dec_reduce x =
  case toDecimal (reduce_ (fromDecimal x :: ExtendedDecimal Prec)) of
    TransNumber n -> n
    e -> error $ "reduce error: " ++ show e

dec_exp :: D.Decimal -> TransResult D.Decimal
dec_exp x =
  case evalArith (Op.exp (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "exp error: " ++ show err
    Right n -> toDecimal n

dec_ln :: D.Decimal -> TransResult D.Decimal
dec_ln x =
  case evalArith (Op.ln (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
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
  case evalArith (Op.power (fromDecimal x :: ExtendedDecimal Prec)
                   (fromDecimal y :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "pow error: " ++ show err
    Right n -> toDecimal n

dec_sqrt :: D.Decimal -> TransResult D.Decimal
dec_sqrt x =
  case evalArith (Op.squareRoot (fromDecimal x :: ExtendedDecimal Prec)
                  :: Arith Prec RoundHalfEven (ExtendedDecimal Prec))
       extendedDefaultContext of
    Left err -> error $ "sqrt error: " ++ show err
    Right n -> toDecimal n
