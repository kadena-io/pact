{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

module Pact.Native.DecimalOps where

import Control.Monad.State.Strict
import Control.Monad.Trans(lift)
import Data.Coerce(coerce)

import Numeric.Decimal
import Numeric.Decimal.Arithmetic(Signal(..), Arith(..))
import Numeric.Decimal.Number
import qualified Numeric.Decimal.Operation as Ops
import qualified Numeric.Decimal.Arithmetic as Arith

import Pact.Types.Gas

data GasArithState
  = GasArithState
  { _garGas :: Gas
  }

newtype GasArith p r a
  = GasArith
  { _runGasArith :: StateT GasArithState (Arith p r) a }
  deriving
  (Functor, Applicative, Monad, MonadState (GasArithState))
  via (StateT GasArithState (Arith p r))

data GasBasicArith p r p' r'
  = GasMult (Decimal p r) (Decimal p' r')
  | GasAdd (Decimal p r) (Decimal p' r')
  | GasSub (Decimal p r) (Decimal p' r')
  | GasDiv (Decimal p r) (Decimal p' r')
  deriving Show

gasBinaryArith :: GasBasicArith a b c d  -> GasArith p r ()
gasBinaryArith _ = pure ()

multiply :: (Precision p, Rounding r) =>
  Decimal a b -> Decimal c d -> GasArith p r (Decimal p r)
multiply a b = gasBinaryArith (GasMult a b) *>  GasArith (lift (Ops.multiply a b))

divide :: (FinitePrecision p, Rounding r) =>
  Decimal a b -> Decimal c d -> GasArith p r (Decimal p r)
divide a b = gasBinaryArith (GasMult a b) *>  GasArith (lift (Ops.divide a b))

add
  :: (Precision p, Rounding r) =>
  Decimal a b -> Decimal c d -> GasArith p r (Decimal p r)
add a b = gasBinaryArith (GasAdd a b) *>  GasArith (lift (Ops.multiply a b))

subArith :: GasArith a b (Decimal a b) -> GasArith p r (Decimal a b)
subArith = undefined

reciprocal :: (FinitePrecision p, Rounding r) => Decimal a b -> GasArith p r (Decimal p r)
reciprocal = GasArith . lift . Ops.divide one

roundDecimal = undefined

raiseSignal :: Arith.Signal -> Decimal p r -> GasArith p r (Decimal p r)
raiseSignal = undefined

generalRules1 = undefined

exp :: FinitePrecision p => Decimal a b -> GasArith p r (Decimal p RoundHalfEven)
exp x@Num { sign = s, coefficient = c }
  | c == 0    = return one
  | s == Neg  = subArith (maclaurin x { sign = Pos } >>= reciprocal) >>=
                subRounded >>= result
  | otherwise = subArith (maclaurin x) >>= subRounded >>= result

  where multiplyExact :: Decimal a b -> Decimal c d
                      -> GasArith PInfinite RoundHalfEven
                         (Decimal PInfinite RoundHalfEven)
        multiplyExact = multiply

        maclaurin :: FinitePrecision p => Decimal a b
                  -> GasArith p RoundHalfEven (Decimal p RoundHalfEven)
        maclaurin x
          | adjustedExponent x >= 0 = subArith (subMaclaurin x) >>= subRounded
          | otherwise = sum one one one one
          where sum :: FinitePrecision p
                    => Decimal p RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> Decimal PInfinite RoundHalfEven
                    -> GasArith p RoundHalfEven (Decimal p RoundHalfEven)
                sum s num den n = do
                  num' <- subArith (multiplyExact num x)
                  den' <- subArith (multiplyExact den n)
                  s' <- add s =<< divide num' den'
                  if s' == s then return s'
                    else sum s' num' den' =<< subArith (add n one)

        subMaclaurin :: FinitePrecision p => Decimal a b
                     -> GasArith p RoundHalfEven (Decimal p RoundHalfEven)
        subMaclaurin x = subArith (multiplyExact x oneHalf) >>= maclaurin >>=
          \r -> multiply r r

        subRounded :: Precision p
                   => Decimal (PPlus1 (PPlus1 p)) a
                   -> GasArith p r (Decimal p RoundHalfEven)
        subRounded = subArith . roundDecimal

        result :: Decimal p a -> GasArith p r (Decimal p a)
        result r = coerce <$> (raiseSignal Rounded =<< raiseSignal Inexact r')
          where r' = coerce r

exp n@Inf { sign = s }
  | s == Pos  = return (coerce n)
  | otherwise = return zero
exp x = coerce <$> generalRules1 x
