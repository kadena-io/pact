{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- |
-- Module      :  Pact.Native.Trans
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Transcendental math functions.
--

module Pact.Native.Trans
    ( trans_exp
    , trans_ln
    , trans_log
    , trans_logInt
    , trans_pow
    , trans_sqrt
    ) where

import Control.Lens
import Control.Monad(when)
import Data.Decimal
import Data.Word(Word64)
import GHC.Integer(smallInteger)
import qualified Numeric.Decimal.Arithmetic as NDec
import qualified Numeric.Decimal.Number as NDec


import qualified Pact.Trans.Musl as Musl
import qualified Pact.Trans.Dec as Dec
import qualified Pact.Trans.Dbl as Dbl
import qualified GHC.Integer.Logarithms as IntLog
import Pact.Trans.Types
import Pact.Types.Runtime
import Pact.Types.Pretty
import Pact.Gas
import Pact.Trans.Dec (TransGasParams)


chooseFunction1
  :: (a -> Eval e b)
  -> (a -> Eval e b)
  -> (a -> Eval e b)
  -> Eval e (a -> Eval e b)
chooseFunction1 fp_double fp_musl fp_dec =
  ifExecutionFlagSet' FlagDisableNewTrans fp_double =<<
    ifExecutionFlagSet' FlagDisableNewTransDec fp_musl fp_dec

chooseFunction2
  :: (a -> b -> Eval e c)
  -> (a -> b -> Eval e c)
  -> (a -> b -> Eval e c)
  -> Eval e (a -> b -> Eval e c)
chooseFunction2 fp_double fp_musl fp_dec =
  ifExecutionFlagSet' FlagDisableNewTrans fp_double =<<
    ifExecutionFlagSet' FlagDisableNewTransDec fp_musl fp_dec

getTransGasParams :: Eval e Dec.TransGasParams
getTransGasParams = do
  MilliGas gUsedMillis <- getGas
  gEnv <- view eeGasEnv
  let chargeFn = getCharge (view (geGasModel . to gasModelType) gEnv)
      MilliGasLimit (MilliGas gLim) = view geGasLimit gEnv
  pure (Dec.TransGasParams (fromIntegral gUsedMillis) (fromIntegral gLim) chargeFn)
  where
  getCharge (ConstantGasModel g) = const (NDec.chargeArithGas (fromIntegral g))
  getCharge TableGasModel = chargePactArith

trans_exp :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_exp i x = go
  where
  f = Dbl.dbl_exp
  exp_double = liftUnDecF i f
  exp_musl = liftUnDecF i Musl.musl_exp
  exp_dec = liftUnDec i Dec.dec_exp
  go = chooseFunction1 exp_double exp_musl exp_dec >>= ($ x)

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
  f = Dbl.dbl_ln
  ln_double = liftUnDecF i f
  ln_musl = liftUnDecF i Musl.musl_ln
  ln_dec = liftUnDec i Dec.dec_ln
  go = chooseFunction1 ln_double ln_musl ln_dec >>= ($ x)

trans_log :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_log i x y = go
  where
  f = Dbl.dbl_log
  logBase_double = liftBinDecF i f
  logBase_musl = liftBinDecF i Musl.musl_log
  logBase_dec = liftBinDec i Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_dec
    >>= (\k -> k x y)

trans_logInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logInt i x y = go
  where
  f = (doubleToTransResult .) . logBase
  logBase_double = liftBinIntF i f
  logBase_musl = liftBinIntF i Musl.musl_log
  logBase_dec = liftBinInt i Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_dec
    >>= (\k -> k x y)

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
  f = Dbl.dbl_pow
  pow_double = liftBinDecF i f
  pow_musl = liftBinDecF i Musl.musl_pow
  pow_dec = liftBinDec i Dec.dec_pow
  go = chooseFunction2 pow_double pow_musl pow_dec >>= (\k -> k x y)

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
  f = Dbl.dbl_sqrt
  sqrt_double = liftUnDecF i f
  sqrt_musl = liftUnDecF i Musl.musl_sqrt
  sqrt_dec = liftUnDec i Dec.dec_sqrt
  go = chooseFunction1 sqrt_double sqrt_musl sqrt_dec >>= ($ x)

{-------------------------------------------------------------------------
 -- HELPER FUNCTIONS
 -------------------------------------------------------------------------}

liftUnDec
  :: HasInfo i => i
  -> (TransGasParams -> Decimal -> TransResult (Decimal, Word64))
  -> Decimal -> Eval e Decimal
liftUnDec i f a = do
  gc <- getTransGasParams
  checkGasTransResult i (f gc a)

liftBinDec
  :: HasInfo i => i
  -> (TransGasParams -> Decimal -> Decimal -> TransResult (Decimal, Word64))
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDec i f a b = do
  gc <- getTransGasParams
  checkGasTransResult i (f gc a b)

liftBinInt
  :: HasInfo i => i
  -> (TransGasParams -> Decimal -> Decimal -> TransResult (Decimal, Word64))
  -> Integer -> Integer -> Eval e Integer
liftBinInt i f a b = do
  gc <- getTransGasParams
  d2Int <$> checkGasTransResult i (f gc (int2D a) (int2D b))

chargePactArith :: NDec.GasArithOp a b c d -> NDec.Arith p r ()
chargePactArith (NDec.GasArithOp o l r) = NDec.chargeArithGas $ case o of
  NDec.ArithAdd -> decimalAddCost l r
  NDec.ArithMult -> decimalMulCost l r
  NDec.ArithDiv -> decimalDivCost l r

-- Below 512 bits: we charge a f(x) = 0.2x+10
-- Above 512 bits: f(x) = 0.01x^2
-- The addition operation is realistically dominated in the decimal-arithmetic implementation
-- by the 10^expdiff calculation, then the addition. It's still quite cheap to do
-- basic integer arithmetic, so we add a scaling cost based on the bits of the largest coefficient.
-- Thus we will base the cost off of integer decimal addition which is O(n).
decimalAddCost :: NDec.Decimal a b -> NDec.Decimal c d -> Word64
decimalAddCost (NDec.Num _xs xc xe) (NDec.Num _ys yc ye) =
    fromInteger $ if nbits <= threshold then (nbits `div` 5) + 20 else (nbits*nbits) `div` 100
  where
  nbits :: Integer
  nbits = smallInteger (IntLog.integerLog2# (toInteger (max xac yac)))
  threshold :: Integer
  threshold = 512
  expdiff = abs (xe - ye)
  (xac, yac)
    | xe == ye = (xc, yc)
    | xe > ye = (xc * 10^expdiff, yc)
    | otherwise = (xc, yc * 10^expdiff)
-- MilliGas $ fromIntegral $ (I# (IntLog.integerLog2# coeff))
-- Should there be a penalty on `NaN` or `Inf`? it might cause a problem when used in intermediate calcs
-- so probably not
decimalAddCost _ _ = 1

-- Below 512 bits: we charge a f(x) = 0.2x+20. We can reuse the addition
-- cost.
-- Above 512 bits: f(x) = x^2
-- The addition operation is realistically dominated in the decimal-arithmetic implementation
-- by the 10^expdiff calculation, then the addition. It's still quite cheap to do
-- basic integer arithmetic, so we add a scaling cost based on the bits of the largest coefficient.
-- Thus we will base the cost off of integer decimal addition which is O(n).
decimalMulCost :: NDec.Decimal a b -> NDec.Decimal c d -> Word64
decimalMulCost (NDec.Num _xs xc _xe) (NDec.Num _ys yc _ye) =
  fromInteger $
    if nbits <= threshold then (nbits `div` 5) + 20 else (nbits*nbits) `div` 1000
  where
  threshold :: Integer
  threshold = 512
  nbits :: Integer
  nbits = smallInteger (IntLog.integerLog2# (toInteger (max xc yc)))
-- MilliGas $ fromIntegral $ (I# (IntLog.integerLog2# coeff))
-- Should there be a penalty on `NaN` or `Inf`? it might cause a problem when used in intermediate calcs
-- so probably not
decimalMulCost _ _ = 1

decimalDivCost :: NDec.Decimal a b -> NDec.Decimal c d -> Word64
decimalDivCost (NDec.Num _xs xc xe) (NDec.Num _ys yc ye) =
  fromInteger $
    if nbits <= threshold then (nbits * nbits) `div` 1000 else (nbits*nbits) `div` 100
  where
  nbits :: Integer
  nbits = smallInteger (IntLog.integerLog2# (toInteger (max xac yac)))
  threshold :: Integer
  threshold = 512
  expdiff = abs (xe - ye)
  (xac, yac)
    | xe == ye = (xc, yc)
    | xe > ye = (xc * 10^expdiff, yc)
    | otherwise = (xc, yc * 10^expdiff)
-- MilliGas $ fromIntegral $ (I# (IntLog.integerLog2# coeff))
-- Should there be a penalty on `NaN` or `Inf`? it might cause a problem when used in intermediate calcs
-- so probably not
decimalDivCost _ _ = 1


int2D :: Integer -> Decimal
int2D = fromIntegral
d2Int :: Decimal -> Integer
d2Int = round

liftUnDecF
  :: HasInfo i => i
  -> (Double -> TransResult Double)
  -> Decimal -> Eval e Decimal
liftUnDecF i f a = f2Dec <$> checkTransResult i (f (dec2F a))

liftBinDecF
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDecF i f a b = f2Dec <$> checkTransResult i (dec2F a `f` dec2F b)

liftBinIntF
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> Integer -> Integer -> Eval e Integer
liftBinIntF i f a b = f2Int <$> checkTransResult i (int2F a `f` int2F b)

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

int2F :: Integer -> Double
int2F = fromIntegral
f2Int :: Double -> Integer
f2Int = round

checkTransResult :: HasInfo i => i -> TransResult a -> Eval e a
checkTransResult i r = case r of
  TransNaN r' -> go r'
  TransInf r' -> go r'
  TransNegInf r' -> go r'
  TransNumber r' -> pure r'
  TransGasExceeded g -> do
    let gUsed = MilliGas (fromIntegral g)
    MilliGasLimit gl <- view (eeGasEnv . geGasLimit)
    -- putGas (fromIntegral gUsed)
    if gUsed > gl then do
        let gasLimit = milliGasToGas gl
        putGas gUsed
        throwErr GasError (getInfo i) $
            "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty (milliGasToGas gUsed :: Gas)
    -- Defensive case: if for whatever reason this signalld overflow but the amount is smaller than
    -- the gas limit, then we will charge the gas limit anyway.
    else do
        let gasLimit = milliGasToGas gl
        putGas gl
        throwErr GasError (getInfo i) $
            "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty gasLimit
  where
  go n = do
    unlessExecutionFlagSet FlagDisablePact43 $
      evalError' i "Operation resulted in +- infinity or NaN"
    pure n

chargeGasAmt :: HasInfo a => a -> Word64 -> Eval e ()
chargeGasAmt i gUsedRaw = do
    MilliGasLimit gl <- view (eeGasEnv . geGasLimit)
    currGas <- getGas
    let gUsed = MilliGas (fromIntegral gUsedRaw)
    when ((gUsed > gl) || (gUsed < currGas)) $ do
      putGas (max gUsed gl)
      throwErr GasError (getInfo i) $ do
        let gasLimit = milliGasToGas gl
        "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty (max gasLimit (milliGasToGas gUsed))
    putGas gUsed

checkGasTransResult :: HasInfo i => i -> TransResult (a, Word64) -> Eval e a
checkGasTransResult i r = case r of
  TransNaN r' -> go r'
  TransInf r' -> go r'
  TransNegInf r' -> go r'
  TransNumber (r', gas)  ->
    r' <$ chargeGasAmt i gas
  TransGasExceeded g -> do
    let gUsed = MilliGas (fromIntegral g)
    MilliGasLimit gl <- view (eeGasEnv . geGasLimit)
    if gUsed > gl then do
        let gasLimit = milliGasToGas gl
        putGas gUsed
        throwErr GasError (getInfo i) $
            "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty (milliGasToGas gUsed :: Gas)
    -- Defensive case: if for whatever reason this signalld overflow but the amount is smaller than
    -- the gas limit, then we will charge the gas limit anyway.
    else do
        let gasLimit = milliGasToGas gl
        putGas gl
        throwErr GasError (getInfo i) $
            "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty gasLimit
  where
  go (_, _) = evalError' i "Operation resulted in +- infinity or NaN"
