{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.IORef(writeIORef, readIORef)
import GHC.Int(Int(..))
import Numeric.Decimal.Arithmetic 
import qualified Numeric.Decimal.Number as NDec


#if !defined(ghcjs_HOST_OS)
import qualified Pact.Trans.Musl as Musl
#endif
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
  gUsed <- view eeGas >>= liftIO . readIORef
  gLim <- view (eeGasEnv . geGasLimit)
  pure (Dec.TransGasParams (fromIntegral gUsed) (fromIntegral gLim) chargePactArith)

trans_exp :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_exp i x = go
  where
  f = Dbl.dbl_exp
  exp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  exp_musl = exp_double
#else
  exp_musl = liftUnDecF i Musl.musl_exp
#endif
  exp_dec = liftUnDec i Dec.dec_exp
  go = chooseFunction1 exp_double exp_musl exp_dec >>= ($ x)

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
  f = Dbl.dbl_ln
  ln_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  ln_musl = ln_double
#else
  ln_musl = liftUnDecF i Musl.musl_ln
#endif
  ln_dec = liftUnDec i Dec.dec_ln
  go = chooseFunction1 ln_double ln_musl ln_dec >>= ($ x)

trans_log :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_log i x y = go
  where
  f = Dbl.dbl_log
  logBase_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  logBase_musl = logBase_double
#else
  logBase_musl = liftBinDecF i Musl.musl_log
#endif
  logBase_dec = liftBinDec i Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_dec
    >>= (\k -> k x y)

trans_logInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logInt i x y = go
  where
  f = (doubleToTransResult .) . logBase
  logBase_double = liftBinIntF i f
#if defined(ghcjs_HOST_OS)
  logBase_musl = logBase_double
#else
  logBase_musl = liftBinIntF i Musl.musl_log
#endif
  logBase_dec = liftBinInt i Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_dec
    >>= (\k -> k x y)

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
  f = Dbl.dbl_pow
  pow_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  pow_musl = pow_double
#else
  pow_musl = liftBinDecF i Musl.musl_pow
#endif
  pow_dec = liftBinDec i Dec.dec_pow
  go = chooseFunction2 pow_double pow_musl pow_dec >>= (\k -> k x y)

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
  f = Dbl.dbl_sqrt
  sqrt_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  sqrt_musl = sqrt_double
#else
  sqrt_musl = liftUnDecF i Musl.musl_sqrt
#endif
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

chargePactArith :: GasArithOp a b c d -> Arith p r ()
chargePactArith (GasArithOp _ l r) = 
  chargeArithGas (fromIntegral (decimalCost l + decimalCost r + 1))

decimalCost :: NDec.Decimal a b -> Gas
decimalCost (NDec.Num _sign coeff _exp) 
  | intValue < threshold = 0
  | otherwise = 
    let !nbytes = (I# (IntLog.integerLog2# intValue) + 1) `quot` 8
    in fromIntegral (nbytes * nbytes `quot` 100)
  where
  threshold :: Integer
  threshold = (10 :: Integer) ^ (80 :: Integer)
  intValue :: Integer
  intValue = fromIntegral coeff
-- Todo: should there be a penalty on `NaN` or `Inf`?
decimalCost _ = 0

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
  TransGasExceeded gUsed -> do
    gasLimit <- view (eeGasEnv . geGasLimit)
    putGas (fromIntegral gUsed)
    throwErr GasError (getInfo i) $ 
      "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty (fromIntegral gUsed :: Gas)
  where
  go n = do
    unlessExecutionFlagSet FlagDisablePact43 $
      evalError' i "Operation resulted in +- infinity or NaN"
    pure n

chargeGasAmt :: HasInfo a => a -> Gas -> Eval e ()
chargeGasAmt i gUsed = do
    gasLimit <- view (eeGasEnv . geGasLimit)
    putGas gUsed
    when (gUsed > fromIntegral gasLimit) $
      throwErr GasError (getInfo i) $ 
        "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty gUsed

checkGasTransResult :: HasInfo i => i -> TransResult (a, Word64) -> Eval e a
checkGasTransResult i r = case r of
  TransNaN r' -> go r'
  TransInf r' -> go r'
  TransNegInf r' -> go r'
  TransNumber (r', gas)  -> 
    r' <$ chargeGasAmt i (fromIntegral gas)
  TransGasExceeded gUsed -> do
    gasRef <- view eeGas
    gasLimit <- view (eeGasEnv . geGasLimit)
    liftIO (writeIORef gasRef (fromIntegral gUsed))
    throwErr GasError (getInfo i) $ 
      "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty (fromIntegral gUsed :: Gas)
  where
  go (_, _) = evalError' i "Operation resulted in +- infinity or NaN"
    
