{-# LANGUAGE BangPatterns #-}
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

import Data.Decimal
#if !defined(ghcjs_HOST_OS)
import Pact.Trans.Types
-- import qualified Pact.Trans.Mpfr as Mpfr
import qualified Pact.Trans.Musl as Musl
#endif
import qualified Pact.Trans.Dec as Dec
import qualified Pact.Trans.Dbl as Dbl
import Pact.Types.Runtime

#if !defined(ghcjs_HOST_OS)
chooseFunction1
  :: (a -> Eval e b)
  -> (a -> Eval e b)
  -> (a -> Eval e b)
  -> Eval e (a -> Eval e b)
chooseFunction1 fp_double fp_musl fp_mpfr =
  ifExecutionFlagSet' FlagDisableNewTrans fp_double =<<
    ifExecutionFlagSet' FlagDisableNewTransMPFR fp_musl fp_mpfr

chooseFunction2
  :: (a -> b -> Eval e c)
  -> (a -> b -> Eval e c)
  -> (a -> b -> Eval e c)
  -> Eval e (a -> b -> Eval e c)
chooseFunction2 fp_double fp_musl fp_mpfr =
  ifExecutionFlagSet' FlagDisableNewTrans fp_double =<<
    ifExecutionFlagSet' FlagDisableNewTransMPFR fp_musl fp_mpfr
#endif

trans_exp :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_exp i x = go
  where
  f = Dbl.dbl_exp
  exp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = exp_double i x
#else
  exp_musl = liftUnDecF i Musl.musl_exp
  exp_mpfr = liftUnDec i f Dec.dec_exp
  go = chooseFunction1 exp_double exp_musl exp_mpfr >>= ($ x)
#endif

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
  f = Dbl.dbl_ln
  ln_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = ln_double i x
#else
  ln_musl = liftUnDecF i Musl.musl_ln
  ln_mpfr = liftUnDec i f Dec.dec_ln
  go = chooseFunction1 ln_double ln_musl ln_mpfr >>= ($ x)
#endif

trans_log :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_log i x y = go
  where
  f = Dbl.dbl_log
  logBase_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = logBase_double i x y
#else
  logBase_musl = liftBinDecF i Musl.musl_log
  logBase_mpfr = liftBinDec i f Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_mpfr
    >>= (\k -> k x y)
#endif

trans_logInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logInt i x y = go
  where
  f = (doubleToTransResult .) . logBase
  logBase_double = liftBinIntF i f
#if defined(ghcjs_HOST_OS)
  go = logBase_double x y
#else
  logBase_musl = liftBinIntF i Musl.musl_log
  logBase_mpfr = liftBinInt i f Dec.dec_log
  go = chooseFunction2 logBase_double logBase_musl logBase_mpfr
    >>= (\k -> k x y)
#endif

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
  f = Dbl.dbl_pow
  pow_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = pow_double i x y
#else
  pow_musl = liftBinDecF i Musl.musl_pow
  pow_mpfr = liftBinDec i f Dec.dec_pow
  go = chooseFunction2 pow_double pow_musl pow_mpfr >>= (\k -> k x y)
#endif

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
  f = Dbl.dbl_sqrt
  sqrt_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = sqrt_double i x
#else
  sqrt_musl = liftUnDecF i Musl.musl_sqrt
  sqrt_mpfr = liftUnDec i f Dec.dec_sqrt
  go = chooseFunction1 sqrt_double sqrt_musl sqrt_mpfr >>= ($ x)
#endif

{-------------------------------------------------------------------------
 -- HELPER FUNCTIONS
 -------------------------------------------------------------------------}

#if !defined(ghcjs_HOST_OS)

liftUnDec
  :: HasInfo i => i
  -> (Double -> TransResult Double)
  -> (Decimal -> TransResult Decimal)
  -> Decimal -> Eval e Decimal
liftUnDec i fp f a = do
  -- Ensure it would yield a value under Double first, since the full
  -- computation could be exceedingly large.
  case fp (dec2F a) of
    TransNumber _ -> pure ()
    _ -> evalError' i "Operation resulted in +- infinity or NaN"
  case f a of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinDec
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDec i fp f a b = do
  -- Ensure it would yield a value under Double first, since the full
  -- computation could be exceedingly large.
  case fp (dec2F a) (dec2F b) of
    TransNumber _ -> pure ()
    _ -> evalError' i "Operation resulted in +- infinity or NaN"
  case a `f` b of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinInt
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Integer -> Integer -> Eval e Integer
liftBinInt i fp f a b = do
  -- Ensure it would yield a value under Double first, since the full
  -- computation could be exceedingly large.
  case fp (int2F a) (int2F b) of
    TransNumber _ -> pure ()
    _ -> evalError' i "Operation resulted in +- infinity or NaN"
  case int2D a `f` int2D b of
    TransNumber num -> pure $ d2Int num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

int2D :: Integer -> Decimal
int2D = fromIntegral
d2Int :: Decimal -> Integer
d2Int = round

#endif

checkDouble :: HasInfo i => i -> TransResult Double -> Eval e Double
checkDouble i r = case r of
  TransNaN r' -> go r'
  TransInf r' -> go r'
  TransNegInf r' -> go r'
  TransNumber r' -> pure r'
  where
  go n = do
    unlessExecutionFlagSet FlagDisablePact43 $
      evalError' i "Operation resulted in +- infinity or NaN"
    pure n

liftUnDecF
  :: HasInfo i => i
  -> (Double -> TransResult Double)
  -> Decimal -> Eval e Decimal
liftUnDecF i f a = do
  out <- checkDouble i (f (dec2F a))
  pure $ f2Dec out

liftBinDecF
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDecF i f a b = do
  out <- checkDouble i (dec2F a `f` dec2F b)
  pure $ f2Dec out

liftBinIntF
  :: HasInfo i => i
  -> (Double -> Double -> TransResult Double)
  -> Integer -> Integer -> Eval e Integer
liftBinIntF i f a b = do
  out <- checkDouble i (int2F a `f` int2F b)
  pure $ f2Int out

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

int2F :: Integer -> Double
int2F = fromIntegral
f2Int :: Double -> Integer
f2Int = round
