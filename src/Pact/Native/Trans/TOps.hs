{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Native.Trans.TOps
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Transcendental math functions.
--

module Pact.Native.Trans.TOps
    (
      trans_exp
    , trans_ln
    , trans_logBase
    , trans_logBaseInt
    , trans_pow
    , trans_sqrt

    , mpfr_exp
    , mpfr_ln
    , mpfr_log
    , mpfr_pow
    , mpfr_sqrt

#if !defined(ghcjs_HOST_OS)
    , c'mpfr_set_default_prec
    , mpfr_arity1
    , mpfr_arity2
#endif
    ) where

import Control.Monad
import Data.Decimal
#if !defined(ghcjs_HOST_OS)
import qualified Pact.Native.Trans.Musl as M
import Pact.Native.Trans.Types
#endif
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
  f = exp
  exp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = exp_double i x
#else
  exp_musl = liftUnDecF i M.trans_exp
  exp_mpfr = liftUnDec i f mpfr_exp
  go = chooseFunction1 exp_double exp_musl exp_mpfr >>= ($ x)
#endif

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
  f = log
  ln_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = ln_double i x
#else
  ln_musl = liftUnDecF i M.trans_ln
  ln_mpfr = liftUnDec i f mpfr_ln
  go = chooseFunction1 ln_double ln_musl ln_mpfr >>= ($ x)
#endif

trans_logBase :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_logBase i x y = go
  where
  f = logBase
  logBase_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = logBase_double i x y
#else
  logBase_musl = liftBinDecF i M.trans_log
  logBase_mpfr = liftBinDec i f mpfr_log
  go = chooseFunction2 logBase_double logBase_musl logBase_mpfr
    >>= (\k -> k x y)
#endif

trans_logBaseInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logBaseInt i x y = go
  where
  f = logBase
  logBase_double = liftBinIntF i f
#if defined(ghcjs_HOST_OS)
  go = logBase_double x y
#else
  logBase_musl = liftBinIntF i M.trans_log
  logBase_mpfr = liftBinInt i f mpfr_log
  go = chooseFunction2 logBase_double logBase_musl logBase_mpfr
    >>= (\k -> k x y)
#endif

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
  f = (**)
  pow_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = pow_double i x y
#else
  pow_musl = liftBinDecF i M.trans_pow
  pow_mpfr = liftBinDec i f mpfr_pow
  go = chooseFunction2 pow_double pow_musl pow_mpfr >>= (\k -> k x y)
#endif

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
  sqrt_double = liftUnDecF i sqrt
#if defined(ghcjs_HOST_OS)
  go = sqrt_double i x
#else
  sqrt_musl = liftUnDecF i M.trans_sqrt
  sqrt_mpfr = liftUnDec i sqrt mpfr_sqrt
  go = chooseFunction1 sqrt_double sqrt_musl sqrt_mpfr >>= ($ x)
#endif

{-------------------------------------------------------------------------
 -- HELPER FUNCTIONS
 -------------------------------------------------------------------------}

#if !defined(ghcjs_HOST_OS)

liftUnDec
  :: HasInfo i => i
  -> (Double -> Double)
  -> (Decimal -> TransResult Decimal)
  -> Decimal -> Eval e Decimal
liftUnDec i fp f a = do
  let !chk = (fp (dec2F a))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
  case f a of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinDec
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDec i fp f a b = do
  let !chk = (fp (dec2F a) (dec2F b))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
  case a `f` b of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinInt
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Integer -> Integer -> Eval e Integer
liftBinInt i fp f a b = do
  let !chk = (fp (int2F a) (int2F b))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
  case int2D a `f` int2D b of
    TransNumber num -> pure $ d2Int num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

int2D :: Integer -> Decimal
int2D = fromIntegral
d2Int :: Decimal -> Integer
d2Int = round

#endif

liftUnDecF :: HasInfo i => i -> (Double -> Double) -> Decimal -> Eval e Decimal
liftUnDecF i f a = do
  let !out = (f (dec2F a))
  unlessExecutionFlagSet FlagDisablePact43 $
    when (isNaN out || isInfinite out) $
      evalError' i "Operation resulted in +- infinity or NaN"
  pure $ f2Dec out

liftBinDecF
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDecF i f a b = do
  let !out = (dec2F a `f` dec2F b)
  unlessExecutionFlagSet FlagDisablePact43 $
    when (isNaN out || isInfinite out) $
      evalError' i "Operation resulted in +- infinity or NaN"
  pure $ f2Dec out

liftBinIntF
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> Integer -> Integer -> Eval e Integer
liftBinIntF i f a b = do
  let !out = (int2F a `f` int2F b)
  unlessExecutionFlagSet FlagDisablePact43 $
    when (isNaN out || isInfinite out) $
      evalError' i "Operation resulted in +- infinity or NaN"
  pure $ f2Int out

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

int2F :: Integer -> Double
int2F = fromIntegral
f2Int :: Double -> Integer
f2Int = round

{-------------------------------------------------------------------------
 -- OPERATIONS
 -------------------------------------------------------------------------}

#if !defined(ghcjs_HOST_OS)

mpfr_exp :: Decimal -> TransResult Decimal
mpfr_exp = mpfr_arity1 c'mpfr_exp

_mpfr_exp2 :: Decimal -> TransResult Decimal
_mpfr_exp2 = mpfr_arity1 c'mpfr_exp2

_mpfr_exp10 :: Decimal -> TransResult Decimal
_mpfr_exp10 = mpfr_arity1 c'mpfr_exp10

mpfr_ln :: Decimal -> TransResult Decimal
mpfr_ln = mpfr_arity1 c'mpfr_log

_mpfr_log2 :: Decimal -> TransResult Decimal
_mpfr_log2 = mpfr_arity1 c'mpfr_log2

_mpfr_log10 :: Decimal -> TransResult Decimal
_mpfr_log10 = mpfr_arity1 c'mpfr_log10

mpfr_log :: Decimal -> Decimal -> TransResult Decimal
mpfr_log = mpfr_arity2 $ \z' x' y' rnd ->
  withTemp $ \x'' ->
  withTemp $ \y'' -> do
    c'mpfr_log x'' x' rnd
    c'mpfr_log y'' y' rnd
    c'mpfr_div z' y'' x'' rnd

mpfr_pow :: Decimal -> Decimal -> TransResult Decimal
mpfr_pow = mpfr_arity2 c'mpfr_pow

mpfr_sqrt :: Decimal -> TransResult Decimal
mpfr_sqrt = mpfr_arity1 c'mpfr_sqrt

#endif
