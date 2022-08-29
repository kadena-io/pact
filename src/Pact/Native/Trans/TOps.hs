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

#define MODE_DOUBLE 1
#define MODE_MUSL 2
#define MODE_MPFR 3

#if defined(ghcjs_HOST_OS)
#define FP_MODE MODE_DOUBLE
#else
#define FP_MODE MODE_MPFR
#endif

module Pact.Native.Trans.TOps
    (
#if FP_MODE != MODE_DOUBLE
      TransResult(..),
#endif
      trans_exp
    , trans_ln
    , trans_logBase
    , trans_logBaseInt
    , trans_pow
    , trans_sqrt
    ) where

import Control.Monad
import Data.Decimal
#if FP_MODE != MODE_DOUBLE
#if FP_MODE == MODE_MPFR
import qualified Pact.Native.Trans.Pow as T
import qualified Pact.Native.Trans.Log as T
import qualified Pact.Native.Trans.Sqrt as T
import qualified Pact.Native.Trans.Exp as T
#elif FP_MODE == MODE_MUSL
import qualified Pact.Native.Trans.Musl as M
#endif
import Pact.Native.Trans.Types
#endif
import Pact.Types.Runtime

trans_exp :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_exp i x = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_exp
#else
  f = exp
#endif
  fp = liftUnDecF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftUnDec i f T.trans_exp
    k x
#endif

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_ln
#else
  f = log
#endif
  fp = liftUnDecF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftUnDec i f T.trans_ln
    k x
#endif

trans_logBase :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_logBase i x y = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_log
#else
  f = logBase
#endif
  fp = liftBinDecF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x y
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftBinDec i f T.trans_logBase
    k x y
#endif

trans_logBaseInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logBaseInt i x y = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_log
#else
  f = logBase
#endif
  fp = liftBinIntF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x y
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftBinInt i f T.trans_logBase
    k x y
#endif

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_pow
#else
  f = (**)
#endif
  fp = liftBinDecF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x y
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftBinDec i f T.trans_pow
    k x y
#endif

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
#if FP_MODE == MODE_MUSL
  f = M.trans_sqrt
#else
  f = sqrt
#endif
  fp = liftUnDecF i f
#if FP_MODE == MODE_DOUBLE || FP_MODE == MODE_MUSL
  go = fp x
#else
  go = do
    k <- ifExecutionFlagSet' FlagDisableNewTrans fp $
          liftUnDec i f T.trans_sqrt
    k x
#endif

{-------------------------------------------------------------------------
 -- HELPER FUNCTIONS
 -------------------------------------------------------------------------}

#if FP_MODE == MODE_MPFR

#define DOUBLE_GUARD 1

liftUnDec
  :: HasInfo i => i
  -> (Double -> Double)
  -> (Decimal -> TransResult Decimal)
  -> Decimal -> Eval e Decimal
liftUnDec i fp f a = do
#if defined(DOUBLE_GUARD)
  let !chk = (fp (dec2F a))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
#endif
  let !out = (f a)
  case out of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinDec
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Decimal -> Decimal -> Eval e Decimal
liftBinDec i fp f a b = do
#if defined(DOUBLE_GUARD)
  let !chk = (fp (dec2F a) (dec2F b))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
#endif
  let !out = (a `f` b)
  case out of
    TransNumber num -> pure num
    _ -> evalError' i "Operation resulted in +- infinity or NaN"

liftBinInt
  :: HasInfo i => i
  -> (Double -> Double -> Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Integer -> Integer -> Eval e Integer
liftBinInt i fp f a b = do
#if defined(DOUBLE_GUARD)
  let !chk = (fp (int2F a) (int2F b))
  when (isNaN chk || isInfinite chk) $
    evalError' i "Operation resulted in +- infinity or NaN"
#endif
  let !out = (int2D a `f` int2D b)
  case out of
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
