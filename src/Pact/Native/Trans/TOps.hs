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
    ) where

import Control.Monad
import Data.Decimal
#if !defined(ghcjs_HOST_OS)
import qualified Pact.Native.Trans.Pow as T
import qualified Pact.Native.Trans.Log as T
import qualified Pact.Native.Trans.Sqrt as T
import qualified Pact.Native.Trans.Exp as T
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
  fp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x
#else
  fp_musl = liftUnDecF i M.trans_exp
  fp_mpfr = liftUnDec i f T.trans_exp
  go = chooseFunction1 fp_double fp_musl fp_mpfr >>= ($ x)
#endif

trans_ln :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_ln i x = go
  where
  f = log
  fp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x
#else
  fp_musl = liftUnDecF i M.trans_ln
  fp_mpfr = liftUnDec i f T.trans_ln
  go = chooseFunction1 fp_double fp_musl fp_mpfr >>= ($ x)
#endif

trans_logBase :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_logBase i x y = go
  where
  f = logBase
  fp_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x y
#else
  fp_musl = liftBinDecF i M.trans_log
  fp_mpfr = liftBinDec i f T.trans_logBase
  go = chooseFunction2 fp_double fp_musl fp_mpfr >>= (\k -> k x y)
#endif

trans_logBaseInt :: HasInfo i => i -> Integer -> Integer -> Eval e Integer
trans_logBaseInt i x y = go
  where
  f = logBase
  fp_double = liftBinIntF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x y
#else
  fp_musl = liftBinIntF i M.trans_log
  fp_mpfr = liftBinInt i f T.trans_logBase
  go = chooseFunction2 fp_double fp_musl fp_mpfr >>= (\k -> k x y)
#endif

trans_pow :: HasInfo i => i -> Decimal -> Decimal -> Eval e Decimal
trans_pow i x y = go
  where
  f = (**)
  fp_double = liftBinDecF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x y
#else
  fp_musl = liftBinDecF i M.trans_pow
  fp_mpfr = liftBinDec i f T.trans_pow
  go = chooseFunction2 fp_double fp_musl fp_mpfr >>= (\k -> k x y)
#endif

trans_sqrt :: HasInfo i => i -> Decimal -> Eval e Decimal
trans_sqrt i x = go
  where
  f = sqrt
  fp_double = liftUnDecF i f
#if defined(ghcjs_HOST_OS)
  go = fp_double x
#else
  fp_musl = liftUnDecF i M.trans_sqrt
  fp_mpfr = liftUnDec i f T.trans_sqrt
  go = chooseFunction1 fp_double fp_musl fp_mpfr >>= ($ x)
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
