{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Decimal
import qualified Pact.Native.Trans.Musl as M
import Pact.Native.Trans.Types
import Pact.Native.Trans.TOps
import Pact.Types.Runtime

x :: Decimal
x = 15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835

y :: Decimal
y = 3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356

main :: IO ()
main = do
  putStrLn $ "(dbl)  x ^ y = " ++ show (f2Dec (dec2F x ** dec2F y))
  putStrLn $ "(musl) x ^ y = " ++ show (f2Dec (M.trans_pow (dec2F x) (dec2F y)))
  case mpfr_pow x y of
    TransNumber z -> putStrLn $ "(mpfr) x ^ y = " ++ show z
    _ -> error "oops"
  -- putStrLn $ "(dbl)  x ^ y = " ++ show (pow_double x y)
  -- putStrLn $ "(musl) x ^ y = " ++ show (pow_musl x y)
  -- putStrLn $ "(mpfr) x ^ y = " ++ show (pow_mpfr x y)
  -- where
  -- f = (**)
  -- pow_double = liftBinDecF (error "info") f
  -- pow_musl = liftBinDecF (error "info") M.trans_pow
  -- pow_mpfr = liftBinDec (error "info") f mpfr_pow

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

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational
