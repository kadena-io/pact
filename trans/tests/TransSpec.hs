{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Decimal
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Pact.Trans.Mpfr as Mpfr
import qualified Pact.Trans.Musl as Musl
import qualified Pact.Trans.Dbl as Dbl
import Pact.Trans.Types

instance (Arbitrary i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

main :: IO ()
main = hspec $ do
  describe "TransSpec" spec

spec :: Spec
spec = do
  floatingPointTests

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

floatingPointTestsArity1
  :: String
  -> (Double -> TransResult Double)
  -> (Double -> TransResult Double)
  -> (Decimal -> TransResult Decimal)
  -> Spec
floatingPointTestsArity1 lbl f_double f_musl f_mpfr =
  describe ("floating point tests: " ++ lbl) $ runIO $ do
    quickCheck $ withMaxSuccess 1_000_000 $ \x -> monadicIO $ do
      let TransNumber dbl = f_double (dec2F x)
      unless (isNaN dbl || isInfinite dbl) $ do
        let TransNumber musl = f_musl (dec2F x)
        unless (isNaN musl || isInfinite musl) $ do
          let TransNumber mpfr = f_mpfr x
          unless (f2Dec musl == mpfr) $ liftIO $ do
            putStrLn $ "op   = " ++ lbl
            putStrLn $ "x    = " ++ show x
            putStrLn $ "dbl  = " ++ show (f2Dec dbl)
            putStrLn $ "musl = " ++ show (f2Dec musl)
            putStrLn $ "mpfr = " ++ show mpfr
            -- error "Results fail to match"

floatingPointTestsArity2
  :: String
  -> (Double -> Double -> TransResult Double)
  -> (Double -> Double -> TransResult Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Spec
floatingPointTestsArity2 lbl f_double f_musl f_mpfr =
  describe ("floating point tests: " ++ lbl) $ runIO $ do
    -- From the MPFR 4.1.0 documentation: ...with a precision of 53 bits and
    -- in any of the four standard rounding modes, MPFR is able to exactly
    -- reproduce all computations with double-precision machine floating-point
    -- numbers (e.g., double type in C, with a C implementation that
    -- rigorously follows Annex F of the ISO C99 standard and FP_CONTRACT
    -- pragma set to OFF) on the four arithmetic operations and the square
    -- root, except the default exponent range is much wider and subnormal
    -- numbers are not implemented (but can be emulated).
    quickCheck $ withMaxSuccess 1_000_000 $ \x y -> monadicIO $ do
      let TransNumber dbl = f_double (dec2F x) (dec2F y)
      unless (isNaN dbl || isInfinite dbl) $ do
        let TransNumber musl = f_musl (dec2F x) (dec2F y)
        unless (isNaN musl || isInfinite musl) $ do
          let TransNumber mpfr = f_mpfr x y
          unless (f2Dec musl == mpfr) $ liftIO $ do
            putStrLn $ "op   = " ++ lbl
            putStrLn $ "x    = " ++ show x
            putStrLn $ "y    = " ++ show y
            putStrLn $ "dbl  = " ++ show (f2Dec dbl)
            putStrLn $ "musl = " ++ show (f2Dec musl)
            putStrLn $ "mpfr = " ++ show mpfr
            -- error "Results fail to match"

floatingPointTests :: Spec
floatingPointTests = do
  floatingPointTestsArity1 "exp"  Dbl.dbl_exp  Musl.musl_exp  Mpfr.mpfr_exp
  floatingPointTestsArity1 "ln"   Dbl.dbl_ln   Musl.musl_ln   Mpfr.mpfr_ln
  floatingPointTestsArity2 "log"  Dbl.dbl_log  Musl.musl_log  Mpfr.mpfr_log
  floatingPointTestsArity2 "pow"  Dbl.dbl_pow  Musl.musl_pow  Mpfr.mpfr_pow
  floatingPointTestsArity1 "sqrt" Dbl.dbl_sqrt Musl.musl_sqrt Mpfr.mpfr_sqrt
