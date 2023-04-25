{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Decimal
import Test.Hspec
import Test.QuickCheck

import qualified Pact.Trans.Musl as Musl
import qualified Pact.Trans.Dec as Dec
import qualified Pact.Trans.Dbl as Dbl
import Pact.Trans.Types
import Test.Hspec.QuickCheck (prop)

instance Arbitrary i => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

-- WARNING: These tests are currently a no-op, because there has been no way
-- discovered yet to reach agreement between either native doubles and MPFR,
-- or Musl and MPFR.

main :: IO ()
main = hspec $ do
  describe "TransSpec" $ do
    floatingPointTestsArity1 "exp"  Dbl.dbl_exp  Musl.musl_exp  Dec.dec_exp
    floatingPointTestsArity1 "ln"   Dbl.dbl_ln   Musl.musl_ln   Dec.dec_ln
    floatingPointTestsArity2 "log"  Dbl.dbl_log  Musl.musl_log  Dec.dec_log
    floatingPointTestsArity2 "pow"  Dbl.dbl_pow  Musl.musl_pow  Dec.dec_pow
    floatingPointTestsArity1 "sqrt" Dbl.dbl_sqrt Musl.musl_sqrt Dec.dec_sqrt
    spec_trans_ln
    spec_trans_exp
    spec_trans_pow
    spec_trans_log
    spec_trans_sqrt
    spec_ln_exp
    spec_log_pow
    spec_pow_sqrt

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

{-
From the MPFR 4.1.0 documentation:

"...with a precision of 53 bits and in any of the four standard rounding
modes, MPFR is able to exactly reproduce all computations with double-
precision machine floating-point numbers (e.g., double type in C, with a C
implementation that rigorously follows Annex F of the ISO C99 standard and
FP_CONTRACT pragma set to OFF) on the four arithmetic operations and the
square root, except the default exponent range is much wider and subnormal
numbers are not implemented (but can be emulated)."
-}

floatingPointTestsArity1
  :: String
  -> (Double -> TransResult Double)
  -> (Double -> TransResult Double)
  -> (Decimal -> TransResult Decimal)
  -> Spec
floatingPointTestsArity1 lbl f_double f_musl f_mpfr =
  it ("floatingPointTestsArity1::" ++ lbl) $
    property $ withMaxSuccess 1_000_000  $ \x -> do
      case f_double (dec2F x) of
        TransNumber dbl ->
          case f_musl (dec2F x) of
            TransNumber musl ->
              case f_mpfr x of
                TransNumber mpfr ->
                  [f2Dec dbl, f2Dec musl, mpfr] `shouldContain` [mpfr]
                _ -> pure ()
            _ -> pure ()
        _ -> pure ()

floatingPointTestsArity2
  :: String
  -> (Double -> Double -> TransResult Double)
  -> (Double -> Double -> TransResult Double)
  -> (Decimal -> Decimal -> TransResult Decimal)
  -> Spec
floatingPointTestsArity2 lbl f_double f_musl f_mpfr =
  it ("floatingPointTestsArity2::" ++ lbl) $
    property $ withMaxSuccess 1_000_000  $ \x y -> do
      case f_double (dec2F x) (dec2F y) of
        TransNumber dbl ->
          case f_musl (dec2F x) (dec2F y) of
            TransNumber musl ->
              case f_mpfr x y of
                TransNumber mpfr ->
                  [f2Dec dbl, f2Dec musl, mpfr] `shouldContain` [mpfr]
                _ -> pure ()
            _ -> pure ()
        _ -> pure ()


shouldBeDblNum :: TransResult Double -> Double -> Expectation
shouldBeDblNum stmt res = stmt `shouldBe` TransNumber res

dblIsNaN :: TransResult Double -> Expectation
dblIsNaN stmt = stmt `shouldSatisfy` (\case
  TransNaN _ -> True
  _ -> False)

dblIsInf :: TransResult Double -> Expectation
dblIsInf stmt = stmt `shouldSatisfy` (\case
  TransInf _ -> True
  _ -> False)

spec_trans_ln :: Spec
spec_trans_ln = describe "trans_ln" $ do
    it "ln 60" $ Dbl.dbl_ln 60 `shouldBeDblNum`
      4.09434456222210041431708305026404559612274169921875

spec_trans_exp :: Spec
spec_trans_exp = describe "trans_exp" $ do
    it "e" $ Dbl.dbl_exp 1 `shouldBeDblNum` 2.718281828459045

spec_ln_exp :: Spec
spec_ln_exp = describe "trans_ln . trans_exp" $ do
    prop "ln(exp x)" $ \x ->
      let (TransNumber a) = Dbl.dbl_exp x
      in Dbl.dbl_ln a `dblRoughlyEq` x

spec_trans_pow :: Spec
spec_trans_pow = describe "trans_pow" $ do
    it "pow 2 0" $ Dbl.dbl_pow 2 0 `shouldBeDblNum` 1
    it "pow 2 1" $ Dbl.dbl_pow 2 1 `shouldBeDblNum` 2
    it "pow 2 2" $ Dbl.dbl_pow 2 2 `shouldBeDblNum` 4
    it "pow 2 3" $ Dbl.dbl_pow 2 3 `shouldBeDblNum` 8
    it "pow 2 4" $ Dbl.dbl_pow 2 4 `shouldBeDblNum` 16
    it "pow 2 10" $ Dbl.dbl_pow 2 10 `shouldBeDblNum` 1024
    it "pow 8.7 85.7" $ Dbl.dbl_pow 8.7 85.7 `shouldBeDblNum` 3.28700185683462e80
    it "pow 8.7 856739.34857" $ dblIsInf (Dbl.dbl_pow 8.7 856739.34857)
    prop "pow x 0" $ \x -> Dbl.dbl_pow x 0 === TransNumber 1
    prop "pow x 1" $ \x -> Dbl.dbl_pow x 1 === TransNumber x
    prop "pow x 2" $ \x -> Dbl.dbl_pow x 2 `dblRoughlyEq` (x * x)

spec_trans_log :: Spec
spec_trans_log = describe "Dbl.dbl_log" $ do
    it "log 2 1" $ Dbl.dbl_log 2 1 `shouldBeDblNum` 0
    it "log 2 2" $ Dbl.dbl_log 2 2 `shouldBeDblNum` 1
    it "log 2 4" $ Dbl.dbl_log 2 4 `shouldBeDblNum` 2
    it "log 2 8" $ Dbl.dbl_log 2 8 `shouldBeDblNum` 3
    it "log 2 16" $ Dbl.dbl_log 2 16 `shouldBeDblNum` 4
    it "log 2 256" $ Dbl.dbl_log 2 256 `shouldBeDblNum` 8
    it "log 2 1024" $ Dbl.dbl_log 2 1024 `shouldBeDblNum` 10
    it "log 2 5" $ Dbl.dbl_log 2 5 `shouldBeDblNum` 2.321928094887362
    it "log 13.7 721.8" $ Dbl.dbl_log 13.7 721.8 `shouldBeDblNum` 2.5146170134618364

spec_log_pow :: Spec
spec_log_pow = describe "Dbl.dbl_log 2 . Dbl.dbl_pow 2" $ do
  prop "log 2 (pow 2 x)" $ \x ->
    let (TransNumber a) = Dbl.dbl_pow 2 x
    in Dbl.dbl_log 2 a `dblRoughlyEq` x

spec_trans_sqrt :: Spec
spec_trans_sqrt = describe "trans_sqrt" $ do
    it "sqrt -1" $ dblIsNaN (Dbl.dbl_sqrt (-1))
    it "sqrt 1" $ Dbl.dbl_sqrt 1 `shouldBeDblNum` 1
    it "sqrt 4" $ Dbl.dbl_sqrt 4 `shouldBeDblNum` 2
    it "sqrt 9" $ Dbl.dbl_sqrt 9 `shouldBeDblNum` 3
    it "sqrt 16" $ Dbl.dbl_sqrt 16 `shouldBeDblNum` 4
    it "sqrt 25" $ Dbl.dbl_sqrt 25 `shouldBeDblNum` 5
    it "sqrt 36" $ Dbl.dbl_sqrt 36 `shouldBeDblNum` 6
    it "sqrt 49" $ Dbl.dbl_sqrt 49 `shouldBeDblNum` 7
    it "sqrt 2" $ Dbl.dbl_sqrt 2 `shouldBeDblNum` 1.4142135623730951
    prop "sqrt (x * x)" $ \x ->
      let (TransNumber a) = Dbl.dbl_sqrt (x * x)
      in a === abs x

spec_pow_sqrt :: Spec
spec_pow_sqrt = describe "pow (sqrt x) 2" $ do
    prop "sqrt (x * x)" $ \(Positive x) ->
      let (TransNumber a) = Dbl.dbl_sqrt x
      in Dbl.dbl_pow a 2 `dblRoughlyEq` x

-- -------------------------------------------------------------------------- --
-- Utils

dblRoughlyEq :: TransResult Double -> Double -> Bool
dblRoughlyEq (TransNumber a) b = abs (a-b) < 0.00000001
dblRoughlyEq _ _ = False
