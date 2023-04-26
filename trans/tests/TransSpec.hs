{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Numeric.Decimal
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
    -- floatingPointTestsArity1 "exp"  Dbl.dbl_exp  Musl.musl_exp  Dec.dec_exp
    -- floatingPointTestsArity1 "ln"   Dbl.dbl_ln   Musl.musl_ln   Dec.dec_ln
    -- floatingPointTestsArity2 "log"  Dbl.dbl_log  Musl.musl_log  Dec.dec_log
    -- floatingPointTestsArity2 "pow"  Dbl.dbl_pow  Musl.musl_pow  Dec.dec_pow
    -- floatingPointTestsArity1 "sqrt" Dbl.dbl_sqrt Musl.musl_sqrt Dec.dec_sqrt

    spec_dbl_trans_ln
    spec_dbl_trans_exp
    spec_dbl_trans_pow
    spec_dbl_trans_log
    spec_dbl_trans_sqrt
    spec_dbl_ln_exp
    spec_dbl_log_pow
    spec_dbl_pow_sqrt

    spec_musl_trans_ln
    spec_musl_trans_exp
    spec_musl_trans_pow
    spec_musl_trans_log
    spec_musl_trans_sqrt
    spec_musl_ln_exp
    spec_musl_log_pow
    spec_musl_pow_sqrt

    spec_dec_trans_ln
    spec_dec_trans_exp
    spec_dec_trans_pow
    spec_dec_trans_log
    spec_dec_trans_sqrt
    spec_dec_ln_exp
    spec_dec_log_pow
    spec_dec_pow_sqrt

{-
dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

From the MPFR 4.1.0 documentation:

{-
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
-}


spec_dbl_trans_ln :: Spec
spec_dbl_trans_ln = describe "dbl_ln" $ do
    it "ln 60" $ Dbl.dbl_ln 60 `shouldBeNum`
      4.09434456222210041431708305026404559612274169921875

spec_dbl_trans_exp :: Spec
spec_dbl_trans_exp = describe "dbl_exp" $ do
    it "e" $ Dbl.dbl_exp 1 `shouldBeNum` 2.718281828459045

spec_dbl_ln_exp :: Spec
spec_dbl_ln_exp = describe "dbl_ln . dbl_exp" $ do
    prop "ln (exp x)" $ \x ->
      let (TransNumber a) = Dbl.dbl_exp x
      in Dbl.dbl_ln a `roughlyEq` x

spec_dbl_trans_pow :: Spec
spec_dbl_trans_pow = describe "dbl_pow" $ do
    it "pow 2 0" $ Dbl.dbl_pow 2 0 `shouldBeNum` 1
    it "pow 2 1" $ Dbl.dbl_pow 2 1 `shouldBeNum` 2
    it "pow 2 2" $ Dbl.dbl_pow 2 2 `shouldBeNum` 4
    it "pow 2 3" $ Dbl.dbl_pow 2 3 `shouldBeNum` 8
    it "pow 2 4" $ Dbl.dbl_pow 2 4 `shouldBeNum` 16
    it "pow 2 10" $ Dbl.dbl_pow 2 10 `shouldBeNum` 1024
    it "pow 8.7 85.7" $ Dbl.dbl_pow 8.7 85.7 `shouldBeNum` 3.28700185683462e80
    it "pow 8.7 856739.34857" $ Main.isInf (Dbl.dbl_pow 8.7 856739.34857)
    prop "pow x 0" $ \x -> Dbl.dbl_pow x 0 === TransNumber 1
    prop "pow x 1" $ \x -> Dbl.dbl_pow x 1 === TransNumber x
    prop "pow x 2" $ \x -> Dbl.dbl_pow x 2 `roughlyEq` (x * x)

spec_dbl_trans_log :: Spec
spec_dbl_trans_log = describe "dbl_log" $ do
    it "log 2 1" $ Dbl.dbl_log 2 1 `shouldBeNum` 0
    it "log 2 2" $ Dbl.dbl_log 2 2 `shouldBeNum` 1
    it "log 2 4" $ Dbl.dbl_log 2 4 `shouldBeNum` 2
    it "log 2 8" $ Dbl.dbl_log 2 8 `shouldBeNum` 3
    it "log 2 16" $ Dbl.dbl_log 2 16 `shouldBeNum` 4
    it "log 2 256" $ Dbl.dbl_log 2 256 `shouldBeNum` 8
    it "log 2 1024" $ Dbl.dbl_log 2 1024 `shouldBeNum` 10
    it "log 2 5" $ Dbl.dbl_log 2 5 `shouldBeNum` 2.321928094887362
    it "log 13.7 721.8" $ Dbl.dbl_log 13.7 721.8 `shouldBeNum` 2.5146170134618364

spec_dbl_log_pow :: Spec
spec_dbl_log_pow = describe "dbl_log 2 . dbl_pow 2" $ do
  prop "log 2 (pow 2 x)" $ \x ->
    let (TransNumber a) = Dbl.dbl_pow 2 x
    in Dbl.dbl_log 2 a `roughlyEq` x

spec_dbl_trans_sqrt :: Spec
spec_dbl_trans_sqrt = describe "dbl_sqrt" $ do
    it "sqrt -1" $ Main.isNaN (Dbl.dbl_sqrt (-1))
    it "sqrt 1" $ Dbl.dbl_sqrt 1 `shouldBeNum` 1
    it "sqrt 4" $ Dbl.dbl_sqrt 4 `shouldBeNum` 2
    it "sqrt 9" $ Dbl.dbl_sqrt 9 `shouldBeNum` 3
    it "sqrt 16" $ Dbl.dbl_sqrt 16 `shouldBeNum` 4
    it "sqrt 25" $ Dbl.dbl_sqrt 25 `shouldBeNum` 5
    it "sqrt 36" $ Dbl.dbl_sqrt 36 `shouldBeNum` 6
    it "sqrt 49" $ Dbl.dbl_sqrt 49 `shouldBeNum` 7
    it "sqrt 2" $ Dbl.dbl_sqrt 2 `shouldBeNum` 1.4142135623730951
    prop "sqrt (x * x)" $ \x ->
      let (TransNumber a) = Dbl.dbl_sqrt (x * x)
      in a === abs x

spec_dbl_pow_sqrt :: Spec
spec_dbl_pow_sqrt = describe "dbl_pow (dbl_sqrt x) 2" $ do
    prop "sqrt (x * x)" $ \(Positive x) ->
      let (TransNumber a) = Dbl.dbl_sqrt x
      in Dbl.dbl_pow a 2 `roughlyEq` x

------------------------------------------------------------------------

spec_musl_trans_ln :: Spec
spec_musl_trans_ln = describe "musl_ln" $ do
    it "ln 60" $ Musl.musl_ln 60 `shouldBeNum`
      4.09434456222210041431708305026404559612274169921875

spec_musl_trans_exp :: Spec
spec_musl_trans_exp = describe "musl_exp" $ do
    it "e" $ Musl.musl_exp 1 `shouldBeNum` 2.718281828459045

spec_musl_ln_exp :: Spec
spec_musl_ln_exp = describe "musl_ln . musl_exp" $ do
    prop "ln (exp x)" $ \x ->
      let (TransNumber a) = Musl.musl_exp x
      in Musl.musl_ln a `roughlyEq` x

spec_musl_trans_pow :: Spec
spec_musl_trans_pow = describe "musl_pow" $ do
    it "pow 2 0" $ Musl.musl_pow 2 0 `shouldBeNum` 1
    it "pow 2 1" $ Musl.musl_pow 2 1 `shouldBeNum` 2
    it "pow 2 2" $ Musl.musl_pow 2 2 `shouldBeNum` 4
    it "pow 2 3" $ Musl.musl_pow 2 3 `shouldBeNum` 8
    it "pow 2 4" $ Musl.musl_pow 2 4 `shouldBeNum` 16
    it "pow 2 10" $ Musl.musl_pow 2 10 `shouldBeNum` 1024
    it "pow 8.7 85.7" $ Musl.musl_pow 8.7 85.7 `shouldBeNum` 3.28700185683462e80
    it "pow 8.7 856739.34857" $ Main.isInf (Musl.musl_pow 8.7 856739.34857)
    prop "pow x 0" $ \x -> Musl.musl_pow x 0 === TransNumber 1
    prop "pow x 1" $ \x -> Musl.musl_pow x 1 === TransNumber x
    prop "pow x 2" $ \x -> Musl.musl_pow x 2 `roughlyEq` (x * x)

spec_musl_trans_log :: Spec
spec_musl_trans_log = describe "musl_log" $ do
    it "log 2 1" $ Musl.musl_log 2 1 `shouldBeNum` 0
    it "log 2 2" $ Musl.musl_log 2 2 `shouldBeNum` 1
    it "log 2 4" $ Musl.musl_log 2 4 `shouldBeNum` 2
    it "log 2 8" $ Musl.musl_log 2 8 `shouldBeNum` 3
    it "log 2 16" $ Musl.musl_log 2 16 `shouldBeNum` 4
    it "log 2 256" $ Musl.musl_log 2 256 `shouldBeNum` 8
    it "log 2 1024" $ Musl.musl_log 2 1024 `shouldBeNum` 10
    it "log 2 5" $ Musl.musl_log 2 5 `shouldBeNum` 2.321928094887362
    it "log 13.7 721.8" $ Musl.musl_log 13.7 721.8 `shouldBeNum` 2.5146170134618364

spec_musl_log_pow :: Spec
spec_musl_log_pow = describe "musl_log 2 . musl_pow 2" $ do
  prop "log 2 (pow 2 x)" $ \x ->
    let (TransNumber a) = Musl.musl_pow 2 x
    in Musl.musl_log 2 a `roughlyEq` x

spec_musl_trans_sqrt :: Spec
spec_musl_trans_sqrt = describe "musl_sqrt" $ do
    it "sqrt -1" $ Main.isNaN (Musl.musl_sqrt (-1))
    it "sqrt 1" $ Musl.musl_sqrt 1 `shouldBeNum` 1
    it "sqrt 4" $ Musl.musl_sqrt 4 `shouldBeNum` 2
    it "sqrt 9" $ Musl.musl_sqrt 9 `shouldBeNum` 3
    it "sqrt 16" $ Musl.musl_sqrt 16 `shouldBeNum` 4
    it "sqrt 25" $ Musl.musl_sqrt 25 `shouldBeNum` 5
    it "sqrt 36" $ Musl.musl_sqrt 36 `shouldBeNum` 6
    it "sqrt 49" $ Musl.musl_sqrt 49 `shouldBeNum` 7
    it "sqrt 2" $ Musl.musl_sqrt 2 `shouldBeNum` 1.4142135623730951
    prop "sqrt (x * x)" $ \x ->
      let (TransNumber a) = Musl.musl_sqrt (x * x)
      in a === abs x

spec_musl_pow_sqrt :: Spec
spec_musl_pow_sqrt = describe "musl_pow (musl_sqrt x) 2" $ do
    prop "sqrt (x * x)" $ \(Positive x) ->
      let (TransNumber a) = Musl.musl_sqrt x
      in Musl.musl_pow a 2 `roughlyEq` x

------------------------------------------------------------------------

spec_dec_trans_ln :: Spec
spec_dec_trans_ln = describe "dec_ln" $ do
    it "ln 60" $ Dec.dec_ln 60 `shouldBeNum`
      4.094344562222100684830468813065066480324092180811777681888702244098460524865656162715476286899749075

spec_dec_trans_exp :: Spec
spec_dec_trans_exp = describe "dec_exp" $ do
    it "e" $ Dec.dec_exp 1 `shouldBeNum` 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427

spec_dec_ln_exp :: Spec
spec_dec_ln_exp = describe "dec_ln . dec_exp" $ do
    prop "ln (exp x)" $ \x ->
      let (TransNumber a) = Dec.dec_exp x
      in Dec.dec_ln a `roughlyEq` x

spec_dec_trans_pow :: Spec
spec_dec_trans_pow = describe "dec_pow" $ do
    it "pow 2 0" $ Dec.dec_pow 2 0 `shouldBeNum` 1
    it "pow 2 1" $ Dec.dec_pow 2 1 `shouldBeNum` 2
    it "pow 2 2" $ Dec.dec_pow 2 2 `shouldBeNum` 4
    it "pow 2 3" $ Dec.dec_pow 2 3 `shouldBeNum` 8
    it "pow 2 4" $ Dec.dec_pow 2 4 `shouldBeNum` 16
    it "pow 2 10" $ Dec.dec_pow 2 10 `shouldBeNum` 1024
    it "pow 8.7 85.7" $ Dec.dec_pow 8.7 85.7 `shouldBeNum` 328700185683462282496121061003702794725025790628487093452976752081804732596239074.4042181146260598068
    -- it "pow 8.7 856739.34857" $ Dec.dec_pow 8.7 856739.34857 `shouldBeNum` 0
    prop "pow x 0" $ \x -> Dec.dec_pow x 0 === TransNumber 1
    prop "pow x 1" $ \x -> Dec.dec_pow x 1 === TransNumber x
    prop "pow x 2" $ \x -> Dec.dec_pow x 2 `roughlyEq` (x * x)

spec_dec_trans_log :: Spec
spec_dec_trans_log = describe "dec_log" $ do
    it "log 2 1" $ Dec.dec_log 2 1 `shouldBeNum` 0
    it "log 2 2" $ Dec.dec_log 2 2 `shouldBeNum` 1
    it "log 2 4" $ Dec.dec_log 2 4 `shouldBeNum` 2
    it "log 2 8" $ Dec.dec_log 2 8 `shouldBeNum` 3
    it "log 2 16" $ Dec.dec_log 2 16 `shouldBeNum` 4
    it "log 2 256" $ Dec.dec_log 2 256 `shouldBeNum` 8
    it "log 2 1024" $ Dec.dec_log 2 1024 `shouldBeNum` 10
    it "log 2 5" $ Dec.dec_log 2 5 `shouldBeNum` 2.321928094887362347870319429489390175864831393024580612054756395815934776608625215850139743359370155528942989732167860752188688264342320140360274935241064413062140827829345802323553906123213597576914981212951182468227233371420773795725734395411795217554322
    it "log 13.7 721.8" $ Dec.dec_log 13.7 721.8 `shouldBeNum` 2.514617013461836355688559174079293172040567566958969840820930353749335907664768819078631238244335781230456186034314492591442213446825228452561173322615235394456218104504606171251988132536653657281211882157574432719250182021880297605068014254481048277717726

spec_dec_log_pow :: Spec
spec_dec_log_pow = describe "dec_log 2 . dec_pow 2" $ do
  prop "log 2 (pow 2 x)" $ \x ->
    let (TransNumber a) = Dec.dec_pow 2 x
    in Dec.dec_log 2 a `roughlyEq` x

spec_dec_trans_sqrt :: Spec
spec_dec_trans_sqrt = describe "dec_sqrt" $ do
    it "sqrt -1" $ Main.isNaN (Dec.dec_sqrt (-1))
    it "sqrt 1" $ Dec.dec_sqrt 1 `shouldBeNum` 1
    it "sqrt 4" $ Dec.dec_sqrt 4 `shouldBeNum` 2
    it "sqrt 9" $ Dec.dec_sqrt 9 `shouldBeNum` 3
    it "sqrt 16" $ Dec.dec_sqrt 16 `shouldBeNum` 4
    it "sqrt 25" $ Dec.dec_sqrt 25 `shouldBeNum` 5
    it "sqrt 36" $ Dec.dec_sqrt 36 `shouldBeNum` 6
    it "sqrt 49" $ Dec.dec_sqrt 49 `shouldBeNum` 7
    it "sqrt 2" $ Dec.dec_sqrt 2 `shouldBeNum` 1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641573
    prop "sqrt (x * x)" $ \x ->
      let (TransNumber a) = Dec.dec_sqrt (x * x)
      in a === abs x

spec_dec_pow_sqrt :: Spec
spec_dec_pow_sqrt = describe "dec_pow (dec_sqrt x) 2" $ do
    prop "sqrt (x * x)" $ \(Positive x) ->
      let (TransNumber a) = Dec.dec_sqrt x
      in Dec.dec_pow a 2 `roughlyEq` x

-- -------------------------------------------------------------------------- --
-- Utils

roughlyEq :: (Ord a, Fractional a) => TransResult a -> a -> Bool
roughlyEq (TransNumber a) b = abs (a-b) < 0.00000001
roughlyEq _ _ = False

shouldBeNum :: (Ord a, Fractional a) => TransResult a -> a -> Expectation
shouldBeNum stmt res =
  unless (stmt `roughlyEq` res) $
    expectationFailure "shouldBeNum failed"

isNaN :: Show a => TransResult a -> Expectation
isNaN stmt = stmt `shouldSatisfy` (\case
  TransNaN _ -> True
  _ -> False)

isInf :: Show a => TransResult a -> Expectation
isInf stmt = stmt `shouldSatisfy` (\case
  TransInf _ -> True
  _ -> False)
