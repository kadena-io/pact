{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module: Test.Pact.Native.Pairing
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Pact.Native.Pairing
( spec
) where

import Data.Mod
import qualified Data.Semiring as S

import GHC.TypeNats (KnownNat)

import Numeric.Natural

import qualified Pact.Native.Pairing as P
import Pact.Native.Pairing hiding (add)

import Prelude hiding (null)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Elliptic Curve Arithmetic" $ do
    spec_mod
    spec_fq
    spec_fx
    (spec_field_ @Fq)
    spec_curveG1
    spec_curveG2

-- -------------------------------------------------------------------------- --
-- Field Properties

prop_closed :: (a -> a -> a) -> a -> a -> Property
prop_closed op a b = op a b `seq` (True === True)

prop_associative :: Eq a => Show a => (a -> a -> a) -> a -> a -> a -> Property
prop_associative op a b c = op a (op b c) === op (op a b) c

prop_commutative :: Eq a => Show a => (a -> a -> a) -> a -> a -> Property
prop_commutative op a b = op a b === op b a

prop_identity_right :: Eq a => Show a => (a -> a -> a) -> a -> a -> Property
prop_identity_right op null a = op a null === a

prop_identity_left :: Eq a => Show a => (a -> a -> a) -> a -> a -> Property
prop_identity_left op null a = op null a === a

prop_inverse_right :: Eq a => Show a => (a -> a -> a) -> (a -> a) -> a -> a -> Property
prop_inverse_right op inv null a = op a (inv a) === null

prop_inverse_left :: Eq a => Show a => (a -> a -> a) -> (a -> a) -> a -> a -> Property
prop_inverse_left op inv null a = op (inv a) a === null

prop_dist_right :: Eq a => Show a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Property
prop_dist_right add mul a b c = mul (add a b) c === add (mul a c) (mul b c)

prop_dist_left :: Eq a => Show a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Property
prop_dist_left add mul a b c = mul a (add b c) === add (mul a b) (mul a c)

-- -------------------------------------------------------------------------- --
-- Field Spec

spec_group
    :: Eq a
    => Show a
    => Arbitrary a
    => (a -> a -> a)
    -> (a -> a)
    -> a
    -> Spec
spec_group = spec_group_ (const True)

spec_group_
    :: Eq a
    => Show a
    => Arbitrary a
    => (a -> Bool)
        -- condition to restrict inverse property
    -> (a -> a -> a)
    -> (a -> a)
    -> a
    -> Spec
spec_group_ cond op inv null = describe "is a group" $ do
    prop "is closed" $ prop_closed op
    prop "is associative" $ prop_associative op
    prop "has right identity" $ prop_identity_right op null
    prop "has left identity" $ prop_identity_left op null
    prop "has right inverse" $ \a -> cond a ==> prop_inverse_right op inv null a
    prop "has left inverse" $ \a -> cond a ==> prop_inverse_left op inv null a

spec_abelianGroup
    :: Eq a
    => Show a
    => Arbitrary a
    => (a -> a -> a)
    -> (a -> a)
    -> a
    -> Spec
spec_abelianGroup = spec_abelianGroup_ (const True)

spec_abelianGroup_
    :: Eq a
    => Show a
    => Arbitrary a
    => (a -> Bool)
        -- condition to restrict inverse property
    -> (a -> a -> a)
    -> (a -> a)
    -> a
    -> Spec
spec_abelianGroup_ cond op inv null = describe "is an abelian group" $ do
    spec_group_ cond op inv null
    prop "is communative" $ prop_commutative op

spec_field
    :: Eq a
    => Show a
    => Arbitrary a
    => (a -> a -> a) -> (a -> a) -> a -- addition
    -> (a -> a -> a) -> (a -> a) -> a -- multiplication
    -> Spec
spec_field add inv null mul rec one = describe "is a field" $ do
    it "null and one are different" $ shouldNotBe one null
    describe "addition" $ spec_abelianGroup add inv null
    describe "multiplication" $ spec_abelianGroup_ (/= null) mul rec one
    prop "is right distributive" $ prop_dist_right add mul
    prop "is left distributive" $ prop_dist_left add mul

spec_field_
    :: forall a
    . Eq a
    => Show a
    => S.Ring a
    => Fractional a
    => Arbitrary a
    => Spec
spec_field_ = describe "is semiring" $ do
    it "null and one are different" $ shouldNotBe @a S.one S.zero
    describe "addition" $ spec_abelianGroup @a S.plus S.negate S.zero
    describe "multiplication" $ spec_abelianGroup_ @a (/= S.zero) S.times recip S.one
    prop "is right distributive" $ prop_dist_right @a S.plus S.times
    prop "is left distributive" $ prop_dist_left @a S.plus S.times

-- -------------------------------------------------------------------------- --
-- Fq

type Q = 21888242871839275222246405745257275088696311157297823662689037894645226208583

q :: Natural
q = 21888242871839275222246405745257275088696311157297823662689037894645226208583

instance KnownNat n => Arbitrary (Mod n) where
    arbitrary = fromIntegral <$> arbitrary @Natural

instance Arbitrary Fq where
    arbitrary = Fq <$> arbitrary

spec_fq :: Spec
spec_fq = describe "Fq" $ do
    spec_field @Fq (+) (\a -> (-a)) 0 (*) recip 1
    it "- 1" $ ((- 1) :: Fq) === fromIntegral (q - 1)
    it "negation of 1" $ ((negate 1) :: Fq) === fromIntegral (q - 1)
    it "0 - 1" $ (0 - 1 :: Fq) === fromIntegral (q - 1)

spec_mod :: Spec
spec_mod = describe "mod" $ do
    spec_field @(Mod Q) (+) (\a -> (-a)) 0 (*) recip 1
    it "- 1" $ ((-1) :: Mod Q) === fromIntegral (q - 1)
    it "0 - 1" $ (0 - 1 :: Mod Q) === fromIntegral (q - 1)
    it "negate 1" $ (negate 1 :: Mod Q) === fromIntegral (q - 1)

-- spec_fq2 :: Spec
-- spec_fq2 = describe "Fq2" $
--     spec_field @Fq2 (+) (\a -> (-a)) 0 (*) recip 1

-- -------------------------------------------------------------------------- --
--

newtype Fx = Fx (Mod Q)
  deriving newtype (Eq, Show, Ord, Num , Fractional)

instance Arbitrary Fx where
    arbitrary = Fx <$> arbitrary

spec_fx :: Spec
spec_fx = describe "Fx" $ do
    spec_field @Fx (+) (\a -> (-a)) 0 (*) recip 1
    it "- 1" $ ((- 1) :: Fx) === fromIntegral (q - 1)
    it "negation of 1" $ ((negate 1) :: Fx) === fromIntegral (q - 1)
    it "0 - 1" $ (0 - 1 :: Fx) === fromIntegral (q - 1)

-- -------------------------------------------------------------------------- --
-- Weirstrass Curves

newtype PointG1 = PointG1 { _getPointG1 :: (CurvePoint Fq) }
    deriving (Eq, Show)
newtype PointG2 = PointG2 { _getPointG2 :: (CurvePoint Fq2) }
    deriving (Eq, Show)

instance Arbitrary (CurvePoint Fq) where
    arbitrary = frequency
        [ (1, pure CurveInf)
        , (10, multiply g1 <$> arbitrary)
        ]

instance Arbitrary PointG1 where
    arbitrary = frequency
        [ (1, pure (PointG1 CurveInf))
        , (10, PointG1 . multiply g1 <$> arbitrary)
        ]

instance Arbitrary PointG2 where
    arbitrary = frequency
        [ (1, pure (PointG2 CurveInf))
        , (10, PointG2 . multiply g2 <$> arbitrary)
        ]

class OnCurve p where
    onCurve :: p -> Bool
    curveAdd :: p -> p -> p
    curveMultiply :: p -> Integer -> p
    curveNegate :: p -> p
    curveDouble :: p -> p
    curveInf :: p

instance OnCurve PointG1 where
    onCurve (PointG1 p) = isOnCurve p b1
    curveAdd (PointG1 a) (PointG1 b) = PointG1 (P.add a b)
    curveMultiply (PointG1 a) n = PointG1 (multiply a n)
    curveNegate (PointG1 a) = PointG1 (negatePt a)
    curveDouble (PointG1 a) = PointG1 (double a)
    curveInf = PointG1 CurveInf

instance OnCurve PointG2 where
    onCurve (PointG2 p) = isOnCurve p b2
    curveAdd (PointG2 a) (PointG2 b) = PointG2 (P.add a b)
    curveMultiply (PointG2 a) n = PointG2 (multiply a n)
    curveNegate (PointG2 a) = PointG2 (negatePt a)
    curveDouble (PointG2 a) = PointG2 (double a)
    curveInf = PointG2 CurveInf

-- -------------------------------------------------------------------------- --
-- Curve Properties

spec_curveG1 :: Spec
spec_curveG1 = describe "Curve G1" $ do
    describe "addition" $ spec_abelianGroup @PointG1 curveAdd curveNegate curveInf
    prop "isOnCurve" $ onCurve @PointG1
    prop "it doubles" $ \a -> curveDouble @PointG1 a === curveAdd a a
    prop "add is on curve" $ \a b -> onCurve @PointG1 (curveAdd a b)
    prop "multiply is on curve" $ \a n -> onCurve @PointG1 (curveMultiply a n)
    prop "double is on curve" $ \a -> onCurve @PointG1 (curveDouble a)
    prop "negateP its on curve" $ \a -> onCurve @PointG1 (curveNegate a)

    describe "simple example" $ do
        it "(Point 1 2) isOnCurve" $ onCurve p12
        it "adds up" $ curveAdd p12 p12 `shouldBe` p
        it "doubles" $ curveDouble p12 `shouldBe` p

  where
    p12 = PointG1 $ Point 1 2
    p = PointG1 $ Point
        1368015179489954701390400359078579693043519447331113978918064868415326638035
        9918110051302171585080402603319702774565515993150576347155970296011118125764

spec_curveG2 :: Spec
spec_curveG2 = describe "Curve G2" $ do
    describe "addition" $ spec_abelianGroup @PointG2 curveAdd curveNegate curveInf
    prop "isOnCurve" $ onCurve @PointG2
    prop "it doubles" $ \a -> curveDouble @PointG2 a === curveAdd a a
    prop "add is on curve" $ \a b -> onCurve @PointG2 (curveAdd a b)
    prop "multiply is on curve" $ \a n -> onCurve @PointG2 (curveMultiply a n)
    prop "double is on curve" $ \a -> onCurve @PointG2 (curveDouble a)
    prop "negateP its on curve" $ \a -> onCurve @PointG2 (curveNegate a)

