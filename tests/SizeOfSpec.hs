{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SizeOfSpec(spec) where

import Test.Hspec
import GHC.Generics(Generic)

import Pact.Types.SizeOf

-- Testing whether derived instance for empty constructors is 1 word
data A = A1 | A2 deriving (Eq, Show, Generic)
data B = B1 Int | B2 Int Bool | B3 Int Bool A deriving (Eq, Show, Generic)
data C a = C a deriving (Eq, Show, Generic)

instance SizeOf A
instance SizeOf B
instance SizeOf a => SizeOf (C a)

newtype D = D Int
  deriving (Eq, Show, Generic)

instance SizeOf D

newtype F = F Int
  deriving (Eq, Show, SizeOf)

sizeOfGenericsTest :: SizeOfVersion -> Spec
sizeOfGenericsTest szVer = do
  describe ("SizeOf " <> show szVer <> " generics conform to specification") $ do
    it "Costs only one word for shared data types" $ do
      sizeOf szVer A1 `shouldBe` wordSize
      sizeOf szVer A2 `shouldBe` wordSize
    it "Costs the constructor size + 1 word per field" $ do
      sizeOf szVer (B1 0) `shouldBe` (sizeOf szVer (0::Int) + constructorCost 1)
      sizeOf szVer (B2 0 True) `shouldBe` (sizeOf szVer (0::Int) + sizeOf szVer True + constructorCost 2)
      let b3Cost = sizeOf szVer (0::Int) + sizeOf szVer True + sizeOf szVer A1 + constructorCost 3
      sizeOf szVer (B3 0 True A1) `shouldBe` b3Cost
    it "Works with parametrically defined instances" $ do
      sizeOf szVer (C (B1 0)) `shouldBe` (sizeOf szVer (B1 0) + constructorCost 1)
    it "Prices newtype instance with standalone deriving like constructor" $ do
        let k = 1
        sizeOf szVer (D k) `shouldBe` (sizeOf szVer k + constructorCost 1)
    it "Prices newtype instance with GND like a newtype" $ do
        let k = 1
        sizeOf SizeOfV0 (F k) `shouldBe` sizeOf szVer k

sizeOfV1ForkTest :: Spec
sizeOfV1ForkTest = describe "SizeOfV1 Changes" $ do
  it "Costs integers differently post-fork" $ do
    let i = (1120381203120310237810238701283710287440918750182730812730817238127328 :: Integer)
    sizeOf SizeOfV0 i `shouldSatisfy` (< sizeOf SizeOfV1 i)
    -- Exact amounts here function as a regression.
    sizeOf SizeOfV0 i `shouldBe` 14
    sizeOf SizeOfV1 i `shouldBe` 28
    -- Regression on old behavior
    sizeOf SizeOfV0 (-i) `shouldBe` 0
    -- New behavior for sizeOf testing abs
    sizeOf SizeOfV1 (-i) `shouldBe` sizeOf SizeOfV1 i


spec :: Spec
spec = do
  sizeOfGenericsTest SizeOfV0
  sizeOfGenericsTest SizeOfV1
  sizeOfV1ForkTest
