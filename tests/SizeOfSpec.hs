{-# LANGUAGE DeriveGeneric #-}
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

spec :: Spec
spec = do
  describe "SizeOf V0 generics conform to specification" $ do
    it "Costs only one word for shared data types" $ do
      sizeOf SizeOfV0 A1 `shouldBe` wordSize
      sizeOf SizeOfV0 A2 `shouldBe` wordSize
    it "Costs the constructor size + 1 word per field" $ do
      sizeOf SizeOfV0 (B1 0) `shouldBe` (sizeOf SizeOfV0 (0::Int) + constructorCost 1)
      sizeOf SizeOfV0 (B2 0 True) `shouldBe` (sizeOf SizeOfV0 (0::Int) + sizeOf SizeOfV0 True + constructorCost 2)
      let b3Cost = sizeOf SizeOfV0 (0::Int) + sizeOf SizeOfV0 True + sizeOf SizeOfV0 A1 + constructorCost 3
      sizeOf SizeOfV0 (B3 0 True A1) `shouldBe` b3Cost
    it "Works with parametrically defined instances" $ do
      sizeOf SizeOfV0 (C (B1 0)) `shouldBe` (sizeOf SizeOfV0 (B1 0) + constructorCost 1)
