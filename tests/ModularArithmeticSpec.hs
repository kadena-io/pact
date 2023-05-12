{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ModularArithmeticSpec (spec) where

import Test.Hspec
import Pact.Native.ModularArithmetic

spec :: Spec
spec = describe "modular arithmetic" $ do
  describe "egcd" $ do
    it "return the extended greatest common divisors of two integers" $ do
      egcd 3 26 `shouldBe` (1, 9, -1)
      egcd 10 11 `shouldBe` (1, -1, 1)
      egcd 5865413254 646787313212 `shouldBe` (2, -136892601753, 1241415322)
      egcd 2094759673937393 7542689557689386379 `shouldBe` (1, -3477782688621484921, 965851620316926)