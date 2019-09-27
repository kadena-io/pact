{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec

import qualified Data.Set           as S

import Pact.GasModel.GasTests       (untestedNatives)
import Pact.Types.Util              (asString)

spec :: Spec
spec = describe "gas model tests" $ do
  describe "untestedNativesCheck" untestedNativesCheck
  --describe "allGasTestsShouldPass" allGasTestsShouldPass

untestedNativesCheck :: Spec
untestedNativesCheck = do
  it "only deprecated natives should be missing gas model tests" $
    S.fromList (map asString untestedNatives)
    `shouldBe`
    (S.fromList ["verify-spv", "public-chain-data", "list"])

{--allGasTestsShouldPass :: Spec
allGasTestsShouldPass = do
  it "" $ undefined
--}
