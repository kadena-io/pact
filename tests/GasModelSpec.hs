{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec

import qualified Data.Set           as S

import Pact.GasModel.GasTests       (untestedNatives)
import Pact.Types.Util              (asString)

spec :: Spec
spec = describe "untested natives" untestedNativesCheck

untestedNativesCheck :: Spec
untestedNativesCheck = do
  it "only deprecated natives should be missing gas model tests" $
    S.fromList (map asString untestedNatives)
    `shouldBe`
    (S.fromList ["verify-spv", "public-chain-data", "list"])
