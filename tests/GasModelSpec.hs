{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM

import Control.Exception (bracket)

import Pact.Types.Util (asString)
import Pact.GasModel.GasModel
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.GasModel.GasTests


spec :: Spec
spec = describe "gas model tests" $ do
  describe "untestedNativesCheck" untestedNativesCheck
  describe "allGasTestsShouldPass" allGasTestsShouldPass

untestedNativesCheck :: Spec
untestedNativesCheck = do
  it "only deprecated natives should be missing gas model tests" $
    S.fromList (map asString untestedNatives)
    `shouldBe`
    (S.fromList ["verify-spv", "public-chain-data", "list"])

allGasTestsShouldPass :: Spec
allGasTestsShouldPass = do
  it "gas model tests should not return a PactError" $ do
    let
      runSingleNativeTests t = mapOverGasUnitTests t run run
      run expr dbSetup = do
        (res,_) <- bracket
                   (setupEnv dbSetup)
                   (gasSetupCleanup dbSetup)
                   (mockRun expr)
        eitherDie (getDescription expr dbSetup) res
    mapM_ (runSingleNativeTests . snd) (HM.toList unitTests)
