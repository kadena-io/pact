{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Control.Exception (bracket)
import Data.List (foldl')

import Pact.Types.Util (asString)
import Pact.GasModel.GasModel
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.GasModel.GasTests
import Pact.Gas.Table
import Pact.Native


spec :: Spec
spec = describe "gas model tests" $ do
  describe "untestedNativesCheck" untestedNativesCheck
  describe "allGasTestsShouldPass" allGasTestsShouldPass
  describe "allNativesInGasTable" allNativesInGasTable

untestedNativesCheck :: Spec
untestedNativesCheck = do
  it "only deprecated or constant natives should be missing gas model tests" $
    S.fromList (map asString untestedNatives)
    `shouldBe`
    (S.fromList ["CHARSET_ASCII", "CHARSET_LATIN1",
                 "verify-spv", "public-chain-data", "list"])

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

allNativesInGasTable :: Spec
allNativesInGasTable = do
  it "all native functions should be in gas table" $ do
    let justNatives = map (asString . fst) (concatMap snd natives)
        absent li name = case (Map.lookup name defaultGasTable) of
          Nothing -> name : li
          Just _ -> li
        absentNatives = foldl' absent [] justNatives
    (S.fromList absentNatives)
    `shouldBe`
    (S.fromList ["CHARSET_ASCII", "CHARSET_LATIN1", "public-chain-data", "list"])
