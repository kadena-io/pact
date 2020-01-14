{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Control.Exception (bracket)
import Data.List (foldl')


import GoldenSpec (golden, cleanupActual)
import Pact.Types.Runtime
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
  describe "allGasTestsAndGoldenShouldPass" allGasTestsAndGoldenShouldPass
  describe "allNativesInGasTable" allNativesInGasTable

untestedNativesCheck :: Spec
untestedNativesCheck = do
  it "only deprecated or constant natives should be missing gas model tests" $
    S.fromList (map asString untestedNatives)
    `shouldBe`
    (S.fromList ["CHARSET_ASCII", "CHARSET_LATIN1",
                 "verify-spv", "public-chain-data", "list"])

allGasTestsAndGoldenShouldPass :: Spec
allGasTestsAndGoldenShouldPass = after_ (cleanupActual "gas-model" []) $ do
  res <- runIO gasTestResults
  -- ^ fails if one of the gas tests throws a pact error

  let gasCost = _evalGas . snd . _gasTestResultSqliteDb
      -- ^ only do golden test for sqlite results
      toGoldenOutput r = (_gasTestResultDesciption r, gasCost r)
      allActualOutputsGolden = map toGoldenOutput res

  it "gas model tests should not return a PactError, but should pass golden" $ do
    (golden "gas-model" allActualOutputsGolden)
      {encodePretty = show} -- TODO effective, but how to minimize the output?

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


gasTestResults :: IO [GasTestResult ([Term Name], EvalState)]
gasTestResults = do
  let runSingleNativeTests t = mapOverGasUnitTests t run run
      run expr dbSetup = do
        (res, st) <- bracket (setupEnv dbSetup) (gasSetupCleanup dbSetup) (mockRun expr)
        res' <- eitherDie (getDescription expr dbSetup) res
        return (res', st)
  concat <$> mapM (runSingleNativeTests . snd) (HM.toList unitTests)
