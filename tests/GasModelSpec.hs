{-# LANGUAGE OverloadedStrings #-}

module GasModelSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Control.Exception (bracket)
import Data.List (foldl')
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Random (mkQCGen)


import GoldenSpec (golden, cleanupActual)
import Pact.Types.SizeOf
import Pact.Types.PactValue
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
  describe "goldenSizeOfPactValues" goldenSizeOfPactValues

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
      { encodePretty = show } -- effective, but a lot of output is shown

goldenSizeOfPactValues :: Spec
goldenSizeOfPactValues = do
  someGoldenSizeOfPactValue "literal" (genSomeLiteralPactValue seed)
  someGoldenSizeOfPactValue "list" (genSomeListPactValue seed)
  someGoldenSizeOfPactValue "object-map" (genSomeObjectPactValue seed)
  someGoldenSizeOfPactValue "guard" (genSomeGuardPactValue seed)

someGoldenSizeOfPactValue :: String -> Gen PactValue -> Spec
someGoldenSizeOfPactValue desc genPV = after_ (cleanupActual (testPrefix <> desc) []) $ do
  pv <- runIO $ generate genPV
  it ("passes sizeOf golden test with pseudo-random " <> desc <> " pact value") $ do
    golden (testPrefix <> desc) (sizeOf pv, pv)
  where testPrefix = "size-of-pactvalue-"

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


-- Utils
--
-- To run a generator in the repl:
-- `import Pact.Types.Pretty`
-- `import Data.Aeson (toJSON)`
-- `fmap (pretty . toJSON) (generate $ genSomeLiteralPactValue seed)`

-- | Generator of some psuedo-random Literal pact value
genSomeLiteralPactValue :: Int -> Gen PactValue
genSomeLiteralPactValue s = genWithSeed s $ PLiteral <$> arbitrary

-- | Generator of some psuedo-random List pact value
genSomeListPactValue :: Int -> Gen PactValue
genSomeListPactValue s = genWithSeed s $ PList <$> genPactValueList RecurseTwice

-- | Generator of some psuedo-random ObjectMap pact value
genSomeObjectPactValue :: Int -> Gen PactValue
genSomeObjectPactValue s = genWithSeed s $ PObject <$> genPactValueObjectMap RecurseTwice

-- | Generator of some psuedo-random Guard pact value
genSomeGuardPactValue :: Int -> Gen PactValue
genSomeGuardPactValue s = genWithSeed s $ PGuard <$> genPactValueGuard RecurseTwice

-- | Generate arbitrary value with the provided "random" seed.
-- Allows for replicating arbitrary values.
genWithSeed :: Int -> Gen a -> Gen a
genWithSeed i (MkGen g) = MkGen (\_ n -> g (mkQCGen i) n)

-- | Random seed used to generate the pact values in the sizeOf golden tests
seed :: Int
seed = 10000000000
