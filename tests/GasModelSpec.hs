{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GasModelSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden as G

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Yaml as Y

import Control.Lens hiding ((.=))
import Control.Exception (bracket, throwIO)
import Control.Monad (when, forM_)
import Data.Aeson
import Data.IORef
import Data.Int (Int64)
import Data.List (foldl', sortOn)
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Random (mkQCGen)
import System.Directory


import GoldenSpec (cleanupActual)
import Pact.Types.Exp
import Pact.Types.SizeOf
import Pact.Types.PactValue
import Pact.Types.PactValue.Arbitrary
import Pact.Types.Runtime
import Pact.GasModel.GasModel
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.GasModel.GasTests
import Pact.Gas.Table
import Pact.Native

import Pact.JSON.Legacy.Value
import qualified Pact.JSON.Legacy.HashMap as LHM

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
    (S.fromList
     [ "CHARSET_ASCII"
     , "CHARSET_LATIN1"
     , "verify-spv"
     , "public-chain-data"
     , "list"
     , "continue"
     ])

allGasTestsAndGoldenShouldPass :: Spec
allGasTestsAndGoldenShouldPass =
  after_ (cleanupActual "gas-model" []) allGasTestsAndGoldenShouldPass'

-- | Calling directly is useful as it doesn't clean up, so you can use the actual
-- as a new golden on expected changes.
allGasTestsAndGoldenShouldPass' :: Spec
allGasTestsAndGoldenShouldPass' = beforeAll (newIORef []) $ sequential $ do

  -- fails if one of the gas tests throws a pact error
  parallel $ do
    -- TODO: make the use of legacy sorting nicer
    -- Update golden once the PR is complete!
    forM_ ([0::Int ..] `zip` LHM.toList (legacyHashMap_ unitTests)) $ \(i, (n, t)) -> do
      it (show n) $ \ref -> do
        !r <- runTest t
        atomicModifyIORef' ref (\x -> ((i,r):x, ()))

  beforeAllWith (fmap formatResults . readIORef) $
    it "gas model tests should not return a PactError, but should pass golden" $
      golden "gas-model"
 where
   formatResults = fmap toGoldenOutput . concatMap snd . sortOn fst

golden :: (FromJSON a,ToJSON a) => String -> a -> Golden a
golden name obj = Golden
  { G.output = obj
  , G.encodePretty = B.unpack . Y.encode
  , G.writeToFile = Y.encodeFile
  , G.readFromFile = Y.decodeFileThrow
  , G.testName = name
  , G.directory = "golden"
  , G.failFirstTime = False
  }


goldenSizeOfPactValues :: Spec
goldenSizeOfPactValues = do
  mapM_ (someGoldenSizeOfPactValue . fst) pactValuesDescAndGen


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

-- | Use this to run a single named test.
_runNative :: NativeDefName -> IO (Maybe [(T.Text,Gas)])
_runNative = traverse (fmap (map toGoldenOutput) . runTest) . unitTestFromDef

runTest :: GasUnitTests -> IO [GasTestResult ([Term Name], EvalState, Gas)]
runTest t = runGasUnitTests t run run
  where
    run expr dbSetup = do
      (res, (gas, st)) <- bracket (setupEnv' dbSetup) (gasSetupCleanup dbSetup) $ \(e,s) -> do
        writeIORef (_eeGas e) 0
        res <- mockRun expr (e,s)
        gas <- readIORef (_eeGas e)
        return ((gas,) <$> res)
      res' <- eitherDie (getDescription expr dbSetup) res
      return (res', st, gas)
    setupEnv' dbs = do
      (r, s) <- setupEnv dbs
      let
        flags = mkExecutionConfig
               [ FlagDisableInlineMemCheck, FlagDisablePactEvents
               , FlagDisablePact43, FlagDisablePact44, FlagDisablePact45]
        r' = set eeExecutionConfig flags r
      pure (r', s)

toGoldenOutput :: GasTestResult ([Term Name], EvalState, Gas) -> (T.Text, Gas)
toGoldenOutput r =
  (_gasTestResultDesciption r, view _3 (_gasTestResultSqliteDb r))

-- Utils
--

-- | Pseudo golden test of the Gas Model's sizeOf function.
-- Enforces that the sizeOf function remains the same for some pre-generated,
-- psuedo-random pact value.
someGoldenSizeOfPactValue :: String -> Spec
someGoldenSizeOfPactValue desc = do
  it ("passes sizeOf golden test with pseudo-random " <> desc <> " pact value") $ do
    (goldenSize, goldenPactValue) <- jsonDecode (goldenPactValueFilePath desc)
    let actualSize = sizeOf SizeOfV0 goldenPactValue
    actualSize `shouldBe` goldenSize

  where jsonDecode :: FilePath -> IO (Int64, PactValue)
        jsonDecode fp = do
          isGoldenFilePresent <- doesFileExist fp
          when (not isGoldenFilePresent) $ throwIO $ userError $
            "Golden pact value file does not exist: " ++ show fp
          r <- eitherDecode <$> BL.readFile fp
          case r of
            Left e -> throwIO $ userError $ "golden decode failed: " ++ show e
            Right v -> return v

-- | Genearates golden files expected in `someGoldenSizeOfPactValue`
-- Creates a golden file in the format of (sizeOf <somePactValue>, <somePactValue>)
_generateGoldenPactValues :: IO ()
_generateGoldenPactValues = mapM_ f pactValuesDescAndGen
  where
    f (desc, genPv) = do
      let fp = goldenPactValueFilePath desc
      _ <- createDirectoryIfMissing False (goldenPactValueDirectory desc)
      isGoldenFilePresent <- doesFileExist fp
      when (not isGoldenFilePresent) (createGolden fp genPv)

    createGolden fp genPv = do
      -- TODO add quickcheck propeties for json roundtrips
      pv <- generate $
            suchThat genPv satisfiesRoundtripJSON
      jsonEncode fp (sizeOf SizeOfV0 pv, pv)

    jsonEncode :: FilePath -> (Int64, PactValue) -> IO ()
    jsonEncode fp = BL.writeFile fp . encode


-- | List of pact value pseudo-random generators and their descriptions
pactValuesDescAndGen :: [(String, Gen PactValue)]
pactValuesDescAndGen =
  genSomeLiteralPactValues seed <>
  genSomeGuardPactValues seed <>
  [ ("list", genSomeListPactValue seed)
  , ("object-map", genSomeObjectPactValue seed) ]

goldenPactValueDirectory :: String -> FilePath
goldenPactValueDirectory desc = goldenDirectory <> "/" <> testPrefix <> desc
  where goldenDirectory = "golden"
        testPrefix = "size-of-pactvalue-"

goldenPactValueFilePath :: String -> FilePath
goldenPactValueFilePath desc = (goldenPactValueDirectory desc) <> "/golden"


-- To run a generator in the repl:
-- `import Pact.Types.Pretty`
-- `import Data.Aeson (toJSON)`
-- `fmap (pretty . toJSON) (generate $ genSomeLiteralPactValue seed)`

-- | Generator of some psuedo-random Literal pact values
genSomeLiteralPactValues :: Int -> [(String, Gen PactValue)]
genSomeLiteralPactValues s =
  [ ("literal-string", f genLiteralString)
  , ("literal-integer", f genLiteralInteger)
  , ("literal-decimal", f genLiteralDecimal)
  , ("literal-bool", f genLiteralBool)
  , ("literal-time", f genLiteralTime) ]
  where f g = PLiteral <$> genWithSeed s g

-- | Generator of some psuedo-random List pact value
genSomeListPactValue :: Int -> Gen PactValue
genSomeListPactValue s = genWithSeed s $ PList <$> genPactValueList RecurseTwice

-- | Generator of some psuedo-random ObjectMap pact value
genSomeObjectPactValue :: Int -> Gen PactValue
genSomeObjectPactValue s = genWithSeed s $ PObject <$> genPactValueObjectMap RecurseTwice

-- | Generator of some psuedo-random Guard pact values
genSomeGuardPactValues :: Int -> [(String, Gen PactValue)]
genSomeGuardPactValues s =
  [ ("guard-pact", f $ GPact <$> arbitrary)
  , ("guard-keySet", f $ GKeySet <$> arbitrary)
  , ("guard-keySetRef", f $ GKeySetRef <$> arbitrary)
  , ("guard-module", f $ GModule <$> arbitrary)
  , ("guard-user", f $ genUserGuard RecurseTwice) ]
  where f g = PGuard <$> genWithSeed s g

-- | Generate arbitrary value with the provided "random" seed.
-- Allows for replicating arbitrary values.
genWithSeed :: Int -> Gen a -> Gen a
genWithSeed i (MkGen g) = MkGen (\_ n -> g (mkQCGen i) n)

-- | Random seed used to generate the pact values in the sizeOf golden tests
seed :: Int
seed = 10000000000


_diffGoldens :: FilePath -> FilePath -> IO ()
_diffGoldens g1 g2 = do
  (y1 :: Map.Map T.Text [Int]) <- fmap pure . Map.fromList <$> Y.decodeFileThrow g1
  (y2 :: Map.Map T.Text [Int]) <- fmap pure . Map.fromList <$> Y.decodeFileThrow g2
  let merge [c1] [c2] = [c1,c2,c2-c1]
      merge _ _ = []
  Y.encodeFile "diff.yaml" $ Map.unionWith merge y1 y2
