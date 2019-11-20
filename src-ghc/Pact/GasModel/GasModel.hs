{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Pact.GasModel.GasModel where

import Control.Exception (bracket)
import Control.Monad (void, replicateM)
import Data.List (foldl', sortOn)
import GHC.Conc (numCapabilities)
import Statistics.Types (Estimate(..))


import qualified Criterion.Main as C
import qualified Criterion as C
import qualified Criterion.Types as C
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Options.Applicative as O


import Pact.Eval (eval)
import Pact.GasModel.GasTests
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.Types.Lang
import Pact.Types.Runtime hiding (GasPrice)


data Option = Option
  { _oFilter :: Maybe T.Text
  , _oBenchOnly :: Bool
  }
  deriving (Eq,Show)

options :: O.ParserInfo Option
options = O.info (O.helper <*> parser)
          (O.fullDesc <> O.header "Gas Model Benchmarks")
  where
    parser =
      Option
      <$> ((Just <$> O.strOption
            (O.short 'f' <> O.long "filter" <> O.metavar "NATIVE" <>
             O.help "Pipe-delimited list of natives to run")) O.<|>
            pure Nothing)
      <*> (O.flag False True
           (O.short 'b' <> O.long "bench" <> O.help "Just bench and display gas price"))

optionToGasTests
  :: Option
  -> [(NativeDefName, GasUnitTests)]
optionToGasTests opt = HM.toList $
  case _oFilter opt of
    Nothing -> unitTests
    Just ts -> HM.filterWithKey matchingFuns unitTests
      where tlist = NativeDefName <$> T.split (== '|') ts
            matchingFuns k _ = k `elem` tlist


type NanoSeconds = Double

secToNs :: Double -> NanoSeconds
secToNs dInSeconds =
  dInSeconds * 1000000000


newtype Average = Average Double
instance Csv.ToField Average where
  toField (Average avg) = Csv.toField avg

average :: (Foldable f) => f Double -> Average
average nums = Average $! totalSum / (fromIntegral size)
  where (totalSum,size) = foldl' f (0.0, 0 :: Int) nums
        f (!sum', !size') x = (sum' + x, size' + 1)


newtype GasPrice = GasPrice Integer
  deriving (Show)
instance Csv.ToField GasPrice where
  toField (GasPrice p) = Csv.toField p

gasPriceDivisor :: Double
gasPriceDivisor = 2500.0

gasPrice :: [NanoSeconds] -> GasPrice
gasPrice measurements = GasPrice price
  where
    (Average avg) = average measurements
    price = ceiling (avg / gasPriceDivisor)


exec
  :: EvalState
  -> EvalEnv e
  -> [Term Name]
  -> IO (Either PactError [Term Name], EvalState)
exec s e terms = runEval' s e $ mapM eval terms


mockRun
  :: PactExpression
  -> (EvalEnv e, EvalState)
  -> IO (Either PactError [Term Name], EvalState)
mockRun expr (env, state) = do
  terms <- compileCode (_pactExpressionFull expr)
  exec state env terms


setupEnv :: GasSetup e -> IO (EvalEnv e, EvalState)
setupEnv (GasSetup e s _ _) = do
  env <- e
  state <- s
  return (env, state)


mockRuns
  :: GasUnitTests
  -> IO ()
mockRuns tests = do
  void $ mapOverGasUnitTests tests run run
  where
    run expr dbSetup = do
      putStrLn $ "Dry run for " ++ T.unpack (getDescription expr dbSetup)

      (res,_) <- bracket (setupEnv dbSetup)
                         (gasSetupCleanup dbSetup)
                         (mockRun expr)
      eitherDie (getDescription expr dbSetup) res


-- | For debugging purposes
mockRuns'
  :: GasUnitTests
  -> IO [GasTestResult (Either PactError [Term Name], EvalState)]
mockRuns' tests = do
  mapOverGasUnitTests tests run run
  where
    run expr dbSetup = do
      bracket (setupEnv dbSetup) (gasSetupCleanup dbSetup) (mockRun expr)


bench
  :: PactExpression
  -> GasSetup e
  -> IO NanoSeconds
bench expr dbSetup = do
  terms <- compileCode (_pactExpressionFull expr)
  putStrLn $ T.unpack (getDescription expr dbSetup)
  report <- bracket setup teardown $ C.benchmark' . (run terms)
  return $ secToNs $
           estPoint $
           C.anMean $
           C.reportAnalysis report
  where
    setup = do
      s <- setupEnv dbSetup
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (gasSetupCleanup dbSetup) env
    run terms ~(NoopNFData (env, state)) =
          C.nfIO (exec state env terms)


benchesOnce
  :: GasUnitTests
  -> IO [GasTestResult Average]
benchesOnce tests =
  mapOverGasUnitTests tests run run
  where
    run expr setup = do
      means <- replicateM 1 $ bench expr setup
      return $! average means


-- | Benchmarks each native function's tests three times
benchesMultiple
  :: (NativeDefName, GasUnitTests)
  -> IO (NativeDefName, [GasTestResult Average])
benchesMultiple (funName, tests) = do
  results <- mapOverGasUnitTests tests run run
  return (funName, results)
  where
    batchSize = 3
    run expr setup = do
      means <- replicateM batchSize $ bench expr setup
      return $! average means


meansToCSVEncoding
  :: [GasTestResult Average]
  -> (Csv.Header, [Csv.NamedRecord])
meansToCSVEncoding results =
  (Csv.header headersField, map toNamedRecord results)
  where
    headersField =
      map Csv.toField (["function", "description",
                        "sqliteDbAverage (ns)", "mockDbAverage (ns)"] :: [T.Text])
    toNamedRecord (GasTestResult funName desc sqlAvg mockAvg) =
      Csv.namedRecord $
      zip headersField [Csv.toField (asString funName),
                        Csv.toField desc,
                        Csv.toField sqlAvg,
                        Csv.toField mockAvg
                       ]

writeRawCSV
  :: [GasTestResult Average]
  -> IO ()
writeRawCSV results = do
  let (headers, records) = meansToCSVEncoding results
      content = Csv.encodeByName headers records
  BSL8.writeFile "gas-model-raw-data.csv" content


sqliteGasPrice :: [GasTestResult Average] -> GasPrice
sqliteGasPrice rawResults = gasPrice dataPoints
  where
    dataPoints =
      map (liftAverage . _gasTestResultSqliteDb) onlySimpleTestResults
      where liftAverage (Average d) = d

    onlySimpleTestResults =
      filter simpleTestCheck rawResults
      where
        simpleTestCheck (GasTestResult _ desc _ _) =
          (not $ T.isInfixOf "med" desc) &&
          (not $ T.isInfixOf "long" desc)


encodeGasPrice
  :: [(NativeDefName, GasPrice)]
  -> (Csv.Header, [Csv.NamedRecord])
encodeGasPrice allPrices =
  (headers,
   map encodeFunctionPrice allPrices
  )
  where
    priceHeader =
      "gas price (" <> T.pack (show gasPriceDivisor) <> " ns)"
    functionNameHeader = "function"
    headers = Csv.header $
      map Csv.toField [functionNameHeader, priceHeader]

    encodeFunctionPrice (funName, price) =
      Csv.namedRecord $
      [(Csv.toField functionNameHeader) Csv..= asString funName] <>
      [(Csv.toField priceHeader) Csv..= price]


gasPriceExplanation :: [T.Text]
gasPriceExplanation = [purpose, numCores, os, hardwareNotes,
                       implementation, numOfIterations, backend, calcPrice, ""]
  where
    purpose = "Purpose: Calculate the gas price of Pact native functions using a data-driven model"
    numCores = "Number of cores available for benchmarks: " <> T.pack (show numCapabilities)
    os = "Operating System: <INSERT OS>"
    hardwareNotes = "More information on hardware used: <INSERT MORE INFORMATION ON HARDWARE USED>"
    implementation = "Implementation: For every native function, executes and benchmarks "
                     <> "(using Criterion) simple examples of said function."
    numOfIterations = "Number of iterations: Each function's tests are run three times against each backend type and averaged."
    backend = "Benchmark backend(s): [Sqlite db with `fastNoJournalPragmas`]"
    calcPrice = "From benchmark to a native function's price: For every backend type, all of the means of the function's"
                <>" benchmark examples are converted into nanoseconds, averaged together, divided by "
                <> T.pack (show gasPriceDivisor)
                <> ", and rounded up to the nearest integer."
                <> "Only the benchmark averages of the simple function tests are used when calculating the function's price."


writeGasPriceCSV
  :: [(NativeDefName, [GasTestResult Average])]
  -> IO ()
writeGasPriceCSV allResults = do
  let gasPrices = map (\(n,res) -> (n, sqliteGasPrice res)) allResults
      (headers, records) = encodeGasPrice gasPrices
      content = Csv.encodeByName headers records
      explanation = Csv.encode $
        map (\t -> Csv.record [Csv.toField t]) gasPriceExplanation
  BSL8.writeFile "gas-prices.csv" (explanation <> content)


coverageReport :: IO ()
coverageReport = do
  print $ "Missing benchmark tests for "
           ++ show (length untestedNatives)
           ++ " out of "
           ++ show (length allNatives)
           ++ " native functions."
  mapM_ (print . show) untestedNatives


main :: IO ()
main = do
  opt <- O.execParser options
  let tests = optionToGasTests opt

  -- Enforces that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  mapM_ (mockRuns . snd) tests

  putStrLn "Running benchmark(s)"

  if _oBenchOnly opt then
    let displayGasPrice (funName, t) = do
          res <- benchesOnce t
          putStrLn $ (T.unpack (asString funName)) ++ ": " ++ show (sqliteGasPrice res)
          putStrLn ""
    in mapM_ displayGasPrice tests
  else do
    let testsSorted = sortOn fst tests
    allBenches <- mapM benchesMultiple testsSorted

    putStrLn "Exporting raw benchmarks data"
    writeRawCSV (concatMap snd allBenches)

    putStrLn "Exporting data-driven gas prices"
    writeGasPriceCSV allBenches

    putStrLn "Reporting coverage"
    coverageReport


-- | For debugging
runGasTestByName :: T.Text -> IO [GasTestResult (Either PactError [Term Name], EvalState)]
runGasTestByName nname = do
  case (unitTestFromDef (NativeDefName nname)) of
    Nothing -> eitherDie nname (Left ("gas tests not found" :: T.Text))
    Just g -> mockRuns' g

runAllGasTests :: IO ()
runAllGasTests = do
  mapM_ (mockRuns . snd) (HM.toList unitTests)
