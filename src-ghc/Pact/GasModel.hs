{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.GasModel where

import Control.Exception              (bracket)
import Control.Monad                  (void)
import Statistics.Types               (Estimate(..))
import Data.List                      (foldl', sortOn)
import GHC.Conc                       (numCapabilities)

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Criterion.Main             as C
import qualified Criterion                  as C
import qualified Criterion.Types            as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Csv                   as Csv
import qualified Options.Applicative        as O


-- Internal exports
--
import Pact.Eval                  (eval)
import Pact.GasModel.Utils
import Pact.GasModel.GasTests
import Pact.Types.GasModel
import Pact.Types.Lang
import Pact.Types.Runtime         hiding (GasPrice)


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
           (O.short 'b' <> O.long "bench" <> O.help "Just bench"))

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

average :: [Double] -> Average
average nums = Average avg
  where meanSum = foldl' (+) 0.0 nums
        total = fromIntegral (length nums)
        avg = meanSum / total


newtype GasPrice = GasPrice Integer
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


setupEnv :: GasSetup e -> IO (EvalEnv e, EvalState)
setupEnv (GasSetup e s _ _) = do
  env <- e
  state <- s
  return (env, state)


mockRun
  :: PactExpression
  -> (EvalEnv e, EvalState)
  -> IO (Either PactError [Term Name], EvalState)
mockRun expr (env, state) = do
  terms <- compileCode (_pactExpressionFull expr)
  exec state env terms


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
      putStrLn $ "Pact results for: " ++ T.unpack (getDescription expr dbSetup)
      (res,state) <- bracket (setupEnv dbSetup)
                         (gasSetupCleanup dbSetup)
                         (mockRun expr)
      printResult res
      return (res,state)


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
  -> IO ()
benchesOnce tests =
  void $ mapOverGasUnitTests tests bench bench


type Means = (NanoSeconds, NanoSeconds, NanoSeconds)

-- | Benchmarks each native function's tests three times
benchesMultiple
  :: (NativeDefName, GasUnitTests)
  -> IO (NativeDefName, [GasTestResult Means])
benchesMultiple (funName, tests) = do
  results <- mapOverGasUnitTests tests run run
  return (funName, results)
  where
    run expr setup = do
      mean1 <- bench expr setup
      mean2 <- bench expr setup
      mean3 <- bench expr setup
      return (mean1, mean2, mean3)


meansToCSVEncoding
  :: [GasTestResult Means]
  -> (Csv.Header, [Csv.NamedRecord])
meansToCSVEncoding results =
  (Csv.header headersField, map toNamedRecord results)
  where
    headersField =
      map Csv.toField (["function", "description",
                        "sqliteDbMean1 (ns)", "sqliteDbMean2 (ns)", "sqliteDbMean3 (ns)",
                        "mockDbMean1 (ns)", "mockDbMean2 (ns)", "mockDbMean3 (ns)"] :: [T.Text])
    toNamedRecord (GasTestResult funName desc (sql1,sql2,sql3) (mock1,mock2,mock3)) =
      Csv.namedRecord $
      zip headersField [Csv.toField (asString funName),
                        Csv.toField desc,
                        Csv.toField sql1,
                        Csv.toField sql2,
                        Csv.toField sql3,
                        Csv.toField mock1,
                        Csv.toField mock2,
                        Csv.toField mock3
                       ]

writeRawCSV
  :: [GasTestResult Means]
  -> IO ()
writeRawCSV results = do
  let (headers, records) = meansToCSVEncoding results
      content = Csv.encodeByName headers records
  BSL8.writeFile "gas-model-raw-data.csv" content


encodeGasPrice
  :: [(NativeDefName, [GasTestResult Means])]
  -> (Csv.Header, [Csv.NamedRecord])
encodeGasPrice allResults =
  (headers,
   map calcPriceAndEncode allResults
  )
  where
    headers = Csv.header $
      map Csv.toField ["function", sqliteHeader, mockHeader]

    calcPriceAndEncode (funName, results) =
      Csv.namedRecord $
      ["function" Csv..= asString funName]
      <> calcPrice (onlySimpleTestResults results)

    sqliteHeader =
      "sqlitedb gas price ("
      <> T.pack (show gasPriceDivisor)
      <> " ns)"
    mockHeader =
      "mockdb gas price ("
      <> T.pack (show gasPriceDivisor)
      <> " ns)"

    calcPrice results =
      [Csv.toField sqliteHeader Csv..= gasPrice sqliteMeans,
       Csv.toField mockHeader Csv..= gasPrice mockMeans
      ]
      where
        sqliteMeans = concatMap
          (toNsList . _gasTestResultSqliteDb) results
        mockMeans = concatMap
          (toNsList . _gasTestResultMockDb) results
        toNsList (n1, n2, n3) = [n1, n2, n3]

    simpleTestCheck (GasTestResult _ desc _ _) =
      (not $ T.isInfixOf "med" desc) &&
      (not $ T.isInfixOf "long" desc)

    onlySimpleTestResults results =
      filter simpleTestCheck results


gasPriceExplanation :: [T.Text]
gasPriceExplanation = [purpose, numCores, os, hardwareNotes,
                       implementation, numOfIterations, backend, calcPrice, ""]
  where
    purpose = "Purpose: Calculate the gas price of Pact native functions using a data-driven model"
    numCores = "Number of cores available for benchmarks: " <> T.pack (show numCapabilities)
    os = "Operating System: <INSERT OS>"
    hardwareNotes = "More inforamtion on hardware used: <INSERT MORE INFORMATION ON HARDWARE USED>"
    implementation = "Implementation: For every native function, executes and benchmarks "
                     <> "(using Criterion) simple examples of said function."
    numOfIterations = "Number of iterations: Each function's tests are run three times against each backend type"
    backend = "Benchmark backend(s): [Sqlite db with `fastNoJournalPragmas`, Mock db]"
    calcPrice = "From benchmark to a native function's price: For every backend type, all of the means of the function's"
                <>" benchmark examples are converted into nanoseconds, averaged together, divided by "
                <> T.pack (show gasPriceDivisor)
                <> ", and rounded up to the nearest integer."


writeGasPriceCSV
  :: [(NativeDefName, [GasTestResult Means])]
  -> IO ()
writeGasPriceCSV results = do
  let (headers, records) = encodeGasPrice results
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
    mapM_ (benchesOnce . snd) tests
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
runGasTestByName :: T.Text -> IO ()
runGasTestByName nname = do
  case (unitTestFromDef (NativeDefName nname)) of
    Nothing -> print $ show nname ++ "'s gas tests not found."
    Just g -> do
      print $ show nname
      _ <- mockRuns' g
      return ()

runAllGasTests :: IO ()
runAllGasTests = do
  mapM_ (mockRuns . snd) (HM.toList unitTests)
