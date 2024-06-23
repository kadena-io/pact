{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Decimal
import Data.Text (Text)
import Data.Aeson
import Data.Default
import Control.Monad
import Criterion
import NeatInterpolation (text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.ProgressBar
import Pact.Types.Lang
import Pact.Interpreter
import System.FilePath
import Pact.Types.RowData
import Pact.Types.Persistence
import Control.Exception
import Pact.Types.Runtime
import Pact.Types.Command
import Pact.JSON.Legacy.Value
import Pact.Types.SPV
import Pact.Gas
import Pact.Bench
import Pact.Runtime.Utils
import Pact.Gas.Table
import qualified Pact.Eval as Eval

import Control.DeepSeq
import Pact.Types.Logger
import Pact.Types.Capability
import Pact.Types.PactValue
import Pact.Types.Pretty
import qualified System.Environment as Env
import Criterion.Main

main :: IO ()
main = do
  v <- Env.lookupEnv "RESET_COIN_BENCH_DB"
  defaultMain [allBenchmarks (v == Just "1")]

data CoinBenchSenders
  = CoinBenchSenderA
  | CoinBenchSenderB
  | CoinBenchSenderC
  | CoinBenchSenderD
  deriving Show

getSender :: CoinBenchSenders -> String
getSender = drop 9 . show

senderKeyA :: Text
senderKeyA = T.replicate 64 "a"
senderKeyB :: Text
senderKeyB = T.replicate 63 "a" <> "b"
senderKeyC :: Text
senderKeyC = T.replicate 63 "a" <> "c"
senderKeyD :: Text
senderKeyD = T.replicate 63 "a" <> "d"


pubKeyFromSenderRaw :: CoinBenchSenders -> Text
pubKeyFromSenderRaw = \case
  CoinBenchSenderA -> senderKeyA
  CoinBenchSenderB -> senderKeyB
  CoinBenchSenderC -> senderKeyC
  CoinBenchSenderD -> senderKeyD

kColonFromSender :: CoinBenchSenders -> Text
kColonFromSender = ("k:" <>) . pubKeyFromSenderRaw

pubKeyFromSender :: CoinBenchSenders -> PublicKeyText
pubKeyFromSender = PublicKeyText . pubKeyFromSenderRaw


getRightIO :: Exception e => Either e a -> IO a
getRightIO = either throwIO pure

coinTransferTxRaw :: Text -> Text -> Text
coinTransferTxRaw sender receiver =
  [text| (coin.transfer "$sender" "$receiver" 200.0) |]

coinTransferCreateTxRaw :: Text -> Text -> Text -> Text
coinTransferCreateTxRaw sender receiver receiverKs =
  [text| (coin.transfer-create "$sender" "$receiver" (read-keyset "$receiverKs") 200.0) |]

loadCoinBenchModule :: FilePath -> PactDbEnv e -> IO (ModuleData Ref,PersistModuleData)
loadCoinBenchModule fp db = do
  m <- T.readFile fp
  pc <- parseCode m
  let md = MsgData
           (toLegacyJson $ object
            [ "a" .= [pubKeyFromSenderRaw CoinBenchSenderA]
            , "b" .= [pubKeyFromSenderRaw CoinBenchSenderB]
            , "c" .= [pubKeyFromSenderRaw CoinBenchSenderC]
            , "d" .= [pubKeyFromSenderRaw CoinBenchSenderD]
            ])
           Nothing
           pactInitialHash
           []
           []
  let ec = ExecutionConfig $ S.fromList [FlagEnforceKeyFormats]
  e <- setupEvalEnv db Nothing Transactional md (versionedNativesRefStore ec)
          freeGasEnv permissiveNamespacePolicy noSPVSupport def ec
  (r :: Either SomeException EvalResult) <- try $ evalExec defaultInterpreter e pc
  void $ eitherDie "loadBenchModule (load)" $ either (Left . show) Right r
  (benchMod,_) <- runEval def e $ getModule (def :: Info) (ModuleName "coin" Nothing)
  p <- either (die "loadBenchModule" . show) (return $!) $ traverse (traverse toPersistDirect) benchMod
  return (benchMod,p)

coinBenchGasEnv :: GasEnv
coinBenchGasEnv = GasEnv (gasLimitToMilliGasLimit 300_000_000) 0.01 $ tableGasModel defaultGasConfig

setupBenchEvalEnv :: PactDbEnv e -> MsgData -> IO (EvalEnv e)
setupBenchEvalEnv db md = do
  let ec = ExecutionConfig $ S.fromList [FlagEnforceKeyFormats]
  setupEvalEnv db Nothing Transactional md (versionedNativesRefStore ec)
          coinBenchGasEnv permissiveNamespacePolicy noSPVSupport def ec

newtype NoForce e
 = NoForce e

instance NFData (NoForce e) where
  rnf _ = ()

transferCapFromSender :: CoinBenchSenders -> CoinBenchSenders -> Decimal -> SigCapability
transferCapFromSender sender receiver amount =
  SigCapability (QualifiedName (ModuleName "coin" Nothing) "TRANSFER" def)
    [ PLiteral (LString (kColonFromSender sender))
    , PLiteral (LString  (kColonFromSender receiver))
    , PLiteral (LDecimal amount)]

transferSigners :: CoinBenchSenders -> CoinBenchSenders -> Signer
transferSigners sender receiver =
  Signer Nothing (pubKeyFromSenderRaw sender) Nothing [transferCapFromSender sender receiver 200]


runCoinTransferTx
  :: PactDbEnv e
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Benchmark
runCoinTransferTx db sender receiver =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, NoForce ee, es) ->
      fst <$> runEval es ee (Eval.reduce term)
    where
    pdb = pdPactDb db
    title =
      "Coin transfer from "
      <> getSender sender
      <> " to "
      <> getSender receiver
    mkTerm = do
      let md = MsgData
              (toLegacyJson $ object [])
              Nothing
              pactInitialHash
              [transferSigners sender receiver]
              []
      ee <- setupBenchEvalEnv db md
      () <$ _beginTx pdb Transactional (pdPactDbVar db)
      let termText = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      [compiledTerm] <- compileExp termText
      (term, es) <- runEval def ee (Eval.enscope compiledTerm)
      pure (term, NoForce ee, es)
    doRollback _ = do
      _rollbackTx pdb (pdPactDbVar db)

factorialNTXRaw :: Int -> Text
factorialNTXRaw n =
  [text| (fold (*) 1 (enumerate 1 ${n'})) |]
  where
  n' = T.pack (show n)

deepLetTXRaw :: Int -> Text
deepLetTXRaw n =
  [text| (let* ($nestedLets) $lastVar) |]
  where
  initial = "(x1 1)"
  nestedLets = T.concat $ initial :
    [ [text| (x$ncurr (* $ncurr x${nprev})) |] | (prev, curr) <- zip [1..n] [2..n]
    , let nprev = T.pack (show prev)
    , let ncurr = T.pack (show curr)]
  lastVar = "x" <> T.pack (show n)

runPureBench
  :: PactDbEnv e
  -> String
  -> Text
  -> Benchmark
runPureBench db benchTitle termText =
  bench benchTitle $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, NoForce ee, es) ->
      fst <$> runEval es ee (Eval.reduce term)
    where
    mkTerm = do
      let md = MsgData
              (toLegacyJson $ object [])
              Nothing
              pactInitialHash
              []
              []
      ee <- setupBenchEvalEnv db md
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      [compiledTerm] <- compileExp termText
      (term, es) <- runEval def ee (Eval.enscope compiledTerm)
      pure (term, NoForce ee, es)
    doRollback _ = pure ()

runCoinTransferTxWithNameReso
  :: PactDbEnv e
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Benchmark
runCoinTransferTxWithNameReso db sender receiver =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, NoForce ee) ->
      fst <$> runEval def ee (Eval.eval term)
    where
    pdb = pdPactDb db
    title =
      "Coin transfer from "
      <> getSender sender
      <> " to "
      <> getSender receiver
    mkTerm = do
      let md = MsgData
              (toLegacyJson $ object [])
              Nothing
              pactInitialHash
              [transferSigners sender receiver]
              []
      ee <- setupBenchEvalEnv db md
      () <$ _beginTx pdb Transactional (pdPactDbVar db)
      let termText = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      [compiledTerm] <- compileExp termText
      pure (compiledTerm, NoForce ee)
    doRollback _ = do
      _rollbackTx pdb (pdPactDbVar db)

coinTableName :: TableName
coinTableName = TableName "coin_coin-table"

prePopulateCoinEntries :: PactDbEnv e -> IO ()
prePopulateCoinEntries pdb = do
  let style = defStyle {stylePrefix = msg "Pre-filling the coin table"}
  putStrLn "Setting up the coin table"
  pbar <- newProgressBar style 10 (Progress 0 100_0 ())
  forM_ [1 :: Integer .. 100_0] $ \i -> do
    let n = renderCompactText $ pactHash $ T.encodeUtf8 $ T.pack (show i)
    writeBench pdb Write (UserTables coinTableName) (RowKey n) (RowData RDV1 (obj n))
    incProgress pbar 1
  where
  writeBench dbe wt domain k v =
    _writeRow (pdPactDb dbe) wt domain k v (pdPactDbVar dbe)
  obj n = ObjectMap $ M.fromList
    [(FieldKey "balance", RDLiteral $ LDecimal 100)
    , (FieldKey "guard"
    , RDGuard (GKeySet (KeySet (S.singleton (PublicKeyText n)) (Name (BareName "keys-all" def)))))
    ]


benchmarkSqliteFile :: String
benchmarkSqliteFile = "core-benches.sqlite"

beginTxBench :: PactDbEnv e -> IO (Maybe TxId)
beginTxBench dbe =
  _beginTx (pdPactDb dbe) Transactional (pdPactDbVar dbe)

commitTxBench :: PactDbEnv e -> IO [TxLogJson]
commitTxBench dbe =
  _commitTx (pdPactDb dbe) (pdPactDbVar dbe)

rollbackTxBench :: PactDbEnv e -> IO ()
rollbackTxBench dbe =
  _rollbackTx (pdPactDb dbe) (pdPactDbVar dbe)


contractsPath :: FilePath
contractsPath = "contract-benchmarks" </> "contracts"

allBenchmarks :: Bool -> Benchmark
allBenchmarks _resetDb =
  env mkPactDb $ \ ~(NoForce pdb) ->
    bgroup "Coin benches"
      [
      runPureBench pdb "factorial 1000" (factorialNTXRaw 1000)
      , runPureBench pdb "Let 100" (deepLetTXRaw 100)
      , runPureBench pdb "Let 1000" (deepLetTXRaw 1000)
      -- , runPureBench pdb "Let 10000" (deepLetTXRaw 10000)
      , coinTransferBenches pdb
      ]
  where
  coinTransferBenches pdb =
    bgroup "CoinTransfer"
    [ runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB
    , runCoinTransferTxWithNameReso pdb CoinBenchSenderA CoinBenchSenderB
    ]
  mkPactDb = do
    db <- mkPureEnv neverLog
    initSchema db
    _ <- loadCoinBenchModule (contractsPath </> "coin-v5-create.pact") db
    pure (NoForce db)

