{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Pact.Bench where

import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad

import Criterion.Main

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, unpack, pack)

import System.CPUTime
import System.Directory
import Unsafe.Coerce

import Pact.Compile
import Pact.Gas
import Pact.Interpreter
import Pact.MockDb
import Pact.Parse
import Pact.Persist
import Pact.Persist.MockPersist
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Lang
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Native.Internal
import Pact.Persist.SQLite
import Pact.PersistPactDb hiding (db)

longStr :: Int -> Text
longStr n = pack $ "\"" ++ take n (cycle "abcdefghijklmnopqrstuvwxyz") ++ "\""

exps :: [(String,Text)]
exps = map (either id (unpack &&& id)) [
  Left ("longStr 10",longStr 10),
  Left ("longStr 100", longStr 100),
  Left ("longStr 1000", longStr 1000),
  Right "(+ 1 2)",
  Right "(+ 1 (+ 1 2))",
  Right "(demo.transfer \"Acct1\" \"Acct2\" 1.00)",
  Right "(+ 1 2) (foo.bar true -23.345875)"
  ]

benchParse :: Benchmark
benchParse = bgroup "parse" $ (`map` exps) $
             \(bname,ex) -> bench bname $ (`whnf` ex) $ \e -> case parseExprs e of
               Right s -> s
               Left er -> error $ "Pact parse failed: " ++ er

benchCompile :: [(String,[Exp Parsed])] -> Benchmark
benchCompile es = bgroup "compile" $ (`map` es) $
  \(bname,exs) -> bench bname $ nf (map (either (error . show) show . compile mkEmptyInfo)) exs

benchVerify :: [(String,Command ByteString)] -> Benchmark
benchVerify cs = bgroup "verify" $ (`map` cs) $
  \(bname,c) ->
    bench bname $
      nf (verifyCommand :: Command ByteString -> ProcessedCommand () ParsedCode) c

eitherDie :: Either String a -> IO a
eitherDie = either (throwIO . userError) (return $!)

entity :: Maybe EntityName
entity = Just $ EntityName "entity"

loadBenchModule :: PactDbEnv e -> IO (ModuleData Ref,PersistModuleData)
loadBenchModule db = do
  m <- pack <$> readFile "tests/bench/bench.pact"
  pc <- parseCode m
  let md = MsgData S.empty
           (object ["keyset" .= object ["keys" .= ["benchadmin"::Text], "pred" .= (">"::Text)]])
           Nothing
           pactInitialHash
  let e = setupEvalEnv db entity Transactional md initRefStore
          freeGasEnv permissiveNamespacePolicy noSPVSupport def
  void $ evalExec def e pc
  (benchMod,_) <- runEval def e $ getModule (def :: Info) (ModuleName "bench" Nothing)
  p <- either (throwIO . userError . show) (return $!) $ traverse (traverse toPersistDirect) benchMod
  return (benchMod,p)


parseCode :: Text -> IO ParsedCode
parseCode m = ParsedCode m <$> eitherDie (parseExprs m)

benchNFIO :: NFData a => String -> IO a -> Benchmark
benchNFIO bname = bench bname . nfIO

runPactExec :: Maybe (ModuleData Ref) -> PactDbEnv e -> ParsedCode -> IO Value
runPactExec benchMod dbEnv pc = do
  let e = setupEvalEnv dbEnv entity Transactional (initMsgData pactInitialHash)
          initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def
      s = maybe def (initStateModules . HM.singleton (ModuleName "bench" Nothing)) benchMod
  toJSON . _erOutput <$> evalExec s e pc

benchKeySet :: KeySet
benchKeySet = KeySet [PublicKey "benchadmin"] (Name ">" def)

acctRow :: ObjectMap PactValue
acctRow = ObjectMap $ M.fromList [("balance",PLiteral (LDecimal 100.0))]

benchRead :: PersistModuleData -> Domain k v -> k -> Method () (Maybe v)
benchRead _ KeySets _ = rc (Just benchKeySet)
benchRead _ UserTables {} _ = rc (Just acctRow)
benchRead benchMod Modules _ = rc (Just benchMod)
benchRead _ _ _ = rc Nothing

benchReadValue :: PersistModuleData -> Table k -> k -> Persist () (Maybe v)
benchReadValue benchMod (DataTable t) _k
  | t == "SYS_keysets" = rcp $ Just (unsafeCoerce benchKeySet)
  | t == "USER_bench_bench-accounts" = rcp $ Just (unsafeCoerce acctRow)
  | t == "SYS_modules" = rcp $ Just (unsafeCoerce benchMod)
  | otherwise = error (show t)
benchReadValue _ (TxTable _t) _k = rcp Nothing


mkBenchCmd :: [SomeKeyPair] -> (String, Text) -> IO (String, Command ByteString)
mkBenchCmd kps (str, t) = do
  cmd <- mkCommand' kps (toStrict $ encode (Payload (Exec (ExecMsg t Null)) "nonce" ()
                                            (keyPairsToSigners kps)))
  return (str, cmd)


main :: IO ()
main = do
  !pub <- eitherDie $ parseB16TextOnly "0c99d911059580819c6f39ca5c203364a20dbf0a02b0b415f8ce7b48ba3a5bad"
  !priv <- eitherDie $ parseB16TextOnly "6c938ed95a8abf99f34a1b5edd376f790a2ea8952413526af91b4c3eb0331b3c"
  !keyPair <- eitherDie $ importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS priv)
  !parsedExps <- force <$> mapM (mapM (eitherDie . parseExprs)) exps
  !pureDb <- mkPureEnv neverLog
  initSchema pureDb
  (benchMod',benchMod) <- loadBenchModule pureDb
  !benchCmd <- parseCode "(bench.bench)"
  print =<< runPactExec Nothing pureDb benchCmd
  !mockDb <- mkMockEnv def { mockRead = MockRead (benchRead benchMod) }
  void $ loadBenchModule mockDb
  print =<< runPactExec Nothing mockDb benchCmd
  !mockPersistDb <- mkMockPersistEnv neverLog def { mockReadValue = MockReadValue (benchReadValue benchMod) }
  void $ loadBenchModule mockPersistDb
  print =<< runPactExec Nothing mockPersistDb benchCmd
  cmds_ <- traverse (mkBenchCmd [keyPair]) exps
  !cmds <- return $!! cmds_
  let sqliteFile = "log/bench.sqlite"
  sqliteDb <- mkSQLiteEnv (newLogger neverLog "") True (SQLiteConfig sqliteFile []) neverLog
  initSchema sqliteDb
  void $ loadBenchModule sqliteDb
  print =<< runPactExec Nothing sqliteDb benchCmd

  let cleanupSqlite = do
        c <- readMVar $ pdPactDbVar sqliteDb
        void $ closeSQLite $ _db c
        removeFile sqliteFile
      sqlEnv b = envWithCleanup (return ()) (const cleanupSqlite) (const b)

  defaultMain
    [ benchParse
    , benchCompile parsedExps
    , benchVerify cmds
    , benchNFIO "puredb" (runPactExec Nothing pureDb benchCmd)
    , benchNFIO "mockdb" (runPactExec Nothing mockDb benchCmd)
    , benchNFIO "mockpersist" (runPactExec Nothing mockPersistDb benchCmd)
    , benchNFIO "sqlite" (runPactExec Nothing sqliteDb benchCmd)
    , benchNFIO "puredb-withmod" (runPactExec (Just benchMod') pureDb benchCmd)
    , benchNFIO "mockdb-withmod" (runPactExec (Just benchMod') mockDb benchCmd)
    , benchNFIO "mockpersist-withmod" (runPactExec (Just benchMod') mockPersistDb benchCmd)
    , sqlEnv $ benchNFIO "sqlite-withmod" (runPactExec (Just benchMod') sqliteDb benchCmd)
    ]
