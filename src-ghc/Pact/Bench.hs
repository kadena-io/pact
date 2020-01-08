{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Pact.Bench where

import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Error
import Control.Monad.Catch
import Control.Monad

import Criterion.Main

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text (unpack, pack)
import Data.Text.Encoding

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
import Pact.Types.Perf
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Native.Internal
import Pact.Persist.SQLite
import Pact.PersistPactDb hiding (db)
import Pact.Types.Capability

data DoPerf = None | Db | Interp | All
  deriving (Eq,Show)
-- | This needs to be 'None' in CI/git.
doPerf :: DoPerf
doPerf = None

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

die :: String -> String -> IO a
die msg er = throwM $ userError $ msg ++ ": " ++ er

eitherDie :: String -> Either String a -> IO a
eitherDie msg = either (die msg) (return $!)

entity :: Maybe EntityName
entity = Just $ EntityName "entity"

loadBenchModule :: PactDbEnv e -> IO (ModuleData Ref,PersistModuleData)
loadBenchModule db = do
  m <- decodeUtf8 <$> BS.readFile "tests/bench/bench.pact"
  pc <- parseCode m
  let md = MsgData
           (object
            [ "keyset" .= object
              [ "keys" .= ["benchadmin"::Text]
              , "pred" .= (">"::Text)]
            , "acct" .= [pk]
            ])
           Nothing
           pactInitialHash
           [Signer Nothing pk Nothing []]
  let e = setupEvalEnv db entity Transactional md initRefStore
          freeGasEnv permissiveNamespacePolicy noSPVSupport def def
  (r :: Either SomeException EvalResult) <- try $ evalExec  defaultInterpreter e pc
  void $ eitherDie "loadBenchModule (load)" $ fmapL show r
  (benchMod,_) <- runEval def e $ getModule (def :: Info) (ModuleName "bench" Nothing)
  p <- either (die "loadBenchModule" . show) (return $!) $ traverse (traverse toPersistDirect) benchMod
  return (benchMod,p)


parseCode :: Text -> IO ParsedCode
parseCode m = ParsedCode m <$> eitherDie "parseCode" (parseExprs m)

benchNFIO :: NFData a => String -> IO a -> Benchmark
benchNFIO bname = bench bname . nfIO

runPactExec :: PerfTimer -> String -> [Signer] -> Value -> Maybe (ModuleData Ref) -> PactDbEnv e -> ParsedCode -> IO Value
runPactExec pt msg ss cdata benchMod dbEnv pc = do
  let md = MsgData cdata Nothing pactInitialHash ss
      e = setupEvalEnv dbEnv entity Transactional md
          initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def def
      s = perfInterpreter (pack msg) pt $ defaultInterpreterState $
        maybe id (const . initStateModules . HM.singleton (ModuleName "bench" Nothing)) benchMod
  (r :: Either SomeException EvalResult) <- try $! evalExec s e pc
  r' <- eitherDie ("runPactExec': " ++ msg) $ fmapL show r
  return $! toJSON $ _erOutput r'

benchKeySet :: KeySet
benchKeySet = mkKeySet [PublicKey "benchadmin"] ">"

acctRow :: ObjectMap PactValue
acctRow = ObjectMap $ M.fromList
  [("balance",PLiteral (LDecimal 100.0))
  ,("guard",PGuard $ GKeySet (mkKeySet [PublicKey $ encodeUtf8 pk] "keys-all"))
  ]

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


mkBenchCmd :: [SomeKeyPairCaps] -> (String, Text) -> IO (String, Command ByteString)
mkBenchCmd kps (str, t) = do
  cmd <- mkCommand' kps
    $ toStrict . encode
    $ Payload payload "nonce" () ss Nothing
  return (str, cmd)
  where
    payload = Exec $ ExecMsg t Null
    ss = keyPairsToSigners kps

pk :: Text
pk = "0c99d911059580819c6f39ca5c203364a20dbf0a02b0b415f8ce7b48ba3a5bad"

perfEnv :: Text -> PerfTimer -> PactDbEnv p -> PactDbEnv p
perfEnv n pt (PactDbEnv db mv) = PactDbEnv (perfPactDb n pt db) mv

perfInterpreter :: Text -> PerfTimer -> Interpreter e -> Interpreter e
perfInterpreter msg pt (Interpreter i) = Interpreter $ \runInput ->
  perf pt (msg <> ":" <> "interp") $! i runInput

main :: IO ()
main = do
  !fperf <- mkFilePerf "pact-bench-perf"
  let !dbpt = if doPerf == Db || doPerf == All then fperf else def
      !ipt = if doPerf == Interp || doPerf == All then fperf else def
  !pub <- eitherDie "pub" $ parseB16TextOnly pk
  !priv <- eitherDie "priv" $
    parseB16TextOnly "6c938ed95a8abf99f34a1b5edd376f790a2ea8952413526af91b4c3eb0331b3c"
  !keyPair <- eitherDie "keyPair" $
    importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS priv)
  !parsedExps <- force <$> mapM (mapM (eitherDie "parseExps" . parseExprs)) exps
  !pureDb <- perfEnv "puredb" dbpt <$> mkPureEnv neverLog
  initSchema pureDb
  (benchMod',benchMod) <- loadBenchModule pureDb
  !benchCmd <- parseCode "(bench.bench)"
  let
    !params = [PLiteral $ LString "Acct1",PLiteral $ LString "Acct2", PLiteral $ LDecimal 1.0]
    !mcaps = [SigCapability (QualifiedName "bench" "MTRANSFER" def) params
             ,SigCapability (QualifiedName "bench" "TRANSFER" def) params]

    !signer = [Signer Nothing pk Nothing []]
    !msigner = [Signer Nothing pk Nothing mcaps]
  void $ runPactExec def "initPureDb" signer Null Nothing pureDb benchCmd
  !mockDb <- mkMockEnv def { mockRead = MockRead (benchRead benchMod) }
  void $ loadBenchModule mockDb
  void $ runPactExec def "initMockDb" signer Null Nothing mockDb benchCmd
  !mockPersistDb <- mkMockPersistEnv neverLog def { mockReadValue = MockReadValue (benchReadValue benchMod) }
  void $ loadBenchModule mockPersistDb
  void $ runPactExec def "initMockPersistDb" signer Null Nothing mockPersistDb benchCmd
  cmds_ <- traverse (mkBenchCmd [(keyPair,[])]) exps
  !cmds <- return $!! cmds_
  let sqliteFile = "log/bench.sqlite"
  sqliteDb <- perfEnv "sqlite" dbpt <$> mkSQLiteEnv (newLogger neverLog "") True (SQLiteConfig sqliteFile fastNoJournalPragmas) neverLog
  initSchema sqliteDb
  void $ loadBenchModule sqliteDb
  void $ runPactExec def "initSqliteDb" signer Null Nothing sqliteDb benchCmd
  mbenchCmd <- parseCode "(bench.mbench)"
  void $ runPactExec def "init-puredb-mbench" msigner Null Nothing pureDb mbenchCmd
  !round0 <- parseCode "(round 123.456789)"
  !round4 <- parseCode "(round 123.456789 4)"



  let cleanupSqlite = do
        c <- readMVar $ pdPactDbVar sqliteDb
        void $ closeSQLite $ _db c
        removeFile sqliteFile
      sqlEnv b = envWithCleanup (return ()) (const cleanupSqlite) (const b)

  defaultMain
    [ benchParse
    , benchCompile parsedExps
    , benchVerify cmds
    , bgroup "db"
      [ bgroup "uncached"
        [ benchNFIO "puredb"
          (runPactExec ipt "uncached/puredb" signer Null Nothing pureDb benchCmd)
        , benchNFIO "mockdb"
          (runPactExec ipt "uncached/mockdb" signer Null Nothing mockDb benchCmd)
        , benchNFIO "mockpersist"
          (runPactExec ipt "uncached/mockpersist" signer Null Nothing mockPersistDb benchCmd)
        , sqlEnv $ benchNFIO "sqlite"
          (runPactExec ipt "uncached/sqlite" signer Null Nothing sqliteDb benchCmd)
        ]
      , bgroup "cached"
        [ benchNFIO "puredb"
          (runPactExec ipt "cached/puredb" signer Null (Just benchMod') pureDb benchCmd)
        , benchNFIO "mockdb"
          (runPactExec ipt "cached/mockdb" signer Null (Just benchMod') mockDb benchCmd)
        , benchNFIO "mockpersist"
          (runPactExec ipt "cached/mockpersist" signer Null (Just benchMod') mockPersistDb benchCmd)
        , sqlEnv $ benchNFIO "sqlite"
          (runPactExec ipt "cached/sqlite" signer Null (Just benchMod') sqliteDb benchCmd)
        ]
      ]
    , bgroup "caps"
      [ benchNFIO "unmanaged" $
        -- puredb transfer caps
        runPactExec def "caps/unmanaged" msigner Null (Just benchMod') pureDb benchCmd
      , benchNFIO "managed" $
        -- puredb mgd-transfer caps
        runPactExec def "caps/managed" msigner Null (Just benchMod') pureDb mbenchCmd
      ]
    , bgroup "round"
      [ benchNFIO "round0" $ runPactExec def "round0" [] Null Nothing pureDb round0
      , benchNFIO "round4" $ runPactExec def "round4" [] Null Nothing pureDb round4
      ]
    ]
