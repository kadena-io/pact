{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pact.Bench where

import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Lens hiding ((.=))

import Criterion.Main hiding (env)

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (unpack, pack, intercalate)
import Data.Text.Encoding
import Pact.Time

import System.IO
import System.Directory
import Unsafe.Coerce

import Pact.Compile
import Pact.Eval (eval)
import Pact.Gas
import Pact.Gas.Table
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
import Pact.Types.RowData
import Pact.Types.Advice
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Persist.SQLite
import Pact.PersistPactDb hiding (db)
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Capability
import Pact.Runtime.Utils
import Pact.JSON.Legacy.Value
import qualified Pact.JSON.Encode as J

-- | Flags for enabling file-based perf bracketing,
-- see 'mkFilePerf' below.
data DoPerf =
    None -- disabled
  | Db -- PactDb method bracketing
  | Interp -- Bracketing around interpreter and in EvalEnv
  | All -- Both
  deriving (Eq,Show)

-- | This needs to be 'None' in CI/git
-- file-based perf is done manually to investigate issues
-- but slows down benchmarks.
-- CI just runs the regular benchmarks.
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

pureExps :: [(String,Text)]
pureExps = map (unpack &&& id)
  [ "1"
  , "1 2"
  , "[1 2]"
  , "[1 2 3]"
  , "{ 'a: 1 }"
  , "{ 'a: 1, 'b: 2 }"
  , "(identity 1)"
  , "(+ 1 2)"
  , "(+ 1 (+ 1 2))"
  , "(+ 1 (+ 1 (+ 1 2)))"
  , "(+ 1 (+ 1 (+ 1 (+ 1 2))))"
  , "(let ((a 1)) (+ a a))"
  , "(let ((a 1) (b 2)) (+ a b))"
  , "(let* ((a 1) (b a)) (+ a b))"
  , "(bind { 'a: 1 } { 'a := a } a)"
  , "(time \"1970-01-01T00:00:00Z\")"
  ]

benchParse :: Benchmark
benchParse = bgroup "parse" $ (`map` exps) $
             \(bname,ex) -> bench bname $ (`whnf` ex) $ \e -> case parseExprs e of
               Right s -> s
               Left er -> error $ "Pact parse failed: " ++ er

benchCompile :: [(String,[Exp Parsed])] -> Benchmark
benchCompile es = bgroup "compile" $ (`map` es) $
  \(bname,exs) -> bench bname $ nf (map (either (error . show) force . compile def mkEmptyInfo)) exs

compileExp :: Text -> IO [Term Name]
compileExp code = do
  pcs <- _pcExps <$> parseCode code
  forM pcs $ \pc -> do
    either (\e -> die "compileExp" $ show code ++ ": " ++ show e) (return $!!) $
      compile def mkEmptyInfo pc


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
           (toLegacyJson $ object
            [ "keyset" .= object
              [ "keys" .= ["benchadmin"::Text]
              , "pred" .= (">"::Text)]
            , "acct" .= [pk]
            ])
           Nothing
           pactInitialHash
           [Signer Nothing pk Nothing []]
           []
  let ec = ExecutionConfig $ S.fromList [FlagDisablePact44]
  e <- setupEvalEnv db entity Transactional md (versionedNativesRefStore ec)
          freeGasEnv permissiveNamespacePolicy noSPVSupport def ec
  (r :: Either SomeException EvalResult) <- try $ evalExec  defaultInterpreter e pc
  void $ eitherDie "loadBenchModule (load)" $ fmapL show r
  (benchMod,_) <- runEval def e $ getModule (def :: Info) (ModuleName "bench" Nothing)
  p <- either (die "loadBenchModule" . show) (return $!) $ traverse (traverse toPersistDirect) benchMod
  return (benchMod,p)

loadCompile :: FilePath -> IO [Term Name]
loadCompile f = do
  m <- decodeUtf8 <$> BS.readFile f
  compileExp m


prodGasEnv :: GasEnv
prodGasEnv = GasEnv (gasLimitToMilliGasLimit 100_000) 0.01 $ tableGasModel defaultGasConfig

parseCode :: Text -> IO ParsedCode
parseCode m = ParsedCode m <$> eitherDie "parseCode" (parseExprs m)

benchNFIO :: NFData a => String -> IO a -> Benchmark
benchNFIO bname = bench bname . nfIO

runPactExec :: Advice -> String -> [Signer] -> Value -> Maybe (ModuleData Ref) ->
               PactDbEnv e -> ParsedCode -> IO [PactValue]
runPactExec pt msg ss cdata benchMod dbEnv pc = do
  let md = MsgData (toLegacyJson cdata) Nothing pactInitialHash ss []
      ec = ExecutionConfig $ S.fromList [FlagDisablePact44]
  e <- set eeAdvice pt <$> setupEvalEnv dbEnv entity Transactional md (versionedNativesRefStore ec)
          prodGasEnv permissiveNamespacePolicy noSPVSupport def ec
  let s = perfInterpreter pt $ defaultInterpreterState $
          maybe id (const . initStateModules . HM.singleton (ModuleName "bench" Nothing)) benchMod
  (r :: Either SomeException EvalResult) <- try $! evalExec s e pc
  r' <- eitherDie ("runPactExec': " ++ msg) $ fmapL show r
  return $!! _erOutput r'

execPure :: Advice -> PactDbEnv e -> (String,[Term Name]) -> IO [Term Name]
execPure pt dbEnv (n,ts) = do
  let md = MsgData (toLegacyJson Null) Nothing pactInitialHash [] []
      ec = ExecutionConfig $ S.fromList [FlagDisablePact44]
  env <- set eeAdvice pt <$> setupEvalEnv dbEnv entity Local md (versionedNativesRefStore ec)
            prodGasEnv permissiveNamespacePolicy noSPVSupport def ec
  o <- try $ runEval def env $ mapM eval ts
  case o of
    Left (e :: SomeException) -> die "execPure" (n ++ ": " ++ show e)
    Right rs -> return $!! fst rs

benchPures :: Advice -> PactDbEnv e -> [(String,[Term Name])] -> Benchmark
benchPures pt dbEnv es = bgroup "pures" $ (`map` es) $
  \p -> benchNFIO (fst p) $ execPure pt dbEnv p

benchKeySet :: KeySet
benchKeySet = mkKeySet [PublicKeyText "benchadmin"] ">"

acctRow :: RowData
acctRow = RowData RDV1 $ fmap pactValueToRowData $ ObjectMap $ M.fromList
  [("balance",PLiteral (LDecimal 100.0))
  ,("guard",PGuard $ GKeySet (mkKeySet [PublicKeyText pk] "keys-all"))
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


mkBenchCmd :: [Ed25519KeyPairCaps] -> (String, Text) -> IO (String, Command ByteString)
mkBenchCmd kps (str, t) = do
  cmd <- mkCommand' kps
    $ J.encodeStrict
    $ Payload payload "nonce" (J.Aeson ()) ss Nothing Nothing
  return (str, cmd)
  where
    payload = Exec $ ExecMsg t (toLegacyJson Null)
    ss = keyPairsToSigners kps

pk :: Text
pk = "0c99d911059580819c6f39ca5c203364a20dbf0a02b0b415f8ce7b48ba3a5bad"

perfEnv :: Advice -> PactDbEnv p -> PactDbEnv p
perfEnv pt (PactDbEnv db mv) = PactDbEnv (advisePactDb pt db) mv

perfInterpreter :: Advice -> Interpreter e -> Interpreter e
perfInterpreter pt (Interpreter i) = Interpreter $ \runInput -> do
  c <- advise def pt (AdviceTx initialHash)
  !r <- i runInput
  c ()
  pure r


mkFilePerf :: FilePath -> IO Advice
mkFilePerf fp = do
  h <- openFile fp WriteMode
  c <- newChan
  void $ forkIO $ forever $ do
    m <- getChanContents c
    (`mapM_` m) $ \s -> do
      hPutStrLn h s
      hFlush h
  return $ Advice $ \_ msg -> do
    s <- liftIO $ time
    return $ const $ liftIO $ do
      e <- time
      writeChan c $! unpack $ (tShow msg) <> ": " <> pack (show (e .-. s))
      -- uncomment below to time the chan write itself
      -- f <- time
      -- writeChan c $! "chan: " <> (show (f .-. e))


  where
    time = getCurrentTime

main :: IO ()
main = do
  -- uncomment below to see if "-N" is working, important for file perf log
  -- print =<< getNumCapabilities
  !fperf <- if doPerf /= None then mkFilePerf "pact-bench-perf" else pure def
  let !dbPerf = if doPerf == Db || doPerf == All then fperf else def
      !interpPerf = if doPerf == Interp || doPerf == All then fperf else def
  !pub <- eitherDie "pub" $ parseB16TextOnly pk
  !priv <- eitherDie "priv" $
    parseB16TextOnly "6c938ed95a8abf99f34a1b5edd376f790a2ea8952413526af91b4c3eb0331b3c"
  !keyPair <- eitherDie "keyPair" $
    importEd25519KeyPair (Just $ PubBS pub) (PrivBS priv)
  !parsedExps <- force <$> mapM (mapM (eitherDie "parseExps" . parseExprs)) exps
  !pureDb <- perfEnv dbPerf <$> mkPureEnv neverLog
  initSchema pureDb
  (benchMod',benchMod) <- loadBenchModule pureDb
  !benchCmd <- parseCode "(bench.bench)"
  !bench10Cmds <- parseCode (intercalate " " (replicate 10 "(bench.bench)"))
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
  !cmds <- force <$> traverse (mkBenchCmd [(keyPair,[])]) exps
  sqliteFile <- "log/bench.sqlite" <$ createDirectoryIfMissing True "log"
  sqliteDb <- perfEnv dbPerf <$> mkSQLiteEnv (newLogger neverLog "") True (SQLiteConfig sqliteFile fastNoJournalPragmas) neverLog
  initSchema sqliteDb
  void $ loadBenchModule sqliteDb
  void $ runPactExec def "initSqliteDb" signer Null Nothing sqliteDb benchCmd
  void $ runPactExec def "initSqliteDb" signer Null Nothing sqliteDb bench10Cmds
  mbenchCmd <- parseCode "(bench.mbench)"
  void $ runPactExec def "init-puredb-mbench" msigner Null Nothing pureDb mbenchCmd
  !round0 <- parseCode "(round 123.456789)"
  !round4 <- parseCode "(round 123.456789 4)"
  !pures <- force <$> mapM (mapM compileExp) pureExps
  !timeTest <- loadCompile "tests/pact/time.repl"
  replS <- setReplLib <$> initReplState Quiet Nothing
  let tt = evalReplEval def replS (mapM eval timeTest)
  void $! eitherDie "timeTest failed" . fmapL show =<< tt

  wrap10Cmd <- parseCode "(bench.wrap10 100)"
  wrap10MonoCmd <- parseCode "(bench.wrap10_integer 100)"

  arityCmd0 <- parseCode "(bench.arity_tc_0)"
  arityCmd1 <- parseCode "(bench.arity_tc_1 1)"
  arityCmd10 <- parseCode "(bench.arity_tc_10 1 1 1 1 1 1 1 1 1 1)"
  arityCmd40 <- parseCode "(bench.arity_tc_40 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)"

  aritySmallObj <- parseCode "(bench.arity_small_obj {\"a\": 1})"
  arityMediumObj <- parseCode "(bench.arity_medium_obj {\"a\":1, \"b\":true, \"c\":1, \"d\":{\"a\":1}, \"e\":1, \"f\":true, \"g\":1, \"h\":{\"a\":1} })"
  arityLargeObj <- parseCode "(bench.arity_large_obj {\"a\":1, \"b\":true, \"c\":1, \"d\":{\"a\":1}, \"e\":1, \"f\":true, \"g\":1, \"h\":{\"a\":1}, \"i\":1, \"j\":true, \"k\":1, \"l\":{\"a\":1}, \"m\":1, \"n\":true, \"o\":1, \"p\":{\"a\":1} })"

  accumCmd0 <- parseCode "(bench.accum (enumerate 1 0))"
  accumCmd1 <- parseCode "(bench.accum (enumerate 1 1))"
  accumCmd100 <- parseCode "(bench.accum (enumerate 1 100))"

  let cleanupSqlite = do
        c <- readMVar $ pdPactDbVar sqliteDb
        void $ closeSQLite $ _db c
        removeDirectoryRecursive "log"
      closeSqlEnv b = envWithCleanup (return ()) (const cleanupSqlite) (const b)

  defaultMain
    [ benchParse
    , benchCompile parsedExps
    , benchVerify cmds
    , benchPures fperf mockDb pures
    , bgroup "db"
      [ bgroup "uncached"
        [ benchNFIO "puredb"
          (runPactExec interpPerf "uncached/puredb" signer Null Nothing pureDb benchCmd)
        , benchNFIO "mockdb"
          (runPactExec interpPerf "uncached/mockdb" signer Null Nothing mockDb benchCmd)
        , benchNFIO "mockpersist"
          (runPactExec interpPerf "uncached/mockpersist" signer Null Nothing mockPersistDb benchCmd)
        , benchNFIO "sqlite"
          (runPactExec interpPerf "uncached/sqlite" signer Null Nothing sqliteDb benchCmd)
        ]
      , bgroup "cached"
        [ benchNFIO "puredb"
          (runPactExec interpPerf "cached/puredb" signer Null (Just benchMod') pureDb benchCmd)
        , benchNFIO "mockdb"
          (runPactExec interpPerf "cached/mockdb" signer Null (Just benchMod') mockDb benchCmd)
        , benchNFIO "mockpersist"
          (runPactExec interpPerf "cached/mockpersist" signer Null (Just benchMod') mockPersistDb benchCmd)
        , benchNFIO "sqlite"
          (runPactExec interpPerf "cached/sqlite" signer Null (Just benchMod') sqliteDb benchCmd)
        ]
      , bgroup "cached-10x"
        [ benchNFIO "puredb"
          (runPactExec interpPerf "cached/puredb" signer Null (Just benchMod') pureDb bench10Cmds)
        , benchNFIO "mockdb"
          (runPactExec interpPerf "cached/mockdb" signer Null (Just benchMod') mockDb bench10Cmds)
        , benchNFIO "mockpersist"
          (runPactExec interpPerf "cached/mockpersist" signer Null (Just benchMod') mockPersistDb bench10Cmds)
        , closeSqlEnv $ benchNFIO "sqlite"
          (runPactExec interpPerf "cached/sqlite" signer Null (Just benchMod') sqliteDb bench10Cmds)
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
    , benchNFIO "time" $ fmap fst <$> evalReplEval def replS (mapM eval timeTest)
    , bgroup "defun"
      [ bgroup "return-type-tc"
        [ benchNFIO "wrap10" $ runPactExec def "wrap10" [] Null Nothing pureDb wrap10Cmd
        , benchNFIO "wrap10_mono" $ runPactExec def "wrap10_mono" [] Null Nothing pureDb wrap10MonoCmd
        , benchNFIO "accum100" $ runPactExec def "accum100" [] Null Nothing pureDb accumCmd100
        ]
      , bgroup "arity"
        [ benchNFIO "00-args" $ runPactExec def "00-args" [] Null Nothing pureDb arityCmd0
        , benchNFIO "01-args" $ runPactExec def "01-args" [] Null Nothing pureDb arityCmd1
        , benchNFIO "10-args" $ runPactExec def "10-args" [] Null Nothing pureDb arityCmd10
        , benchNFIO "40-args" $ runPactExec def "40-args" [] Null Nothing pureDb arityCmd40
        ]
      , bgroup "object-size"
        [ benchNFIO "small-obj" $ runPactExec def "small-obj" [] Null Nothing pureDb aritySmallObj
        , benchNFIO "medium-obj" $ runPactExec def "medium-obj" [] Null Nothing pureDb arityMediumObj
        , benchNFIO "large-obj" $ runPactExec def "large-obj" [] Null Nothing pureDb arityLargeObj
        ]
      , bgroup "list-tc"
        [ benchNFIO "000-items" $ runPactExec def "000-items" [] Null Nothing pureDb accumCmd0
        , benchNFIO "001-items" $ runPactExec def "001-items" [] Null Nothing pureDb accumCmd1
        , benchNFIO "100-items" $ runPactExec def "100-items" [] Null Nothing pureDb accumCmd100
        ]
      ]
    ]
