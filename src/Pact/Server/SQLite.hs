{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Pact.Server.SQLite where

import Database.SQLite3.Direct as SQ3
import qualified Data.Text as T
import System.Directory
import Data.Monoid
import Control.Lens
import Data.String
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Criterion hiding (env)
import Data.Text.Encoding
import System.CPUTime
import Data.Default
import Prelude hiding (log)
import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import qualified Data.Attoparsec.Text as AP

import Pact.Server.Types

import Pact.Types
import Pact.Types.Orphans ()
import Pact.Compile
import Pact.Eval
import Pact.Repl

psl :: PactDb PSL
psl =
  PactDb {

   _readRow = \d k e ->
       case d of
           KeySets -> readSysTable e keysetsTable (tmpSysCache.cachedKeySets) (asString k)
           Modules -> readSysTable e modulesTable (tmpSysCache.cachedModules) (asString k)
           (UserTables t) -> readUserTable e t k

 , _writeRow = \wt d k v e ->
       case d of
           KeySets -> writeSys e wt cachedKeySets keysetsTable k v
           Modules -> writeSys e wt cachedModules modulesTable k v
           (UserTables t) -> writeUser e wt t k v

 , _keys = \tn e -> withMVar e $ \m ->
       mapM decodeText_ =<<
           qry_ (_conn m) ("select key from " <> userTable tn <> " order by key") [RText]

 , _txids = \tn tid e -> withMVar e $ \m ->
       mapM decodeInt_ =<<
           qry (_conn m) ("select txid from " <> userTxRecord tn <> " where txid > ? order by txid")
               [SInt (fromIntegral tid)] [RInt]

 , _createUserTable = \tn mn ksn e ->
       createUserTable' e tn mn ksn

 , _getUserTableInfo = \tn e -> getUserTableInfo' e tn

 , _beginTx = \s -> withMVar s resetTemp >>= \m -> execs_ (tBegin (_txStmts m))

 , _commitTx = \tid s -> modifyMVar_ s $ \m -> do
       let _tid' = SInt (fromIntegral tid)
           m' = m { _txRecord = M.empty, _sysCache = _tmpSysCache m }
#if ENABLE_PACT_PERSISTENCE
       forM_ (M.toList $ _txRecord m) $ \(t,es) -> execs (sRecordTx (_tableStmts m M.! t)) [tid',sencode es]
#endif
       execs_ (tCommit $ _txStmts m)
       return m'

 , _rollbackTx = \s -> withMVar s resetTemp >>= \m -> execs_ (tRollback (_txStmts m))

 , _getTxLog = \d tid e -> withMVar e $ \m -> do
      let tn :: Domain k v -> Utf8
          tn KeySets = keysetsTxRecord
          tn Modules = modulesTxRecord
          tn (UserTables t) = userTxRecord t
      r <- qry1 m ("select txlogs from " <> tn d <> " where txid = ?")
                     [SInt (fromIntegral tid)] [RBlob]
      decodeBlob r

}


readUserTable :: MVar PSL -> TableName -> RowKey -> IO (Maybe (Columns Persistable))
readUserTable e t k = modifyMVar e $ \m -> readUserTable' m t k

readUserTable' :: PSL -> TableName -> RowKey -> IO (PSL,Maybe (Columns Persistable))
readUserTable' m t k = do
  let k' = asString k
      tbl = HM.lookup t $ _cachedUserTables (_tmpSysCache m)
      cached = maybe Nothing (HM.lookup k') tbl
  case cached of
    j@Just {} -> return (m,j)
    Nothing -> do
      _log m "readUserTable: cache miss" k
      r <- qrys' m (userTable t) sRead [stext k] [RBlob]
      case r of
        [] -> return (m,Nothing)
        [a] -> do
          v <- decodeBlob a
          return (over (tmpSysCache.cachedUserTables) (HM.insert t (HM.insert k' v (fromMaybe HM.empty tbl))) m,Just v)
        _ -> throwDbError $ "readUserTable: found more than one row for key " ++ k' ++ ", user table " ++ show t
{-# INLINE readUserTable #-}

readSysTable :: FromJSON v => MVar PSL -> Utf8 -> Lens' PSL (HM.HashMap String v) -> String -> IO (Maybe v)
readSysTable e t l k = modifyMVar e $ \m -> do
  case HM.lookup k (view l m) of
    j@Just {} -> return (m,j)
    Nothing -> do
      _log m "readSysTable: cache miss" k
      r <- qrys' m t sRead [stext k] [RBlob]
      case r of
        [] -> return (m,Nothing)
        [a] -> do
          v <- decodeBlob a
          return (over l (HM.insert k v) m,Just v)
        _ -> throwDbError $ "readUserTable: found more than one row for key " ++ k ++ ", user table " ++ show t
{-# INLINE readSysTable #-}

resetTemp :: PSL -> IO PSL
resetTemp s = return $ s { _txRecord = M.empty, _tmpSysCache = _sysCache s }

writeSys :: (AsString k,ToJSON v) => MVar PSL -> WriteType ->
            Setter' SysCache (HM.HashMap String v) -> Utf8 -> k -> v -> IO ()
writeSys s wt cache tbl k v = modifyMVar_ s $ \m -> do
    _log m "writeSys" (tbl,asString k)
    let q = case wt of
              Write -> sInsertReplace
              Insert -> sInsert
              Update -> sReplace
    execs' m tbl q [stext k,sencode v]
    return $ m { _tmpSysCache = over cache (HM.insert (asString k) v) (_tmpSysCache m) }
{-# INLINE writeSys #-}

writeUser :: MVar PSL -> WriteType -> TableName -> RowKey -> Columns Persistable -> IO ()
writeUser s wt tn rk row = modifyMVar_ s $ \m -> do
  let ut = userTable tn
      rk' = stext rk
  (_,olds) <- readUserTable' m tn rk
  let ins = do
        _log m "writeUser: insert" (tn,rk)
        let row' = sencode row
        execs' m ut sInsert [rk',row']
        finish row
      upd oldrow = do
        let row' = Columns (M.union (_columns row) (_columns oldrow))
            v = sencode row'
        execs' m ut sReplace [rk',v]
        finish row'
      finish row' = do
        let tbl = fromMaybe HM.empty $ HM.lookup tn $ _cachedUserTables (_tmpSysCache m)
        return $
           over txRecord (M.insertWith (++) ut [TxLog (asString tn) (asString rk) (toJSON row)]) $
           over (tmpSysCache.cachedUserTables) (HM.insert tn (HM.insert (asString rk) row' tbl)) m

  case (olds,wt) of
    (Nothing,Insert) -> ins
    (Just _,Insert) -> throwDbError $ "Insert: row found for key " ++ show rk
    (Nothing,Write) -> ins
    (Just old,Write) -> upd old
    (Just old,Update) -> upd old
    (Nothing,Update) -> throwDbError $ "Update: no row found for key " ++ show rk
{-# INLINE writeUser #-}

getUserTableInfo' :: MVar PSL -> TableName -> IO (ModuleName, KeySetName)
getUserTableInfo' e tn = modifyMVar e $ \m -> do
  case HM.lookup tn (_cachedTableInfo $ _tmpSysCache m) of
    Just r -> return (m,r)
    Nothing -> do
      _log m "getUserTableInfo': cache miss" tn
      r <- qry (_conn m) "select module,keyset from usertables where name = ?" [stext tn] [RText,RText]
      case r of
        [[SText mn,SText kn]] -> do
          let p = (convertUtf8 mn,convertUtf8 kn)
          return (over (tmpSysCache.cachedTableInfo) (HM.insert tn p) m,p)
        [] -> throwDbError $ "getUserTableInfo: no such table: " ++ show tn
        v -> throwDbError $ "getUserTableInfo: bad data for " ++ show tn ++ ": " ++ show v

getTableStmts :: PSL -> Utf8 -> TableStmts
getTableStmts s tn = (M.! tn) . _tableStmts $ s

decodeBlob :: (FromJSON v) => [SType] -> IO v
decodeBlob [SBlob old] = liftEither (return $ eitherDecodeStrict' old)
decodeBlob v = throwDbError $ "Expected single-column blob, got: " ++ show v
{-# INLINE decodeBlob #-}

decodeInt_ :: (Integral v) => [SType] -> IO v
decodeInt_ [SInt i] = return $ fromIntegral $ i
decodeInt_ v = throwDbError $ "Expected single-column int, got: " ++ show v
{-# INLINE decodeInt_ #-}

convertUtf8 :: IsString a => Utf8 -> a
convertUtf8 (Utf8 t) = fromString $ T.unpack $ decodeUtf8 t

decodeText :: (IsString v) => SType -> IO v
decodeText (SText t) = return $ convertUtf8 t
decodeText v = throwDbError $ "Expected text, got: " ++ show v
{-# INLINE decodeText #-}

decodeText_ :: (IsString v) => [SType] -> IO v
decodeText_ [SText t] = return $ convertUtf8 t
decodeText_ v = throwDbError $ "Expected single-column text, got: " ++ show v
{-# INLINE decodeText_ #-}


userTable :: TableName -> Utf8
userTable tn = "UTBL_" <> (Utf8 $ encodeUtf8 $ sanitize tn)
{-# INLINE userTable #-}
userTxRecord :: TableName -> Utf8
userTxRecord tn = "UTXR_" <> (Utf8 $ encodeUtf8 $ sanitize tn)
{-# INLINE userTxRecord #-}
sanitize :: AsString t => t -> T.Text
sanitize tn = T.replace "-" "_" $ T.pack (asString tn)
{-# INLINE sanitize #-}

keysetsTable :: Utf8
keysetsTable = "STBL_keysets"
modulesTable :: Utf8
modulesTable = "STBL_modules"
keysetsTxRecord :: Utf8
keysetsTxRecord = "STXR_keysets"
modulesTxRecord :: Utf8
modulesTxRecord = "STXR_modules"

sencode :: ToJSON a => a -> SType
sencode a = SBlob $ BSL.toStrict $ encode a
{-# INLINE sencode #-}

stext :: AsString a => a -> SType
stext a = SText $ fromString $ asString a
{-# INLINE stext #-}


createUserTable' :: MVar PSL -> TableName -> ModuleName -> KeySetName -> IO ()
createUserTable' s tn mn ksn = modifyMVar_ s $ \m -> do
  exec' (_conn m) "insert into usertables values (?,?,?)" [stext tn,stext mn,stext ksn]
  m' <- return $ over (tmpSysCache . cachedTableInfo) (HM.insert tn (mn,ksn)) m
  createTable (userTable tn) (userTxRecord tn) m'

createTable :: Utf8 -> Utf8 -> PSL -> IO PSL
createTable ut ur e = do
  _log e "createTables" (ut,ur)
  exec_ (_conn e) $ "create table " <> ut <>
              " (key text primary key not null unique, value SQLBlob) without rowid;"
  exec_ (_conn e) $ "create table " <> ur <>
          " (txid integer primary key not null unique, txlogs SQLBlob);" -- 'without rowid' crashes!!
  let mkstmt q = prepStmt' e q
  ss <- TableStmts <$>
           mkstmt ("INSERT OR REPLACE INTO " <> ut <> " VALUES (?,?)") <*>
           mkstmt ("INSERT INTO " <> ut <> " VALUES (?,?)") <*>
           mkstmt ("REPLACE INTO " <> ut <> " VALUES (?,?)") <*>
           mkstmt ("select value from " <> ut <> " where key = ?") <*>
           mkstmt ("INSERT INTO " <> ur <> " VALUES (?,?)")
  return (over tableStmts (M.insert ut ss) e)


createSchema :: MVar PSL -> IO ()
createSchema e = modifyMVar_ e $ \s -> do
  exec_ (_conn s)
  -- CPP breaks multi-line strings
    "CREATE TABLE IF NOT EXISTS usertables (name TEXT PRIMARY KEY NOT NULL UNIQUE,module text NOT NULL,keyset text NOT NULL);"
  createTable keysetsTable keysetsTxRecord s >>= createTable modulesTable modulesTxRecord


execs' :: PSL -> Utf8 -> (TableStmts -> Statement) -> [SType] -> IO ()
execs' s tn stmtf as = do
  stmt <- return $ stmtf $ getTableStmts s tn
  execs stmt as
{-# INLINE execs' #-}



prepStmt' :: PSL -> Utf8 -> IO Statement
prepStmt' c q = prepStmt (_conn c) q

qrys' :: PSL -> Utf8 -> (TableStmts -> Statement) -> [SType] -> [RType] -> IO [[SType]]
qrys' s tn stmtf as rts = do
  stmt <- return $ stmtf $ getTableStmts s tn
  qrys stmt as rts
{-# INLINE qrys' #-}


initPSL :: FilePath -> IO PSL
initPSL f = do
  c <- liftEither $ open (fromString f)
  ts <- TxStmts <$> prepStmt c "BEGIN TRANSACTION"
         <*> prepStmt c "COMMIT TRANSACTION"
         <*> prepStmt c "ROLLBACK TRANSACTION"
  s <- return $ PSL c (\m s -> putStrLn $ m ++ ": " ++ show s) M.empty M.empty ts def def
  runPragmas s
  return s



qry1 :: PSL -> Utf8 -> [SType] -> [RType] -> IO [SType]
qry1 e q as rts = do
  r <- qry (_conn e) q as rts
  case r of
    [r'] -> return r'
    [] -> throwDbError "qry1: no results!"
    rs -> throwDbError $ "qry1: multiple results! (" ++ show (length rs) ++ ")"

runPragmas :: PSL -> IO ()
runPragmas e = do
  exec_ (_conn e) "PRAGMA synchronous = OFF"
  exec_ (_conn e) "PRAGMA journal_mode = MEMORY"
  exec_ (_conn e) "PRAGMA locking_mode = EXCLUSIVE"
  exec_ (_conn e) "PRAGMA temp_store = MEMORY"


_initPSL :: IO PSL
_initPSL = do
  let f = "foo.sqllite"
  doesFileExist f >>= \b -> when b (removeFile f)
  initPSL f

_run :: (MVar PSL -> IO ()) -> IO ()
_run a = do
  m <- _initPSL
  s <- newMVar m
  a s
  void $ close (_conn m)



_test1 :: IO ()
_test1 =
    _run $ \e -> do
      t <- getCPUTime
      _beginTx psl e
      createSchema e
      createUserTable' e "stuff" "module" "keyset"
      withMVar e $ \m -> qry_ (_conn m) "select * from usertables" [RText,RText,RText] >>= print
      void $ commit' e
      _beginTx psl e
      print =<< _getUserTableInfo psl "stuff" e
      _writeRow psl Insert (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
      print =<< _readRow psl (UserTables "stuff") "key1" e
      _writeRow psl Update (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
      print =<< _readRow psl (UserTables "stuff") "key1" e
      _writeRow psl Write KeySets "ks1"
               (PactKeySet [PublicKey "frah"] "stuff") e
      print =<< _readRow psl KeySets "ks1" e
      _writeRow psl Write Modules "mod1"
               (Module "mod1" "mod-admin-keyset" Nothing "code") e
      print =<< _readRow psl Modules "mod1" e
      void $ commit' e
      tids <- _txids psl "stuff" (fromIntegral t) e
      print tids
      print =<< _getTxLog psl (UserTables "stuff") (head tids) e


commit' :: MVar PSL -> IO TxId
commit' e = do
  t <- fromIntegral <$> getCPUTime
  _commitTx psl t e
  return t



_bench :: IO ()
_bench = _run $ \e -> do
  _beginTx psl e
  createSchema e
  _createUserTable psl "stuff" "module" "keyset" e
  void $ commit' e
  nolog e
  benchmark $ whnfIO $ do
       _beginTx psl e
       _writeRow psl Write (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
       void $ _readRow psl (UserTables "stuff") "key1" e
       _writeRow psl Update (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
       r <- _readRow psl  (UserTables "stuff") "key1" e
       void $ commit' e
       return r
  benchmark $ whnfIO $ do
       _beginTx psl e
       _writeRow psl Update (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
       commit' e

nolog :: MVar PSL -> IO ()
nolog e = modifyMVar_ e $ \m -> return $ m { _log = \_ _ -> return () }


parseCompile :: T.Text -> [Term Name]
parseCompile code = compiled where
    (Right es) = AP.parseOnly exprs code
    (Right compiled) = mapM (compile mkEmptyInfo) es


_pact :: Bool -> IO ()
_pact doBench = do
      m <- _initPSL
      let body = object ["keyset" A..= object ["keys" A..= ["demoadmin" :: T.Text], "pred" A..= (">" :: T.Text)]]
      evalEnv <- set eeMsgBody body <$> initEvalEnv m psl
      e <- return (_eePactDbVar evalEnv)
      cf <- BS.readFile "demo/demo.pact"
      _beginTx psl e
      createSchema e
      void $ commit' e
      (r,es) <- runEval def evalEnv $ do
          evalBeginTx
          rs <- mapM eval (parseCompile $ decodeUtf8 cf)
          evalCommitTx
          return rs
      print r
      let evalEnv' = over (eeRefStore.rsModules) (HM.union (HM.fromList (_rsNew (_evalRefs es)))) evalEnv
          pactBench benchterm = do
                                tid <- fromIntegral <$> getCPUTime
                                er <- runEval def (set eeTxId tid evalEnv') $ do
                                      evalBeginTx
                                      r' <- eval (head benchterm)
                                      evalCommitTx
                                      return r'
                                return (fst er)
          pactSimple = fmap fst . runEval def evalEnv' . eval . head . parseCompile
          benchy :: Show a => String -> IO a -> IO ()
          benchy n a | doBench = putStr n >> putStr " " >> benchmark (whnfIO $ a)
                     | otherwise = putStrLn "===========" >> putStrLn (n ++ ": ") >> a >>= print
      when doBench $ nolog e
      benchy "read-account no tx" $ pactSimple $ "(demo.read-account \"Acct1\")"
      benchy "read-account tx" $ pactBench $ parseCompile "(demo.read-account \"Acct1\")"
      benchy "_readRow tx" $ do
                   _beginTx psl e
                   rr <- _readRow psl (UserTables "demo-accounts") "Acct1" e
                   void $ commit' e
                   return rr
      benchy "_readRow no tx" $ runEval def evalEnv' $ readRow (UserTables "demo-accounts") "Acct1"
      benchy "describe-table" $ pactSimple "(describe-table 'demo-accounts)"
      benchy "transfer" $ pactBench  $ parseCompile "(demo.transfer \"Acct1\" \"Acct2\" 1.0)"
      benchy "_getUserTableInfo" $ _getUserTableInfo psl "demo-accounts" e
      benchy "_writeRow tx" $ do
                   _beginTx psl e
                   rr <- _writeRow psl Update (UserTables "demo-accounts") "Acct1"
                         (Columns (M.fromList [("balance",PLiteral (LDecimal 1000.0)),
                                               ("amount",PLiteral (LDecimal 1000.0)),
                                               ("data",PLiteral (LString "Admin account funding"))]))
                         e
                   void $ commit' e
                   return rr
      benchy "fund-account tx" $ pactBench $ parseCompile "(demo.fund-account \"Acct1\" 1000.0)"
