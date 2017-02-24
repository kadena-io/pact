{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Pact.Server.SQLite
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- SQLite backend for Pact service.
--

module Pact.Server.SQLite
  ( PSL(..), conn, log, txRecord, tableStmts, txStmts, txId
  , psl
  , initPSL
  , createSchema
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.String
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Encoding
import qualified Data.Text as T

import Data.Default
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

import Criterion hiding (env)
import Database.SQLite3.Direct as SQ3
import System.CPUTime
import System.Directory

import Pact.Types.Runtime hiding ((<>))
import Pact.Types.SQLite
import Pact.Types.Orphans ()
import Pact.Compile
import Pact.Eval
import Pact.Native (initEvalEnv)

data TableStmts = TableStmts
  { sInsertReplace :: Statement
  , sInsert :: Statement
  , sReplace :: Statement
  , sRead :: Statement
  , sRecordTx :: Statement
  }

data TxStmts = TxStmts
  {  tBegin :: Statement
  , tCommit :: Statement
  , tRollback :: Statement
  }

data PSL = PSL
  { _conn :: Database
  , _log :: forall s . Show s => (String -> s -> IO ())
  , _txRecord :: M.Map Utf8 [TxLog]
  , _tableStmts :: M.Map Utf8 TableStmts
  , _txStmts :: TxStmts
  , _txId :: Maybe TxId
  }
makeLenses ''PSL

psl :: PactDb PSL
psl = PactDb
  { _readRow = \d k e ->
       case d of
           KeySets -> readSysTable e keysetsTable (asString k)
           Modules -> readSysTable e modulesTable (asString k)
           (UserTables t) -> readUserTable e t k

 , _writeRow = \wt d k v e ->
       case d of
           KeySets -> writeSys e wt keysetsTable k v
           Modules -> writeSys e wt modulesTable k v
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

 , _beginTx = \tidm s -> modifyMVar_ s $ \m -> do
     m' <- case _txId m of
             Just _ -> do
               _log m "beginTx" ("In transaction, rolling back" :: String)
               rollback m
             Nothing -> return m
     execs_ (tBegin (_txStmts m'))
     return $ set txId tidm $ resetTemp m'

 , _commitTx = \s -> do
     r <- modifyMVar s $ \m -> case _txId m of
       Nothing -> rollback m >>= \m' -> return (m',Just "Not in transaction")
       Just tid -> do
         let tid' = SInt (fromIntegral tid)
         forM_ (M.toList $ _txRecord m) $ \(t,es) ->
           execs (sRecordTx (_tableStmts m M.! t)) [tid',sencode es]
         execs_ (tCommit $ _txStmts m)
         return (resetTemp m,Nothing)
     mapM_ throwDbError r

 , _rollbackTx = \s -> modifyMVar_ s rollback

 , _getTxLog = \d tid e -> withMVar e $ \m -> do
      let tn :: Domain k v -> Utf8
          tn KeySets = keysetsTxRecord
          tn Modules = modulesTxRecord
          tn (UserTables t) = userTxRecord t
      r <- qry1 m ("select txlogs from " <> tn d <> " where txid = ?")
                     [SInt (fromIntegral tid)] [RText]
      decodeBlob r
 }

rollback :: PSL -> IO PSL
rollback m = do
  (r :: Either SomeException ()) <- try $ execs_ (tRollback (_txStmts m))
  case r of
    Left e -> _log m "rollback" $ "ERROR: " ++ show e
    Right {} -> return ()
  return (resetTemp m)

_checkInTx :: String -> PSL -> (TxId -> IO a) -> IO a
_checkInTx msg m act = case _txId m of
  Just tid -> act tid
  Nothing -> throwDbError $ msg ++ ": Not in transaction"

readUserTable :: MVar PSL -> TableName -> RowKey -> IO (Maybe (Columns Persistable))
readUserTable e t k = modifyMVar e $ \m -> readUserTable' m t k

readUserTable' :: PSL -> TableName -> RowKey -> IO (PSL,Maybe (Columns Persistable))
readUserTable' m t k = do
  r <- qrys' m (userTable t) sRead [stext k] [RText]
  case r of
    [] -> return (m,Nothing)
    [a] -> (m,) . Just <$> decodeBlob a
    _ -> throwDbError $ "readUserTable: found more than one row for key " ++ show k ++ ", user table " ++ show t
{-# INLINE readUserTable #-}

readSysTable :: FromJSON v => MVar PSL -> Utf8 -> Text -> IO (Maybe v)
readSysTable e t k = modifyMVar e $ \m -> do
  r <- qrys' m t sRead [stext k] [RText]
  case r of
    [] -> return (m,Nothing)
    [a] -> (m,) . Just <$> decodeBlob a
    _ -> throwDbError $ "readUserTable: found more than one row for key " ++ unpack k ++ ", user table " ++ show t
{-# INLINE readSysTable #-}

resetTemp :: PSL -> PSL
resetTemp = set txRecord M.empty . set txId Nothing

writeSys :: (AsString k,ToJSON v) => MVar PSL -> WriteType -> Utf8 -> k -> v -> IO ()
writeSys s wt tbl k v = modifyMVar_ s $ \m -> do
    _log m "writeSys" (tbl,asString k)
    let q = case wt of
              Write -> sInsertReplace
              Insert -> sInsert
              Update -> sReplace
    execs' m tbl q [stext k,sencode v]
    return m
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
      finish row' = return $
           over txRecord (M.insertWith (++) ut [TxLog (asString tn) (asString rk) (toJSON row')]) m
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
  r <- qry (_conn m) "select module,keyset from usertables where name = ?" [stext tn] [RText,RText]
  case r of
    [[SText mn,SText kn]] -> do
      let p = (convertUtf8 mn,convertUtf8 kn)
      return (m,p)
    [] -> throwDbError $ "getUserTableInfo: no such table: " ++ show tn
    v -> throwDbError $ "getUserTableInfo: bad data for " ++ show tn ++ ": " ++ show v

getTableStmts :: PSL -> Utf8 -> TableStmts
getTableStmts s tn = (M.! tn) . _tableStmts $ s

decodeBlob :: (FromJSON v) => [SType] -> IO v
decodeBlob [SText old] = liftEither (return $ eitherDecodeStrict' (convertUtf8 old))
decodeBlob v = throwDbError $ "Expected single-column blob, got: " ++ show v
{-# INLINE decodeBlob #-}

decodeInt_ :: (Integral v) => [SType] -> IO v
decodeInt_ [SInt i] = return $ fromIntegral i
decodeInt_ v = throwDbError $ "Expected single-column int, got: " ++ show v
{-# INLINE decodeInt_ #-}

convertUtf8 :: IsString a => Utf8 -> a
convertUtf8 (Utf8 t) = fromString $ T.unpack $ decodeUtf8 t

_decodeText :: (IsString v) => SType -> IO v
_decodeText (SText t) = return $ convertUtf8 t
_decodeText v = throwDbError $ "Expected text, got: " ++ show v
{-# INLINE _decodeText #-}

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
sanitize tn = T.replace "-" "_" $ asString tn
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
sencode a = SText $ Utf8 $ BSL.toStrict $ encode a
{-# INLINE sencode #-}

stext :: AsString a => a -> SType
stext a = SText $ fromString $ unpack $ asString a
{-# INLINE stext #-}

createUserTable' :: MVar PSL -> TableName -> ModuleName -> KeySetName -> IO ()
createUserTable' s tn mn ksn = modifyMVar_ s $ \m -> do
  exec' (_conn m) "insert into usertables values (?,?,?)" [stext tn,stext mn,stext ksn]
  createTable (userTable tn) (userTxRecord tn) m

createTable :: Utf8 -> Utf8 -> PSL -> IO PSL
createTable ut ur e = do
  _log e "createTables" (ut,ur)
  exec_ (_conn e) $ "create table " <> ut <>
              " (key text primary key not null unique, value text);" --  without rowid;"
  exec_ (_conn e) $ "create table " <> ur <>
          " (txid integer primary key not null unique, txlogs text);" -- 'without rowid' crashes!!
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

initPSL :: [Pragma] -> (String -> IO ()) -> FilePath -> IO PSL
initPSL ps logFn f = do
  c <- liftEither $ open (fromString f)
  ts <- TxStmts <$> prepStmt c "BEGIN TRANSACTION"
         <*> prepStmt c "COMMIT TRANSACTION"
         <*> prepStmt c "ROLLBACK TRANSACTION"
  s <- return PSL {
    _conn = c,
    _log = \m s -> logFn $ m ++ ": " ++ show s,
    _txRecord = M.empty,
    _tableStmts = M.empty,
    _txStmts = ts,
    _txId = def }
  runPragmas (_conn s) ps
  return s

qry1 :: PSL -> Utf8 -> [SType] -> [RType] -> IO [SType]
qry1 e q as rts = do
  r <- qry (_conn e) q as rts
  case r of
    [r'] -> return r'
    [] -> throwDbError "qry1: no results!"
    rs -> throwDbError $ "qry1: multiple results! (" ++ show (length rs) ++ ")"

fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]

_initPSL :: IO PSL
_initPSL = do
  let f = "deleteme.sqllite"
  doesFileExist f >>= \b -> when b (removeFile f)
  initPSL fastNoJournalPragmas putStrLn f

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
      begin e
      createSchema e
      createUserTable' e "stuff" "module" "keyset"
      withMVar e $ \m -> qry_ (_conn m) "select * from usertables" [RText,RText,RText] >>= print
      void $ commit' e
      begin e
      print . _txId =<< readMVar e
      print =<< _getUserTableInfo psl "stuff" e
      _writeRow psl Insert (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
      print =<< _readRow psl (UserTables "stuff") "key1" e
      _writeRow psl Update (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
      print =<< _readRow psl (UserTables "stuff") "key1" e
      _writeRow psl Write KeySets "ks1"
               (KeySet [PublicKey "frah"] "stuff") e
      print =<< _readRow psl KeySets "ks1" e
      _writeRow psl Write Modules "mod1"
               (Module "mod1" "mod-admin-keyset" Nothing "code") e
      print =<< _readRow psl Modules "mod1" e
      void $ commit' e
      tids <- _txids psl "stuff" (pred $ fromIntegral t) e
      print tids
      print =<< _getTxLog psl (UserTables "stuff") (head tids) e

begin :: MVar PSL -> IO ()
begin e = do
  t <- Just . fromIntegral <$> getCPUTime
  _beginTx psl t e

commit' :: MVar PSL -> IO ()
commit' e = _commitTx psl e

_bench :: IO ()
_bench = _run $ \e -> do
  begin e
  createSchema e
  _createUserTable psl "stuff" "module" "keyset" e
  void $ commit' e
  nolog e
  benchmark $ whnfIO $ do
       begin e
       _writeRow psl Write (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
       void $ _readRow psl (UserTables "stuff") "key1" e
       _writeRow psl Update (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
       r <- _readRow psl  (UserTables "stuff") "key1" e
       void $ commit' e
       return r
  benchmark $ whnfIO $ do
       begin e
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
      begin e
      createSchema e
      void $ commit' e
      (r,es) <- runEval def evalEnv $ do
          evalBeginTx def
          rs <- mapM eval (parseCompile $ decodeUtf8 cf)
          evalCommitTx def
          return rs
      print r
      let evalEnv' = over (eeRefStore.rsModules) (HM.union (HM.fromList (_rsNew (_evalRefs es)))) evalEnv
          pactBench benchterm = do
                                tid <- Just . fromIntegral <$> getCPUTime
                                er <- runEval def (set eeTxId tid evalEnv') $ do
                                      evalBeginTx def
                                      r' <- eval (head benchterm)
                                      evalCommitTx def
                                      return r'
                                return (fst er)
          pactSimple = fmap fst . runEval def evalEnv' . eval . head . parseCompile
          benchy :: Show a => String -> IO a -> IO ()
          benchy n a | doBench = putStr n >> putStr " " >> benchmark (whnfIO a)
                     | otherwise = putStrLn "===========" >> putStrLn (n ++ ": ") >> a >>= print
      when doBench $ nolog e
      benchy "read-account no tx" $ pactSimple "(demo.read-account \"Acct1\")"
      benchy "read-account tx" $ pactBench $ parseCompile "(demo.read-account \"Acct1\")"
      benchy "_readRow tx" $ do
                   begin e
                   rr <- _readRow psl (UserTables "demo-accounts") "Acct1" e
                   void $ commit' e
                   return rr
      benchy "_readRow no tx" $ runEval def evalEnv' $ readRow def (UserTables "demo-accounts") "Acct1"
      benchy "describe-table" $ pactSimple "(describe-table 'demo-accounts)"
      benchy "transfer" $ pactBench  $ parseCompile "(demo.transfer \"Acct1\" \"Acct2\" 1.0)"
      benchy "_getUserTableInfo" $ _getUserTableInfo psl "demo-accounts" e
      benchy "_writeRow tx" $ do
                   begin e
                   rr <- _writeRow psl Update (UserTables "demo-accounts") "Acct1"
                         (Columns (M.fromList [("balance",PLiteral (LDecimal 1000.0)),
                                               ("amount",PLiteral (LDecimal 1000.0)),
                                               ("data",PLiteral (LString "Admin account funding"))]))
                         e
                   void $ commit' e
                   return rr
      benchy "fund-account tx" $ pactBench $ parseCompile "(demo.fund-account \"Acct1\" 1000.0)"
