{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module PersistSpec (spec) where

import Test.Hspec
import Pact.PersistPactDb(createSchema, pactdb)
import Pact.PersistPactDb.Regression
import qualified Pact.Persist.SQLite as SQLite
import System.Directory
import Control.Lens
import Control.Monad
import Pact.Types.Logger
import Pact.Types.Persistence
import Pact.Persist.Taped
import qualified Pact.Persist.Pure as Pure
import Control.Concurrent
import qualified Data.Map as Map
import Pact.Types.RowData
import Pact.Types.Term(ObjectMap(..))
import Pact.Types.Exp(Literal(..))

spec :: Spec
spec = do
  it "regress Pure" (void $ regressPure neverLog)
  describe "regress SQLite" regressSQLite
  describe "reflected pure db" reflectedTests


regressSQLite :: Spec
regressSQLite = it "SQLite successfully closes" $ do
  let f = "deleteme.sqlite"
  db <- do
    doesFileExist f >>= \b -> when b (removeFile f)
    sl <- SQLite.initSQLite (SQLite.SQLiteConfig "deleteme.sqlite" []) neverLog
    mv <- newMVar $ initDbEnv neverLog SQLite.persister sl
    createSchema mv
    runRegression pactdb mv
    _db <$> readMVar mv
  SQLite.closeSQLite db `shouldReturn` Right ()
  removeFile f

regressReflected :: IO ()
regressReflected = do
  -- create an empty reflecting db
  reflectedV <- do
    pureDbEnv <- initPureDbEnv
    newMVar ReflectingDbEnv
      { _reflectingWriteSet = mempty
      , _reflectingInputMirror = pureDbEnv
      , _reflectingOutputMirror = pureDbEnv
      , _reflectingSource = pureDbEnv
      }
  -- do a bunch of things with the reflecting db, grab the resulting db state
  reflectedOut <- do
    runRegression (reflectingDb pactdb pactdb pactdb) reflectedV
    readMVar reflectedV
  -- do the same things with just a pure db, grab the resulting db state
  pureOut <- do
    v <- newMVar =<< initPureDbEnv
    runRegression pactdb v
    readMVar v
  -- reflected source should have the same contents afterward as pure db
  _db (_reflectingSource reflectedOut) `shouldBe` _db pureOut

reflectedTests :: Spec
reflectedTests = do
   it "should reflect reads to the mirrordb" reflectionTest
   it "should regress the same as the read db" regressReflected

initPureDbEnv :: IO (DbEnv Pure.PureDb)
initPureDbEnv = do
  v <- newMVar (initDbEnv neverLog Pure.persister Pure.initPureDb)
  createSchema v
  readMVar v

reflectionTest :: IO ()
reflectionTest = do
  sourceDb <- do
    mv <- newMVar $ initDbEnv neverLog Pure.persister Pure.initPureDb
    createSchema mv
    -- prepare initial database
    _ <- _beginTx pactdb Transactional mv
    _createUserTable pactdb "mod.tbl" "mod" mv
    _writeRow pactdb Insert (UserTables "mod.tbl") "key" (RowData RDV1 (ObjectMap $ Map.singleton "vkey" (RDLiteral $ LString "value"))) mv
    _ <- _commitTx pactdb mv
    readMVar mv
  -- initialize, read and write reflecting database
  (inputValue, reflectedOut) <- do
    inputMirrorDb <- initPureDbEnv
    outputMirrorDb <- initPureDbEnv
    reflectedV <- newMVar ReflectingDbEnv
      { _reflectingWriteSet = mempty
      , _reflectingInputMirror = inputMirrorDb
      , _reflectingOutputMirror = outputMirrorDb
      , _reflectingSource = sourceDb
      }
    let rdb = reflectingDb pactdb pactdb pactdb
    -- read a value from the source database, reflecting it into the input mirror database
    inputValue <- _readRow rdb (UserTables "mod.tbl") "key" reflectedV
    -- write a new value to the read database, which should not be reflected by further reads into the input mirror,
    -- but into the output mirror instead
    _writeRow rdb Write (UserTables "mod.tbl") "key2"
      (RowData RDV1 (ObjectMap $ Map.singleton "vkey" (RDLiteral $ LString "new value")))
      reflectedV
    _ <- _readRow rdb (UserTables "mod.tbl") "key2" reflectedV
    reflectedOut <- readMVar reflectedV
    return (inputValue, reflectedOut)
  do
    -- check that the input mirror has the read value
    inputMirrorV <- newMVar $ view reflectingInputMirror reflectedOut
    reflectedValue <- _readRow pactdb (UserTables "mod.tbl") "key" inputMirrorV
    reflectedValue `shouldBe` inputValue
    -- check that the input mirror does not have the written value
    unreflectedValue <- _readRow pactdb (UserTables "mod.tbl") "key2" inputMirrorV
    unreflectedValue `shouldBe` Nothing
  do
    -- check that the output mirror has the written value
    outputMirrorV <- newMVar $ view reflectingOutputMirror reflectedOut
    reflectedOutputValue <- _readRow pactdb (UserTables "mod.tbl") "key2" outputMirrorV
    reflectedOutputValue `shouldBe` Just (RowData RDV1 (ObjectMap $ Map.singleton "vkey" $ RDLiteral (LString "new value")))
