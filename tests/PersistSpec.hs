{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module PersistSpec (spec) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Either
import qualified Data.Map.Strict as M
import Database.SQLite3.Direct
import System.Directory
import Test.Hspec

import Pact.Interpreter
import qualified Pact.Persist.SQLite as SQLite
import qualified Pact.PersistPactDb as Pdb
import Pact.PersistPactDb.Regression
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Logger
import Pact.Types.Runtime

import Utils

spec :: Spec
spec = do
  it "regress Pure" (void $ regressPure neverLog)
  describe "regress SQLite" regressSQLite
  describe "simpleTableMungerTest" simpleTableMungerTest
  describe "ucaseMungerTest" ucaseMungerTest


regressSQLite :: Spec
regressSQLite = it "SQLite successfully closes" $ do
  let f = "deleteme.sqllite"
  db <- do
    doesFileExist f >>= \b -> when b (removeFile f)
    sl <- SQLite.initSQLite (SQLite.SQLiteConfig "deleteme.sqllite" []) neverLog
    mv <- runRegression (initDbEnv neverLog SQLite.persister sl)
    _db <$> readMVar mv
  SQLite.closeSQLite db `shouldReturn` Right ()
  removeFile f

dbScript :: FilePath
dbScript = "tests/pact/db.repl"

simpleTableMungerTest :: Spec
simpleTableMungerTest = do
  (test,tblNames) <- runIO $ runDbRepl simpleTableMunger
  test
  forM_
    [ "[USER_mungeModule_mungeTable_DATA]",
      "[USER_mungeNamespace.mungeModule_mungeTable_DATA]"
    ] $ \t ->
      it ("found table " ++ show t) $ tblNames `shouldSatisfy` (t `elem`)

ucaseMungerTest :: Spec
ucaseMungerTest = do
  (test,tblNames) <- runIO $ runDbRepl ucaseEncodeTableMunger
  test
  forM_
    [ "[USER_mungem:odule.munget:able_DATA]",
      "[USER_mungen:amespace.mungem:odule.munget:able_DATA]"
    ] $ \t ->
      it ("found table " ++ show t) $ tblNames `shouldSatisfy` (t `elem`)

-- | Munger test fixture.
-- munged names are further modified by backend, we're really
-- just ensuring that db functions work on sqlite with all
-- munge types, and verifying the mungers run.
runDbRepl :: TableMunger -> IO (SpecWith (),[Utf8])
runDbRepl munger = do
  (PactDbEnv _ pdb) <- mkInMemSQLiteEnv neverLog
  rs <- set (rEnv . eeTableMunger) munger <$> initReplStateDb pdb Quiet Nothing
  (r,_) <- execScriptState' dbScript rs id
  -- statements are indexed by table name so grab them to test
  -- easier than running `.tables` on the connection directly
  ks <- M.keys . SQLite.tableStmts . Pdb._db <$> readMVar pdb
  -- return assertion as 'Spec' is hard to factor
  return (it "db repl succeeds" $ r `shouldSatisfy` isRight, ks)
