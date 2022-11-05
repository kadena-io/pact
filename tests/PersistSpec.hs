{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module PersistSpec (spec) where

import Control.Concurrent
import Control.Monad
import Data.Default
import qualified Data.Map.Strict as M
import System.Directory
import Test.Hspec

import Pact.Interpreter
import Pact.Persist
import qualified Pact.Persist.SQLite as SQLite
import qualified Pact.Persist.Pure as Pure
import qualified Pact.PersistPactDb as Pdb
import Pact.PersistPactDb.Regression
import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.Logger
import Pact.Types.Runtime

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
  tblNames <- runIO $ runDbRepl simpleTableMunger
  forM_
    [ "USER_mungeModule_mungeTable",
      "USER_mungeNamespace.mungeModule_mungeTable"
    ] $ \t ->
      it ("found table " ++ show t) $ tblNames `shouldSatisfy` (DataTable t `elem`)

ucaseMungerTest :: Spec
ucaseMungerTest = do
  tblNames <- runIO $ runDbRepl ucaseEncodeTableMunger
  forM_
    [ "USER_mungem:odule.munget:able",
      "USER_mungen:amespace.mungem:odule.munget:able"
    ] $ \t ->
      it ("found table " ++ show t) $ tblNames `shouldSatisfy` (DataTable t `elem`)

runDbRepl :: TableMunger -> IO [Table DataKey]
runDbRepl munger = do
  (PactDbEnv _ pdb) <- mkPureEnv neverLog
  ls <- initLibState' (LibDb pdb) Nothing
  ee <- initEvalEnv ls
  let rs = ReplState (ee { _eeTableMunger = munger }) def Quiet def def def
  void $ execScriptState' dbScript rs id
  M.keys . Pure._tbls . Pure._dataTables . Pure._committed . Pdb._db <$> readMVar pdb
