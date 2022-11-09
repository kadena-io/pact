{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module PersistSpec (spec) where

import Test.Hspec
import Pact.PersistPactDb.Regression
import qualified Pact.Persist.SQLite as SQLite
import System.Directory
import Control.Monad
import Pact.Types.Logger
import Control.Concurrent

spec :: Spec
spec = do
  it "regress Pure" (void $ regressPure neverLog)
  describe "regress SQLite" regressSQLite


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
