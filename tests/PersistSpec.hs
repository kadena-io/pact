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
  it "regress Pure" (void regressPure)
  describe "regress SQLite" regressSQLite


regressSQLite :: Spec
regressSQLite = do
  let f = "deleteme.sqllite"
  db <- runIO $ do
    doesFileExist f >>= \b -> when b (removeFile f)
    sl <- SQLite.initSQLite (SQLite.SQLiteConfig "deleteme.sqllite" []) alwaysLog
    mv <- runRegression (initDbEnv alwaysLog SQLite.persister sl)
    _db <$> readMVar mv
  it "SQLite successfully closes" $ SQLite.closeSQLite db `shouldReturn` (Right ())
  runIO $ removeFile f
