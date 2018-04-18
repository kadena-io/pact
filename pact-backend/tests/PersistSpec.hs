module PersistSpec (spec) where

import Test.Hspec
import Pact.PersistPactDb.Regression
import qualified Pact.Persist.SQLite as SQLite
import System.Directory
import Control.Monad
import Pact.Types.Logger

spec :: Spec
spec = do
  it "regress Pure" (void regressPure)
  it "regress SQLite" regressSQLite


regressSQLite :: IO ()
regressSQLite = do
  let f = "deleteme.sqllite"
  doesFileExist f >>= \b -> when b (removeFile f)
  sl <- SQLite.initSQLite (SQLite.SQLiteConfig "deleteme.sqllite" []) alwaysLog
  void $ runRegression (initDbEnv alwaysLog SQLite.persister sl)
  void $ SQLite.closeSQLite sl
  removeFile f
