module PersistSpec (spec) where

import Test.Hspec
import Pact.PersistPactDb.Regression
import qualified Pact.Persist.SQLite as SQLite
import System.Directory
import Control.Monad

spec :: Spec
spec = do
  it "regress Pure" regressPure
  it "regress SQLite" regressSQLite


regressSQLite :: IO ()
regressSQLite = do
  let f = "deleteme.sqllite"
  doesFileExist f >>= \b -> when b (removeFile f)
  sl <- SQLite.initSQLite [] putStrLn "deleteme.sqllite"
  runRegression (initDbEnv sl putStrLn SQLite.persister)
  void $ SQLite.closeSQLite sl
  removeFile f
