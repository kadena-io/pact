{-# LANGUAGE OverloadedStrings #-}
module Pact.PersistPactDb.Regression
  (DbEnv(..),
   initDbEnv,
   runRegression,
   regressPure) where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Map.Strict as M

import Pact.PersistPactDb
import Pact.Persist
import Pact.Types.Runtime
import Pact.Persist.Pure (initPureDb,persister)
import Data.Aeson

runRegression :: DbEnv p -> IO ()
runRegression p = do
  v <- newMVar p
  createSchema v
  let t1 = 1
  t2 <- begin v (Just t1)
  let user1 = "user1"
      usert = UserTables user1
  createUserTable' v user1 "someModule" "someKeyset"
  t3 <- commit v t2
  assertEquals' "user table info correct" ("someModule","someKeyset") $ _getUserTableInfo pactdb user1 v
  let row = Columns $ M.fromList [("gah",PLiteral (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" row v
  assertEquals' "user insert" (Just row) $  _readRow pactdb usert "key1" v
  let row' = Columns $ M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)]
  _writeRow pactdb Update usert "key1" row' v
  assertEquals' "user update" (Just row') $  _readRow pactdb usert "key1" v
  let ks = KeySet [PublicKey "skdjhfskj"] "predfun"
  _writeRow pactdb Write KeySets "ks1" ks v
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" v
  let mod' = Module "mod1" "mod-admin-keyset" Nothing "code"
  _writeRow pactdb Write Modules "mod1" mod' v
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules "mod1" v
  _ <- commit v t3
  tids <- _txids pactdb user1 t1 v
  assertEquals "user txids" [2] tids
  assertEquals' "user txlogs"
    [TxLog "user1" "key1" (object ["gah" .= String "123.454345"]),
     TxLog "user1" "key1" (object ["fh" .= Null, "gah" .= False])] $
    _getTxLog pactdb usert (head tids) v


begin :: MVar (DbEnv p) -> Maybe TxId -> IO (Maybe TxId)
begin v t = do
  _beginTx pactdb t v
  return (fmap succ t)

commit :: MVar (DbEnv p) -> Maybe TxId -> IO (Maybe TxId)
commit v t = _commitTx pactdb v >> begin v t

throwFail :: String -> IO a
throwFail = throwIO . userError

assertEquals :: (Eq a,Show a) => String -> a -> a -> IO ()
assertEquals msg a b | a == b = return ()
                     | otherwise =
                         throwFail $ "FAILURE: " ++ msg ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

assertEquals' :: (Eq a, Show a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

regressPure :: IO ()
regressPure = do
  let e = initDbEnv putStrLn persister initPureDb
  runRegression e
