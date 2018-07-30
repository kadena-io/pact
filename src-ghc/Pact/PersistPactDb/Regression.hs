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
import Pact.Persist.Pure (initPureDb,persister,PureDb)
import Data.Aeson
import Data.Hashable
import Pact.Types.Logger
import qualified Pact.Types.Hash as H
import Data.Default (def)


runRegression :: DbEnv p -> IO (MVar (DbEnv p))
runRegression p = do
  v <- newMVar p
  createSchema v
  let t1 = 1
  t2 <- begin v (Just t1)
  let user1 = "user1"
      usert = UserTables user1
  createUserTable' v user1 "someModule" "someKeyset"
  assertEquals' "hash of commit 2" 206390449035706898 (hashWithSalt 1 <$> commit v)
  t3 <- begin v t2
  assertEquals' "user table info correct" ("someModule","someKeyset") $ _getUserTableInfo pactdb user1 v
  let row = Columns $ M.fromList [("gah",toTerm' (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" (fmap toPersistable row) v
  assertEquals' "user insert" (Just row) (fmap (fmap toTerm) <$> _readRow pactdb usert "key1" v)
  let row' = Columns $ M.fromList [("gah",toTerm' False),("fh",toTerm' Null)]
  _writeRow pactdb Update usert "key1" (fmap toPersistable row') v
  assertEquals' "user update" (Just row') (fmap (fmap toTerm) <$> _readRow pactdb usert "key1" v)
  let ks = KeySet [PublicKey "skdjhfskj"] (Name "predfun" def)
  _writeRow pactdb Write KeySets "ks1" ks v
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" v
  let mod' = Module "mod1" "mod-admin-keyset" (DocModel Nothing M.empty) "code" (H.hash "code") mempty
  _writeRow pactdb Write Modules "mod1" mod' v
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules "mod1" v
  assertEquals' "hash of commit 3" 5222047446822369474 (hashWithSalt 1 <$> commit v)
  _t4 <- begin v t3
  tids <- _txids pactdb user1 t1 v
  assertEquals "user txids" [2] tids
  assertEquals' "user txlogs"
    [TxLog "USER_user1" "key1" row,
     TxLog "USER_user1" "key1" row'] $
    fmap (map (fmap (fmap toTerm))) $
    _getTxLog pactdb usert (head tids) v
  _writeRow pactdb Insert usert "key2" (fmap toPersistable row) v
  assertEquals' "user insert key2 pre-rollback" (Just row) (fmap (fmap toTerm) <$> _readRow pactdb usert "key2" v)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb user1 v
  _rollbackTx pactdb v
  assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" v
  assertEquals' "keys" ["key1"] $ _keys pactdb user1 v
  return v

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

begin :: MVar (DbEnv p) -> Maybe TxId -> IO (Maybe TxId)
begin v t = do
  _beginTx pactdb t v
  return (fmap succ t)

commit :: MVar (DbEnv p) -> IO [TxLog Value]
commit v = _commitTx pactdb v

throwFail :: String -> IO a
throwFail = throwIO . userError

assertEquals :: (Eq a,Show a) => String -> a -> a -> IO ()
assertEquals msg a b | a == b = return ()
                     | otherwise =
                         throwFail $ "FAILURE: " ++ msg ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

assertEquals' :: (Eq a, Show a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

regressPure :: IO (MVar (DbEnv PureDb))
regressPure = do
  let e = initDbEnv alwaysLog persister initPureDb
  runRegression e
