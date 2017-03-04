{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Pact.Server.PersistPactDb
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- PactDb implementing pact-specific logic using a Persist implementation.
--

module Pact.PersistPactDb
  ( DbEnv(..),db,persist,log,txRecord,txId
  , initDbEnv
  , pactdb
  , createSchema
  , createUserTable'
  , UserTableInfo(..)
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict

import Data.Aeson hiding ((.=))
import Data.Aeson.Lens
import GHC.Generics

import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Maybe

import Pact.Types.Runtime hiding ((<>))
import Pact.Persist as P

-- | Environment/MVar variable for pactdb impl.
data DbEnv p = DbEnv
  { _db :: p
  , _persist :: Persister p
  , _log :: forall s . Show s => (String -> s -> IO ())
  , _txRecord :: M.Map TxTable [TxLog]
  , _txId :: Maybe TxId
  }
makeLenses ''DbEnv

initDbEnv :: p -> (String -> IO ()) -> Persister p -> DbEnv p
initDbEnv p logFn funrec = DbEnv {
  _db = p,
  _persist = funrec,
  _log = \s a -> logFn ("[PactPersist] " ++ s ++ ": " ++ show a),
  _txRecord = M.empty,
  _txId = Nothing
  }

data UserTableInfo = UserTableInfo {
  utModule :: ModuleName,
  utKeySet :: KeySetName
  } deriving (Eq,Show,Generic)

instance FromJSON UserTableInfo
instance ToJSON UserTableInfo

userTable :: TableName -> TableId
userTable tn = TableId $ "USER_" <> asString tn
{-# INLINE userTable #-}

userDataTable :: TableName -> DataTable
userDataTable = DataTable . userTable
{-# INLINE userDataTable #-}

userTxRecord :: TableName -> TxTable
userTxRecord = TxTable . userTable
{-# INLINE userTxRecord #-}

keysetsTable :: TableId
keysetsTable = "SYS_keysets"
modulesTable :: TableId
modulesTable = "SYS_modules"
userTableInfo :: TableId
userTableInfo = "SYS_usertables"

type MVState p a = StateT (DbEnv p) IO a

runMVState :: MVar (DbEnv p) -> MVState p a -> IO a
runMVState v a = modifyMVar v (runStateT a >=> \(r,m') -> return (m',r))
{-# INLINE runMVState #-}


doPersist :: (Persister p -> Persist p a) -> MVState p a
doPersist f = get >>= \m -> liftIO (f (_persist m) (_db m)) >>= \(db',r) -> db .= db' >> return r
{-# INLINE doPersist #-}


pactdb :: PactDb (DbEnv p)
pactdb = PactDb
  { _readRow = \d k e ->
       case d of
           KeySets -> readSysTable e (DataTable keysetsTable) (asString k)
           Modules -> readSysTable e (DataTable modulesTable) (asString k)
           (UserTables t) -> readUserTable e t k

 , _writeRow = \wt d k v e ->
       case d of
           KeySets -> writeSys e wt keysetsTable k v
           Modules -> writeSys e wt modulesTable k v
           (UserTables t) -> writeUser e wt t k v

 , _keys = \tn e -> runMVState e
     (map RowKey <$> doPersist (\p -> queryKeys p (userDataTable tn) Nothing))


 , _txids = \tn tid e -> runMVState e
     (map fromIntegral <$> doPersist
       (\p -> queryKeys p (userTxRecord tn) (Just (KQKey KGT (fromIntegral tid)))))


 , _createUserTable = \tn mn ksn e ->
       createUserTable' e tn mn ksn

 , _getUserTableInfo = \tn e -> getUserTableInfo' e tn

 , _beginTx = \tidm s -> runMVState s $ doBegin tidm

 , _commitTx = \s -> runMVState s doCommit

 , _rollbackTx = \s -> runMVState s rollback

 , _getTxLog = \d tid e -> runMVState e $ do
      let tn :: Domain k v -> TxTable
          tn KeySets = TxTable keysetsTable
          tn Modules = TxTable modulesTable
          tn (UserTables t) = userTxRecord t
      convUserTxLogs d . fromMaybe [] <$> doPersist (\p -> readValue p (tn d) (fromIntegral tid))
 }

doBegin :: Maybe TxId -> MVState p ()
doBegin tidm = do
  use txId >>= \t -> case t of
    Just _ -> do
      debug "beginTx" ("In transaction, rolling back" :: String)
      rollback
    Nothing -> return ()
  resetTemp
  doPersist P.beginTx
  txId .= tidm
{-# INLINE doBegin #-}

doCommit :: MVState p ()
doCommit = do
  use txId >>= \otid -> case otid of
    Nothing -> rollback >> throwDbError "Not in transaction"
    Just tid -> do
      let tid' = fromIntegral tid
      use txRecord >>= \rs -> forM_ (M.toList rs) $ \(t,es) -> doPersist $ \p -> writeValue p t Write tid' es
      doPersist P.commitTx
      resetTemp
{-# INLINE doCommit #-}



convUserTxLogs :: Domain k v -> [TxLog] -> [TxLog]
convUserTxLogs UserTables {} = map $ \tl -> case fromJSON (_txValue tl) of
  Error s -> over txValue (set (key "error") (toJSON s)) tl
  Success (v :: Columns Persistable) ->
    set txValue (toJSON (fmap (toTerm :: Persistable -> Term Name) v)) tl
convUserTxLogs _ = id
{-# INLINE convUserTxLogs #-}



debug :: Show a => String -> a -> MVState p ()
debug s a = use log >>= \l -> liftIO $ l s a

rollback :: MVState p ()
rollback = do
  (r :: Either SomeException ()) <- try (doPersist P.rollbackTx)
  case r of
    Left e -> debug "rollback" ("ERROR: " ++ show e)
    Right _ -> return ()
  resetTemp

readUserTable :: MVar (DbEnv p) -> TableName -> RowKey -> IO (Maybe (Columns Persistable))
readUserTable e t k = runMVState e $ readUserTable' t k
{-# INLINE readUserTable #-}

readUserTable' :: TableName -> RowKey -> MVState p (Maybe (Columns Persistable))
readUserTable' t k = doPersist $ \p -> readValue p (userDataTable t) (asString k)
{-# INLINE readUserTable' #-}

readSysTable :: FromJSON v => MVar (DbEnv p) -> DataTable -> Text -> IO (Maybe v)
readSysTable e t k = runMVState e $ doPersist $ \p -> readValue p t k
{-# INLINE readSysTable #-}

resetTemp :: MVState p ()
resetTemp = txRecord .= M.empty >> txId .= Nothing
{-# INLINE resetTemp #-}

writeSys :: (AsString k,ToJSON v) => MVar (DbEnv p) -> WriteType -> TableId -> k -> v -> IO ()
writeSys s wt tbl k v = runMVState s $ do
  debug "writeSys" (tbl,asString k)
  doPersist $ \p -> writeValue p (DataTable tbl) wt (asString k) v
{-# INLINE writeSys #-}

writeUser :: MVar (DbEnv p) -> WriteType -> TableName -> RowKey -> Columns Persistable -> IO ()
writeUser s wt tn rk row = runMVState s $ do
  let ut = userDataTable tn
      tt = userTxRecord tn
      rk' = asString rk
  olds <- readUserTable' tn rk
  let ins = do
        debug "writeUser: insert" (tn,rk)
        doPersist $ \p -> writeValue p ut Insert rk' row
        finish row
      upd oldrow = do
        let row' = Columns (M.union (_columns row) (_columns oldrow))
        doPersist $ \p -> writeValue p ut Update rk' row'
        finish row'
      finish row' = txRecord %= M.insertWith (flip (++)) tt [TxLog (asString tn) (asString rk) (toJSON row')]
  case (olds,wt) of
    (Nothing,Insert) -> ins
    (Just _,Insert) -> throwDbError $ "Insert: row found for key " ++ show rk
    (Nothing,Write) -> ins
    (Just old,Write) -> upd old
    (Just old,Update) -> upd old
    (Nothing,Update) -> throwDbError $ "Update: no row found for key " ++ show rk
{-# INLINE writeUser #-}



getUserTableInfo' :: MVar (DbEnv p) -> TableName -> IO (ModuleName, KeySetName)
getUserTableInfo' e tn = runMVState e $ do
  r <- doPersist $ \p -> readValue p (DataTable userTableInfo) (asString tn)
  case r of
    (Just (UserTableInfo mn ksn)) -> return (mn,ksn)
    Nothing -> throwDbError $ "getUserTableInfo: no such table: " ++ show tn
{-# INLINE getUserTableInfo' #-}


createUserTable' :: MVar (DbEnv p) -> TableName -> ModuleName -> KeySetName -> IO ()
createUserTable' s tn mn ksn = runMVState s $ do
  doPersist $ \p -> writeValue p (DataTable userTableInfo) Insert (asString tn) (UserTableInfo mn ksn)
  createTable' (userTable tn)

createTable' :: TableId -> MVState p ()
createTable' tn = do
  debug "createTable" tn
  doPersist $ \p -> P.createTable p (DataTable tn)
  doPersist $ \p -> P.createTable p (TxTable tn)


createSchema :: MVar (DbEnv p) -> IO ()
createSchema e = runMVState e $ do
  doPersist P.beginTx
  createTable' userTableInfo
  createTable' keysetsTable
  createTable' modulesTable
  doPersist P.commitTx
