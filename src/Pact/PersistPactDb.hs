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
-- Module      :  Pact.Server.SQLite
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- SQLite backend for Pact service.
--

module Pact.PersistPactDb
  ( PSL(..), persist, db, log, txId
  , psl
  , createSchema
  , createUserTable'
  , UserTableInfo(..)
  ) where

import Prelude hiding (log)

import Control.Arrow
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding ((.=))
import GHC.Generics

import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Maybe

import Pact.Types.Runtime hiding ((<>))
import Pact.Persist as P


data PSL p = PSL
  { _db :: p
  , _persist :: Persister p
  , _log :: forall s . Show s => (String -> s -> IO ())
  , _txRecord :: M.Map TxTable [TxLog]
  , _txId :: Maybe TxId
  }
makeLenses ''PSL

psl :: PactDb (PSL p)
psl = PactDb
  { _readRow = \d k e ->
       case d of
           KeySets -> readSysTable e keysetsTable (asString k)
           Modules -> readSysTable e modulesTable (asString k)
           (UserTables t) -> readUserTable e t k

 , _writeRow = \wt d k v e ->
       case d of
           KeySets -> writeSys e wt keysetsTable k v
           Modules -> writeSys e wt modulesTable k v
           (UserTables t) -> writeUser e wt t k v

 , _keys = \tn e -> modifyMVar e $ \m ->
     second (map RowKey) <$> withDB (queryKeys (_persist m) (userTable tn) Nothing) m


 , _txids = \tn tid e -> modifyMVar e $ \m -> do
     second (map fromIntegral) <$> withDB
       (queryKeys (_persist m) (userTxRecord tn) (Just (KQKey KGT (fromIntegral tid)))) m


 , _createUserTable = \tn mn ksn e ->
       createUserTable' e tn mn ksn

 , _getUserTableInfo = \tn e -> getUserTableInfo' e tn

 , _beginTx = \tidm s -> modifyMVar_ s $ \m -> do
     m' <- case _txId m of
             Just _ -> do
               _log m "beginTx" ("In transaction, rolling back" :: String)
               rollback m
             Nothing -> return m
     (m'',()) <- withDB (P.beginTx (_persist m)) m'
     return $ set txId tidm $ resetTemp m''

 , _commitTx = \s -> do
     r <- modifyMVar s $ \m -> case _txId m of
       Nothing -> (,Just "Not in transaction") <$> rollback m
       Just tid -> do
         let tid' = fromIntegral tid
         e' <- foldM (\sl (t,es) -> fst <$> writeValue (_persist m) t Write tid' es sl) (_db m) (M.toList (_txRecord m))
         (e'',()) <- P.commitTx (_persist m) e'
         return (resetTemp (set db e'' m),Nothing)
     mapM_ throwDbError r

 , _rollbackTx = \s -> modifyMVar_ s rollback

 , _getTxLog = \d tid e -> modifyMVar e $ \m -> do
      let tn :: Domain k v -> TxTable
          tn KeySets = keysetsTxRecord
          tn Modules = modulesTxRecord
          tn (UserTables t) = userTxRecord t
      fmap (fromMaybe []) <$> withDB (readValue (_persist m) (tn d) (fromIntegral tid)) m
 }

withDB :: (p -> IO (p, a)) -> PSL p -> IO (PSL p, a)
withDB a m = a (_db m) >>= \(s',r) -> return (set db s' m,r)

withDB_ :: (p -> IO (p, ())) -> PSL p -> IO (PSL p)
withDB_ a m = fst <$> withDB a m

rollback :: PSL p -> IO (PSL p)
rollback m = do
  (r :: Either SomeException (PSL p)) <- try $ withDB_ (P.rollbackTx (_persist m)) m
  m' <- case r of
          Left e -> _log m "rollback" ("ERROR: " ++ show e) >> return m
          Right m' -> return m'
  return (resetTemp m')

readUserTable :: MVar (PSL p) -> TableName -> RowKey -> IO (Maybe (Columns Persistable))
readUserTable e t k = modifyMVar e $ \m -> readUserTable' m t k

readUserTable' :: PSL p -> TableName -> RowKey -> IO (PSL p,Maybe (Columns Persistable))
readUserTable' m t k = do
  withDB (readValue (_persist m) (userTable t) (asString k)) m
{-# INLINE readUserTable' #-}

readSysTable :: FromJSON v => MVar (PSL p) -> DataTable -> Text -> IO (Maybe v)
readSysTable e t k = modifyMVar e $ \m -> do
  withDB (readValue (_persist m) t k) m
{-# INLINE readSysTable #-}

resetTemp :: PSL p -> PSL p
resetTemp = set txRecord M.empty . set txId Nothing

writeSys :: (AsString k,ToJSON v) => MVar (PSL p) -> WriteType -> DataTable -> k -> v -> IO ()
writeSys s wt tbl k v = modifyMVar_ s $ \m -> do
    _log m "writeSys" (tbl,asString k)
    withDB_ (writeValue (_persist m) tbl wt (asString k) v) m
{-# INLINE writeSys #-}

writeUser :: MVar (PSL p) -> WriteType -> TableName -> RowKey -> Columns Persistable -> IO ()
writeUser s wt tn rk row = modifyMVar_ s $ \m -> do
  let ut = userTable tn
      tt = userTxRecord tn
      rk' = asString rk
  (_,olds) <- readUserTable' m tn rk
  let ins = do
        _log m "writeUser: insert" (tn,rk)
        finish row <$> withDB_ (writeValue (_persist m) ut Insert rk' row) m
      upd oldrow = do
        let row' = Columns (M.union (_columns row) (_columns oldrow))
        finish row' <$> withDB_ (writeValue (_persist m) ut Update rk' row') m
      finish row' m' =
           over txRecord (M.insertWith (++) tt [TxLog (asString tn) (asString rk) (toJSON row')]) m'
  case (olds,wt) of
    (Nothing,Insert) -> ins
    (Just _,Insert) -> throwDbError $ "Insert: row found for key " ++ show rk
    (Nothing,Write) -> ins
    (Just old,Write) -> upd old
    (Just old,Update) -> upd old
    (Nothing,Update) -> throwDbError $ "Update: no row found for key " ++ show rk
{-# INLINE writeUser #-}

data UserTableInfo = UserTableInfo {
  utModule :: ModuleName,
  utKeySet :: KeySetName
  } deriving (Eq,Show,Generic)

instance FromJSON UserTableInfo
instance ToJSON UserTableInfo

userTableInfo :: DataTable
userTableInfo = DataTable "SYS_usertables"

getUserTableInfo' :: MVar (PSL p) -> TableName -> IO (ModuleName, KeySetName)
getUserTableInfo' e tn = modifyMVar e $ \m -> do
  (m',r) <- withDB (readValue (_persist m) userTableInfo (asString tn)) m
  case r of
    (Just (UserTableInfo mn ksn)) -> return (m',(mn,ksn))
    Nothing -> throwDbError $ "getUserTableInfo: no such table: " ++ show tn

userTable :: TableName -> DataTable
userTable tn = DataTable $ "USER_" <> asString tn
{-# INLINE userTable #-}
userTxRecord :: TableName -> TxTable
userTxRecord tn = TxTable $ "USER_" <> asString tn
{-# INLINE userTxRecord #-}

keysetsTable :: DataTable
keysetsTable = DataTable "SYS_keysets"
modulesTable :: DataTable
modulesTable = DataTable "SYS_modules"
keysetsTxRecord :: TxTable
keysetsTxRecord = TxTable "SYS_keysets"
modulesTxRecord :: TxTable
modulesTxRecord = TxTable "SYS_modules"

createUserTable' :: MVar (PSL p) -> TableName -> ModuleName -> KeySetName -> IO ()
createUserTable' s tn mn ksn = modifyMVar_ s $ \m ->
  withDB_ (writeValue (_persist m) userTableInfo Insert (asString tn) (UserTableInfo mn ksn)) m
    >>= createTable' (userTable tn) (userTxRecord tn)

createTable' :: DataTable -> TxTable -> PSL p -> IO (PSL p)
createTable' ut ur m = do
  _log m "createTables" (ut,ur)
  withDB_ (P.createTable (_persist m) ut) m
    >>= withDB_ (P.createTable (_persist m) ur)


createSchema :: MVar (PSL p) -> IO ()
createSchema e = modifyMVar_ e $ \m -> do
  withDB_ (P.createTable (_persist m) userTableInfo) m
    >>= createTable' keysetsTable keysetsTxRecord
    >>= createTable' modulesTable modulesTxRecord
