{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Pure
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- "Pure" in-memory backend for Pact, used by repl.
--


module Pact.Pure
    (PureState(..),db,dbCommitted,dbTempLog
    ,Db(..),dbUserTables,dbModules,dbKeySets
    ,UserTable(..),utTable,utModule,utKeySet
    ,Table(..),tKV,tLog
    ,DbLog
    ,TempLog(..),tlUserTables,tlModules,tlKeySets
    --,PurePact(..),runPurePact
    ,puredb)
    where

import Control.Lens hiding (op,use,modifying)
import Data.List
import Control.Monad
import Prelude hiding (exp)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Default
import Data.Aeson
import Control.Concurrent
import Data.String

import Pact.Types.Runtime

type DbLog v = M.Map TxId (HashMap String v)

data Table v = Table {
      _tKV :: HashMap String v
    , _tLog :: DbLog v
} deriving (Eq,Show)
makeLenses ''Table
instance Default (Table v) where def = Table HM.empty M.empty

data UserTable = UserTable {
      _utTable :: Table (Columns Persistable)
    , _utModule :: ModuleName
    , _utKeySet :: KeySetName
} deriving (Eq,Show)
makeLenses ''UserTable

data Db = Db {
      _dbUserTables :: HashMap TableName UserTable
    , _dbModules :: Table Module
    , _dbKeySets :: Table KeySet
} deriving (Show)
makeLenses ''Db
instance Default Db where def = Db HM.empty def def

data TempLog = TempLog {
      _tlUserTables :: HashMap TableName (HashMap String (Columns Persistable))
    , _tlModules :: HashMap String Module
    , _tlKeySets :: HashMap String KeySet
}
makeLenses ''TempLog
instance Default TempLog where def = TempLog HM.empty HM.empty HM.empty


data PureState = PureState {
      _db :: Db
    , _dbCommitted :: Db
    , _dbTempLog :: TempLog
}
makeLenses ''PureState
instance Default PureState where def = PureState def def def



puredb :: PactDb PureState
puredb = PactDb {

    _readRow = \d k e ->
        case d of
          KeySets -> useMVar e (db.dbKeySets.tKV) >>= \m -> return $ HM.lookup (asString k) m
          Modules -> useMVar e (db.dbModules.tKV) >>= \m -> return $ HM.lookup (asString k) m
          (UserTables t) ->
              onUserTable e t $ \ut -> HM.lookup (asString k) (_tKV $ _utTable ut)

  , _writeRow = \wt d k v e ->
        case d of
          (UserTables t) -> writeUser e wt t k v
          KeySets -> writeSimple e wt KeySets dbKeySets tlKeySets k v
          Modules -> writeSimple e wt Modules dbModules tlModules k v

  , _keys = \t e ->
         onUserTable e t $ \ut -> map fromString $ HM.keys (_tKV $ _utTable ut)

  , _txids = \t tid e ->
         onUserTable e t $ \ut -> M.keys $ snd $ M.split tid (_tLog $ _utTable ut)

  , _createUserTable = \t m k e ->
         modifyingMVar e (db.dbUserTables) $ \uts ->
             case HM.lookup t uts of
               Just {} -> throwDbError $ "createUserTable: table already exists: " ++ show t
               Nothing -> return $ HM.insert t (UserTable def m k) uts

  , _getUserTableInfo = \t e ->
        onUserTable e t $ \UserTable {..} -> (_utModule,_utKeySet)

  , _beginTx = \e ->
        modifyMVar' e $ \s -> PureState (_dbCommitted s) (_dbCommitted s) def

  , _commitTx = \tid e ->
        modifyMVar' e (commitDb tid)

  , _rollbackTx = \e ->
        modifyMVar' e (\s -> PureState (_dbCommitted s) (_dbCommitted s) def)

  , _getTxLog = \d t e -> getLogs d t <$> useMVar e dbCommitted

}

writeUser :: MVar PureState -> WriteType -> TableName -> RowKey -> Columns Persistable -> IO ()
writeUser e wt t k v =
    let k' = asString k in
    modifyMVar_ e $ \ps ->
        case HM.lookup t (ps ^. db.dbUserTables) of
          Just ut@UserTable {..} -> do
            newrow <- case (HM.lookup k' (_tKV _utTable),wt) of
                        (Nothing,Update) -> throwDbError $ "Update: row not found: " ++ show (t,k)
                        (Nothing,_) -> return v
                        (Just {},Insert) -> throwDbError $ "Insert: row found: " ++ show (t,k)
                        (Just row,_) -> return (Columns (M.union (_columns v) (_columns row)))
            return $ ps &
                   db.dbUserTables %~ HM.insert t (ut & utTable.tKV %~ HM.insert k' newrow) &
                   dbTempLog.tlUserTables %~ HM.insertWith HM.union t (HM.singleton k' v)
          Nothing -> throwDbError $ "User table not found: " ++ show t
{-# INLINE writeUser #-}

writeSimple :: (AsString k,ToJSON v) => MVar PureState -> WriteType -> Domain k v ->
               Lens' Db (Table v) -> Lens' TempLog (HashMap String v) -> k -> v -> IO ()
writeSimple e wt d tableL logL k v =
    modifyMVar_ e $ \ps ->
        let k' = asString k
            write = return $ ps &
                    db.tableL.tKV %~ HM.insert k' v &
                    dbTempLog.logL %~ HM.insert k' v
            maybeLookup na ja =
                case HM.lookup k' (ps ^. db.tableL.tKV) of Nothing -> na; Just _ -> ja
        in case wt of
          Write -> write
          Update -> throwDbError ("Update: row not found: " ++ show (d,asString k)) `maybeLookup` write
          Insert -> write `maybeLookup` throwDbError ("Insert: row found: " ++ show (d,asString k))
{-# INLINE writeSimple #-}

onUserTable :: MVar PureState -> TableName -> (UserTable -> a) -> IO a
onUserTable e t f = do
  uts <- useMVar e $ db.dbUserTables
  case HM.lookup t uts of
    (Just ut) -> return $ f ut
    Nothing -> throwDbError $ "user table not found: " ++ show t
{-# INLINE onUserTable #-}


commitDb :: TxId -> PureState -> PureState
commitDb tid (PureState (Db us ms ks) _ (TempLog uls ml kl)) = PureState committed committed def
    where
      uls' = HM.toList uls
      committed = Db (foldl' commitUT us uls')
                  (ms & tLog %~ M.insert tid ml)
                  (ks & tLog %~ M.insert tid kl)
      commitUT :: HashMap TableName UserTable -> (TableName,HashMap String (Columns Persistable)) ->
                  HashMap TableName UserTable
      commitUT us' (tn,ul) = HM.adjust (& utTable.tLog %~ M.insert tid ul) tn us'
{-# INLINE commitDb #-}

getLogs :: Domain k v -> TxId -> Db -> [TxLog]
getLogs d@Modules tid db' = toLog d $ firstOf (dbModules.tLog.ix tid) db'
getLogs d@KeySets tid db' = toLog d $ firstOf (dbKeySets.tLog.ix tid) db'
getLogs d@(UserTables t) tid db' =
  toLog d $ fmap (fmap (fmap (toTerm :: Persistable -> Term Name))) $
  firstOf (dbUserTables.ix t.utTable.tLog.ix tid) db'
{-# INLINE getLogs #-}

toLog :: (ToJSON j, AsString k) =>
     Domain k v -> Maybe (HashMap String j) -> [TxLog]
toLog d (Just m) = map (\(k,v) -> TxLog (asString d) (asString k) (toJSON v)) $ HM.toList m
toLog _ Nothing = []
{-# INLINE toLog #-}
