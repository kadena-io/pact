{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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

import Control.Arrow ((&&&))
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Typeable
import Data.String

import Data.Aeson hiding ((.=))
import Data.Text (unpack)
import GHC.Generics

import qualified Data.Map.Strict as M
import Data.Maybe

import Pact.Types.RowData
import Pact.Types.Pretty
import Pact.Types.Runtime
import Pact.Persist as P
import Pact.Types.Logger

-- | Environment/MVar variable for pactdb impl.
data DbEnv p = DbEnv
  { _db :: !p
  , _persist :: !(Persister p)
  , _logger :: !Logger
  , _txRecord :: !(M.Map TxTable [TxLog Value])
  , _txId :: !TxId
  , _mode :: !(Maybe ExecutionMode)
  }
makeLenses ''DbEnv

initDbEnv :: Loggers -> Persister p -> p -> DbEnv p
initDbEnv loggers funrec p = DbEnv {
  _db = p,
  _persist = funrec,
  _logger = newLogger loggers "PactPersist",
  _txRecord = M.empty,
  _txId = 0,
  _mode = Nothing
  }

data UserTableInfo = UserTableInfo
  { utModule :: ModuleName
  } deriving (Eq,Show,Generic,Typeable)

instance Pretty UserTableInfo where
  pretty (UserTableInfo mod') = "UserTableInfo " <> commaBraces
    [ "module: " <> pretty mod'
    ]

instance PactDbValue UserTableInfo where prettyPactDbValue = pretty
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
namespacesTable :: TableId
namespacesTable = "SYS_namespaces"
pactsTable :: TableId
pactsTable = "SYS_pacts"
userTableInfo :: TableId
userTableInfo = "SYS_usertables"

type MVState p a = StateT (DbEnv p) IO a
instance Logging (StateT (DbEnv p) IO) where
  log c s = use logger >>= \l -> liftIO $ logLog l c s
  {-# INLINE log #-}

infix 4 .=!

(.=!) :: MonadState s m => ASetter s s a b -> b -> m ()
l .=! b = modify' (l .~ b)
{-# INLINE (.=!) #-}


runMVState :: MVar (DbEnv p) -> MVState p a -> IO a
runMVState v a = modifyMVar v $! \s -> do
    (!r, !m') <- runStateT a s
    return (m',r)
{-# INLINE runMVState #-}


doPersist :: (Persister p -> Persist p a) -> MVState p a
doPersist f = get >>= \m -> do
    (!db', !r) <- liftIO $ f (_persist m) (_db m)
    db .=! db'
    return r
{-# INLINE doPersist #-}

toTableId :: Domain k v -> TableId
toTableId KeySets = keysetsTable
toTableId Modules = modulesTable
toTableId Namespaces = namespacesTable
toTableId Pacts = pactsTable
toTableId (UserTables t) = userTable t

pactdb :: PactDb (DbEnv p)
pactdb = PactDb
  { _readRow = \d k e ->
       case d of
           KeySets -> readSysTable e (DataTable keysetsTable) (asString k)
           Modules -> readSysTable e (DataTable modulesTable) (asString k)
           Namespaces -> readSysTable e (DataTable namespacesTable) (asString k)
           Pacts -> readSysTable e (DataTable pactsTable) (asString k)
           (UserTables t) -> readUserTable e t k

 , _writeRow = \wt d k v e ->
       case d of
           KeySets -> writeSys e wt keysetsTable k v
           Modules -> writeSys e wt modulesTable k v
           Namespaces -> writeSys e wt namespacesTable k v
           Pacts -> writeSys e wt pactsTable k v
           (UserTables t) -> writeUser e wt t k v

 , _keys = \tn e -> runMVState e
     (map (fromString . unpack . asString) <$> doPersist (\p -> queryKeys p (DataTable $ toTableId tn) Nothing))


 , _txids = \tn tid e -> runMVState e
     (map fromIntegral <$> doPersist
       (\p -> queryKeys p (userTxRecord tn) (Just (KQKey KGTE (fromIntegral tid)))))


 , _createUserTable = \tn mn e ->
       createUserTable' e tn mn

 , _getUserTableInfo = \tn e -> getUserTableInfo' e tn

 , _beginTx = \tidm s -> runMVState s $ doBegin tidm

 , _commitTx = \s -> runMVState s doCommit

 , _rollbackTx = \s -> runMVState s rollback

 , _getTxLog = \d tid e -> runMVState e $ getLogs d tid

 }

doBegin :: ExecutionMode -> MVState p (Maybe TxId)
doBegin m = do
  use mode >>= \m' -> case m' of
    Just {} -> do
      logError "beginTx: In transaction, rolling back"
      rollback
    Nothing -> return ()
  resetTemp
  doPersist $ \p -> P.beginTx p m
  mode .=! Just m
  case m of
    Transactional -> Just <$> use txId
    Local -> pure Nothing
{-# INLINE doBegin #-}

doCommit :: MVState p [TxLog Value]
doCommit = use mode >>= \mm -> case mm of
    Nothing -> rollback >> throwDbError "doCommit: Not in transaction"
    Just m -> do
      txrs <- M.toList <$> use txRecord
      if (m == Transactional) then do
        -- grab current txid and increment in state
        tid' <- state (fromIntegral . _txId &&& over txId succ)
        -- write txlog
        forM_ txrs $ \(t,es) -> doPersist $ \p -> writeValue p t Write tid' es
        -- commit
        doPersist P.commitTx
        resetTemp
      else rollback
      return (concatMap snd txrs)
{-# INLINE doCommit #-}


rollback :: MVState p ()
rollback = do
  (r :: Either SomeException ()) <- try (doPersist P.rollbackTx)
  case r of
    Left e -> logError $ "rollback: " ++ show e
    Right !_ -> return ()
  resetTemp


getLogs :: FromJSON v => Domain k v -> TxId -> MVState p [TxLog v]
getLogs d tid = mapM convLog . fromMaybe [] =<< doPersist (\p -> readValue p (tn d) (fromIntegral tid))
  where
    tn :: Domain k v -> TxTable
    tn KeySets    = TxTable keysetsTable
    tn Modules    = TxTable modulesTable
    tn Namespaces = TxTable namespacesTable
    tn Pacts = TxTable pactsTable
    tn (UserTables t) = userTxRecord t
    convLog tl = case fromJSON (_txValue tl) of
      Error s -> throwDbError $ "Unexpected value, unable to deserialize log: " <> prettyString s
      Success v -> return $ set txValue v tl
{-# INLINE getLogs #-}



debug :: Show a => String -> a -> MVState p ()
debug s a = logDebug $ s ++ ": " ++ show a


readUserTable :: MVar (DbEnv p) -> TableName -> RowKey -> IO (Maybe RowData)
readUserTable e t k = runMVState e $ readUserTable' t k
{-# INLINE readUserTable #-}

readUserTable' :: TableName -> RowKey -> MVState p (Maybe RowData)
readUserTable' t k = doPersist $ \p -> readValue p (userDataTable t) (DataKey $ asString k)
{-# INLINE readUserTable' #-}

readSysTable :: PactDbValue v => MVar (DbEnv p) -> DataTable -> Text -> IO (Maybe v)
readSysTable e t k = runMVState e $ doPersist $ \p -> readValue p t (DataKey k)
{-# INLINE readSysTable #-}

resetTemp :: MVState p ()
resetTemp = txRecord .=! M.empty >> mode .=! Nothing
{-# INLINE resetTemp #-}

writeSys :: (AsString k,PactDbValue v) => MVar (DbEnv p) -> WriteType -> TableId -> k -> v -> IO ()
writeSys s wt tbl k v = runMVState s $ do
  debug "writeSys" (tbl,asString k)
  doPersist $ \p -> writeValue p (DataTable tbl) wt (DataKey $ asString k) v
  record (TxTable tbl) k v

{-# INLINE writeSys #-}

writeUser :: MVar (DbEnv p) -> WriteType -> TableName -> RowKey -> RowData -> IO ()
writeUser s wt tn rk row = runMVState s $ do
  let ut = userDataTable tn
      tt = userTxRecord tn
      rk' = DataKey (asString rk)
  olds <- readUserTable' tn rk
  let ins = do
        debug "writeUser: insert" (tn,rk)
        doPersist $ \p -> writeValue p ut Insert rk' row
        finish row
      upd oldrow = do
        -- version follows new input
        let row' = RowData (_rdVersion row) $ ObjectMap (M.union (_objectMap $ _rdData row) (_objectMap $ _rdData oldrow))
        doPersist $ \p -> writeValue p ut Update rk' row'
        finish row'
      finish row' = record tt rk row'
  case (olds,wt) of
    (Nothing,Insert) -> ins
    (Just _,Insert) -> throwDbError $ "Insert: row found for key " <> pretty rk
    (Nothing,Write) -> ins
    (Just old,Write) -> upd old
    (Just old,Update) -> upd old
    (Nothing,Update) -> throwDbError $ "Update: no row found for key " <> pretty rk
{-# INLINE writeUser #-}

record :: (AsString k, PactDbValue v) => TxTable -> k -> v -> MVState p ()
record tt k v = modify'
    $ over txRecord
    $ M.insertWith (flip append) tt [TxLog (asString (tableId tt)) (asString k) (toJSON v)]
  where
    -- strict append (it would be better to use a datastructure with efficient append)
    append [] b = b
    append (h:t) b = let !x = append t b in h : x
{-# INLINE record #-}

getUserTableInfo' :: MVar (DbEnv p) -> TableName -> IO ModuleName
getUserTableInfo' e tn = runMVState e $ do
  r <- doPersist $ \p -> readValue p (DataTable userTableInfo) (DataKey $ asString tn)
  case r of
    (Just (UserTableInfo mn)) -> return mn
    Nothing -> throwDbError $ "getUserTableInfo: no such table: " <> pretty tn
{-# INLINE getUserTableInfo' #-}


createUserTable' :: MVar (DbEnv p) -> TableName -> ModuleName -> IO ()
createUserTable' s tn mn = runMVState s $ do
  let uti = UserTableInfo mn
  doPersist $ \p -> writeValue p (DataTable userTableInfo) Insert (DataKey $ asString tn) uti
  record (TxTable userTableInfo) tn uti
  createTable' (userTable tn)

createTable' :: TableId -> MVState p ()
createTable' tn = do
  log "DDL" $ "createTable: " ++ show tn
  doPersist $ \p -> P.createTable p (DataTable tn)
  doPersist $ \p -> P.createTable p (TxTable tn)


createSchema :: MVar (DbEnv p) -> IO ()
createSchema e = runMVState e $ do
  doPersist (\p -> P.beginTx p Transactional)
  createTable' userTableInfo
  createTable' keysetsTable
  createTable' modulesTable
  createTable' namespacesTable
  createTable' pactsTable
  doPersist P.commitTx
