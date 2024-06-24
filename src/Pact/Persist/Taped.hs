{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Pact.Persist.Taped
-- Copyright   :  (C) 2024 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edmund Noble <edmund@kadena.io>
--
-- Tapes for recording and playing back database access.
--
module Pact.Persist.Taped
  ( WriteSet(..)
  , reflectingDb
  , ReflectingDbEnv(..)
  , reflectingInputMirror
  , reflectingOutputMirror
  , reflectingSource
  , reflectingWriteSet
  ) where

import Control.Concurrent.MVar
import Control.Lens

import Data.Default
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Pact.Types.Term
import Pact.Types.Persistence
import Control.Monad.State
import Control.Monad
import qualified Data.Map as Map
import Pact.Types.RowData
import Pact.Types.Lang (Code(..))
import Pact.Types.Namespace
import Data.Maybe (isNothing)

data WriteSet
  = WriteSet
    { _userTableWrites :: Map TableName (Set RowKey)
    , _keySetWrites :: Set KeySetName
    , _moduleWrites :: Set ModuleName
    , _namespaceWrites :: Set NamespaceName
    , _defPactWrites :: Set PactId
    }

instance Semigroup WriteSet where
  ws <> ws' = WriteSet
    { _userTableWrites = Map.unionWith Set.union (_userTableWrites ws) (_userTableWrites ws')
    , _keySetWrites = Set.union (_keySetWrites ws) (_keySetWrites ws')
    , _moduleWrites = Set.union (_moduleWrites ws) (_moduleWrites ws')
    , _namespaceWrites = Set.union (_namespaceWrites ws) (_namespaceWrites ws')
    , _defPactWrites = Set.union (_defPactWrites ws) (_defPactWrites ws')
    }
instance Monoid WriteSet where
  mempty = WriteSet mempty mempty mempty mempty mempty

data ReflectingDbEnv mi mo s = ReflectingDbEnv
  { _reflectingWriteSet :: !WriteSet
  , _reflectingInputMirror :: !mi
  , _reflectingOutputMirror :: !mo
  , _reflectingSource :: !s
  }

makeLenses ''ReflectingDbEnv

-- Given inputMirrorDb, outputMirrorDb, and sourceDb, this function creates a PactDb which forwards its read and
-- write requests to sourceDb, and simultaneously populates inputMirrorDb with enough data to allow re-running
-- the same requests without the entire source db. It also populates outputMirrorDb with all of the writes made.
-- Together these allow making small, self-contained tests by:
-- 1. running Pact transactions on reflectingDb with larger source databases
-- 2. extracting the mirror databases in JSON format
-- 3. running the same Pact transactions on reflectingDb with the source set to the original input mirror database
-- 4. asserting that the final output mirror is equal to the original output mirror
--
-- Note: createSchema operates on Persister, which is a layer lower than PactDb;
-- one must have valid schemas in all database values before using them with reflectingDb.
reflectingDb :: PactDb mi -> PactDb mo -> PactDb s -> PactDb (ReflectingDbEnv mi mo s)
reflectingDb inputMirrorDb outputMirrorDb sourceDb = PactDb
  { _readRow = \d k -> modifyMVarStateT $ do
      -- when read from, the reflecting db reflects the write from the source to the mirror
      -- and returns the read value
      mv <- wrapMethod reflectingSource (_readRow sourceDb d k)
      createTableForDomainIfMissing reflectingInputMirror inputMirrorDb d
      WriteSet{..} <- use reflectingWriteSet
      wrapMethod reflectingInputMirror $ \var -> do
        -- write to the mirror db only if this row wasn't already written to the source during this transaction
        () <- case d of
          UserTables tn
            | Just tableWrites <- Map.lookup tn _userTableWrites
            , Set.member k tableWrites -> return ()
            | otherwise -> forM_ mv $ \v ->
              _writeRow inputMirrorDb Write d k v var
          KeySets
            | Set.member k _keySetWrites -> return ()
            | otherwise -> forM_ mv $ \v ->
              _writeRow inputMirrorDb Write d k v var
          Modules
            | Set.member k _moduleWrites -> return ()
            | otherwise -> forM_ mv $ \v ->
              _writeRow inputMirrorDb Write d k v var
          Namespaces
            | Set.member k _namespaceWrites -> return ()
            | otherwise -> forM_ mv $ \v ->
              _writeRow inputMirrorDb Write d k v var
          Pacts
            | Set.member k _defPactWrites -> return ()
            | otherwise -> forM_ mv $ \v ->
              _writeRow inputMirrorDb Write d k v var
        return ()
      return mv

  , _writeRow = \wt d k v -> modifyMVarStateT $ do
      wrapMethod reflectingSource (_writeRow sourceDb wt d k v)
      createTableForDomainIfMissing reflectingOutputMirror outputMirrorDb d
      -- we always use Write mode here, because the above write must have succeeded
      wrapMethod reflectingOutputMirror (_writeRow outputMirrorDb Write d k v)
      reflectingWriteSet <>= case d of
        UserTables tn -> mempty { _userTableWrites = Map.singleton tn (Set.singleton k) }
        KeySets -> mempty { _keySetWrites = Set.singleton k }
        Modules -> mempty { _moduleWrites = Set.singleton k }
        Namespaces -> mempty { _namespaceWrites = Set.singleton k }
        Pacts -> mempty { _defPactWrites = Set.singleton k }
  , _keys = \d -> modifyMVarStateT $ do
      ks <- wrapMethod reflectingSource (_keys sourceDb d)
      createTableForDomainIfMissing reflectingInputMirror inputMirrorDb d
      -- we have no way of knowing the values at these keys, so we write fake values
      () <- case d of
        UserTables tn -> do
          forM_ ks $ \k -> wrapMethod reflectingInputMirror (_writeRow inputMirrorDb Write (UserTables tn) k
            RowData { _rdVersion = RDV1, _rdData = ObjectMap mempty })
        KeySets -> do
          forM_ ks $ \k -> wrapMethod reflectingInputMirror (_writeRow inputMirrorDb Write KeySets k
            KeySet { _ksKeys = mempty, _ksPredFun = Name $ BareName "fake" def })
        Modules -> do
          forM_ ks $ \k ->
            wrapMethod reflectingInputMirror
              (_writeRow inputMirrorDb Write Modules k
                (ModuleData (MDInterface (Interface "fake" (Code "") (Meta Nothing []) []) ) mempty mempty))
        Namespaces -> do
          forM_ ks $ \k ->
            wrapMethod reflectingInputMirror
              (_writeRow inputMirrorDb Write Namespaces k
                (Namespace (NamespaceName "fake") (GKeySetRef "fake") (GKeySetRef "fake")))
        Pacts -> do
          forM_ ks $ \k ->
            wrapMethod reflectingInputMirror (_writeRow inputMirrorDb Write Pacts k Nothing)

      return ks

  , _txids = \tn txid -> modifyMVarStateT $ wrapMethod reflectingSource (_txids sourceDb tn txid)
  , _createUserTable = \tn mn -> modifyMVarStateT $ do
      wrapMethod reflectingSource (_createUserTable sourceDb tn mn)
      wrapMethod reflectingOutputMirror (_createUserTable outputMirrorDb tn mn)

  , _getUserTableInfo = \tn -> modifyMVarStateT $ wrapMethod reflectingSource (_getUserTableInfo sourceDb tn)
  , _beginTx = \em -> modifyMVarStateT $ do
    txid <- wrapMethod reflectingSource (_beginTx sourceDb em)
    _ <- wrapMethod reflectingInputMirror (_beginTx inputMirrorDb em)
    _ <- wrapMethod reflectingOutputMirror (_beginTx outputMirrorDb em)
    return txid
  , _commitTx = modifyMVarStateT $ do
    logs <- wrapMethod reflectingSource (_commitTx sourceDb)
    _ <- wrapMethod reflectingInputMirror (_commitTx inputMirrorDb)
    _ <- wrapMethod reflectingOutputMirror (_commitTx outputMirrorDb)
    -- reset write set outside of transaction
    reflectingWriteSet .= mempty
    return logs
  , _rollbackTx = modifyMVarStateT $ do
    wrapMethod reflectingSource (_rollbackTx sourceDb)
    -- we commit to the mirror databases even on a rollback, to allow observing even failed txs
    _ <- wrapMethod reflectingInputMirror (_commitTx inputMirrorDb)
    _ <- wrapMethod reflectingOutputMirror (_commitTx outputMirrorDb)
    -- reset write set outside of transaction
    reflectingWriteSet .= mempty
  , _getTxLog = \d txid -> modifyMVarStateT $ do
    wrapMethod reflectingSource (_getTxLog sourceDb d txid)
  }
  where
  wrapMethod :: Lens' s a -> (MVar a -> IO r) -> StateT s IO r
  wrapMethod l m = do
    v <- get
    var <- liftIO $ newMVar (v ^. l)
    r <- liftIO $ m var
    v' <- liftIO $ takeMVar var
    put (v & l .~ v')
    return r

  modifyMVarStateT act var = do
    modifyMVar var $ \v -> do
      (a, v') <- runStateT act v
      return (v', a)

  createTableForDomainIfMissing :: Lens' s a -> PactDb a -> Domain k v -> StateT s IO ()
  createTableForDomainIfMissing l db d = wrapMethod l $ \var ->
    -- create a user table for this write if missing, do this even if the value
    -- itself is Nothing, to avoid missing table errors
    case d of
      UserTables tn -> do
        tableModule <- _getUserTableInfo db tn var
        when (isNothing tableModule) $
          _createUserTable db tn "fake module name" var
      _ -> return ()
