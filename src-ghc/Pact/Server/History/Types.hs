{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pact.Server.History.Types
  ( HistoryEnv(..), historyChannel, inboundPactChannel, debugPrint, dbPath, replayFromDisk
  , HistoryState(..), registeredListeners, persistence
  , PersistenceSystem(..)
  , HistoryService
  , DbEnv(..)
  ) where

import Control.Lens hiding (Index)

import Control.Monad.Trans.RWS.Strict
import Control.Concurrent.MVar

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)

import Database.SQLite3.Direct

import Pact.Types.Command
import Pact.Types.Server

data HistoryEnv = HistoryEnv
  { _historyChannel :: !HistoryChannel
  , _inboundPactChannel :: !InboundPactChan
  , _debugPrint :: !(String -> IO ())
  , _dbPath :: !(Maybe FilePath)
  , _replayFromDisk :: !ReplayFromDisk
  }
makeLenses ''HistoryEnv

data DbEnv = DbEnv
  { _conn :: !Database
  , _insertStatement :: !Statement
  , _qryExistingStmt :: !Statement
  , _qryCompletedStmt :: !Statement
  , _qrySelectAllCmds :: !Statement
  }

data PersistenceSystem =
  InMemory
    { inMemResults :: !(HashMap RequestKey (Command ByteString, Maybe CommandResult))} |
  OnDisk
    { incompleteRequestKeys :: !(HashMap RequestKey (Command ByteString))
    , dbConn :: !DbEnv}

data HistoryState = HistoryState
  { _registeredListeners :: !(HashMap RequestKey [MVar ListenerResult])
  , _persistence :: !PersistenceSystem
  }
makeLenses ''HistoryState

type HistoryService = RWST HistoryEnv () HistoryState IO
