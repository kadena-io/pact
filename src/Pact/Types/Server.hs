{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Types.Server
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types specific to the HTTP server and Pact service.
--
module Pact.Types.Server
  ( userSigToPactPubKey, userSigsToPactKeySet
  , CommandConfig(..), ccDbFile, ccDebugFn, ccEntity, ccPragmas
  , CommandState(..), csRefStore
  , CommandEnv(..), ceConfig, ceMode, ceDBVar, ceState
  , CommandM, runCommand, throwCmdEx
  , DBVar(..)
  , History(..)
  , ExistenceResult(..)
  , PossiblyIncompleteResults(..)
  , ListenerResult(..)
  , initChans
  , InboundPactChan(..), readInbound, writeInbound
  , HistoryChannel(..), readHistory, writeHistory
  , ReplayFromDisk(..)
  , Inbound(..)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Concurrent.Chan
import Data.Maybe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.Text.Encoding
import Data.Aeson

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)

import Prelude hiding (log,exp)

import Pact.Pure
import Pact.Server.SQLite
import Pact.Types.Runtime as Pact
import Pact.Types.Orphans ()
import Pact.Types.SQLite
import Pact.Types.Command

userSigToPactPubKey :: UserSig -> Pact.PublicKey
userSigToPactPubKey UserSig{..} = Pact.PublicKey $ encodeUtf8 _usPubKey

userSigsToPactKeySet :: [UserSig] -> S.Set Pact.PublicKey
userSigsToPactKeySet = S.fromList . fmap userSigToPactPubKey


data CommandConfig = CommandConfig {
      _ccDbFile :: Maybe FilePath
    , _ccDebugFn :: String -> IO ()
    , _ccEntity :: Text
    , _ccPragmas :: [Pragma]
    }
$(makeLenses ''CommandConfig)

data CommandState = CommandState {
     _csRefStore :: RefStore
    }
$(makeLenses ''CommandState)

data DBVar = PureVar (MVar PureState) | PSLVar (MVar PSL)

data CommandEnv = CommandEnv {
      _ceConfig :: CommandConfig
    , _ceMode :: ExecutionMode
    , _ceDBVar :: DBVar
    , _ceState :: MVar CommandState
    }
$(makeLenses ''CommandEnv)

data CommandException = CommandException String deriving (Typeable)
instance Show CommandException where show (CommandException e) = e
instance Exception CommandException


type CommandM a = ReaderT CommandEnv IO a

runCommand :: CommandEnv -> CommandM a -> IO a
runCommand e a = runReaderT a e

throwCmdEx :: MonadThrow m => String -> m a
throwCmdEx = throw . CommandException


newtype InboundPactChan = InboundPactChan (Chan Inbound)
newtype HistoryChannel = HistoryChannel (Chan History)
newtype ReplayFromDisk = ReplayFromDisk (MVar [Command ByteString])

initChans :: IO (InboundPactChan,HistoryChannel)
initChans = (,) <$> (InboundPactChan <$> newChan)
                <*> (HistoryChannel <$> newChan)

writeInbound :: InboundPactChan -> Inbound -> IO ()
writeInbound (InboundPactChan c) = writeChan c

readInbound :: InboundPactChan -> IO Inbound
readInbound (InboundPactChan c) = readChan c

readHistory :: HistoryChannel -> IO History
readHistory (HistoryChannel c) = readChan c

writeHistory :: HistoryChannel -> History -> IO ()
writeHistory (HistoryChannel c) h = writeChan c h

newtype ExistenceResult = ExistenceResult
  { rksThatAlreadyExist :: HashSet RequestKey
  } deriving (Show, Eq)

newtype PossiblyIncompleteResults = PossiblyIncompleteResults
  { possiblyIncompleteResults :: HashMap RequestKey CommandResult
  } deriving (Show, Eq)

data ListenerResult =
  ListenerResult CommandResult |
  GCed String
  deriving (Show, Eq)

data History =
  AddNew
    { hNewKeys :: ![Command ByteString]} |
  Update
    { hUpdateRks :: !(HashMap RequestKey CommandResult) } |
  QueryForResults
    { hQueryForResults :: !(HashSet RequestKey, MVar PossiblyIncompleteResults) } |
  RegisterListener
    { hNewListener :: !(HashMap RequestKey (MVar ListenerResult))}
  deriving (Eq)


data Inbound =
  TxCmds { iCmds :: [Command ByteString] } |
  LocalCmd { iCmd :: Command ByteString,
             iLocalResult :: MVar Value
           }
  deriving (Eq)
