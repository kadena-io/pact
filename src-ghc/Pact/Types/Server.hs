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
  , CommandConfig(..), ccSqlite, ccEntity, ccGasLimit, ccGasRate
  , CommandState(..), csRefStore, csPacts
  , CommandEnv(..), ceEntity, ceMode, ceDbEnv, ceState, ceLogger, cePublicData, ceGasEnv
  , CommandM, runCommand, throwCmdEx
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
import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.Chan
import Data.Maybe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import Data.Text.Encoding
import Data.Aeson
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Word
import Data.Int

import Prelude

import Pact.Types.Runtime as Pact
import Pact.Types.Orphans ()
import Pact.Types.SQLite
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Interpreter


userSigToPactPubKey :: UserSig -> Pact.PublicKey
userSigToPactPubKey UserSig{..} = Pact.PublicKey $ encodeUtf8 _usAddress


userSigsToPactKeySet :: [UserSig] -> S.Set Pact.PublicKey
userSigsToPactKeySet = S.fromList . fmap userSigToPactPubKey


data CommandConfig = CommandConfig {
      _ccSqlite :: Maybe SQLiteConfig
    , _ccEntity :: Maybe EntityName
    , _ccGasLimit :: Maybe Int
    , _ccGasRate :: Maybe Int
    }
$(makeLenses ''CommandConfig)



data CommandState = CommandState {
       _csRefStore :: RefStore
     , _csPacts :: M.Map PactId PactExec
     } deriving Show
$(makeLenses ''CommandState)

data CommandEnv p = CommandEnv {
      _ceEntity :: Maybe EntityName
    , _ceMode :: ExecutionMode
    , _ceDbEnv :: PactDbEnv p
    , _ceState :: MVar CommandState
    , _ceLogger :: Logger
    , _ceGasEnv :: GasEnv
    , _cePublicData :: PublicData
    }
$(makeLenses ''CommandEnv)

newtype CommandException = CommandException String deriving (Typeable)
instance Show CommandException where show (CommandException e) = e
instance Exception CommandException


type CommandM p a = ReaderT (CommandEnv p) IO a

runCommand :: CommandEnv p -> CommandM p a -> IO a
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
