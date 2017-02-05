{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Server.Types.API
  ( ApiResponse(..), apiResponse, apiError
  , RequestKeys(..), rkRequestKeys
  , SubmitBatch(..), sbCmds
  , SubmitBatchResponse
  , Poll(..)
  , PollResult(..), prRequestKey, prLatency, prResponse
  , PollResponse
  , ListenerRequest(..)
  , ListenerResponse
  , InboundPactChan(..),OutboundPactChan(..),PactResult(..)
  , initChans,writeInbound,writeOutbound,readInbound,readOutbound
  ) where

import Data.Text (Text)
import Data.Aeson hiding (Success)
import qualified Data.Aeson as A
import Data.Aeson.Types (Options(..))
import Control.Lens hiding ((.=))
import GHC.Generics
import Data.Int
import Control.Concurrent.Chan




import Pact.Server.Types.Base
import Pact.Server.Types.Command

data ApiResponse a =
  ApiSuccess
    { _apiResponse :: !a} |
  ApiFailure
    { _apiError :: !String}
  deriving (Show, Eq, Generic)
makeLenses ''ApiResponse

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON (ApiSuccess a)= object [ "status" .= A.String "success", "response" .= a]
  toJSON (ApiFailure a)= object [ "status" .= A.String "failure", "response" .= a]
instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON (Object o) = do
    st <- o .: "status"
    if st == A.String "success"
    then ApiSuccess <$> o .: "response"
    else ApiFailure <$> o .: "response"
  parseJSON _ = mempty

data RequestKeys = RequestKeys
  { _rkRequestKeys :: ![RequestKey]
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''RequestKeys

instance ToJSON RequestKeys where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = lensyConstructorToNiceJson 3})
instance FromJSON RequestKeys

-- | Submit new commands for execution
data SubmitBatch = SubmitBatch
  { _sbCmds :: ![Command Text]
  } deriving (Eq,Generic)
makeLenses ''SubmitBatch
instance ToJSON SubmitBatch where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = lensyConstructorToNiceJson 3})
instance FromJSON SubmitBatch

-- | What you get back from a SubmitBatch
type SubmitBatchResponse = ApiResponse RequestKeys

-- | Poll for results by RequestKey
newtype Poll = Poll [RequestKey]
  deriving (Eq,Show,Generic)
instance ToJSON Poll where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Poll

-- | What you get back from a Poll
data PollResult = PollResult
  { _prRequestKey :: !RequestKey
  , _prLatency :: !Int64
  , _prResponse :: !CommandResult
  } deriving (Eq,Show,Generic)
makeLenses ''PollResult
instance ToJSON PollResult where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = lensyConstructorToNiceJson 3})
instance FromJSON PollResult

type PollResponse = ApiResponse [PollResult]

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest RequestKey
  deriving (Eq,Show,Generic)
instance ToJSON ListenerRequest where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ListenerRequest

type ListenerResponse = ApiResponse PollResult


newtype InboundPactChan = InboundPactChan (Chan [Command Payload])
data PactResult = PactResult {
  _prCommand :: Command Payload,
  _prResult :: Value
  } deriving (Eq,Show)
newtype OutboundPactChan = OutboundPactChan (Chan [PactResult])

initChans :: IO (InboundPactChan,OutboundPactChan)
initChans = (,) <$> (InboundPactChan <$> newChan) <*> (OutboundPactChan <$> newChan)

writeInbound :: InboundPactChan -> [Command Payload] -> IO ()
writeInbound (InboundPactChan c) = writeChan c

readInbound :: InboundPactChan -> IO [Command Payload]
readInbound (InboundPactChan c) = readChan c

writeOutbound :: OutboundPactChan -> [PactResult] -> IO ()
writeOutbound (OutboundPactChan c) = writeChan c

readOutbound :: OutboundPactChan -> IO [PactResult]
readOutbound (OutboundPactChan c) = readChan c
