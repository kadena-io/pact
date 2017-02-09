{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


-- |
-- Module      :  Pact.Types.API
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact REST API types.
--

module Pact.Types.API
  ( ApiResponse(..), apiResponse, apiError
  , RequestKeys(..), rkRequestKeys
  , SubmitBatch(..), sbCmds
  , SubmitBatchResponse
  , Poll(..)
  , PollResult(..), prRequestKey, prLatency, prResponse
  , PollResponse
  , ListenerRequest(..)
  , ListenerResponse
  ) where

import Data.Text (Text)
import Data.Aeson hiding (Success)
import qualified Data.Aeson as A
import Control.Lens hiding ((.=))
import GHC.Generics
import Data.Int

import Pact.Types.Command
import Pact.Types.Util

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
  toJSON = lensyToJSON 3
instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3


-- | Submit new commands for execution
data SubmitBatch = SubmitBatch
  { _sbCmds :: ![Command Text]
  } deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch
instance ToJSON SubmitBatch where
  toJSON = lensyToJSON 3
instance FromJSON SubmitBatch where
  parseJSON = lensyParseJSON 3

-- | What you get back from a SubmitBatch
type SubmitBatchResponse = ApiResponse RequestKeys

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestIds :: [RequestKey] }
  deriving (Eq,Show,Generic)
instance ToJSON Poll where
  toJSON = lensyToJSON 2
instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

-- | What you get back from a Poll
data PollResult = PollResult
  { _prRequestKey :: !RequestKey
  , _prLatency :: !Int64
  , _prResponse :: !Value
  } deriving (Eq,Show,Generic)
makeLenses ''PollResult
instance ToJSON PollResult where
  toJSON = lensyToJSON 3
instance FromJSON PollResult where
  parseJSON = lensyParseJSON 3

type PollResponse = ApiResponse [PollResult]

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest RequestKey
  deriving (Eq,Show,Generic)
instance ToJSON ListenerRequest where
  toJSON (ListenerRequest r) = object ["listen" .= r]
instance FromJSON ListenerRequest where
  parseJSON = withObject "ListenerRequest" $ \o -> ListenerRequest <$> o .: "listen"

type ListenerResponse = ApiResponse PollResult
