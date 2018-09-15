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
  , PollResponses(..)
  , ListenerRequest(..)
  , ListenerResponse
  , ApiResult(..), arMetaData, arResult, arTxId
  ) where

import Data.Aeson hiding (Success)
import qualified Data.Aeson as A
import Control.Lens hiding ((.=))
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Control.Arrow
import Control.Monad

import Pact.Types.Command
import Pact.Types.Util
import Pact.Types.Runtime

data ApiResponse a =
  ApiSuccess
    { _apiResponse :: !a} |
  ApiFailure
    { _apiError :: !String}
  deriving (Show, Eq, Generic)
makeLenses ''ApiResponse

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON (ApiSuccess a)= object [ "status" .= A.String "success", "response" .= a]
  toJSON (ApiFailure a)= object [ "status" .= A.String "failure", "error" .= a]
instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON (Object o) = do
    st <- o .: "status"
    if st == A.String "success"
    then ApiSuccess <$> o .: "response"
    else ApiFailure <$> o .: "error"
  parseJSON _ = mempty

newtype RequestKeys = RequestKeys { _rkRequestKeys :: [RequestKey] } deriving (Show, Eq, Ord, Generic)
makeLenses ''RequestKeys

instance ToJSON RequestKeys where
  toJSON = lensyToJSON 3
instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3


-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: [Command Text] } deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch

instance ToJSON SubmitBatch where
  toJSON = lensyToJSON 3
instance FromJSON SubmitBatch where
   parseJSON = lensyParseJSON 3

-- | What you get back from a SubmitBatch
type SubmitBatchResponse = ApiResponse RequestKeys

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestKeys :: [RequestKey] }
  deriving (Eq,Show,Generic)
instance ToJSON Poll where
  toJSON = lensyToJSON 2
instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

data ApiResult = ApiResult {
  _arResult :: !Value,
  _arTxId :: !(Maybe TxId),
  _arMetaData :: !(Maybe Value)
  } deriving (Eq,Show,Generic)
makeLenses ''ApiResult
instance FromJSON ApiResult where parseJSON = lensyParseJSON 3
instance ToJSON ApiResult where toJSON = lensyToJSON 3

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey ApiResult)
instance ToJSON PollResponses where
  toJSON (PollResponses m) = object $ map (requestKeyToB16Text *** toJSON) $ HM.toList m
instance FromJSON PollResponses where
  parseJSON = withObject "PollResponses" $ \o ->
    (PollResponses . HM.fromList <$> forM (HM.toList o)
      (\(k,v) -> (,) <$> parseJSON (String k) <*> parseJSON v))

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest RequestKey
  deriving (Eq,Show,Generic)
instance ToJSON ListenerRequest where
  toJSON (ListenerRequest r) = object ["listen" .= r]
instance FromJSON ListenerRequest where
  parseJSON = withObject "ListenerRequest" $ \o -> ListenerRequest <$> o .: "listen"

type ListenerResponse = ApiResponse ApiResult
