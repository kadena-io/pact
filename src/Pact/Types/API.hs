{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Pact.Types.API
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact REST API types.
--

module Pact.Types.API
  ( RequestKeys(..), rkRequestKeys
  , SubmitBatch(..), sbCmds
  , Poll(..)
  , PollResponses(..)
  , ListenerRequest(..)
  , ListenResponse(..)
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)

import GHC.Generics

import Pact.Types.Command
import Pact.Types.Runtime

newtype RequestKeys = RequestKeys { _rkRequestKeys :: NonEmpty RequestKey }
  deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''RequestKeys

requestKeysProperties :: JsonProperties RequestKeys
requestKeysProperties o = [ "requestKeys" .= _rkRequestKeys o ]
{-# INLINE requestKeysProperties #-}

instance ToJSON RequestKeys where
  toJSON = enableToJSON "RequestKeys" . lensyToJSON 3
  toEncoding = pairs . mconcat . requestKeysProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3

-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }
  deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch

submitBatchProperties :: JsonProperties SubmitBatch
submitBatchProperties o = [ "cmds" .= _sbCmds o ]
{-# INLINE submitBatchProperties #-}

instance ToJSON SubmitBatch where
  toJSON = enableToJSON "SubmitBatch" . object . submitBatchProperties
  toEncoding = pairs . mconcat . submitBatchProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON SubmitBatch where
   parseJSON = lensyParseJSON 3

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestKeys :: NonEmpty RequestKey }
  deriving (Eq,Show,Generic)

pollProperties :: JsonProperties Poll
pollProperties o = [ "requestKeys" .= _pRequestKeys o ]
{-# INLINE pollProperties #-}

instance ToJSON Poll where
  toJSON = enableToJSON "Poll" . lensyToJSON 2
  toEncoding = pairs . mconcat . pollProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey (CommandResult Hash))
  deriving (Eq, Show, Generic)

instance ToJSON PollResponses where
  toJSON (PollResponses m) = enableToJSON "PollResponses" $ toJSON m
  toEncoding (PollResponses m) = toEncoding m
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON PollResponses where
  parseJSON v = PollResponses <$> withObject "PollResponses" (\_ -> parseJSON v) v

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest { _lrListen :: RequestKey }
  deriving (Eq,Show,Generic)

listenerRequestProperties :: JsonProperties ListenerRequest
listenerRequestProperties o = [ "listen" .= _lrListen o ]
{-# INLINE listenerRequestProperties #-}

instance ToJSON ListenerRequest where
  toJSON = enableToJSON "ListenerRequest" . object . listenerRequestProperties
  toEncoding = pairs . mconcat . listenerRequestProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ListenerRequest where
  parseJSON = withObject "ListenerRequest" $ \o -> ListenerRequest <$> o .: "listen"

-- -------------------------------------------------------------------------- --
-- ListenResponse

data ListenResponse
  = ListenTimeout Int
  | ListenResponse (CommandResult Hash)
  deriving (Eq,Show,Generic)

listenResponseTimeoutProperties :: JsonProperties Int
listenResponseTimeoutProperties i =
  [ "status" .= ("timeout" :: String)
  , "timeout-micros" .= i
  ]
{-# INLINE listenResponseTimeoutProperties #-}

instance ToJSON ListenResponse where
  toJSON = enableToJSON "ListenResponse" . \case
    (ListenResponse r) -> toJSON r
    (ListenTimeout i) -> object $ listenResponseTimeoutProperties i
  toEncoding (ListenResponse r) = toEncoding r
  toEncoding (ListenTimeout i) = pairs . mconcat $ listenResponseTimeoutProperties i
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ListenResponse where
  parseJSON v =
    (ListenResponse <$> parseJSON v) <|>
    (ListenTimeout <$> parseTimeout v)
    where
      parseTimeout = withObject "ListenTimeout" $ \o -> do
        (s :: Text) <- o .: "status"
        case s of
          "timeout" -> o .: "timeout-micros"
          _ -> fail "Expected timeout status"
