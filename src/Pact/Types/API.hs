{-# LANGUAGE CPP #-}
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

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.Utils as JL

newtype RequestKeys = RequestKeys { _rkRequestKeys :: NonEmpty RequestKey }
  deriving (Show, Eq, Ord, Generic, NFData)
makeLenses ''RequestKeys

#ifdef PACT_TOJSON
requestKeysProperties :: JsonProperties RequestKeys
requestKeysProperties o = [ "requestKeys" .= _rkRequestKeys o ]
{-# INLINE requestKeysProperties #-}

instance ToJSON RequestKeys where
  toJSON = enableToJSON "Pact.Types.API.RequestKeys" . lensyToJSON 3
  toEncoding = pairs . mconcat . requestKeysProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3

instance J.Encode RequestKeys where
  build o = J.object [ "requestKeys" J..= J.Array (_rkRequestKeys o) ]
  {-# INLINE build #-}

-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }
  deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch

#ifdef PACT_TOJSON
submitBatchProperties :: JsonProperties SubmitBatch
submitBatchProperties o = [ "cmds" .= _sbCmds o ]
{-# INLINE submitBatchProperties #-}

instance ToJSON SubmitBatch where
  toJSON = enableToJSON "Pact.Types.API.SubmitBatch" . object . submitBatchProperties
  toEncoding = pairs . mconcat . submitBatchProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance FromJSON SubmitBatch where
   parseJSON = lensyParseJSON 3

instance J.Encode SubmitBatch where
  build o = J.object [ "cmds" J..= J.Array (_sbCmds o) ]
  {-# INLINE build #-}

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestKeys :: NonEmpty RequestKey }
  deriving (Eq,Show,Generic)

#ifdef PACT_TOJSON
pollProperties :: JsonProperties Poll
pollProperties o = [ "requestKeys" .= _pRequestKeys o ]
{-# INLINE pollProperties #-}

instance ToJSON Poll where
  toJSON = enableToJSON "Pact.Types.API.Poll" . lensyToJSON 2
  toEncoding = pairs . mconcat . pollProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

instance J.Encode Poll where
  build o = J.object [ "requestKeys" J..= J.Array (_pRequestKeys o) ]
  {-# INLINE build #-}

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey (CommandResult Hash))
  deriving (Eq, Show, Generic)

#ifdef PACT_TOJSON
instance ToJSON PollResponses where
  toJSON (PollResponses m) = enableToJSON "Pact.Types.API.PollResponses" $ toJSON m
  toEncoding (PollResponses m) = toEncoding m
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance FromJSON PollResponses where
  parseJSON v = PollResponses <$> withObject "PollResponses" (\_ -> parseJSON v) v

instance J.Encode PollResponses where
  build (PollResponses m) = J.build $ JL.legacyHashMap asString m
  {-# INLINE build #-}

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest { _lrListen :: RequestKey }
  deriving (Eq,Show,Generic)

#ifdef PACT_TOJSON
listenerRequestProperties :: JsonProperties ListenerRequest
listenerRequestProperties o = [ "listen" .= _lrListen o ]
{-# INLINE listenerRequestProperties #-}

instance ToJSON ListenerRequest where
  toJSON = enableToJSON "Pact.Types.API.ListenerRequest" . object . listenerRequestProperties
  toEncoding = pairs . mconcat . listenerRequestProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance FromJSON ListenerRequest where
  parseJSON = withObject "ListenerRequest" $ \o -> ListenerRequest <$> o .: "listen"

instance J.Encode ListenerRequest where
  build o = J.object [ "listen" J..= _lrListen o ]
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- ListenResponse

data ListenResponse
  = ListenTimeout Int
  | ListenResponse !(CommandResult Hash)
  deriving (Eq,Show,Generic)

#ifdef PACT_TOJSON
listenResponseTimeoutProperties :: JsonProperties Int
listenResponseTimeoutProperties i =
  [ "status" .= ("timeout" :: String)
  , "timeout-micros" .= i
  ]
{-# INLINE listenResponseTimeoutProperties #-}

instance ToJSON ListenResponse where
  toJSON = enableToJSON "Pact.Types.API.ListenResponse" . \case
    (ListenResponse r) -> toJSON r
    (ListenTimeout i) -> object $ listenResponseTimeoutProperties i
  toEncoding (ListenResponse r) = toEncoding r
  toEncoding (ListenTimeout i) = pairs . mconcat $ listenResponseTimeoutProperties i
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

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

instance J.Encode ListenResponse where
  build (ListenResponse r) = J.build r
  build (ListenTimeout i) = J.object
    [ "status" J..= J.text "timeout"
    , "timeout-micros" J..= J.Aeson i
    ]
  {-# INLINE build #-}
