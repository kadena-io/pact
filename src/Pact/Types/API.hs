{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

import Pact.Types.Command
import Pact.Types.Runtime

newtype RequestKeys = RequestKeys { _rkRequestKeys :: NonEmpty RequestKey }
  deriving (Show, Eq, Ord, Generic)
makeLenses ''RequestKeys

instance ToJSON RequestKeys where
  toJSON = lensyToJSON 3
instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3


-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }
  deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch

instance ToJSON SubmitBatch where
  toJSON = lensyToJSON 3
instance FromJSON SubmitBatch where
   parseJSON = lensyParseJSON 3

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestKeys :: NonEmpty RequestKey }
  deriving (Eq,Show,Generic)

instance ToJSON Poll where
  toJSON = lensyToJSON 2
instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey (CommandResult Hash))
  deriving (Eq, Show, Generic)

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

data ListenResponse
  = ListenTimeout Int
  | ListenResponse (CommandResult Hash)
  deriving (Eq,Show,Generic)
instance ToJSON ListenResponse where
  toJSON (ListenResponse r) = toJSON r
  toJSON (ListenTimeout i) =
    object [ "status" .= ("timeout" :: String),
             "timeout-micros" .= i ]
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
