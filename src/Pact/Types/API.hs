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

instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3

instance J.Encode RequestKeys where
  build o = J.object [ "requestKeys" J..= J.Array (_rkRequestKeys o) ]
  {-# INLINABLE build #-}

-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }
  deriving (Eq,Generic,Show)
makeLenses ''SubmitBatch

instance FromJSON SubmitBatch where
   parseJSON = lensyParseJSON 3

instance J.Encode SubmitBatch where
  build o = J.object [ "cmds" J..= J.Array (_sbCmds o) ]
  {-# INLINABLE build #-}

-- | Poll for results by RequestKey
newtype Poll = Poll { _pRequestKeys :: NonEmpty RequestKey }
  deriving (Eq,Show,Generic)

instance FromJSON Poll where
  parseJSON = lensyParseJSON 2

instance J.Encode Poll where
  build o = J.object [ "requestKeys" J..= J.Array (_pRequestKeys o) ]
  {-# INLINABLE build #-}

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey (CommandResult Hash))
  deriving (Eq, Show, Generic)


instance FromJSON PollResponses where
  parseJSON v = PollResponses <$> withObject "PollResponses" (\_ -> parseJSON v) v

instance J.Encode PollResponses where
  build (PollResponses m) = J.build $ JL.legacyHashMap asString m
  {-# INLINABLE build #-}

-- | ListenerRequest for results by RequestKey
newtype ListenerRequest = ListenerRequest { _lrListen :: RequestKey }
  deriving (Eq,Show,Generic)

instance FromJSON ListenerRequest where
  parseJSON = withObject "ListenerRequest" $ \o -> ListenerRequest <$> o .: "listen"

instance J.Encode ListenerRequest where
  build o = J.object [ "listen" J..= _lrListen o ]
  {-# INLINABLE build #-}

-- -------------------------------------------------------------------------- --
-- ListenResponse

data ListenResponse
  = ListenTimeout Int
  | ListenResponse !(CommandResult Hash)
  deriving (Eq,Show,Generic)

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
  {-# INLINABLE build #-}
