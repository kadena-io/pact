{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Server.Client
  ( PactServerAPI
  , PactServerAPIClient(..)
  , pactServerAPI
  , pactServerApiClient
  ) where

import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client.Core
import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)

import Pact.Server.API

data PactServerAPIClient m = PactServerAPIClient
  { send :: SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ApiResult
  , local :: Command Text -> m (CommandSuccess Value)
  , verify :: Analyze.Request -> m Analyze.Response
  , version :: m Text
  }

pactServerApiClient :: forall m. RunClient m => PactServerAPIClient m
pactServerApiClient = let
  (send :<|> poll :<|> listen :<|> local) :<|> verify :<|> version =
    clientIn pactServerAPI (Proxy :: Proxy m)
  in PactServerAPIClient{ send, poll, listen, local, verify, version }

