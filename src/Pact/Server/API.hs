{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Server.API
  ( ApiV1API
  , PactServerAPI
  , pactServerAPI
  ) where

import Data.Aeson
import Data.Proxy
import Servant.API
import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)

type ApiV1API =
  (    "send" :> ReqBody '[JSON] SubmitBatch :>
    Post '[JSON] RequestKeys
  :<|> "poll" :> ReqBody '[JSON] Poll :>
    Post '[JSON] PollResponses
  :<|> "listen" :> ReqBody '[JSON] ListenerRequest :>
    Post '[JSON] ApiResult
  :<|> "local" :> ReqBody '[JSON] (Command Text) :>
    Post '[JSON] (CommandSuccess Value)
  )

type PactServerAPI =
       "api" :> "v1" :> ApiV1API
  :<|> "verify" :> ReqBody '[JSON] Analyze.Request :> Post '[JSON] Analyze.Response
  :<|> "version" :> Get '[PlainText] Text

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy
