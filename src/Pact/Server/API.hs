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
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)

type ApiV1API =
  (    "send" :> ReqBody '[JSON] SubmitBatch :>
    Post '[JSON] (ApiResponse RequestKeys)
  :<|> "poll" :> ReqBody '[JSON] Poll :>
    Post '[JSON] (ApiResponse PollResponses)
  :<|> "listen" :> ReqBody '[JSON] ListenerRequest :>
    Post '[JSON] (ApiResponse ApiResult)
  :<|> "local" :> ReqBody '[JSON] (Command Text) :>
    Post '[JSON] (ApiResponse (CommandSuccess Value))
  )

type PactServerAPI =
       "api" :> "v1" :> ApiV1API
  :<|> "verify" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "version" :> Get '[PlainText] Text

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy
