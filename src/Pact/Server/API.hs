{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Server.API
  ( PactServerAPI
  , pactServerAPI
  ) where

import Data.Aeson
import Data.Proxy
import Servant.API
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)

type PactServerAPI =
       "api" :> "v1" :>
      (    "send" :> ReqBody '[JSON] SubmitBatch :>
        Post '[JSON] (ApiResponse RequestKeys)
      :<|> "private" :> ReqBody '[JSON] SubmitBatch :>
        Post '[JSON] (ApiResponse RequestKeys)
      :<|> "poll" :> ReqBody '[JSON] Poll :>
        Post '[JSON] (ApiResponse PollResponses)
      :<|> "listen" :> ReqBody '[JSON] ListenerRequest :>
        Post '[JSON] (ApiResponse ApiResult)
      :<|> "local" :> ReqBody '[JSON] (Command Text) :>
        Post '[JSON] (ApiResponse (CommandSuccess Value))
      )
  :<|> "verify" :> ReqBody '[JSON] Value :> Post '[JSON] Value

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy
