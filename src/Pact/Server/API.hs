{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}


module Pact.Server.API
  ( ApiV1API
  , apiV1API
  , PactServerAPI
  , pactServerAPI
  -- | endpoints
  , ApiSend
  , ApiPoll
  , ApiListen
  , ApiLocal
#ifdef BUILD_TOOL
  , ApiVerify
#endif
  , ApiVersion
#if !defined(ghcjs_HOST_OS)
  -- | client
  , sendClient
  , pollClient
  , listenClient
  , localClient
  , versionClient
#endif
#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
  , verifyClient
#endif
  ) where

import Data.Proxy
import Data.Text (Text)
import Servant.API
#if !defined(ghcjs_HOST_OS)
import Servant.Client
import Servant.Client.Core
#endif

#ifdef BUILD_TOOL
import qualified Pact.Analyze.Remote.Types as Analyze
#endif
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash

-- | Public Pact REST API.
type ApiV1API = "api" :> "v1" :> (ApiSend :<|> ApiPoll :<|> ApiListen :<|> ApiLocal)

apiV1API :: Proxy ApiV1API
apiV1API = Proxy

type ApiSend = "send"
  :> ReqBody '[JSON] SubmitBatch
  :> Post '[JSON] RequestKeys

type ApiPoll = "poll"
  :> ReqBody '[JSON] Poll
  :> Post '[JSON] PollResponses

type ApiListen = "listen"
  :> ReqBody '[JSON] ListenerRequest
  :> Post '[JSON] ListenResponse

type ApiLocal = "local"
  :> ReqBody '[JSON] (Command Text)
  :> Post '[JSON] (CommandResult Hash)

#ifdef BUILD_TOOL
type ApiVerify = "verify"
  :> ReqBody '[JSON] Analyze.Request
  :> Post '[JSON] Analyze.Response
#endif

type ApiVersion = "version"
  :> Get '[PlainText] Text

-- | "pact -s" REST API.
#ifdef BUILD_TOOL
type PactServerAPI = ApiV1API :<|> ApiVerify :<|> ApiVersion
#else
type PactServerAPI = ApiV1API :<|> ApiVersion
#endif

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy

-- | Need this because seems to be only way to reuse the "api/v1" prefix
data ApiV1Client m = ApiV1Client
  { v1Send :: SubmitBatch -> m RequestKeys
  , v1Poll :: Poll -> m PollResponses
  , v1Listen :: ListenerRequest -> m ListenResponse
  , v1Local :: Command Text -> m (CommandResult Hash)
  }


#if !defined(ghcjs_HOST_OS)
sendClient :: SubmitBatch -> ClientM RequestKeys
sendClient = v1Send apiV1Client

pollClient :: Poll -> ClientM PollResponses
pollClient = v1Poll apiV1Client

listenClient :: ListenerRequest -> ClientM ListenResponse
listenClient = v1Listen apiV1Client

localClient :: Command Text -> ClientM (CommandResult Hash)
localClient = v1Local apiV1Client
#endif

#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
verifyClient :: Analyze.Request -> ClientM Analyze.Response
verifyClient = client (Proxy @ApiVerify)
#endif

#if !defined(ghcjs_HOST_OS)
versionClient :: ClientM Text
versionClient = client (Proxy @ApiVersion)

apiV1Client :: forall m. RunClient m => ApiV1Client m
apiV1Client = ApiV1Client send poll listen local
  where
#endif
#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
    (send :<|> poll :<|> listen :<|> local) :<|> _verify :<|> _version =
#endif
#if !defined(ghcjs_HOST_OS) && !defined(BUILD_TOOL)
    (send :<|> poll :<|> listen :<|> local) :<|> _version =
#endif
#if !defined(ghcjs_HOST_OS)
      clientIn pactServerAPI (Proxy :: Proxy m)
#endif
