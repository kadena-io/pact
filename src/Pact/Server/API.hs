{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


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
  , ApiVerify
  , ApiVersion
  -- | client
  , sendClient
  , pollClient
  , listenClient
  , localClient
  , verifyClient
  , versionClient
  -- | swagger
  , apiV1Swagger
  , pactServerSwagger
  , writeSwagger
  ) where

-- Swagger 2.2 compat
import Control.Lens (set)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Decimal (Decimal)
import Data.Proxy
import Data.Swagger as Swagger hiding (Info,version)
import Data.Text (Text)
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Time.Core (fromMicroseconds,fromGregorian,mkUTCTime)
import GHC.Generics
import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Swagger

import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Types.API
import Pact.Types.Codec (timeCodec, encoder)
import Pact.Types.Command
import Pact.Types.Exp (Literal)
import Pact.Types.Hash
import Pact.Types.Info (Info)
import Pact.Types.PactValue (PactValue)
import Pact.Types.Persistence (PactContinuation,PactExec,TxId)
import Pact.Types.Pretty (Doc)
import Pact.Types.Runtime (PactError,PactErrorType,FieldKey,StackFrame)
import Pact.Types.Swagger
import Pact.Types.Term
import Pact.Types.Util

-- | Public Pact REST API.
type ApiV1API = "api" :> "v1" :> (ApiSend :<|> ApiPoll :<|> ApiListen :<|> ApiLocal)

apiV1API :: Proxy ApiV1API
apiV1API = Proxy

type ApiSend = "send"
  :> ReqBody '[JSON] SubmitBatch
  :> Post '[JSON] RequestKeys

sendClient :: SubmitBatch -> ClientM RequestKeys
sendClient = v1Send apiV1Client

type ApiPoll = "poll"
  :> ReqBody '[JSON] Poll
  :> Post '[JSON] PollResponses

pollClient :: Poll -> ClientM PollResponses
pollClient = v1Poll apiV1Client

type ApiListen = "listen"
  :> ReqBody '[JSON] ListenerRequest
  :> Post '[JSON] ListenResponse

listenClient :: ListenerRequest -> ClientM ListenResponse
listenClient = v1Listen apiV1Client

type ApiLocal = "local"
  :> ReqBody '[JSON] (Command Text)
  :> Post '[JSON] (CommandResult Hash)

localClient :: Command Text -> ClientM (CommandResult Hash)
localClient = v1Local apiV1Client

type ApiVerify = "verify"
  :> ReqBody '[JSON] Analyze.Request
  :> Post '[JSON] Analyze.Response

verifyClient :: Analyze.Request -> ClientM Analyze.Response
verifyClient = client (Proxy @ ApiVerify)

type ApiVersion = "version"
  :> Get '[PlainText] Text

versionClient :: ClientM Text
versionClient = client (Proxy @ ApiVersion)

-- | "pact -s" REST API.
type PactServerAPI = ApiV1API :<|> ApiVerify :<|> ApiVersion

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy

-- | Need this because seems to be only way to reuse the "api/v1" prefix
data ApiV1Client m = ApiV1Client
  { v1Send :: SubmitBatch -> m RequestKeys
  , v1Poll :: Poll -> m PollResponses
  , v1Listen :: ListenerRequest -> m ListenResponse
  , v1Local :: Command Text -> m (CommandResult Hash)
  }

apiV1Client :: forall m. RunClient m => ApiV1Client m
apiV1Client = ApiV1Client send poll listen local
  where
    (send :<|> poll :<|> listen :<|> local) :<|> _verify :<|> _version =
      clientIn pactServerAPI (Proxy :: Proxy m)


-- | Public Pact REST API Swagger
apiV1Swagger :: Swagger
apiV1Swagger = toSwagger (Proxy :: Proxy ApiV1API)

-- | Full `pact -s` REST API Swagger
pactServerSwagger :: Swagger
pactServerSwagger = toSwagger pactServerAPI

writeSwagger :: FilePath -> Swagger -> IO ()
writeSwagger fn = BSL8.writeFile fn . encode



-- ORPHANS for swagger

instance ToSchema (Command Text) where
  declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions
    { Swagger.fieldLabelModifier = go }
    where go n = case n of
            "_cmdPayload" -> "cmd"
            _ -> lensyConstructorToNiceJson 4 n


instance ToSchema (CommandResult Hash)

instance ToSchema SubmitBatch where
  declareNamedSchema = declareLensyNamedSchema 3

instance ToSchema RequestKeys

instance ToSchema Poll

instance ToSchema PollResponses

instance ToSchema ListenerRequest

instance ToSchema ListenResponse

instance ToSchema Analyze.Request

instance ToSchema Analyze.Response

pactHashSchema :: Schema
pactHashSchema = withSchema byteBase64url $ fixedLength pactHashLength

instance ToSchema RequestKey where
  declareNamedSchema = declareGenericSchema pactHashSchema

instance ToSchema Hash where
  declareNamedSchema = declareGenericSchema pactHashSchema

instance ToSchema (TypedHash 'Blake2b_256) where
  declareNamedSchema = declareGenericSchema pactHashSchema

instance ToSchema PublicKey where
  declareNamedSchema = declareGenericString

instance ToSchema UserSig where
  declareNamedSchema = declareLensyNamedSchema 3

instance ToSchema TxId

instance ToSchema Gas

instance ToSchema Doc where
  declareNamedSchema = declareGenericString

instance ToSchema Info where
  declareNamedSchema = declareGenericString


instance ToSchema (ModuleDef t) where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)

instance ToSchema PactErrorType

instance ToSchema PactError

instance ToSchema StackFrame where
  declareNamedSchema = declareGenericString

instance ToSchema PactExec

instance ToSchema PactContinuation

instance ToSchema Value where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)

instance ToSchema PactResult

instance ToSchema Guard
instance ToSchema PactGuard
instance ToSchema KeySet
instance ToSchema KeySetName
instance ToSchema PactId
instance ToSchema Name
instance ToSchema ModuleName
instance ToSchema NamespaceName
instance ToSchema ModuleGuard
instance ToSchema UserGuard where
  declareNamedSchema = declareGenericString

instance ToSchema PactValue

-- | Adapted from 'Map k v' as a naive instance will cause an infinite loop!!
-- 2.2 swagger2 compat means not using 'additionalProperties' for now
instance ToSchema (ObjectMap PactValue) where
  declareNamedSchema _ = do
    -- Swagger 2.2 compat, not doing schema ref for pact value
    sref <- declareSchemaRef (Proxy :: Proxy PactValue)
    return $ NamedSchema (Just "ObjectMap") $
      (schemaOf $ swaggerType SwaggerObject .
        set additionalProperties (Just $ AdditionalPropertiesSchema sref))

instance ToSchema FieldKey

newtype DummyTime = DummyTime UTCTime deriving (Generic)
instance ToJSON DummyTime where toJSON (DummyTime t) = encoder timeCodec t

instance ToSchema UTCTime where
  declareNamedSchema = namedSchema "UTCTime" $ sketchSchema
    (DummyTime $ mkUTCTime (fromGregorian 1970 01 01) $ fromMicroseconds 0)

instance ToSchema Literal

instance ToSchema Decimal where
  declareNamedSchema _ = return $ NamedSchema (Just "Decimal")
    (schemaOf $ swaggerType SwaggerNumber)
