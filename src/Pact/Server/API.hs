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
import Data.Aeson hiding (Object)
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
import Pact.Types.ChainId
import Pact.Types.Codec (timeCodec, encoder)
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Exp (Literal)
import Pact.Types.Hash
import Pact.Types.Info (Info)
import Pact.Types.PactValue (PactValue)
import Pact.Types.Persistence (TxId)
import Pact.Types.Pretty (Doc)
import Pact.Types.Runtime (PactError,PactErrorType,StackFrame)
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


------------------------------------------------------------------
------------------------------------------------------------------


-- | Swagger ORPHANS

instance ToSchema SubmitBatch where
  declareNamedSchema = lensyDeclareNamedSchema 3

instance (ToSchema a) => ToSchema (Command a) where
  declareNamedSchema =
    (swaggerDescription "transaction command with stringified JSON payload (cmd)") .
    (genericDeclareNamedSchema $
      optionsOf $ optionFieldLabel transform)
    where transform n = case n of
            "_cmdPayload" -> "cmd"
            _ -> lensyConstructorToNiceJson 4 n

instance ToSchema UserSig where
  declareNamedSchema =
    (swaggerDescription "crypto signature by secret key of command payload") .
    (lensyDeclareNamedSchema 3)

pactHashSchema :: Schema
pactHashSchema = withSchema byteBase64url $ fixedLength pactHashLength

instance ToSchema (TypedHash 'Blake2b_256) where
  declareNamedSchema = (swaggerDescription "blake2 hash in base64 of command payload") .
    (declareGenericSchema pactHashSchema)



instance ToSchema RequestKeys where
  declareNamedSchema = lensyDeclareNamedSchema 3

instance ToSchema RequestKey where
  declareNamedSchema = (swaggerDescription "command's request key (i.e. the hash of command payload)") .
    (declareGenericSchema pactHashSchema)



instance ToSchema Poll where
  declareNamedSchema = lensyDeclareNamedSchema 2


-- Bug fix: Created data type PollResponse with field names to use
-- in proxy type for PollResponses and thus prevent swagger2
-- from creating a nested array of different types (which is invalid swagger code) by default
data PollResponse = PollResponse
  { _sprRequestKey :: RequestKey
  , _sprResponse :: (CommandResult Hash)
  } deriving Generic
instance ToSchema PollResponse where
  declareNamedSchema = lensyDeclareNamedSchema 4
instance ToSchema PollResponses where
  declareNamedSchema _ = swaggerDescription "request key to command result map" $
    declareNamedSchema (Proxy :: Proxy [PollResponse])


instance (ToSchema a) => ToSchema (CommandResult a) where
  declareNamedSchema = (swaggerDescription "result of attempting to execute a pact command") .
    (lensyDeclareNamedSchema 3)

instance ToSchema TxId where
  declareNamedSchema = (swaggerDescription "command's transaction id") .
    (genericDeclareNamedSchema defaultSchemaOptions)
instance ToSchema Gas where
  declareNamedSchema = (swaggerDescription "gas consummed by command") .
    (genericDeclareNamedSchema defaultSchemaOptions)
instance ToSchema Hash where
  declareNamedSchema = (swaggerDescription "the hash of the pact execution's logs") .
    (declareGenericSchema pactHashSchema)
instance ToSchema PactExec where
  declareNamedSchema = (swaggerDescription "output of a Continuation if one occurred in the command.") .
    (lensyDeclareNamedSchema 3)
instance ToSchema PactId
instance ToSchema PactContinuation where
   declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema Name where
  declareNamedSchema = declareGenericString
instance ToSchema Value where
  declareNamedSchema = (swaggerDescription "Platform-specific data") .
    (declareGenericSchema $
      (schemaOf $ swaggerType SwaggerObject))


-- Source: https://github.com/haskell-servant/servant-swagger/issues/80
data PactResultStatus = Success | Failure
  deriving (Generic)
instance ToSchema PactResultStatus
instance ToSchema PactResult where
    declareNamedSchema _ = do
      pactErrRef <- declareSchemaRef (Proxy :: Proxy PactError)
      pactValRef <- declareSchemaRef (Proxy :: Proxy PactValue)
      statusRef <- declareSchemaRef (Proxy :: Proxy PactResultStatus)
      let p = [ ("status",statusRef), ("error", pactErrRef),("data" , pactValRef) ]
          ns = namedSchema "PactResult"
               (schemaOf $ swaggerType SwaggerObject .
                           swaggerProperties p .
                           set minProperties (Just 2) .
                           set maxProperties (Just 2)
               )
      swaggerDescription "either a pact error or the last pact expression output as a pact value" ns

instance ToSchema PactError where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionFieldLabel transform
    where transform n = case n of
            "peDoc" -> "message"
            _ -> lensyConstructorToNiceJson 2 n
instance ToSchema PactErrorType
instance ToSchema Info where
  declareNamedSchema = declareGenericString
instance ToSchema StackFrame where
  declareNamedSchema = declareGenericString
instance ToSchema Doc where
  declareNamedSchema = declareGenericString

instance ToSchema ChainId where
  declareNamedSchema = declareGenericString
instance ToSchema ModuleHash where
  declareNamedSchema = declareGenericSchema pactHashSchema
instance ToSchema Provenance where
   declareNamedSchema = lensyDeclareNamedSchema 2
instance ToSchema Yield where
   declareNamedSchema = lensyDeclareNamedSchema 2

instance ToSchema PactValue where
  declareNamedSchema = (swaggerDescription "data from Pact execution represented as JSON") .
    (genericDeclareNamedSchema $
      optionsOf $ optionConstructor $ toNiceString 1)


instance ToSchema Literal where
  declareNamedSchema = genericDeclareNamedSchema $
    (optionsOf $ optionConstructor $ toNiceString 1)
      { Swagger.unwrapUnaryRecords = True }

newtype DummyTime = DummyTime UTCTime
  deriving (Generic)
instance ToJSON DummyTime where
  toJSON (DummyTime t) = encoder timeCodec t
instance ToSchema UTCTime where
  declareNamedSchema _ = namedSchema "UTCTime" $ sketchSchema $
    DummyTime $ mkUTCTime
                (fromGregorian 1970 01 01)
                (fromMicroseconds 0)
instance ToSchema Decimal where
  declareNamedSchema _ = return $
    NamedSchema (Just "Decimal")
                (schemaOf $ swaggerType SwaggerNumber)


-- Adapted from 'Map k v' as a naive instance will cause an infinite loop!!
-- 2.2 swagger2 compat means not using 'additionalProperties' for now
instance ToSchema (ObjectMap PactValue) where
  declareNamedSchema _ = do
    -- Swagger 2.2 compat, not doing schema ref for pact value
    sref <- declareSchemaRef (Proxy :: Proxy PactValue)
    namedSchema "ObjectMap"
      (schemaOf $
        swaggerType SwaggerObject .
        set additionalProperties (Just $ AdditionalPropertiesSchema sref)
      )

instance ToSchema Guard where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionConstructor $ toNiceString 1

instance ToSchema PactGuard where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema KeySet where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema PublicKey where
  declareNamedSchema = declareGenericString
instance ToSchema KeySetName
instance ToSchema ModuleGuard where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema ModuleName where
   declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema NamespaceName
instance ToSchema (Object a) where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)
instance ToSchema UserGuard where
  declareNamedSchema = lensyDeclareNamedSchema 3


instance ToSchema ListenerRequest where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema ListenResponse where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionConstructor transform
    where transform n = case n of
            "ListenTimeout" -> "timeout-micros"
            _ -> toNiceString 6 n


instance ToSchema Analyze.Request where
  declareNamedSchema _ = do
    modulesRef <- declareSchemaRef (Proxy :: Proxy [ModuleDef Name])
    verifyRef <- declareSchemaRef (Proxy :: Proxy ModuleName)
    let reqSchema = schemaOf $
                    swaggerType SwaggerObject .
                    swaggerProperties [("modules",modulesRef), ("verify",verifyRef)]
    namedSchema "AnalyzeRequest" reqSchema

instance ToSchema Analyze.Response where
  declareNamedSchema _ = namedSchema "AnalyzeResponse" $ sketchSchema $
    Analyze.Response ["Dummy Response"]
instance ToSchema (ModuleDef t) where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)
