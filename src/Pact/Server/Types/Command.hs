{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Server.Types.Command
--  (Command(..)
--  ) where
  where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Lens hiding ((.=))
import Control.Monad.Reader

import Data.Semigroup

import Data.Aeson
import Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize as SZ hiding (get)
import Data.String
import Data.Text
import Data.Text.Encoding

import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

import Pact.Pure
import Pact.Types hiding (PublicKey)
import Pact.Types.Orphans ()

import Pact.Server.Types.Base
import Pact.Server.Types.SQLite

data CommandMessage =
    PublicMessage {
      _cmMessage :: !ByteString
    } deriving (Eq,Ord,Show,Generic)
instance Serialize CommandMessage

instance ToJSON CommandMessage where
  toJSON (PublicMessage m) = toJSON $ decodeUtf8 m
instance FromJSON CommandMessage where
  parseJSON (A.String m) = return $ PublicMessage $ encodeUtf8 m
  parseJSON _ = mempty

data ExecMsg = ExecMsg {
      _pmCode :: Text
    , _pmData :: Value
    }
    deriving (Eq,Generic,Show)
instance FromJSON ExecMsg where
    parseJSON =
        withObject "PactMsg" $ \o ->
            ExecMsg <$> o .: "code" <*> o .: "data"
instance ToJSON ExecMsg where
    toJSON (ExecMsg c d) = object [ "code" .= c, "data" .= d]

data ContMsg = ContMsg {
      _cmTxId :: TxId
    , _cmStep :: Int
    , _cmRollback :: Bool
    }
    deriving (Eq,Show)
instance FromJSON ContMsg where
    parseJSON =
        withObject "ContMsg" $ \o ->
            ContMsg <$> o .: "txid" <*> o .: "step" <*> o .: "rollback"
instance ToJSON ContMsg where
    toJSON (ContMsg t s r) = object [ "txid" .= t, "step" .= s, "rollback" .= r]

data RPCDigest = RPCDigest {
      _rdPublicKey :: ByteString
    , _rdSignature :: ByteString
    } deriving (Eq)
instance FromJSON RPCDigest where
    parseJSON =
        withObject "RPCDigest" $ \o ->
            RPCDigest <$> fmap encodeUtf8 (o .: "key") <*> fmap encodeUtf8 (o .: "sig")

data Command = Command {
      _pmEnvelope :: ByteString
    , _pmKey :: PublicKey
    , _pmSig :: Signature
    , _pmHsh :: Hash
    } deriving (Eq,Generic)
instance Serialize Command
instance ToJSON Command where
    toJSON (Command payload pk (Sig s) hsh) =
        object [ "env" .= decodeUtf8 payload
               , "key" .= pk
               , "sig" .= toB16JSON s
               , "hsh" .= hsh
               ]
instance FromJSON Command where
    parseJSON = withObject "Command" $ \o ->
                Command <$> (encodeUtf8 <$> o .: "env")
                            <*> o .: "key"
                            <*> (o .: "sig" >>= \t -> Sig <$> parseB16JSON t)
                            <*> (o .: "hsh")

mkCommand :: ToJSON a => PrivateKey -> PublicKey -> Alias -> String -> a -> Command
mkCommand sk pk al rid a = mkCommand' sk pk $ BSL.toStrict $ A.encode (PactEnvelope a rid al)

mkCommand' :: PrivateKey -> PublicKey -> ByteString -> Command
mkCommand' sk pk env = Command env pk sig hsh
  where
    hsh = hash env
    sig = sign hsh sk pk

data PactEnvelope a = PactEnvelope {
      _pePayload :: a
    , _peRequestId :: String
    , _peAlias :: Alias
} deriving (Eq)
instance (ToJSON a) => ToJSON (PactEnvelope a) where
    toJSON (PactEnvelope r rid al) = object [ "payload" .= r,
                                              "rid" .= rid,
                                              "alias" .= al ]
instance (FromJSON a) => FromJSON (PactEnvelope a) where
    parseJSON = withObject "PactEnvelope" $ \o ->
                    PactEnvelope <$> o .: "payload"
                                     <*> o .: "rid"
                                     <*> o .: "alias"

data PactRPC =
    Exec ExecMsg |
    Continuation ContMsg
    deriving (Eq,Show)
instance FromJSON PactRPC where
    parseJSON =
        withObject "RPC" $ \o ->
            (Exec <$> o .: "exec") <|> (Continuation <$> o .: "yield")
instance ToJSON PactRPC where
    toJSON (Exec p) = object ["exec" .= p]
    toJSON (Continuation p) = object ["yield" .= p]

class ToRPC a where
    toRPC :: a -> PactRPC

instance ToRPC ExecMsg where toRPC = Exec
instance ToRPC ContMsg where toRPC = Continuation


data CommandConfig = CommandConfig {
      _ccDbFile :: Maybe FilePath
    , _ccDebugFn :: String -> IO ()
    }
$(makeLenses ''CommandConfig)

data CommandState = CommandState {
     _csRefStore :: RefStore
    }
$(makeLenses ''CommandState)


data ExecutionMode =
    Transactional { _emTxId :: TxId } |
    Local
    deriving (Eq,Show)
$(makeLenses ''ExecutionMode)


data DBVar = PureVar (MVar PureState) | PSLVar (MVar PSL)

data CommandEnv = CommandEnv {
      _ceConfig :: CommandConfig
    , _ceMode :: ExecutionMode
    , _ceDBVar :: DBVar
    , _ceState :: MVar CommandState
    }
$(makeLenses ''CommandEnv)

data CommandException = CommandException String
                        deriving (Typeable)
instance Show CommandException where show (CommandException e) = e
instance Exception CommandException

data CommandError = CommandError {
      _ceMsg :: String
    , _ceDetail :: Maybe String
}
instance ToJSON CommandError where
    toJSON (CommandError m d) =
        object $ [ "status" .= ("Failure" :: String)
                 , "msg" .= m ] ++
        maybe [] ((:[]) . ("detail" .=)) d

data CommandSuccess a = CommandSuccess {
      _csData :: a
    }
instance (ToJSON a) => ToJSON (CommandSuccess a) where
    toJSON (CommandSuccess a) =
        object [ "status" .= ("Success" :: String)
               , "result" .= a ]

type ApplyLocal = ByteString -> IO CommandResult

type CommandM a = ReaderT CommandEnv IO a

runCommand :: CommandEnv -> CommandM a -> IO a
runCommand e a = runReaderT a e


throwCmdEx :: MonadThrow m => String -> m a
throwCmdEx = throw . CommandException
