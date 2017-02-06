{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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

import Data.Aeson
import Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize as SZ hiding (get)
import Data.String
import Data.Text hiding (filter, null, all)

import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

import Pact.Pure
import Pact.Types hiding (PublicKey)
import Pact.Types.Orphans ()

import Pact.Server.Types.Base
import Pact.Server.Types.SQLite

data Command a = PublicCommand
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !Hash
  } deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (Serialize a) => Serialize (Command a)
instance (ToJSON a) => ToJSON (Command a) where
    toJSON (PublicCommand payload uSigs hsh) =
        object [ "cmd" .= payload
               , "sigs" .= toJSON uSigs
               , "hash" .= hsh
               ]
instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                PublicCommand <$> (o .: "cmd")
                              <*> (o .: "sigs" >>= parseJSON)
                              <*> (o .: "hash")

mkCommand :: [(UserName, PPKScheme, PrivateKey, PublicKey)] -> RequestId -> PactRPC -> Command ByteString
mkCommand creds rid a = mkCommand' creds $ BSL.toStrict $ A.encode (Payload a rid)

mkCommand' :: [(UserName, PPKScheme, PrivateKey, PublicKey)] -> ByteString -> Command ByteString
mkCommand' creds env = PublicCommand env (sig <$> creds) hsh
  where
    hsh = hash env
    sig (u, scheme, sk, pk) = UserSig
                                { _usUserName = u
                                , _usScheme = scheme
                                , _usPubKey = pk
                                , _usSig = sign hsh sk pk}

data ProcessedCommand =
  ProcFail !String |
  ProcSucc !(Command Payload)
  deriving (Show, Eq, Generic)

verifyCommand :: Command ByteString -> ProcessedCommand
verifyCommand orig@PublicCommand{..} = case (ppcmdPayload', ppcmdHash', mSigIssue) of
      (Right env', Right _, Nothing) -> ProcSucc $! orig { _cmdPayload = env' }
      (e, h, s) -> ProcFail $! "Invalid command: " ++ (toErrStr e) ++ (toErrStr h) ++ (maybe "" id s)
  where
    !ppcmdPayload' = A.eitherDecodeStrict' _cmdPayload
    ppcmdSigs' :: [(UserSig, Bool)]
    !ppcmdSigs' = (\u -> (u,verifyUserSig _cmdHash u)) <$> _cmdSigs
    !ppcmdHash' = verifyHash _cmdHash _cmdPayload
    mSigIssue = if all snd ppcmdSigs' then Nothing else Just $ "Invalid sig(s) found: " ++ show (fst <$> filter (not.snd) ppcmdSigs')
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}

data Payload = Payload
  { _pPayload :: !PactRPC
  , _pRequestId :: !RequestId
  } deriving (Show, Eq, Generic)
instance ToJSON Payload where
  toJSON (Payload r rid) = object [ "payload" .= r, "rid" .= rid]
instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o ->
                    Payload <$> o .: "payload" <*> o .: "rid"

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

data ExecMsg = ExecMsg
  { _pmCode :: Text
  , _pmData :: Value
  } deriving (Eq,Generic,Show)
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

data CommandConfig = CommandConfig {
      _ccDbFile :: Maybe FilePath
    , _ccDebugFn :: String -> IO ()
    , _ccEntity :: String
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

data CommandException = CommandException String deriving (Typeable)
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


data PactResult = PactResult {
  _prReqKey :: RequestKey,
  _prResult :: Value
  } deriving (Eq,Show)

type ApplyCmd = ExecutionMode -> Command ByteString -> IO PactResult
type ApplyPPCmd = ExecutionMode -> Command ByteString -> ProcessedCommand -> IO PactResult

data CommandExecInterface = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd
  , _ceiApplyPPCmd :: ApplyPPCmd
  }
makeLenses ''CommandExecInterface

type CommandM a = ReaderT CommandEnv IO a

runCommand :: CommandEnv -> CommandM a -> IO a
runCommand e a = runReaderT a e

throwCmdEx :: MonadThrow m => String -> m a
throwCmdEx = throw . CommandException
