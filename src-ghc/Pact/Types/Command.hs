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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Types.Command
-- Copyright   :  (C) 2016 Stuart Popejoy, Will Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Will Martino <will@kadena.io>
--
-- Specifies types for commands in a consensus/DL setting.
--

module Pact.Types.Command
  ( Command(..),mkCommand,mkCommand'
  , ProcessedCommand(..)
  , Address(..)
  , Payload(..)
  , ParsedCode(..)
  , UserSig(..),usScheme,usPubKey,usSig
  , verifyUserSig, verifyCommand
  , CommandError(..)
  , CommandSuccess(..)
  , CommandResult(..)
  , ExecutionMode(..), emTxId
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey, requestKeyToB16Text, initialRequestKey
  ) where


import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.DeepSeq
import Data.Aeson as A
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize as SZ
import Data.String
import Data.Text hiding (filter, all)
import Data.Hashable (Hashable)
import qualified Data.Set as S
import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Crypto.Hash as H


import GHC.Generics
import Prelude

import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Orphans ()
import Pact.Types.Crypto as Base
import Pact.Parse
import Pact.Types.RPC

data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !Hash
  } deriving (Eq,Show,Ord,Generic,Functor,Foldable,Traversable)
instance (Serialize a) => Serialize (Command a)
instance (ToJSON a) => ToJSON (Command a) where
    toJSON (Command payload uSigs hsh) =
        object [ "cmd" .= payload
               , "sigs" .= toJSON uSigs
               , "hash" .= hsh
               ]
instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                Command <$> (o .: "cmd")
                        <*> (o .: "sigs" >>= parseJSON)
                        <*> (o .: "hash")
    {-# INLINE parseJSON #-}

instance NFData a => NFData (Command a)

mkCommand :: (Scheme (SPPKScheme s), ToJSON a) => [KeyPair s] -> Maybe Address -> Text -> a -> Command ByteString
mkCommand creds addy nonce a = mkCommand' creds $ BSL.toStrict $ A.encode (Payload a nonce addy)

mkCommand' :: (Scheme (SPPKScheme s)) => [KeyPair s] -> ByteString -> Command ByteString
mkCommand' creds env = makeCommand (makeSigs <$> creds)
  where makeCommand sigs = Command env sigs hsh
        hsh = hashTx H.Blake2b_512 env    -- hash associated with a Command, aka a Command's Request Key
        makeSigs KeyPair{..} =
          let pubBS = toB16Text $ exportPublic _kpScheme _kpPublicKey
              sig = toB16Text $ exportSignature _kpScheme $ sign _kpScheme env _kpPublicKey _kpPrivateKey
          in (UserSig (toScheme _kpScheme) pubBS sig)

verifyCommand :: Command ByteString -> ProcessedCommand (PactRPC ParsedCode)
verifyCommand orig@Command{..} = case (ppcmdPayload', ppcmdHash', mSigIssue) of
      (Right env', Right _, Nothing) -> ProcSucc $ orig { _cmdPayload = env' }
      (e, h, s) -> ProcFail $ "Invalid command: " ++ toErrStr e ++ toErrStr h ++ fromMaybe "" s
  where
    ppcmdPayload' = traverse (traverse parsePact) =<< A.eitherDecodeStrict' _cmdPayload
    parsePact :: Text -> Either String ParsedCode
    parsePact code = ParsedCode code <$> parseExprs code
    (ppcmdSigs' :: [(UserSig,Bool)]) = (\u -> (u,verifyUserSig _cmdPayload u)) <$> _cmdSigs
    ppcmdHash' = verifyHashTx H.Blake2b_512 _cmdHash _cmdPayload
    mSigIssue = if all snd ppcmdSigs' then Nothing
      else Just $ "Invalid sig(s) found: " ++ show (A.encode . fst <$> filter (not.snd) ppcmdSigs')
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}

data ProcessedCommand a =
  ProcSucc !(Command (Payload a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance NFData a => NFData (ProcessedCommand a)

data ParsedCode = ParsedCode {
  _pcCode :: !Text,
  _pcExps :: ![Exp Parsed]
  } deriving (Eq,Show,Generic)
instance NFData ParsedCode

data Address = Address {
    _aFrom :: EntityName
  , _aTo :: S.Set EntityName
  } deriving (Eq,Show,Ord,Generic)
instance NFData Address
instance Serialize Address
instance ToJSON Address where toJSON = lensyToJSON 2
instance FromJSON Address where parseJSON = lensyParseJSON 2

data Payload a = Payload
  { _pPayload :: !a
  , _pNonce :: !Text
  , _pAddress :: !(Maybe Address)
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Payload a)
instance ToJSON a => ToJSON (Payload a) where toJSON = lensyToJSON 2
instance FromJSON a => FromJSON (Payload a) where parseJSON = lensyParseJSON 2



data UserSig = UserSig
  { _usScheme :: !PPKScheme
  , _usPubKey :: !Text
  , _usSig :: !Text }
  deriving (Eq, Ord, Show, Generic)
instance NFData UserSig


instance Serialize UserSig
instance ToJSON UserSig where
  toJSON UserSig {..} = object [
    "scheme" .= _usScheme, "pubKey" .= _usPubKey, "sig" .= _usSig ]
instance FromJSON UserSig where
  parseJSON = withObject "UserSig" $ \o ->
    UserSig <$> (o .: "scheme" >>= parseJSON)  <*> o .: "pubKey" <*> o .: "sig"
  {-# INLINE parseJSON #-}


verifyUserSig :: ByteString -> UserSig -> Bool
verifyUserSig msg UserSig{..} = 
  let (pubParsed, sigParsed, scheme) = case _usScheme of
        ED25519 -> (fromText _usPubKey :: A.Result (Ed25519.PublicKey),
                    fromText _usSig :: A.Result (Ed25519.Signature),
                    SED25519)
  in case (pubParsed, sigParsed) of
    (Success pub, Success sig) -> valid scheme msg pub sig
    _ -> False


data CommandError = CommandError {
      _ceMsg :: String
    , _ceDetail :: Maybe String
}
instance ToJSON CommandError where
    toJSON (CommandError m d) =
        object $ [ "status" .= ("failure" :: String)
                 , "error" .= m ] ++
        maybe [] ((:[]) . ("detail" .=)) d

newtype CommandSuccess a = CommandSuccess { _csData :: a }

instance (ToJSON a) => ToJSON (CommandSuccess a) where
    toJSON (CommandSuccess a) =
        object [ "status" .= ("success" :: String)
               , "data" .= a ]


data CommandResult = CommandResult {
  _crReqKey :: RequestKey,
  _crTxId :: Maybe TxId,
  _crResult :: Value
  } deriving (Eq,Show)


cmdToRequestKey :: Command a -> RequestKey
cmdToRequestKey Command {..} = RequestKey _cmdHash


data ExecutionMode =
    Transactional { _emTxId :: TxId } |
    Local
    deriving (Eq,Show)


type ApplyCmd = ExecutionMode -> Command ByteString -> IO CommandResult
type ApplyPPCmd a = ExecutionMode -> Command ByteString -> ProcessedCommand a -> IO CommandResult

data CommandExecInterface a = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd
  , _ceiApplyPPCmd :: ApplyPPCmd a
  }


requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey h) = hashToB16Text h


newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, Serialize, Hashable, ParseText, FromJSON, ToJSON)

instance Show RequestKey where
  show (RequestKey rk) = show rk

initialRequestKey :: RequestKey
initialRequestKey = RequestKey $ initialHashTx H.Blake2b_512



makeLenses ''UserSig
makeLenses ''CommandExecInterface
makeLenses ''ExecutionMode
