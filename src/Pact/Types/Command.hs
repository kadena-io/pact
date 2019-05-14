{-# LANGUAGE CPP #-}
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
  ( Command(..),cmdPayload,cmdSigs,cmdHash
#if !defined(ghcjs_HOST_OS)
  , mkCommand, keyPairsToSigners, mkCommand', verifyUserSig, verifyCommand
#else
  , PPKScheme(..)
#endif
  , ProcessedCommand(..),_ProcSucc,_ProcFail
  , Payload(..),pMeta,pNonce,pPayload,pSigners
  , ParsedCode(..),pcCode,pcExps
  , Signer(..),siScheme, siPubKey, siAddress
  , UserSig(..),usSig
  , CommandError(..),ceData
  , CommandSuccess(..),csData
  , CommandResponse(..)
  , CommandResult(..),crReqKey,crTxId,crResult,crGas
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey, requestKeyToB16Text
  ) where


import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Serialize as SZ
import Data.String
import Data.Hashable (Hashable)
import Data.Aeson as A
import Data.Text (Text)
import Data.Maybe  (fromMaybe)


import GHC.Generics
import Prelude

import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Orphans ()
import Pact.Types.RPC
import Pact.Types.PactValue (PactValue(..))


#if !defined(ghcjs_HOST_OS)
import qualified Data.ByteString.Lazy as BSL

import Pact.Parse (parseExprs)
import Pact.Types.Crypto              as Base
#else
import Pact.Types.Scheme (PPKScheme(..), defPPKScheme)
#endif


-- | Command is the signed, hashed envelope of a Pact execution instruction or command.
-- In 'Command ByteString', the 'ByteString' payload is hashed and signed; the ByteString
-- being the JSON serialization of 'Payload Text', where the 'Text' is the pact code; when
-- executed this is parsed to 'ParsedCode'.
-- Thus, 'Command (Payload m ParsedCode)' (with m representing platform-specific metadata)
-- is the fully executable specialization.
data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !PactHash
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


-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand m a =
  ProcSucc !(Command (Payload m a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (ProcessedCommand m a)


#if !defined(ghcjs_HOST_OS)


-- CREATING AND SIGNING TRANSACTIONS

mkCommand :: (ToJSON m, ToJSON c) =>
             [SomeKeyPair] ->
             m ->
             Text ->
             PactRPC c ->
             IO (Command ByteString)
mkCommand creds meta nonce rpc = mkCommand' creds encodedPayload
  where encodedPayload = BSL.toStrict $ A.encode payload
        payload = Payload rpc nonce meta $ keyPairsToSigners creds

keyPairsToSigners :: [SomeKeyPair] -> [Signer]
keyPairsToSigners creds = map toSigner creds
  where toSigner cred = Signer
                        (kpToPPKScheme cred)
                        (toB16Text $ getPublic cred)
                        (toB16Text $ formatPublicKey cred)


mkCommand' :: [SomeKeyPair] -> ByteString -> IO (Command ByteString)
mkCommand' creds env = do
  let hsh = hash env    -- hash associated with a Command, aka a Command's Request Key
      toUserSig cred = UserSig . toB16Text <$>
                       sign cred (toUntypedHash hsh)
  sigs <- traverse toUserSig creds
  return $ Command env sigs hsh


-- VALIDATING TRANSACTIONS

verifyCommand :: FromJSON m => Command ByteString -> ProcessedCommand m ParsedCode
verifyCommand orig@Command{..} =
  case parsedPayload of
    Right env' -> case verifiedHash of
      Right _ -> case (hasInvalidSigs _cmdHash _cmdSigs $ _pSigners env') of
        Nothing -> toProcSucc env'
        Just sigErr -> toProcFail sigErr
      Left hshErr -> toProcFail hshErr
    Left payloadErr -> toProcFail payloadErr
  where
    toProcSucc payload = ProcSucc $ orig { _cmdPayload = payload }
    toProcFail errStr = ProcFail $ "Invalid command: " ++ errStr

    parsePact :: Text -> Either String ParsedCode
    parsePact code = ParsedCode code <$> parseExprs code
    parsedPayload = traverse parsePact
                    =<< A.eitherDecodeStrict' _cmdPayload

    verifiedHash = verifyHash _cmdHash _cmdPayload
{-# INLINE verifyCommand #-}


hasInvalidSigs :: PactHash -> [UserSig] -> [Signer] -> Maybe String
hasInvalidSigs hsh sigs signers
  | not (length sigs == length signers)  = Just "Number of sig(s) does not match number of signer(s)"
  | otherwise                            = if (length failedSigs == 0)
                                           then Nothing else formatIssues
  where verifyFailed (sig, signer) = not $ verifyUserSig hsh sig signer
        -- assumes nth Signer is responsible for the nth UserSig
        failedSigs = filter verifyFailed (zip sigs signers)
        formatIssues = Just $ "Invalid sig(s) found: " ++ show (A.encode <$> failedSigs)


verifyUserSig :: PactHash -> UserSig -> Signer -> Bool
verifyUserSig msg UserSig{..} Signer{..} =
  case (pubT, sigT, addrT) of
    (Right p, Right sig, Right addr) ->
      (isValidAddr addr p) && verify (toScheme _siScheme) (toUntypedHash msg) (PubBS p) (SigBS sig)
    _ -> False
  where pubT = parseB16TextOnly _siPubKey
        sigT = parseB16TextOnly _usSig
        addrT = parseB16TextOnly _siAddress
        isValidAddr givenAddr pubBS =
          case formatPublicKeyBS (toScheme _siScheme) (PubBS pubBS) of
            Right expectAddr -> givenAddr == expectAddr
            Left _           -> False

#endif


-- | Pair parsed Pact expressions with the original text.
data ParsedCode = ParsedCode
  { _pcCode :: !Text
  , _pcExps :: ![Exp Parsed]
  } deriving (Eq,Show,Generic)
instance NFData ParsedCode



-- | Signer combines PPKScheme, PublicKey, and the Address (aka the
--   formatted PublicKey).
data Signer = Signer
 { _siScheme :: !PPKScheme
 , _siPubKey :: !Text
 , _siAddress :: !Text
 } deriving (Eq, Ord, Show, Generic)

instance NFData Signer
instance Serialize Signer
instance ToJSON Signer where
  toJSON Signer{..} = object [
    "scheme" .= _siScheme,
    "pubKey" .= _siPubKey,
    "addr" .= _siAddress]
instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> do
    pub <- o .: "pubKey"
    scheme <- o .:? "scheme"   -- defaults to PPKScheme default
    addr <- o .:? "addr"       -- defaults to full Public Key

    return $ Signer
             (fromMaybe defPPKScheme scheme)
             pub
             (fromMaybe pub addr)



-- | Payload combines a 'PactRPC' with a nonce and platform-specific metadata.
data Payload m c = Payload
  { _pPayload :: !(PactRPC c)
  , _pNonce :: !Text
  , _pMeta :: !m
  , _pSigners :: ![Signer]
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (Payload m a)
instance (ToJSON a,ToJSON m) => ToJSON (Payload m a) where toJSON = lensyToJSON 2
instance (FromJSON a,FromJSON m) => FromJSON (Payload m a) where parseJSON = lensyParseJSON 2



newtype UserSig = UserSig { _usSig :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData UserSig
instance Serialize UserSig
instance ToJSON UserSig where
  toJSON UserSig {..} = object [ "sig" .= _usSig ]
instance FromJSON UserSig where
  parseJSON = withObject "UserSig" $ \o -> do
    UserSig <$> o .: "sig"


-- Linda TODO
newtype CommandError = CommandError { _ceData :: PactError }
instance ToJSON CommandError where
    toJSON (CommandError m) =
        object $ [ "status" .= ("failure" :: String)
                 , "data" .= m ]

newtype CommandSuccess = CommandSuccess { _csData :: PactValue }
  deriving (Eq, Show)

instance ToJSON CommandSuccess where
    toJSON (CommandSuccess a) =
        object [ "status" .= ("success" :: String)
               , "data" .= a ]

instance FromJSON CommandSuccess where
    parseJSON = withObject "CommandSuccess" $ \o ->
        CommandSuccess <$> o .: "data"

data CommandResponse l = CommandResponse
  { _crReqKey' :: RequestKey
  , _crTxId' :: Maybe TxId
  , _crResult' :: Value --Either PactError PactValue
  , _crGas' :: Gas
  , _crLogs' :: Maybe l                  -- this would be [TxLog Value] in pact -s
  , _crContinuation' :: Maybe PactExec
  , _crMeta' :: Maybe Value
  } deriving (Generic)
instance (ToJSON l) => ToJSON (CommandResponse l) where toJSON = lensyToJSON 3
instance (FromJSON l) => FromJSON (CommandResponse l) where parseJSON = lensyParseJSON 3





data CommandResult = CommandResult
  { _crReqKey :: RequestKey
  , _crTxId :: Maybe TxId
  , _crResult :: Value
  , _crGas :: Gas
  } deriving (Eq,Show)


cmdToRequestKey :: Command a -> RequestKey
cmdToRequestKey Command {..} = RequestKey (toUntypedHash _cmdHash)




type ApplyCmd = ExecutionMode -> Command ByteString -> IO CommandResult
type ApplyPPCmd m a = ExecutionMode -> Command ByteString -> ProcessedCommand m a -> IO CommandResult

data CommandExecInterface m a = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd
  , _ceiApplyPPCmd :: ApplyPPCmd m a
  }


requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey h) = hashToText h


newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, Serialize, Hashable, ParseText, FromJSON, ToJSON)

instance Show RequestKey where
  show (RequestKey rk) = show rk



makeLenses ''UserSig
makeLenses ''Signer
makeLenses ''CommandExecInterface
makeLenses ''ExecutionMode
makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandError
makeLenses ''CommandSuccess
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
makePrisms ''ExecutionMode
