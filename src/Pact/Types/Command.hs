{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , mkCommand
  , mkCommand'
  , mkUnsignedCommand
  , signHash
  , keyPairToSigner
  , keyPairsToSigners
  , verifyUserSig
  , verifyCommand
#else
  , PPKScheme(..)
#endif
  , Ed25519KeyPairCaps
  , ProcessedCommand(..),_ProcSucc,_ProcFail
  , Payload(..),pMeta,pNonce,pPayload,pSigners,pNetworkId
  , ParsedCode(..),pcCode,pcExps
  , Signer(..),siScheme, siPubKey, siAddress, siCapList
  , UserSig(..),usSig
  , PactResult(..)
  , CommandResult(..),crReqKey,crTxId,crResult,crGas,crLogs,crEvents
  , crContinuation,crMetaData
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey
  , requestKeyToB16Text
  -- , verifyWebAuthnSig
  ) where

import Control.Applicative
import Control.Lens hiding ((.=), elements)
import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.Serialize as SZ
import Data.Hashable (Hashable)
import Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Maybe  (fromMaybe)

import GHC.Generics

import Test.QuickCheck

import Pact.Parse (parsePact)
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.Orphans ()
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.RPC
import Pact.Types.Runtime

import Pact.JSON.Legacy.Value
import qualified Pact.JSON.Encode as J


#if !defined(ghcjs_HOST_OS)
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

instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                Command <$> (o .: "cmd")
                        <*> (o .: "sigs")
                        <*> (o .: "hash")
    {-# INLINE parseJSON #-}

instance J.Encode a => J.Encode (Command a) where
  build o = J.object
    [ "hash" J..= _cmdHash o
    , "sigs" J..= J.Array (_cmdSigs o)
    , "cmd" J..= _cmdPayload o
    ]
  {-# INLINABLE build #-}

instance NFData a => NFData (Command a)

instance Arbitrary a => Arbitrary (Command a) where
  arbitrary = Command <$> arbitrary <*> arbitrary <*> arbitrary

-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand m a =
  ProcSucc !(Command (Payload m a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (ProcessedCommand m a)

#if !defined(ghcjs_HOST_OS)

type Ed25519KeyPairCaps = (Ed25519KeyPair ,[SigCapability])

-- CREATING AND SIGNING TRANSACTIONS

mkCommand
  :: J.Encode c
  => J.Encode m
  => [(Ed25519KeyPair, [SigCapability])]
  -> m
-> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommand creds meta nonce nid rpc = mkCommand' creds encodedPayload
  where
    encodedPayload = J.encodeStrict $ toLegacyJsonViaEncode payload
    payload = Payload rpc nonce meta (keyPairsToSigners creds) nid

keyPairToSigner :: Ed25519KeyPair -> [SigCapability] -> Signer
keyPairToSigner cred caps = Signer scheme pub addr caps
      where
        scheme = Just ED25519
        pub = toB16Text $ toBS $ fst cred
        addr = Just pub


keyPairsToSigners :: [Ed25519KeyPairCaps] -> [Signer]
keyPairsToSigners creds = map (uncurry keyPairToSigner) creds


mkCommand' :: [(Ed25519KeyPair ,a)] -> ByteString -> IO (Command ByteString)
mkCommand' creds env = do
  let hsh = hash env    -- hash associated with a Command, aka a Command's Request Key
      toUserSig (cred,_) = signHash hsh cred
  sigs <- traverse toUserSig creds
  return $ Command env sigs hsh

mkUnsignedCommand
  :: J.Encode m
  => J.Encode c
  => [Signer]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkUnsignedCommand signers meta nonce nid rpc = mkCommand' [] encodedPayload
  where encodedPayload = J.encodeStrict payload
        payload = Payload rpc nonce meta signers nid

signHash :: TypedHash h -> Ed25519KeyPair -> IO UserSig
signHash hsh (pub,priv) = UserSig . toB16Text <$> sign pub priv (toUntypedHash hsh)

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
        formatIssues = Just $ "Invalid sig(s) found: " ++ show (J.encode . J.Array <$> failedSigs)


verifyUserSig :: PactHash -> UserSig -> Signer -> Bool
verifyUserSig msg UserSig{..} Signer{..} =
  case (pubT, sigT, addrT) of
    (Right p, sig, addr) ->
      (isValidAddr addr p) &&
      verify (toScheme $ fromMaybe defPPKScheme _siScheme)
             (toUntypedHash msg) (PubBS p) sig
    _ -> False
  where pubT = parseB16TextOnly _siPubKey
        sigT = case parseB16TextOnly _usSig of
          Left _ -> SigBS (Text.encodeUtf8 _usSig)
          Right bs -> SigBS bs
        addrT = parseB16TextOnly <$> _siAddress
        isValidAddr addrM pubBS = case addrM of
          Nothing -> True
          Just (Left _) -> False
          -- All current cases of `_siScheme` require the same relationship
          -- between `pubBS` and `givenAddr`. But we enumerate them anyway,
          -- so that if another scheme is added in the future, this use site
          -- will warn us that we need to consider the `pubBS`/`givenAddr`
          -- relationship for that scheme, since it may be different.
          Just (Right givenAddr) -> case _siScheme of
            Nothing -> pubBS == givenAddr
            Just ED25519 -> pubBS == givenAddr
            Just WebAuthn -> pubBS == givenAddr
#endif



-- | Signer combines PPKScheme, PublicKey, and the Address (aka the
--   formatted PublicKey).
data Signer = Signer
 { _siScheme :: !(Maybe PPKScheme)
 -- ^ PPKScheme, which is defaulted to 'defPPKScheme' if not present
 , _siPubKey :: !Text
 -- ^ pub key value
 , _siAddress :: !(Maybe Text)
 -- ^ optional "address", for different pub key formats like ETH
 , _siCapList :: [SigCapability]
 -- ^ clist for designating signature to specific caps
 } deriving (Eq, Ord, Show, Generic)

instance NFData Signer

instance J.Encode Signer where
  build o = J.object
    [ "addr" J..?= _siAddress o
    , "scheme" J..?= _siScheme o
    , "pubKey" J..= _siPubKey o
    , "clist" J..??= J.Array (_siCapList o)
    ]

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "scheme"
    <*> o .: "pubKey"
    <*> o .:? "addr"
    <*> (listMay <$> (o .:? "clist"))
    where
      listMay = fromMaybe []

instance Arbitrary Signer where
  arbitrary = Signer <$> arbitrary <*> arbitrary <*> arbitrary <*> scale (min 5) arbitrary

-- | Payload combines a 'PactRPC' with a nonce and platform-specific metadata.
data Payload m c = Payload
  { _pPayload :: !(PactRPC c)
  , _pNonce :: !Text
  , _pMeta :: !m
  , _pSigners :: ![Signer]
  , _pNetworkId :: !(Maybe NetworkId)
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (Payload m a)

instance (J.Encode a, J.Encode m) => J.Encode (Payload m a) where
  build o = J.object
    [ "networkId" J..= _pNetworkId o
    , "payload" J..= _pPayload o
    , "signers" J..= J.Array (_pSigners o)
    , "meta" J..= _pMeta o
    , "nonce" J..= _pNonce o
    ]
  {-# INLINE build #-}

instance (FromJSON a,FromJSON m) => FromJSON (Payload m a) where parseJSON = lensyParseJSON 2

instance (Arbitrary m, Arbitrary c) => Arbitrary (Payload m c) where
  arbitrary = Payload
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (min 10) arbitrary
    <*> arbitrary

newtype UserSig = UserSig { _usSig :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData UserSig
instance Serialize UserSig

instance J.Encode UserSig where
  build s = J.object [ "sig" J..= _usSig s ]
  {-# INLINE build #-}

instance FromJSON UserSig where
  parseJSON = withObject "UserSig" $ \o -> do
    UserSig <$> o .: "sig"

instance Arbitrary UserSig where
  arbitrary = UserSig <$> arbitrary

newtype PactResult = PactResult
  { _pactResult :: Either PactError PactValue
  } deriving (Eq, Show, Generic,NFData)

instance J.Encode PactResult where
  build (PactResult (Right s)) = J.object
    [ "status" J..= J.text "success"
    , "data" J..= s
    ]
  build (PactResult (Left f)) = J.object
    [ "status" J..= J.text "failure"
    , "error" J..= f
    ]
  {-# INLINE build #-}

instance FromJSON PactResult where
  parseJSON (A.Object o) =
    PactResult <$> ((Left . _getUxPactError <$> o .: "error") <|> (Right <$> o .: "data"))
  parseJSON p = fail $ "Invalid PactResult " ++ show p

instance Arbitrary PactResult where
  arbitrary = PactResult <$> oneof
    [ Left . _getUxPactError <$> arbitrary
    , Right <$> arbitrary
    ]

-- | API result of attempting to execute a pact command, parametrized over level of logging type
data CommandResult l = CommandResult {
  -- | Request Key of command (the hash of the command payload)
    _crReqKey :: !RequestKey
  -- | Transaction id of this CommandResult
  , _crTxId :: !(Maybe TxId)
  -- | Pact execution result, either a PactError or the last pact expression output as a PactValue
  , _crResult :: !PactResult
  -- | Gas consummed by command
  , _crGas :: !Gas
  -- | Level of logging (i.e. full TxLog vs hashed logs)
  , _crLogs :: !(Maybe l)
  -- | Output of a Continuation if one occurred in the command.
  , _crContinuation :: !(Maybe PactExec)
  -- | Platform-specific data
  , _crMetaData :: !(Maybe Value)
  -- | Events
  , _crEvents :: ![PactEvent]
  } deriving (Eq,Show,Generic,Functor)

instance J.Encode l => J.Encode (CommandResult l) where
  build o = J.object
    [ "gas" J..= _crGas o
    , "result" J..= _crResult o
    , "reqKey" J..= _crReqKey o
    , "logs" J..= _crLogs o
    , "events" J..??= J.Array (_crEvents o)
    , "metaData" J..= fmap toLegacyJson (_crMetaData o)
    , "continuation" J..= _crContinuation o
    , "txId" J..= _crTxId o
    ]
  {-# INLINE build #-}

instance (FromJSON l) => FromJSON (CommandResult l) where
  parseJSON = withObject "CommandResult" $ \o -> CommandResult
      <$> o .: "reqKey"
      <*> o .: "txId"
      <*> o .: "result"
      <*> o .: "gas"
      <*> o .: "logs"
      <*> o .: "continuation"
      <*> o .: "metaData"
      <*> (events <$> o .:? "events")
    where
      events Nothing = []
      events (Just es) = es
instance NFData a => NFData (CommandResult a)

instance Arbitrary l => Arbitrary (CommandResult l) where
  arbitrary = CommandResult
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> elements [Nothing, Just (String "JSON VALUE")]
    <*> scale (min 10) arbitrary

cmdToRequestKey :: Command a -> RequestKey
cmdToRequestKey Command {..} = RequestKey (toUntypedHash _cmdHash)

type ApplyCmd l = ExecutionMode -> Command ByteString -> IO (CommandResult l)
type ApplyPPCmd m a l = ExecutionMode -> Command ByteString -> ProcessedCommand m a -> IO (CommandResult l)

data CommandExecInterface m a l = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd l
  , _ceiApplyPPCmd :: ApplyPPCmd m a l
  }


requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey h) = hashToText h


newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic)
  deriving newtype (Serialize, Hashable, ParseText, FromJSON, FromJSONKey, NFData, J.Encode, AsString)

instance Show RequestKey where
  show (RequestKey rk) = show rk

instance Arbitrary RequestKey where
  arbitrary = RequestKey <$> arbitrary

makeLenses ''UserSig
makeLenses ''Signer
makeLenses ''CommandExecInterface
makeLenses ''ExecutionMode
makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
makePrisms ''ExecutionMode
