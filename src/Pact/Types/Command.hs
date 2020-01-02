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
  , mkCommand
  , mkCommand'
  , mkUnsignedCommand
  , signHash
  , signCommand
  , normalizeSigs
  , keyPairToSigner
  , keyPairsToSigners
  , verifyUserSig
  , verifyCommand
  , validateNonemptySigs
  , SomeKeyPairCaps
#else
  , PPKScheme(..)
#endif
  , ProcessedCommand(..),_ProcSucc,_ProcFail
  , Payload(..),pMeta,pNonce,pPayload,pSigners,pNetworkId
  , ParsedCode(..),pcCode,pcExps
  , Signer(..),siScheme, siPubKey, siAddress, siCapList
  , UserSig(..),usSig
  , PactResult(..)
  , CommandResult(..),crReqKey,crTxId,crResult,crGas,crLogs,crContinuation,crMetaData
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey, requestKeyToB16Text
  ) where


import Control.Applicative
import Control.Lens hiding ((.=))
import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.Serialize as SZ
import Data.Hashable (Hashable)
import Data.Aeson as A
import Data.Text (Text)
import Data.Maybe  (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import GHC.Generics


import Pact.Parse (parsePact)
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.Orphans ()
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)


#if !defined(ghcjs_HOST_OS)
import qualified Data.ByteString.Lazy as BSL
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

type SomeKeyPairCaps = (SomeKeyPair,[SigCapability])

-- CREATING AND SIGNING TRANSACTIONS

mkCommand
  :: ToJSON m
  => ToJSON c
  => [SomeKeyPairCaps]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommand creds meta nonce nid rpc = mkCommand' creds encodedPayload
  where encodedPayload = BSL.toStrict $ A.encode payload
        payload = Payload rpc nonce meta (keyPairsToSigners creds) nid

keyPairToSigner :: SomeKeyPair -> [SigCapability] -> Signer
keyPairToSigner cred caps = Signer scheme pub addr caps
      where scheme = case kpToPPKScheme cred of
              ED25519 -> Nothing
              s -> Just s
            pub = toB16Text $ getPublic cred
            addr = case scheme of
              Nothing -> Nothing
              Just {} -> Just $ toB16Text $ formatPublicKey cred


keyPairsToSigners :: [SomeKeyPairCaps] -> [Signer]
keyPairsToSigners creds = map (uncurry keyPairToSigner) creds


mkCommand' :: [(SomeKeyPair,a)] -> ByteString -> IO (Command ByteString)
mkCommand' creds env = do
  let hsh = hash env    -- hash associated with a Command, aka a Command's Request Key
      toUserSig (cred,_) = signHash hsh cred
  sigs <- traverse toUserSig creds
  return $ Command env sigs hsh

mkUnsignedCommand
  :: ToJSON m
  => ToJSON c
  => [Signer]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkUnsignedCommand signers meta nonce nid rpc = mkCommand' [] encodedPayload
  where encodedPayload = BSL.toStrict $ A.encode payload
        payload = Payload rpc nonce meta signers nid

signHash :: TypedHash h -> SomeKeyPair -> IO UserSig
signHash hsh cred = UserSig . toB16Text <$> sign cred (toUntypedHash hsh)

-- | Normalizes a 'Command'. This function is idempotent and the returned
-- 'Command' will satisfy the following properties:
--
-- * The sigs array will be the same length as the signers array
-- * All sigs in the input command that are valid for any of the signers will be present in the output command
-- * All non-empty sigs will be valid signatures for the corresponding signers entry
-- * All other elements of the sigs array will be empty strings
-- * normalizeSigs c >>= validateNonemptySigs == True
--
-- TODO Write tests for these properties
normalizeSigs :: Command (Payload Value ParsedCode) -> Command (Payload Value ParsedCode)
normalizeSigs c@Command{..} = do
  if numSigners == numSigs && validateNonemptySigs c
    then c
    else Command _cmdPayload (V.toList normalizedSigs) _cmdHash
  where
    signers = _pSigners _cmdPayload
    numSigners = length signers
    numSigs = length _cmdSigs
    initialSigs = V.replicate numSigners (UserSig "")
    normalizedSigs = V.modify (\v -> mapM_ (addSig v) _cmdSigs) initialSigs
    addSig v s =
      case findSigLocation _cmdHash signers s of
        Nothing -> return ()
        Just i -> MV.write v i s

-- | Signs a 'Command' with the supplied key pair.
signCommand :: SomeKeyPair -> Command (Payload v p) -> IO (Command (Payload v p))
signCommand kp c = do
  sig <- signHash (_cmdHash c) kp
  let signers = _pSigners $ _cmdPayload c
  let numSigners = length signers
  let initialSigs =
        if length (_cmdSigs c) == numSigners
          then V.fromList $ _cmdSigs c
          else V.replicate numSigners (UserSig "")
      sigs = V.modify (addSigs (_cmdHash c) signers sig) initialSigs
  return $ Command (_cmdPayload c) (V.toList sigs) (_cmdHash c)
  where
    addSigs h signers newSig sigs =
      case findSigLocation h signers newSig of
        Nothing -> return ()
        Just i -> MV.write sigs i newSig

-- | Returns the index of a particular 'UserSig' in a list of signers or Nothing
-- if the signature does not match any of the signers.
findSigLocation :: PactHash -> [Signer] -> UserSig -> Maybe Int
findSigLocation h signers sig = go 0 signers
  where
    go _ [] = Nothing
    go i (s:ss) = if verifyUserSig h sig s then Just i else go (i+1) ss

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

-- | Returns Left if there was some kind of decoding error, otherwise returns
-- Right Bool indicating whether the nonempty signatures in the command were
-- valid.
validateNonemptySigs :: Command (Payload Value ParsedCode) -> Bool
validateNonemptySigs Command{..} =
  all (\(sig, signer) -> emptySig sig || verifyUserSig _cmdHash sig signer) sigSignerPairs
  where
    sigSignerPairs = zip _cmdSigs $ _pSigners _cmdPayload
    emptySig (UserSig s) = s == ""

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
    (Right p, Right sig, addr) ->
      (isValidAddr addr p) &&
      verify (toScheme $ fromMaybe defPPKScheme _siScheme)
             (toUntypedHash msg) (PubBS p) (SigBS sig)
    _ -> False
  where pubT = parseB16TextOnly _siPubKey
        sigT = parseB16TextOnly _usSig
        addrT = parseB16TextOnly <$> _siAddress
        toScheme' = toScheme . fromMaybe ED25519
        isValidAddr addrM pubBS = case addrM of
          Nothing -> True
          Just (Left _) -> False
          Just (Right givenAddr) ->
            case formatPublicKeyBS (toScheme' _siScheme) (PubBS pubBS) of
              Right expectAddr -> givenAddr == expectAddr
              Left _           -> False

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
instance ToJSON Signer where
  toJSON Signer{..} = object $
    consMay "scheme" _siScheme $
    consMay "addr" _siAddress $
    consListMay "clist" _siCapList $
    [ "pubKey" .= _siPubKey ]
    where
      consMay f mv ol = maybe ol (consPair f ol) mv
      consPair f ol v = (f .= v):ol
      consListMay f cl ol
        | null cl = ol
        | otherwise = consPair f ol cl
instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "scheme"
    <*> o .: "pubKey"
    <*> o .:? "addr"
    <*> (listMay <$> (o .:? "clist"))
    where
      listMay = fromMaybe []



-- | Payload combines a 'PactRPC' with a nonce and platform-specific metadata.
data Payload m c = Payload
  { _pPayload :: !(PactRPC c)
  , _pNonce :: !Text
  , _pMeta :: !m
  , _pSigners :: ![Signer]
  , _pNetworkId :: !(Maybe NetworkId)
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


newtype PactResult = PactResult (Either PactError PactValue)
  deriving (Eq, Show, Generic)
instance ToJSON PactResult where
  toJSON (PactResult (Right s)) =
    object [ "status" .= ("success" :: String)
           , "data" .= s ]
  toJSON (PactResult (Left f)) =
    object [ "status" .= ("failure" :: String)
           , "error" .= f ]
instance FromJSON PactResult where
  parseJSON (A.Object o) = PactResult <$>
                           ((Left <$> o .: "error") <|>
                            (Right <$> o .: "data"))
  parseJSON p = fail $ "Invalid PactResult " ++ show p

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
  } deriving (Eq,Show,Generic)
instance (ToJSON l) => ToJSON (CommandResult l) where toJSON = lensyToJSON 3
instance (FromJSON l) => FromJSON (CommandResult l) where parseJSON = lensyParseJSON 3


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
  deriving (Eq, Ord, Generic, Serialize, Hashable, ParseText, FromJSON, ToJSON, ToJSONKey)

instance Show RequestKey where
  show (RequestKey rk) = show rk



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
