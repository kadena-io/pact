{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Crypto
-- Copyright   :  (C) 2016 Stuart Popejoy, William Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, William Martino <will@kadena.io>
--
-- Hashing types and Scheme class.
--
module Pact.Types.Crypto
  ( ST.PPKScheme(..)
  , ST.defPPKScheme
  , SomeScheme
  , SPPKScheme(..)
  , defaultScheme
  , toScheme
  , PublicKeyBS(..)
  , PrivateKeyBS(..)
  , SignatureBS(..)
  , sign
  , verify
  , getPublic
  , getPrivate
  , genKeyPair
  , importKeyPair
  , Scheme(..)
  , ConvertBS(..)
  , Ed25519KeyPair
  ) where


import Prelude
import GHC.Generics

import qualified Codec.Serialise as Serialise
import qualified Crypto.Hash as H
import qualified Crypto.WebAuthn as WA
import qualified Crypto.WebAuthn.Cose.Internal.Verify as WAVerify
import Data.Bifunctor (first)
import Data.ByteString    (ByteString)
import Data.ByteString.Short (fromShort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.String        (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson                        as A
import Data.Hashable
import Data.Serialize                    as SZ
import qualified Data.Serialize          as S

import Pact.Types.Util
import Pact.Types.Hash
import qualified Pact.Types.Hash as PactHash
import Pact.Types.Scheme               as ST

#ifdef CRYPTONITE_ED25519
import qualified Crypto.Error          as E
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray        as B
#else
import "crypto-api" Crypto.Random
import qualified Crypto.Ed25519.Pure as Ed25519
#endif

import qualified Pact.JSON.Encode as J

import Test.QuickCheck

--------- INTERNAL SCHEME CLASS ---------

class ConvertBS a where
  toBS :: a -> ByteString
  fromBS :: ByteString -> Either String a


class Scheme a where
  _valid :: a -> Hash -> PublicKeyBS -> SignatureBS -> Bool
  -- ^ Validate a signature given a public key and hash


--------- CONNECTS PPKSCHEME TO SPPKSCHEME ---------

data SomeScheme = forall a. Scheme (SPPKScheme a) => SomeScheme (SPPKScheme a)

defaultScheme :: SomeScheme
defaultScheme = toScheme defPPKScheme


-- Connects each PPKScheme to some SPPKScheme a

toScheme :: PPKScheme -> SomeScheme
toScheme ED25519 = SomeScheme SED25519
toScheme WebAuthn = SomeScheme SWebAuthn


--------- SCHEME ED25519 INSTANCES --------

#ifdef CRYPTONITE_ED25519
instance Scheme (SPPKScheme 'ED25519) where

  _valid _ (Hash msg) (PubBS pubBS) (SigBS sigBS) =
    case (fromBS pubBS, fromBS sigBS) of
      (Right pubKey, Right sig) -> Ed25519.verify pubKey (fromShort msg) sig
      _ -> False


instance ConvertBS (Ed25519.PublicKey) where
  toBS = B.convert
  fromBS s = E.onCryptoFailure
             (const $ Left ("Invalid ED25519 Public Key: " ++ show (toB16Text s)))
             Right
             (Ed25519.publicKey s)
instance ConvertBS (Ed25519.SecretKey) where
  toBS = B.convert
  fromBS s = E.onCryptoFailure
             (const $ Left ("Invalid ED25519 Private Key: " ++ show (toB16Text s)))
             Right
             (Ed25519.secretKey s)
instance ConvertBS Ed25519.Signature where
  toBS = B.convert
  fromBS s = E.onCryptoFailure
             (const $ Left ("Invalid ED25519 Signature: " ++ show (toB16Text s)))
             Right
             (Ed25519.signature s)
#else
instance Scheme (SPPKScheme 'ED25519) where
  type PublicKey (SPPKScheme 'ED25519) = Ed25519.PublicKey
  type PrivateKey (SPPKScheme 'ED25519) = Ed25519.PrivateKey
  type Signature (SPPKScheme 'ED25519) = Ed25519.Signature

  _valid _ (Hash msg) pub sig =
    case (fromBS pubBS, fromBS sigBS) of
      (Right pubKey, Right sig) -> Ed25519.verify pubKey (fromShort msg) sig
      _ -> False

instance ConvertBS (Ed25519.PublicKey) where
  toBS = Ed25519.exportPublic
  fromBS s = maybeToEither ("Invalid ED25519 Public Key: " ++ show (toB16Text s))
             (Ed25519.importPublic s)
instance ConvertBS (Ed25519.PrivateKey) where
  toBS = Ed25519.exportPrivate
  fromBS s = maybeToEither ("Invalid ED25519 Private Key: " ++ show (toB16Text s))
             (Ed25519.importPrivate s)
instance ConvertBS Ed25519.Signature where
  toBS (Ed25519.Sig bs) = bs
  fromBS = Right . Ed25519.Sig
#endif

instance Scheme (SPPKScheme 'WebAuthn) where

  _valid _ msg (PubBS pubBS) (SigBS sigBS) =
    case runVerification of
      Left _ -> False
      Right () -> True
    where
      -- Verifying a WebAuthn signature requires that we know the payload
      -- signed by the WebAuthn keys on the client device. This payload is
      -- a combination of the Challenge (in our case, the PactHash of a
      -- transaction), a JSON object of "client metadata", and data about
      -- the authenticator hardware.
      -- We require this data to be part of our WebAuthn signature so that
      -- we can reconstitute the payload that was signed on the browser.
      runVerification :: Either String ()
      runVerification = do

        -- Decode our WebAuthn signature object from a `UserSig` string.
        WebAuthnSignature
          { authenticatorData
          , signature
          , clientDataJSON } <- A.eitherDecode (BSL.fromStrict sigBS)

        -- Decode the signer's public key.
        publicKey <- first show $
          Serialise.deserialiseOrFail @WA.CosePublicKey
          (BSL.fromStrict pubBS)

        -- Recover the signature, clientData, and authData bytestrings.
        sig <- Base64.decode (T.encodeUtf8 signature)
        clientData <- Base64URL.decode (T.encodeUtf8 clientDataJSON)
        authData <- Base64.decode (T.encodeUtf8 authenticatorData)

        -- Reconstitute the payload signed by the WebAuthn client.
        clientDataDigest <- Base16.decode $ BS.pack (show (H.hashWith H.SHA256 clientData))
        let
          payload = authData <> clientDataDigest

        -- Check the signature's validity.
        first T.unpack $ WAVerify.verify publicKey payload sig

        -- Extract the original challenge from client data.
        ClientDataJSON { challenge } <- A.eitherDecode (BSL.fromStrict clientData)

        -- Check that the input `PactHash` matches the hash of the transaction
        -- that was signed by WebAuthn keys.
        let pactHashText :: T.Text = PactHash.hashToText msg
        if pactHashText == challenge
          then return ()
          else Left "Hash mismatch"

--------- SCHEME HELPER DATA TYPES ---------

type Ed25519KeyPair = (Ed25519.PublicKey, Ed25519.SecretKey)


newtype PublicKeyBS = PubBS { _pktPublic :: ByteString }
  deriving (Eq, Generic, Hashable)

instance FromJSON PublicKeyBS where
  parseJSON = withText "PublicKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PubBS s'
instance J.Encode PublicKeyBS where
  build (PubBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance IsString PublicKeyBS where
  fromString s = case parseB16TextOnly (T.pack s) of
    Left e -> PubBS $ "Bad public key: " <> T.encodeUtf8 (T.pack e)
    Right b -> PubBS b
instance Show PublicKeyBS where
  show (PubBS b) = T.unpack $ toB16Text b

instance FromJSONKey PublicKeyBS where
    fromJSONKey = FromJSONKeyTextParser (either fail (return . PubBS) . parseB16TextOnly)
    {-# INLINE fromJSONKey #-}
instance Arbitrary PublicKeyBS where
  arbitrary = PubBS . B.pack <$> vector 32


newtype PrivateKeyBS = PrivBS { _pktSecret :: ByteString }
  deriving (Eq, Generic, Hashable)

instance FromJSON PrivateKeyBS where
  parseJSON = withText "PrivateKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PrivBS s'
instance J.Encode PrivateKeyBS where
  build (PrivBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance IsString PrivateKeyBS where
  fromString s = case parseB16TextOnly (T.pack s) of
    Left e -> PrivBS $ "Bad private key: " <> T.encodeUtf8 (T.pack e)
    Right b -> PrivBS b
instance Show PrivateKeyBS where
  show (PrivBS b) = T.unpack $ toB16Text b
instance Arbitrary PrivateKeyBS where
  arbitrary = PrivBS . B.pack <$> vector 32

newtype SignatureBS = SigBS ByteString
  deriving (Eq, Show, Generic, Hashable)

instance FromJSON SignatureBS where
  parseJSON = withText "SignatureBS" $ \s -> do
    s' <- parseB16Text s
    return $ SigBS s'
instance J.Encode SignatureBS where
  build (SigBS p) = J.text $ toB16Text p
  {-# INLINE build #-}
instance Arbitrary SignatureBS where
  arbitrary = SigBS . B.pack <$> vector 64

--------- SCHEME HELPER FUNCTIONS ---------

sign :: Ed25519.PublicKey -> Ed25519.SecretKey -> Hash -> IO ByteString
sign pub priv (Hash msg) = return $ toBS $ Ed25519.sign priv pub (fromShort msg)


verify :: SomeScheme -> Hash -> PublicKeyBS -> SignatureBS -> Bool
verify (SomeScheme scheme) msg pBS sigBS = _valid scheme msg pBS sigBS



getPublic :: Ed25519KeyPair -> ByteString
getPublic = toBS . fst

getPrivate :: Ed25519KeyPair -> ByteString
getPrivate = toBS . snd


-- Key Pair setter functions

genKeyPair :: IO (Ed25519.PublicKey, Ed25519.SecretKey)
genKeyPair = ed25519GenKeyPair


-- | Parse a pair of keys (where the public key is optional) into an Ed25519 keypair.
-- Derives Public Key from Private Key if none provided. Trivial in some
-- Crypto schemes (i.e. Elliptic curve ones).
-- Checks that Public Key provided matches the Public Key derived from the Private Key.
importKeyPair :: Maybe PublicKeyBS -> PrivateKeyBS -> Either String Ed25519KeyPair
importKeyPair maybePubBS (PrivBS privBS) = do
  priv <- fromBS privBS
  let derivedPub = ed25519GetPublicKey priv
  suppliedPub <- case maybePubBS of
    Nothing -> Right Nothing
    Just (PubBS pubBS) -> Just <$> fromBS pubBS

  case suppliedPub of
    Nothing -> return (derivedPub, priv)
    Just pub ->
      if pub == derivedPub
      then return (derivedPub, priv)
      else Left $ "Expected PublicKey "
                ++ show (toB16Text $ toBS pub)
                ++ " but received "
                ++ show (toB16Text $ toBS derivedPub)



--------- ED25519 FUNCTIONS AND ORPHANS ---------

#ifdef CRYPTONITE_ED25519
ed25519GenKeyPair :: IO (Ed25519.PublicKey, Ed25519.SecretKey)
ed25519GenKeyPair = do
    secret <- Ed25519.generateSecretKey
    let public = Ed25519.toPublic secret
    return (public, secret)


ed25519GetPublicKey :: Ed25519.SecretKey -> Ed25519.PublicKey
ed25519GetPublicKey = Ed25519.toPublic




instance Ord Ed25519.PublicKey where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (B.convert s :: ByteString)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (E.maybeCryptoError . Ed25519.publicKey <$> S.getByteString 32)


instance Ord Ed25519.SecretKey where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.SecretKey where
  put s = S.putByteString (B.convert s :: ByteString)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (E.maybeCryptoError . Ed25519.secretKey <$> S.getByteString 32)



instance Ord Ed25519.Signature where
  b <= b' = (B.convert b :: ByteString) <= (B.convert b' :: ByteString)
instance Serialize Ed25519.Signature where
  put s = S.put (B.convert s :: ByteString)
  get = maybe (fail "Invalide ED25519 Signature") return =<<
        (E.maybeCryptoError . Ed25519.signature <$> (S.get >>= S.getByteString))
#else
ed25519GenKeyPair :: IO (Ed25519.PublicKey, Ed25519.PrivateKey)
ed25519GenKeyPair = do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return (p, s)


ed25519GetPublicKey :: Ed25519.PrivateKey -> Ed25519.PublicKey
ed25519GetPublicKey = Ed25519.generatePublic


instance Eq Ed25519.PublicKey where
  b == b' = (Ed25519.exportPublic b) == (Ed25519.exportPublic b')
instance Ord Ed25519.PublicKey where
  b <= b' = (Ed25519.exportPublic b) <= (Ed25519.exportPublic b')
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (Ed25519.exportPublic s)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (Ed25519.importPublic <$> S.getByteString 32)



instance Eq Ed25519.PrivateKey where
  b == b' = (Ed25519.exportPrivate b) == (Ed25519.exportPrivate b')
instance Ord Ed25519.PrivateKey where
  b <= b' = (Ed25519.exportPrivate b) <= (Ed25519.exportPrivate b')
instance Serialize Ed25519.PrivateKey where
  put s = S.putByteString (Ed25519.exportPrivate s)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (Ed25519.importPrivate <$> S.getByteString 32)



deriving instance Eq Ed25519.Signature
deriving instance Ord Ed25519.Signature
instance Serialize Ed25519.Signature where
  put (Ed25519.Sig s) = S.put s
  get = Ed25519.Sig <$> (S.get >>= S.getByteString)
#endif


-- | This type specifies the format of a WebAuthn signature.
data WebAuthnSignature = WebAuthnSignature
  { clientDataJSON :: T.Text
  , authenticatorData :: T.Text
  , signature :: T.Text
  } deriving (Show, Generic)

instance A.FromJSON WebAuthnSignature where
  parseJSON = A.withObject "WebAuthnSignature" $ \o -> do
    clientDataJSON <- o .: "clientDataJSON"
    authenticatorData <- o .: "authenticatorData"
    signature <- o .: "signature"
    pure $ WebAuthnSignature {..}

-- | This type represents a challenge that was used during
-- a WebAuthn "assertion" flow. For signing Pact payloads, this
-- is the PactHash of a transaction.
newtype ClientDataJSON = ClientDataJSON {
  challenge :: T.Text
  } deriving (Show, Generic)

instance A.FromJSON ClientDataJSON where
  parseJSON = A.withObject "ClientDataJSON" $ \o -> do
    challenge <- o .: "challenge"
    pure $ ClientDataJSON { challenge }
