{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}

-- |
-- Module      :  Pact.Types.Crypto
-- Copyright   :  (C) 2016 Stuart Popejoy, William Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, William Martino <will@kadena.io>
--
-- PPK and hashing types.
--
module Pact.Types.Crypto
  ( hashTx
  , verifyHashTx
  , initialHashTx
  , PPKScheme(..)
  , SomeScheme
  , defaultScheme
  , toScheme
  , SomeKeyPair
  , PublicKeyBS(..)
  , PrivateKeyBS(..)
  , SignatureBS(..)
  , sign
  , verify
  , formatPublicKey
  , kpToPPKScheme
  , getPublic
  , getPrivate
  , genKeyPair
  , importKeyPair
  ) where


import Control.Applicative
import Control.Monad.Reader
import Control.DeepSeq

import "crypto-api" Crypto.Random
import qualified    Crypto.Hash    as H

import Data.Aeson.Types  (toJSONKeyText)
import Data.Maybe
import Data.Text.Encoding
import Data.ByteString  (ByteString)

import Data.Aeson                        as A
import qualified Data.ByteArray          as B
import Data.Serialize                    as SZ
import qualified Data.Serialize          as S


import GHC.Generics
import Prelude
import Pact.Types.Util


import qualified Crypto.Ed25519.Pure   as Ed25519
import qualified Pact.Types.ECDSA      as ECDSA


--------- HASHING ---------

hashTx :: (H.HashAlgorithm a) => ByteString -> a -> Hash
hashTx b algo = (Hash . B.convert . H.hashWith algo) b

verifyHashTx :: (H.HashAlgorithm a) => Hash -> ByteString -> a -> Either String Hash
verifyHashTx h b algo = if hashTx b algo == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h
       ++ " but our hashing resulted in " ++ show (hashTx b algo)
{-# INLINE verifyHashTx #-}

initialHashTx :: (H.HashAlgorithm a) => a -> Hash
initialHashTx algo = hashTx mempty algo




--------- PPKSCHEME DATA TYPE ---------

data PPKScheme = ED25519 | ETH
  deriving (Show, Eq, Ord, Generic)


instance NFData PPKScheme
instance Serialize PPKScheme
instance ToJSON PPKScheme where
  toJSON ED25519 = "ED25519"
  toJSON ETH = "ETH"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    "ETH" -> return ETH
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseJSON #-}





--------- INTERNAL SCHEME CLASS ---------

-- Run-time witness to PPKScheme kind.

data SPPKScheme :: PPKScheme -> * where
  SED25519 :: SPPKScheme 'ED25519
  SETH :: SPPKScheme 'ETH
data SomeScheme = forall a. Scheme a => SomeScheme (SPPKScheme a)



-- Connects each PPKScheme to some SPPKScheme a

toScheme :: PPKScheme -> SomeScheme
toScheme ED25519 = SomeScheme SED25519
toScheme ETH     = SomeScheme SETH


-- Connects each SPPKScheme a with a PPKScheme

toPPKScheme :: SPPKScheme a -> PPKScheme
toPPKScheme SED25519 = ED25519
toPPKScheme SETH     = ETH



defaultScheme :: SomeScheme
defaultScheme = SomeScheme SED25519




-- Scheme class created to enforce at the type level that Public Key,
-- Private Key, and Signature are of the same scheme when signing
-- and validating.

-- Also ensures that each scheme specifies its payload hash and
-- how it will transform its public keys to Pact Runtime public keys.

-- Not exported to hide implementation details. The PPKScheme
-- is always known and this module controls the relationship
-- between PPKScheme and SPPKScheme a.

class (ConvertBS (PublicKey a),
       ConvertBS (PrivateKey a),
       ConvertBS (Signature a))  =>
      Scheme (a :: PPKScheme) where

  type PublicKey a
  type PrivateKey a
  type Signature a

  _hashPayload :: SPPKScheme a -> ByteString -> ByteString
  _sign :: SPPKScheme a -> ByteString -> PublicKey a -> PrivateKey a -> IO (Signature a)
  _valid :: SPPKScheme a -> ByteString -> PublicKey a -> Signature a -> Bool
  _genKeyPair :: SPPKScheme a -> IO (PublicKey a, PrivateKey a)
  _formatPublicKey :: SPPKScheme a -> PublicKey a -> ByteString




instance Scheme 'ED25519 where
  type PublicKey 'ED25519 = Ed25519.PublicKey
  type PrivateKey 'ED25519 = Ed25519.PrivateKey
  type Signature 'ED25519 = Ed25519.Signature

  _hashPayload _ msg = hsh
    where (Hash hsh) = hashTx msg H.Blake2b_512
  _sign s msg pub priv = return $ Ed25519.sign (_hashPayload s msg) priv pub
  _valid s msg pub sig = Ed25519.valid (_hashPayload s msg) pub sig
  _genKeyPair _ = ed25519GenKeyPair
  _formatPublicKey _ p = toBS p




instance Scheme 'ETH where
  type PublicKey 'ETH = ECDSA.PublicKey
  type PrivateKey 'ETH = ECDSA.PrivateKey
  type Signature 'ETH = ECDSA.Signature

  _hashPayload _ msg = hsh
    where (Hash hsh) = hashTx msg ECDSA.hashAlgoETH
  _sign s msg pub priv = ECDSA.signETH (_hashPayload s msg) pub priv
  _valid s msg pub sig = ECDSA.validETH (_hashPayload s msg) pub sig
  _genKeyPair _ = ECDSA.genKeyPair
  _formatPublicKey _ p = ECDSA.formatPublicKeyETH (toBS p)




--------- CRYPTO KEYS AND SIGNATURE CONVERSIONS ---------

class ConvertBS a where
  toBS :: a -> ByteString
  fromBS :: ByteString -> Either String a

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a)  = Right a




-- Ed25519 instances

instance ConvertBS (Ed25519.PublicKey) where
  toBS = Ed25519.exportPublic
  fromBS s = maybeToEither ("Invalid ED25519 Public Key: " ++ show s)
             (Ed25519.importPublic s)
instance ConvertBS (Ed25519.PrivateKey) where
  toBS = Ed25519.exportPrivate
  fromBS s = maybeToEither ("Invalid ED25519 Private Key: " ++ show s)
             (Ed25519.importPrivate s)
instance ConvertBS Ed25519.Signature where
  toBS (Ed25519.Sig bs) = bs
  fromBS = Right . Ed25519.Sig




-- ECDSA instances

instance ConvertBS (ECDSA.PublicKey) where
  toBS = ECDSA.exportPublic
  fromBS s = maybeToEither ("Invalid ECDSA Public Key: " ++ show s)
             (ECDSA.importPublic s)
instance ConvertBS (ECDSA.PrivateKey) where
  toBS = ECDSA.exportPrivate
  fromBS s = maybeToEither ("Invalid ECDSA Private Key: " ++ show s)
             (ECDSA.importPrivate s)
instance ConvertBS (ECDSA.Signature) where
  toBS = ECDSA.exportSignature
  fromBS s = maybeToEither ("Invalid ECDSA Signature: " ++ show s)
             (ECDSA.importSignature s)




--------- KEY PAIR DATA TYPES ---------

-- KeyPair existential allows a transaction to be signed by
-- key pairs of different schemes.

data KeyPair (a :: PPKScheme) = KeyPair
  { _kpScheme :: SPPKScheme a
  , _kpPublicKey :: PublicKey a
  , _kpPrivateKey :: PrivateKey a
  }
data SomeKeyPair = forall a. Scheme a => SomeKeyPair (KeyPair a)




--------- HELPER DATA TYPES AND FUNCTIONS --------

newtype PublicKeyBS = PubBS { _pktPublic :: ByteString }
  deriving (Eq, Show, Generic)
instance ToJSON PublicKeyBS where
  toJSON (PubBS p) = toB16JSON p
instance FromJSON PublicKeyBS where
  parseJSON = withText "PublicKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PubBS s'


newtype PrivateKeyBS = PrivBS { _pktSecret :: ByteString }
  deriving (Eq, Show, Generic)
instance ToJSON PrivateKeyBS where
  toJSON (PrivBS p) = toB16JSON p
instance FromJSON PrivateKeyBS where
  parseJSON = withText "PrivateKeyBS" $ \s -> do
    s' <- parseB16Text s
    return $ PrivBS s'


newtype SignatureBS = SigBS ByteString
  deriving (Eq, Show, Generic)




-- Scheme class helper functions

sign :: SomeKeyPair -> ByteString -> IO ByteString
sign (SomeKeyPair KeyPair{..}) msg = toBS <$> _sign _kpScheme msg _kpPublicKey _kpPrivateKey


verify :: SomeScheme -> ByteString -> PublicKeyBS -> SignatureBS -> Bool
verify (SomeScheme scheme) msg (PubBS pBS) (SigBS sigBS) =
  let pParsed = fromBS pBS
      sigParsed = fromBS sigBS
  in case (pParsed, sigParsed) of
       (Right p, Right sig) -> _valid scheme msg p sig
       _ -> False


-- Converts Cryptographic public keys to Pact Runtime public keys depending on the scheme.
-- Pact uses Cryptographic PKs to sign/validate transactions.
-- While it uses Runtime PKs during keyset enforcement to denote ownership and permissions.
-- Certain schemes (i.e. Ethereum) call Runtime public keys 'addresses'.

formatPublicKey :: SomeKeyPair -> ByteString
formatPublicKey (SomeKeyPair KeyPair{..}) = _formatPublicKey _kpScheme _kpPublicKey




-- Key Pair getter functions

kpToPPKScheme :: SomeKeyPair -> PPKScheme
kpToPPKScheme (SomeKeyPair kp) = toPPKScheme (_kpScheme kp)

getPublic :: SomeKeyPair -> ByteString
getPublic (SomeKeyPair kp) = toBS (_kpPublicKey kp)

getPrivate :: SomeKeyPair -> ByteString
getPrivate (SomeKeyPair kp) = toBS (_kpPrivateKey kp)




-- Key Pair setter functions

genKeyPair :: SomeScheme -> IO SomeKeyPair
genKeyPair (SomeScheme scheme) = do
  (pub, priv) <- _genKeyPair scheme
  return $ SomeKeyPair $ KeyPair scheme pub priv


importKeyPair :: SomeScheme -> PublicKeyBS -> PrivateKeyBS -> Either String SomeKeyPair
importKeyPair (SomeScheme scheme) (PubBS pubBS) (PrivBS privBS) = do
  pub <- fromBS pubBS
  priv <- fromBS privBS
  return $ SomeKeyPair $ KeyPair scheme pub priv




--------- BYTESTRING INSTANCES ---------

instance ToJSON ByteString where
  toJSON = String . decodeUtf8
instance FromJSON ByteString where
  parseJSON = withText "ByteString" (return . encodeUtf8)
instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText decodeUtf8
instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8




--------- ED25519 FUNCTIONS AND INSTANCES ---------

ed25519GenKeyPair :: IO (Ed25519.PublicKey, Ed25519.PrivateKey)
ed25519GenKeyPair = do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return (p, s)



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
