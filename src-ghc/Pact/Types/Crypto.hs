{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}

-- |
-- Module      :  Pact.Types.Crypto
-- Copyright   :  (C) 2016 Stuart Popejoy, William Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, William Martino <will@kadena.io>
--
-- Hashing types and Scheme class.
--
module Pact.Types.Crypto
  ( hashTx
  , verifyHashTx
  , initialHashTx
  , ST.PPKScheme(..)
  , ST.defPPKScheme
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
  , formatPublicKeyBS
  , kpToPPKScheme
  , getPublic
  , getPrivate
  , genKeyPair
  , importKeyPair
  ) where


import Prelude
import GHC.Generics

import qualified Crypto.Hash  as H

import Data.Aeson.Types   (toJSONKeyText)
import Data.Text.Encoding
import Data.ByteString    (ByteString)

import Data.Aeson                        as A
import qualified Data.ByteArray          as B
import Data.Serialize                    as SZ
import qualified Data.Serialize          as S


import Pact.Types.Util
import Pact.Types.Scheme               as ST
import qualified Pact.Types.ECDSA      as ECDSA

#ifdef CRYPTONITE_ED25519
import qualified Crypto.Error          as E
import qualified Crypto.PubKey.Ed25519 as Ed25519
#else
import "crypto-api" Crypto.Random
import qualified Crypto.Ed25519.Pure as Ed25519
#endif



--------- INTERNAL SCHEME CLASS ---------

class ConvertBS a where
  toBS :: a -> ByteString
  fromBS :: ByteString -> Either String a




-- Scheme class created to enforce at the type level that Public Key,
-- Private Key, and Signature are of the same scheme when signing
-- and validating.

-- Also ensures that each scheme specifies its payload hash and
-- how it will transform its public keys to Pact Runtime public keys.

class (ConvertBS (PublicKey a),  Eq (PublicKey a),
       ConvertBS (PrivateKey a),
       ConvertBS (Signature a))  =>
      Scheme a where

  type PublicKey a
  type PrivateKey a
  type Signature a

  _sign :: a -> Hash -> PublicKey a -> PrivateKey a -> IO (Signature a)
  _valid :: a -> Hash -> PublicKey a -> Signature a -> Bool
  _genKeyPair :: a -> IO (PublicKey a, PrivateKey a)


  -- Trivial to derive in Elliptic Curve Cryptography.
  -- Return Nothing if not possible to derive.

  _getPublic :: a -> PrivateKey a -> Maybe (PublicKey a)


  -- Converts Cryptographic public keys to Pact Runtime public keys depending on the scheme.
  -- Pact uses Cryptographic PKs to sign/validate transactions.
  -- While it uses Runtime PKs during keyset enforcement to denote ownership and permissions.
  -- Certain schemes (i.e. Ethereum) call Runtime public keys 'addresses'.

  _formatPublicKey :: a -> PublicKey a -> ByteString




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




--------- CONNECTS PPKSCHEME TO SPPKSCHEME ---------

data SomeScheme = forall a. Scheme (SPPKScheme a) => SomeScheme (SPPKScheme a)

defaultScheme :: SomeScheme
defaultScheme = toScheme defPPKScheme


-- Connects each PPKScheme to some SPPKScheme a

toScheme :: PPKScheme -> SomeScheme
toScheme ED25519 = SomeScheme SED25519
toScheme ETH     = SomeScheme SETH


-- Connects each SPPKScheme a with a PPKScheme

toPPKScheme :: SPPKScheme a -> PPKScheme
toPPKScheme SED25519 = ED25519
toPPKScheme SETH     = ETH




--------- SCHEME ED25519 INSTANCES --------

#ifdef CRYPTONITE_ED25519
instance Scheme (SPPKScheme 'ED25519) where
  type PublicKey (SPPKScheme 'ED25519) = Ed25519.PublicKey
  type PrivateKey (SPPKScheme 'ED25519) = Ed25519.SecretKey
  type Signature (SPPKScheme 'ED25519) = Ed25519.Signature

  _sign _ (Hash msg) pub priv = return $ Ed25519.sign priv pub msg
  _valid _ (Hash msg) pub sig = Ed25519.verify pub msg sig
  _genKeyPair _ = ed25519GenKeyPair
  _getPublic _ = Just . ed25519GetPublicKey
  _formatPublicKey _ p = toBS p



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

  _sign _ (Hash msg) pub priv = return $ Ed25519.sign msg priv pub
  _valid _ (Hash msg) pub sig = Ed25519.valid msg pub sig
  _genKeyPair _ = ed25519GenKeyPair
  _getPublic _ = Just . ed25519GetPublicKey
  _formatPublicKey _ p = toBS p



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



--------- SCHEME ETH INSTANCES --------

instance Scheme (SPPKScheme 'ETH) where
  type PublicKey (SPPKScheme 'ETH) = ECDSA.PublicKey
  type PrivateKey (SPPKScheme 'ETH) = ECDSA.PrivateKey
  type Signature (SPPKScheme 'ETH) = ECDSA.Signature

  _sign _ (Hash msg) pub priv = ECDSA.signETH msg pub priv
  _valid _ (Hash msg) pub sig = ECDSA.validETH msg pub sig
  _genKeyPair _ = ECDSA.genKeyPair
  _getPublic _ = Just . ECDSA.getPublicKey
  _formatPublicKey _ p = ECDSA.formatPublicKeyETH (toBS p)



instance ConvertBS (ECDSA.PublicKey) where
  toBS = ECDSA.exportPublic
  fromBS s = maybeToEither ("Invalid ECDSA Public Key: " ++ show (toB16Text s))
             (ECDSA.importPublic s)
instance ConvertBS (ECDSA.PrivateKey) where
  toBS = ECDSA.exportPrivate
  fromBS s = maybeToEither ("Invalid ECDSA Private Key: " ++ show (toB16Text s))
             (ECDSA.importPrivate s)
instance ConvertBS (ECDSA.Signature) where
  toBS = ECDSA.exportSignature
  fromBS s = maybeToEither ("Invalid ECDSA Signature: " ++ show (toB16Text s))
             (ECDSA.importSignature s)




--------- SCHEME HELPER DATA TYPES ---------

-- KeyPair existential allows a transaction to be signed by
-- key pairs of different schemes

data KeyPair a = KeyPair
  { _kpScheme :: a
  , _kpPublicKey :: PublicKey a
  , _kpPrivateKey :: PrivateKey a
  }
data SomeKeyPair = forall a. Scheme (SPPKScheme a) => SomeKeyPair (KeyPair (SPPKScheme a))



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




--------- SCHEME HELPER FUNCTIONS ---------

sign :: SomeKeyPair -> Hash -> IO ByteString
sign (SomeKeyPair KeyPair{..}) msg = toBS <$> _sign _kpScheme msg _kpPublicKey _kpPrivateKey


verify :: SomeScheme -> Hash -> PublicKeyBS -> SignatureBS -> Bool
verify (SomeScheme scheme) msg (PubBS pBS) (SigBS sigBS) =
  let pParsed = fromBS pBS
      sigParsed = fromBS sigBS
  in case (pParsed, sigParsed) of
       (Right p, Right sig) -> _valid scheme msg p sig
       _ -> False


formatPublicKey :: SomeKeyPair -> ByteString
formatPublicKey (SomeKeyPair KeyPair{..}) = _formatPublicKey _kpScheme _kpPublicKey

formatPublicKeyBS :: SomeScheme -> PublicKeyBS -> Either String ByteString
formatPublicKeyBS (SomeScheme scheme) (PubBS pBS) = do
  pub <- fromBS pBS
  return $ _formatPublicKey scheme pub



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


-- Derives Public Key from Private Key if none provided. Trivial in some
-- Crypto schemes (i.e. Elliptic curve ones).
-- Checks that Public Key provided matches the Public Key derived from the Private Key.

importKeyPair :: SomeScheme -> Maybe PublicKeyBS -> PrivateKeyBS -> Either String SomeKeyPair
importKeyPair (SomeScheme scheme) maybePubBS (PrivBS privBS) = do
  priv <- fromBS privBS
  pub <- case maybePubBS of
           Nothing  -> maybeToEither
                       (show (toPPKScheme scheme) ++ " Key Pair import failed: Need Public Key")
                       (_getPublic scheme priv)
           Just (PubBS pubBS) -> do
             pubActual <- fromBS pubBS
             case (_getPublic scheme priv) of
               Nothing -> Right pubActual
               Just pubExpect | pubExpect == pubActual -> Right pubActual
                              | otherwise              -> Left $ "Expected PublicKey "
                                                          ++ show (toB16Text $ toBS pubExpect)
                                                          ++ " but received "
                                                          ++ show (toB16Text $ toBS pubActual)
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
