{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( hashTx, verifyHashTx, initialHashTx
  ,  PPKScheme(..), SPPKScheme(..), KeyPair(..), SomeKeyPair(..), toScheme, Scheme(..)
  , hashLengthAsBS, hashLengthAsBase16, hashToB16Text
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.DeepSeq

import "crypto-api" Crypto.Random
import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Crypto.Hash as H

import Data.Aeson as A
import Data.Aeson.Types (toJSONKeyText)

import Data.ByteString (ByteString)
import qualified Data.ByteArray as B
import Data.Serialize as SZ
import qualified Data.Serialize as S
import Data.Maybe
import Data.Text.Encoding
import Data.Kind

import GHC.Generics
import Prelude

import Pact.Types.Util


hashTx :: (H.HashAlgorithm a) => a -> ByteString -> Hash
hashTx algo b = (Hash . B.convert . H.hashWith algo) b

verifyHashTx :: (H.HashAlgorithm a) => a -> Hash -> ByteString -> Either String Hash
verifyHashTx algo h b = if hashTx algo b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h
       ++ " but our hashing resulted in " ++ show (hashTx algo b)
{-# INLINE verifyHashTx #-}

initialHashTx :: (H.HashAlgorithm a) => a -> Hash
initialHashTx algo = hashTx algo mempty

class Scheme a where
  type PublicKey a
  type PrivateKey a
  type Signature a
  type PayloadHash a
  payloadHash ::  a -> PayloadHash a
  importPublic :: a -> ByteString -> Maybe (PublicKey a)
  importPrivate :: a -> ByteString -> Maybe (PrivateKey a)
  exportPublic :: a -> PublicKey a -> ByteString
  exportPrivate :: a -> PrivateKey a -> ByteString
  exportSignature :: a -> Signature a -> ByteString
  sign :: a -> ByteString -> PublicKey a -> PrivateKey a -> Signature a
  valid :: a -> ByteString -> PublicKey a -> Signature a -> Bool
  formatPublicKey :: a -> PublicKey a -> ByteString
  genKey :: a -> IO (PrivateKey a, PublicKey a)

data PPKScheme = ED25519 | ETH
  deriving (Show, Eq, Ord, Generic)
instance NFData PPKScheme
instance ToJSON PPKScheme where
  toJSON ED25519 = "ED25519"
  toJSON ETH = "ETH"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    "ETH" -> return ETH
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize PPKScheme


data SPPKScheme :: PPKScheme -> Type where
  SED25519 :: SPPKScheme 'ED25519
  SETH :: SPPKScheme 'ETH

data KeyPair a = KeyPair
  { _kpScheme :: SPPKScheme a
  , _kpPublicKey :: PublicKey (SPPKScheme a)
  , _kpPrivateKey :: PrivateKey (SPPKScheme a)
  }

data SomeKeyPair = forall n . SomeKeyPair (KeyPair n)
instance FromJSON (SomeKeyPair) where
    parseJSON = withObject "SomeKeyPair" $ \o -> do
      addr <- (o .:? "scheme")
      secret <- (o .: "secret")
      public <- (o .: "public")
      sAddr <- maybe "ED25519" return addr
      case (sAddr :: String) of
          "ED25519" -> return $ SomeKeyPair $ KeyPair SED25519 public secret
          "ETH" -> return $ SomeKeyPair $ KeyPair SETH public secret

toScheme :: SPPKScheme a -> PPKScheme
toScheme SED25519 = ED25519
toScheme SETH = ETH

instance Scheme (SPPKScheme 'ED25519) where
  type PublicKey (SPPKScheme 'ED25519) = Ed25519.PublicKey
  type PrivateKey (SPPKScheme 'ED25519) = Ed25519.PrivateKey
  type Signature (SPPKScheme 'ED25519) = Ed25519.Signature
  type PayloadHash (SPPKScheme 'ED25519) = H.Blake2b_512
  payloadHash _ = H.Blake2b_512
  importPublic _ bs = Ed25519.importPublic bs
  importPrivate _ bs = Ed25519.importPrivate bs
  exportPublic _ pub = Ed25519.exportPublic pub
  exportPrivate _ priv = Ed25519.exportPrivate priv
  exportSignature _ (Ed25519.Sig bs) = bs
  sign scheme msg priv pub =
    let (Hash hsh) = hashTx (payloadHash scheme) msg
    in Ed25519.sign hsh pub priv
  valid scheme msg pub sig =
     let (Hash hsh) = hashTx (payloadHash scheme) msg
     in Ed25519.valid hsh pub sig
  formatPublicKey _ p = Ed25519.exportPublic p
  genKey _ = do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return (s,p)


instance ToJSON ByteString where
  toJSON = String . decodeUtf8
instance FromJSON ByteString where
  parseJSON = withText "ByteString" (return . encodeUtf8)
instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText decodeUtf8
instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8


-- Ed25519 PublicKey, PrivateKey, and Signature instances
instance Eq Ed25519.PublicKey where
  b == b' = (exportPublic SED25519 b) == (exportPublic SED25519 b')
instance Ord Ed25519.PublicKey where
  b <= b' = (exportPublic SED25519 b) <= (exportPublic SED25519 b')
instance ToJSON Ed25519.PublicKey where
  toJSON = toB16JSON . (exportPublic SED25519)
instance FromJSON Ed25519.PublicKey where
  parseJSON = withText "PublicKey" parseText
  {-# INLINE parseJSON #-}
instance ParseText Ed25519.PublicKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("ED25519 Public Key import failed: " ++ show s)
      (importPublic SED25519 s')
  {-# INLINE parseText #-}
instance ToJSONKey Ed25519.PublicKey
instance FromJSONKey Ed25519.PublicKey
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (exportPublic SED25519 s)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (importPublic SED25519 <$> S.getByteString 32)


instance Eq Ed25519.PrivateKey where
  b == b' = (exportPrivate SED25519 b) == (exportPrivate SED25519 b')
instance Ord Ed25519.PrivateKey where
  b <= b' = (exportPrivate SED25519 b) <= (exportPrivate SED25519 b')
instance ToJSON Ed25519.PrivateKey where
  toJSON = toB16JSON . (exportPrivate SED25519)
instance FromJSON Ed25519.PrivateKey where
  parseJSON = withText "PrivateKey" parseText
  {-# INLINE parseJSON #-}
instance ParseText Ed25519.PrivateKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("ED25519 Private Key import failed: " ++ show s)
      (importPrivate SED25519 s')
  {-# INLINE parseText #-}
instance Serialize Ed25519.PrivateKey where
  put s = S.putByteString (exportPrivate SED25519 s)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (importPrivate SED25519 <$> S.getByteString 32)


deriving instance Eq Ed25519.Signature
deriving instance Ord Ed25519.Signature
instance Serialize Ed25519.Signature where
  put (Ed25519.Sig s) = S.put s
  get = Ed25519.Sig <$> (S.get >>= S.getByteString)
instance ToJSON Ed25519.Signature where
  toJSON (Ed25519.Sig s) = toB16JSON s
instance FromJSON Ed25519.Signature where
  parseJSON = withText "Signature" parseText
  {-# INLINE parseJSON #-}
instance ParseText Ed25519.Signature where
  parseText s = Ed25519.Sig <$> parseB16Text s
  {-# INLINE parseText #-}
