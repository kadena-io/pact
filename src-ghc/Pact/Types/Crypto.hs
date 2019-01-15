{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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
  , PPKScheme(..), SPPKScheme, SomeScheme(..)
  , InitScheme(..), Scheme(..), defaultScheme, formatPublicKey
  , KeyPair(..), SomeKeyPair(..), importKeyPair
  , PublicKeyText(..), PrivateKeyText(..), SignatureText(..)
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
import qualified Data.Text as T


import GHC.Generics
import Prelude

import Pact.Types.Util


--------- HASHING ---------

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




--------- SCHEME ---------

data PPKScheme = ED25519
  deriving (Show, Eq, Ord, Generic)


-- Run-time witness to PPKScheme kind
data SPPKScheme (a :: PPKScheme) = MakeScheme
data SomeScheme = SED25519 (SPPKScheme 'ED25519)




class InitScheme s where
  toScheme :: s -> SomeScheme



class (Export (PublicKey a), Export (Signature a),
       ParseText (PublicKey a), ParseText (Signature a), ParseText (PrivateKey a)) =>
      Scheme (a :: PPKScheme) where
  type PublicKey a
  type PrivateKey a
  type Signature a

  toPPKScheme :: SPPKScheme a -> PPKScheme
  hashPayload :: SPPKScheme a -> ByteString -> ByteString
  sign :: SPPKScheme a -> ByteString -> PublicKey a -> PrivateKey a -> Signature a
  valid :: SPPKScheme a -> ByteString -> PublicKey a -> Signature a -> Bool
  genKeyPair :: SPPKScheme a -> IO (KeyPair a)




--------- KEY PAIR ---------

newtype PublicKeyText = PubT { _pktPublic :: T.Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
newtype PrivateKeyText = PrivT { _pktSecret :: T.Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
newtype SignatureText = SigT T.Text deriving (Eq, Show, Generic)



data KeyPair (a :: PPKScheme) = KeyPair
  { _kpScheme :: SPPKScheme a
  , _kpPublicKey :: PublicKey a
  , _kpPrivateKey :: PrivateKey a
  }
data SomeKeyPair = forall a. (Scheme a) => SomeKeyPair (KeyPair a)



importKeyPair :: (Scheme a, Monad m) =>
                 SPPKScheme a -> PublicKeyText -> PrivateKeyText -> m (KeyPair a)
importKeyPair scheme (PubT pubText) (PrivT privText) = do
  pub <- failEither (fromText' pubText)
  priv <- failEither (fromText' privText)
  return $ KeyPair scheme pub priv




--------- SCHEME INSTANCES AND FUNCTIONS ---------

instance NFData PPKScheme
instance ToJSON PPKScheme where
  toJSON ED25519 = "ED25519"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize PPKScheme
instance InitScheme PPKScheme where
  toScheme ED25519 = SED25519 MakeScheme




defaultScheme :: SPPKScheme 'ED25519
defaultScheme = MakeScheme

formatPublicKey :: PPKScheme -> T.Text -> T.Text
formatPublicKey _ p = p




instance Scheme 'ED25519 where
  type PublicKey 'ED25519 = Ed25519.PublicKey
  type PrivateKey 'ED25519 = Ed25519.PrivateKey
  type Signature 'ED25519 = Ed25519.Signature

  toPPKScheme _ = ED25519
  hashPayload _ msg = hsh
    where Hash hsh = hashTx H.Blake2b_512 msg
  sign s msg pub priv = Ed25519.sign (hashPayload s msg) priv pub
  valid s msg pub sig = Ed25519.valid (hashPayload s msg) pub sig
  genKeyPair scheme = do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return $ KeyPair scheme p s




--------- BYTESTRING INSTANCES ---------

instance ToJSON ByteString where
  toJSON = String . decodeUtf8
instance FromJSON ByteString where
  parseJSON = withText "ByteString" (return . encodeUtf8)
instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText decodeUtf8
instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8




--------- ED25519 INSTANCES ---------

instance Eq Ed25519.PublicKey where
  b == b' = (export b) == (export b')
instance Ord Ed25519.PublicKey where
  b <= b' = (export b) <= (export b')
instance ParseText Ed25519.PublicKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("ED25519 Public Key import failed: " ++ show s)
      (Ed25519.importPublic s')
  {-# INLINE parseText #-}
instance Serialize Ed25519.PublicKey where
  put s = S.putByteString (export s)
  get = maybe (fail "Invalid ED25519 Public Key") return =<<
        (Ed25519.importPublic <$> S.getByteString 32)
instance Export (Ed25519.PublicKey) where
  export = Ed25519.exportPublic




instance Eq Ed25519.PrivateKey where
  b == b' = (export b) == (export b')
instance Ord Ed25519.PrivateKey where
  b <= b' = (export b) <= (export b')
instance ParseText Ed25519.PrivateKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("ED25519 Private Key import failed: " ++ show s)
      (Ed25519.importPrivate s')
  {-# INLINE parseText #-}
instance Serialize Ed25519.PrivateKey where
  put s = S.putByteString (export s)
  get = maybe (fail "Invalid ED25519 Private Key") return =<<
        (Ed25519.importPrivate <$> S.getByteString 32)
instance Export (Ed25519.PrivateKey) where
  export = Ed25519.exportPrivate




deriving instance Eq Ed25519.Signature
deriving instance Ord Ed25519.Signature
instance Serialize Ed25519.Signature where
  put (Ed25519.Sig s) = S.put s
  get = Ed25519.Sig <$> (S.get >>= S.getByteString)
instance ParseText Ed25519.Signature where
  parseText s = Ed25519.Sig <$> parseB16Text s
  {-# INLINE parseText #-}
instance Export Ed25519.Signature where
  export (Ed25519.Sig bs) = bs

