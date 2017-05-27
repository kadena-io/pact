{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Crypto
-- Copyright   :  (C) 2016 Stuart Popejoy, William Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, William Martino <will@kadena.io>
--
-- PPK and hashing types.
--
module Pact.Types.Crypto
  ( PublicKey, importPublic, exportPublic
  , PrivateKey, importPrivate, exportPrivate
  , Signature(..), exportSignature
  , sign, valid, verifyHash
  , PPKScheme(..)
  , Hash(..), initialHash, hash, hashLengthAsBS, hashLengthAsBase16, hashToB16Text
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.DeepSeq

import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), importPublic, importPrivate, exportPublic, exportPrivate)
import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE

import Data.Hashable (Hashable)

import Data.Aeson as A
import Data.Aeson.Types (toJSONKeyText)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Serialize as SZ hiding (get)
import qualified Data.Serialize as S
import Data.String
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding

import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

import Pact.Types.Util

deriving instance Eq Signature
deriving instance Ord Signature
instance Serialize Signature where
  put (Sig s) = S.put s
  get = Sig <$> (S.get >>= S.getByteString)
instance ToJSON Signature where
  toJSON (Sig s) = toB16JSON s
instance FromJSON Signature where
  parseJSON = withText "Signature" parseText
  {-# INLINE parseJSON #-}
instance ParseText Signature where
  parseText s = Sig <$> parseB16Text s
  {-# INLINE parseText #-}

exportSignature :: Signature -> ByteString
exportSignature (Sig s) = s

data PPKScheme = ED25519
  deriving (Show, Eq, Ord, Generic)

instance NFData PPKScheme
-- default instance with only one value is empty array!!
instance ToJSON PPKScheme where toJSON ED25519 = "ED25519"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize PPKScheme

-- NB: this hash is also used for the bloom filter, which needs 32bit keys
-- if you want to change this, you need to retool the bloom filter as well
-- So long as this is divisible by 4 you're fine
hashLengthAsBS :: Int
hashLengthAsBS = 64

hashLengthAsBase16 :: Int
hashLengthAsBase16 = hashLengthAsBS * 2

hash :: ByteString -> Hash
hash = Hash . BLAKE.hash hashLengthAsBS mempty
{-# INLINE hash #-}

newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic, Hashable)
instance Show Hash where
  show (Hash h) = show $ B16.encode h
instance NFData Hash

initialHash :: Hash
initialHash = hash B.empty

instance Serialize Hash where
  put (Hash h) = S.put h
  get = do
    raw <- S.get >>= S.getByteString
    if hashLengthAsBS == B.length raw
      then return $ Hash raw
      else fail $ "Unable to decode hash, wrong length: "
                ++ show (B.length raw)
                ++ " from original bytestring " ++ show raw

hashToB16Text :: Hash -> Text
hashToB16Text (Hash h) = toB16Text h

instance ToJSON Hash where
  toJSON = String . hashToB16Text
instance FromJSON Hash where
  parseJSON = withText "Hash" parseText
  {-# INLINE parseJSON #-}
instance ParseText Hash where
  parseText s = Hash <$> parseB16Text s
  {-# INLINE parseText #-}

valid :: Hash -> PublicKey -> Signature -> Bool
valid (Hash h) = Ed25519.valid h
{-# INLINE valid #-}

sign :: Hash -> PrivateKey -> PublicKey -> Signature
sign (Hash h) = Ed25519.sign h
{-# INLINE sign #-}


verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hash b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash b)
{-# INLINE verifyHash #-}

instance Eq PublicKey where
  b == b' = exportPublic b == exportPublic b'
instance Ord PublicKey where
  b <= b' = exportPublic b <= exportPublic b'
instance ToJSON PublicKey where
  toJSON = toB16JSON . exportPublic
instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" parseText
  {-# INLINE parseJSON #-}
instance ParseText PublicKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("Public key import failed: " ++ show s) $ importPublic s'
  {-# INLINE parseText #-}

instance Eq PrivateKey where
  b == b' = exportPrivate b == exportPrivate b'
instance Ord PrivateKey where
  b <= b' = exportPrivate b <= exportPrivate b'
instance ToJSON PrivateKey where
  toJSON = toB16JSON . exportPrivate
instance FromJSON PrivateKey where
  parseJSON = withText "PrivateKey" parseText
  {-# INLINE parseJSON #-}
instance ToJSONKey PublicKey
instance FromJSONKey PublicKey
instance ParseText PrivateKey where
  parseText s = do
    s' <- parseB16Text s
    failMaybe ("Private key import failed: " ++ show s) $ importPrivate s'
  {-# INLINE parseText #-}

instance Serialize PublicKey where
  put s = S.putByteString (exportPublic s)
  get = maybe (fail "Invalid PubKey") return =<< (importPublic <$> S.getByteString 32)
instance Serialize PrivateKey where
  put s = S.putByteString (exportPrivate s)
  get = maybe (fail "Invalid PubKey") return =<< (importPrivate <$> S.getByteString 32)



instance ToJSON ByteString where
  toJSON = String . decodeUtf8
instance FromJSON ByteString where
  parseJSON = withText "ByteString" (return . encodeUtf8)
instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText decodeUtf8
instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8
