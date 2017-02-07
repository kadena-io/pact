{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Types.Crypto
  ( PublicKey, importPublic, exportPublic
  , PrivateKey, importPrivate, exportPrivate
  , Signature(..), exportSignature
  , sign, valid, verifyHash
  , PPKScheme(..)
  , Hash(..), initialHash, hash, hashLengthAsBS, hashLengthAsBase16
  ) where

import Control.Applicative
import Control.Monad.Reader

import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), importPublic, importPrivate, exportPublic, exportPrivate)
import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE

import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Serialize as SZ hiding (get)
import qualified Data.Serialize as S
import Data.Text hiding (drop, toLower)
import Data.Text.Encoding
import Data.String
import Data.Maybe

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
  parseJSON s = Sig <$> parseB16JSON s

exportSignature :: Signature -> ByteString
exportSignature (Sig s) = s

data PPKScheme = ED25519
  deriving (Show, Eq, Ord, Generic)
-- default instance with only one value is empty array!!
instance ToJSON PPKScheme where toJSON ED25519 = "ED25519"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
instance Serialize PPKScheme

instance Serialize Text where
  put = S.put . encodeUtf8
  get = decodeUtf8 <$> S.get

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

instance ToJSON Hash where
  toJSON (Hash h) = toB16JSON h
instance FromJSON Hash where
  parseJSON s = Hash <$> parseB16JSON s

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
  parseJSON s = do
    s' <- parseB16JSON s
    failMaybe ("Public key import failed: " ++ show s) $ importPublic s'

instance (ToJSON k,ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSON . Map.toList
instance (FromJSON k,Ord k,FromJSON v) => FromJSON (Map k v) where
  parseJSON = fmap Map.fromList . parseJSON

instance Eq PrivateKey where
  b == b' = exportPrivate b == exportPrivate b'
instance Ord PrivateKey where
  b <= b' = exportPrivate b <= exportPrivate b'
instance ToJSON PrivateKey where
  toJSON = toB16JSON . exportPrivate
instance FromJSON PrivateKey where
  parseJSON s = do
    s' <- parseB16JSON s
    failMaybe ("Private key import failed: " ++ show s) $ importPrivate s'

instance Serialize PublicKey where
  put s = S.putByteString (exportPublic s)
  get = maybe (fail "Invalid PubKey") return =<< (importPublic <$> S.getByteString 32)
instance Serialize PrivateKey where
  put s = S.putByteString (exportPrivate s)
  get = maybe (fail "Invalid PubKey") return =<< (importPrivate <$> S.getByteString 32)
