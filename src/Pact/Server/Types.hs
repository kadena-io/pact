{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pact.Server.Types
  ( RequestId(..)
  -- for simplicity, re-export some core types that we need all over the place
  , parseB16JSON, toB16JSON, toB16Text, parseB16Text, failMaybe
  , PublicKey, PrivateKey, Signature(..), sign, valid, importPublic, importPrivate, exportPublic
  , EncryptionKey(..)
  , Alias(..)
  , hash, hashLengthAsBS, hashLengthAsBase16
  , Hash(..), initialHash
  ) where

import Control.Lens
import Control.Monad (mzero)
import qualified Crypto.Ed25519.Pure as Ed25519
import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..)
                           , importPublic, importPrivate, exportPublic, exportPrivate)
import qualified Crypto.Hash.BLAKE2.BLAKE2bp as BLAKE


import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Data.String

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable (Hashable)

import GHC.Generics hiding (from)

import Pact.Types.Orphans ()

newtype Alias = Alias { unAlias :: BSC.ByteString }
  deriving (Eq, Ord, Generic, Serialize)
instance IsString Alias where fromString s = Alias $ BSC.pack s

instance Show Alias where
  show (Alias a) = BSC.unpack a

instance ToJSON Alias where
  toJSON = toJSON . decodeUtf8 . unAlias
instance FromJSON Alias where
  parseJSON (String s) = do
    return $ Alias $ encodeUtf8 s
  parseJSON _ = mzero

newtype RequestId = RequestId {_unRequestId :: String }
  deriving (Eq, Ord, Generic, Serialize, IsString, ToJSON, FromJSON)
instance Show RequestId where show (RequestId i) = i

-- NB: this hash is also used for the bloom filter, which needs 32bit keys
-- if you want to change this, you need to retool the bloom filter as well
-- So long as this is divisible by 4 you're fine
hashLengthAsBS :: Int
hashLengthAsBS = 32

hashLengthAsBase16 :: Int
hashLengthAsBase16 = hashLengthAsBS * 2

hash :: ByteString -> Hash
hash = Hash . BLAKE.hash hashLengthAsBS B.empty
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

parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
                 (s,leftovers) | leftovers == B.empty -> return s
                               | otherwise -> fail $ "Base16 decode failed: " ++ show t

toB16JSON :: ByteString -> Value
toB16JSON s = String $ toB16Text s

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

failMaybe :: Monad m => String -> Maybe a -> m a
failMaybe err m = maybe (fail err) return m

newtype EncryptionKey = EncryptionKey { unEncryptionKey :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)
instance ToJSON EncryptionKey where
  toJSON = toB16JSON . unEncryptionKey
instance FromJSON EncryptionKey where
  parseJSON s = EncryptionKey <$> parseB16JSON s

deriving instance Eq Signature
deriving instance Ord Signature
instance Serialize Signature where
  put (Sig s) = S.put s
  get = Sig <$> (S.get >>= S.getByteString)

valid :: Hash -> PublicKey -> Signature -> Bool
valid (Hash h) = Ed25519.valid h
{-# INLINE valid #-}

sign :: Hash -> PrivateKey -> PublicKey -> Signature
sign (Hash h) = Ed25519.sign h
{-# INLINE sign #-}

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

-- These instances suck, but I can't figure out how to use the Get monad to fail out if not
-- length = 32. For the record, if the getByteString 32 works the imports will not fail
instance Serialize PublicKey where
  put s = S.putByteString (exportPublic s)
  get = fromMaybe (error "Invalid PubKey") . importPublic <$> S.getByteString (32::Int)
instance Serialize PrivateKey where
  put s = S.putByteString (exportPrivate s)
  get = fromMaybe (error "Invalid PubKey") . importPrivate <$> S.getByteString (32::Int)

data Digest = Digest
  { _digNodeId :: !Alias
  , _digSig    :: !Signature
  , _digPubkey :: !PublicKey
  , _digHash   :: !Hash
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''Digest

instance Serialize Digest


data PactCommand = PactCommand
  { _pcDigest :: Di}
