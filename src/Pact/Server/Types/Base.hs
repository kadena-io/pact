{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Server.Types.Base
  ( parseB16JSON, toB16JSON, toB16Text, parseB16Text, failMaybe
  , PublicKey, PrivateKey, Signature(..), sign, valid, importPublic, importPrivate, exportPublic
  , verifyUserSig, verifyHash
  , UserName(..)
  , PPKScheme(..)
  , UserSig(..), usUserName, usScheme, usPubKey, usSig
  , hash, hashLengthAsBS, hashLengthAsBase16
  , Hash(..), initialHash
  , CommandEntry(..)
  , CommandResult(..)
  , RequestKey(..), initialRequestKey
  , RequestId(..)
  , lensyConstructorToNiceJson
  ) where

import Control.Lens
import Control.Applicative
import Control.Monad.Reader

import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), importPublic, importPrivate, exportPublic, exportPrivate)
import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Crypto.Hash.BLAKE2.BLAKE2bp as BLAKE

import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Data.Aeson
import Data.Aeson.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Serialize as SZ hiding (get)
import qualified Data.Serialize as S
import Data.Text hiding (drop, toLower)
import Data.Text.Encoding
import Data.Char (toLower)

import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error "You've managed to screw up the drop number or the field name"

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

deriving instance Eq Signature
deriving instance Ord Signature
instance Serialize Signature where
  put (Sig s) = S.put s
  get = Sig <$> (S.get >>= S.getByteString)
instance ToJSON Signature where
  toJSON (Sig s) = toB16JSON s
instance FromJSON Signature where
  parseJSON s = Sig <$> parseB16JSON s

data PPKScheme = ED25519
  deriving (Show, Eq, Ord, Generic, Serialize, ToJSON, FromJSON)

newtype RequestId = RequestId { unRequestId :: Text } deriving (Show, Eq, Ord, Generic)

instance Serialize RequestId where
  put = put . encodeUtf8 . unRequestId
  get = RequestId . decodeUtf8 <$> S.get
instance ToJSON RequestId where
  toJSON (RequestId u) = String u
instance FromJSON RequestId where
  parseJSON (String u) = return $ RequestId u
  parseJSON _ = mzero

newtype UserName = UserName { unUserName :: Text } deriving (Show, Eq, Ord, Generic)

instance Serialize UserName where
  put = put . encodeUtf8 . unUserName
  get = UserName . decodeUtf8 <$> S.get
instance ToJSON UserName where
  toJSON (UserName u) = String u
instance FromJSON UserName where
  parseJSON (String u) = return $ UserName u
  parseJSON _ = mzero

data UserSig = UserSig
  { _usUserName :: !UserName
  , _usScheme :: !PPKScheme
  , _usPubKey :: !PublicKey
  , _usSig :: !Signature }
  deriving (Eq, Ord, Show, Generic)

instance Serialize UserSig
instance ToJSON UserSig where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = lensyConstructorToNiceJson 3})
instance FromJSON UserSig

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

valid :: Hash -> PublicKey -> Signature -> Bool
valid (Hash h) = Ed25519.valid h
{-# INLINE valid #-}

sign :: Hash -> PrivateKey -> PublicKey -> Signature
sign (Hash h) = Ed25519.sign h
{-# INLINE sign #-}

verifyUserSig :: Hash -> UserSig -> Bool
verifyUserSig h UserSig{..} = case _usScheme of
  ED25519 -> valid h _usPubKey _usSig
{-# INLINE verifyUserSig #-}

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

-- These instances suck, but I can't figure out how to use the Get monad to fail out if not
-- length = 32. For the record, if the getByteString 32 works the imports will not fail
instance Serialize PublicKey where
  put s = S.putByteString (exportPublic s)
  get = fromMaybe (error "Invalid PubKey") . importPublic <$> S.getByteString (32::Int)
instance Serialize PrivateKey where
  put s = S.putByteString (exportPrivate s)
  get = fromMaybe (error "Invalid PubKey") . importPrivate <$> S.getByteString (32::Int)

newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)
instance ToJSON CommandResult where
  toJSON (CommandResult a) = toJSON $ decodeUtf8 a
instance FromJSON CommandResult where
  parseJSON (String t) = return $ CommandResult (encodeUtf8 t)
  parseJSON _ = mempty

newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Serialize, Hashable)

instance Show RequestKey where
  show (RequestKey rk) = show rk

initialRequestKey :: RequestKey
initialRequestKey = RequestKey initialHash

makeLenses ''UserSig
