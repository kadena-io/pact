{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Pact.Core.Hash
( Hash(..)
, ModuleHash(..)
, hash
, hashToText
, verifyHash
, initialHash
, hashLength
, TypedHash(..)
, toUntypedHash, fromUntypedHash
, HashAlgo(..)
, PactHash
, pactHash
, pactInitialHash
, pactHashLength
, typedHashToText
, toB64UrlUnpaddedText
, fromB64UrlUnpaddedText
) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Reflection
import Data.Proxy
import Data.Word
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as T

import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

import Pact.Core.Pretty

-- | Untyped hash value, encoded with unpadded base64url.
-- Within Pact these are blake2b_256 but unvalidated as such,
-- so other hash values are kosher (such as an ETH sha256, etc).
newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, NFData)

instance Show Hash where
  show (Hash h) = show $ encodeBase64UrlUnpadded h

instance Pretty Hash where
  pretty (Hash h) =
    pretty $ decodeUtf8 (encodeBase64UrlUnpadded h)

hashToText :: Hash -> Text
hashToText (Hash h) = toB64UrlUnpaddedText h

-- | All supported hashes in Pact (although not necessarily GHCJS pact).
data HashAlgo =
  Blake2b_256 |
  SHA3_256
  deriving (Eq,Show,Ord,Bounded,Enum)

instance Reifies 'Blake2b_256 HashAlgo where
  reflect _ = Blake2b_256
instance Reifies 'SHA3_256 HashAlgo where
  reflect _ = SHA3_256

hashLength :: HashAlgo -> Int
hashLength Blake2b_256 = 32
hashLength SHA3_256 = 32

-- | Typed hash, to indicate algorithm
data TypedHash (h :: HashAlgo) where
  TypedHash :: ByteString -> TypedHash h
  deriving (Eq, Ord, Generic)

instance Show (TypedHash h) where
  show (TypedHash h) = show $ encodeBase64UrlUnpadded h

instance Pretty (TypedHash h) where
  pretty (TypedHash h) =
    pretty $ decodeUtf8 (encodeBase64UrlUnpadded h)

instance NFData (TypedHash h)


typedHashToText :: TypedHash h -> Text
typedHashToText (TypedHash h) = toB64UrlUnpaddedText h

toUntypedHash :: TypedHash h -> Hash
toUntypedHash (TypedHash h) = Hash h

fromUntypedHash :: Hash -> TypedHash h
fromUntypedHash (Hash h) = TypedHash h

type PactHash = TypedHash 'Blake2b_256

pactHash :: ByteString -> Hash
pactHash = toUntypedHash . (hash :: ByteString -> PactHash)

pactInitialHash :: Hash
pactInitialHash = toUntypedHash $ (initialHash :: PactHash)

pactHashLength :: Int
pactHashLength = hashLength Blake2b_256


hash :: forall h . Reifies h HashAlgo => ByteString -> TypedHash h
hash = TypedHash . go
  where
    algo = reflect (Proxy :: Proxy h)
    go = case algo of
      Blake2b_256 -> ByteArray.convert . Crypto.hashWith Crypto.Blake2b_256
      SHA3_256 -> ByteArray.convert . Crypto.hashWith Crypto.SHA3_256
{-# INLINE hash #-}

verifyHash :: Reifies h HashAlgo => TypedHash h -> ByteString -> Either String Hash
verifyHash h b = if hashed == h
  then Right (toUntypedHash h)
  else Left $ "Hash Mismatch, received " ++ renderCompactString h ++
              " but our hashing resulted in " ++ renderCompactString hashed
  where hashed = hash b
{-# INLINE verifyHash #-}

initialHash :: Reifies h HashAlgo => TypedHash h
initialHash = hash mempty

equalWord8 :: Word8
equalWord8 = toEnum $ fromEnum '='

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText  = decodeUtf8 . encodeBase64UrlUnpadded

encodeBase64UrlUnpadded :: ByteString -> ByteString
encodeBase64UrlUnpadded = fst . B.spanEnd (== equalWord8) . B64URL.encode

decodeBase64UrlUnpadded :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded = B64URL.decode . pad
  where pad t = let s = B.length t `mod` 4 in t <> B.replicate ((4 - s) `mod` 4) equalWord8

fromB64UrlUnpaddedText :: ByteString -> Either String Text
fromB64UrlUnpaddedText bs = case decodeBase64UrlUnpadded bs of
  Right bs' -> case T.decodeUtf8' bs' of
    Left _ -> Left $ "Base64URL decode failed: invalid unicode"
    Right t -> Right t
  Left e -> Left $ "Base64URL decode failed: " ++ e


newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show)
  deriving newtype (NFData)
