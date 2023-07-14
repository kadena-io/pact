{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pact.Types.Hash
  (
  -- Bytestring hashing
    Hash(..)
  , hash
  , hashToText
  , verifyHash
  , initialHash
  , hashLength
  ) where


import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Hashable (Hashable)

import Data.Serialize (Serialize(..))
import Pact.Types.Pretty
import Pact.Types.Util
import Data.Aeson
import GHC.Generics
import Pact.Types.SizeOf
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.QuickCheck
import Test.QuickCheck.Instances()

#if !defined(ghcjs_HOST_OS)

import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

#else

import Crypto.Hash.Blake2Native

#endif



-- | Untyped hash value, encoded with unpadded base64url.
-- Within Pact these are blake2b_256 but unvalidated as such.
newtype Hash = Hash { unHash :: ShortByteString }
  deriving (Eq, Ord, Generic, Hashable, Serialize,SizeOf)

instance Arbitrary Hash where
  arbitrary = hash <$> encodeUtf8 <$> resize 1000 arbitrary

instance Show Hash where
  show (Hash h) = show $ encodeBase64UrlUnpadded $ fromShort h

instance Pretty Hash where
  pretty = pretty . asString

instance AsString Hash where
  asString (Hash h) = decodeUtf8 (encodeBase64UrlUnpadded $ fromShort h)

instance NFData Hash

instance ToJSON Hash where
  toJSON = String . hashToText

instance ToJSONKey Hash

instance FromJSON Hash where
  parseJSON = withText "Hash" parseText
  {-# INLINE parseJSON #-}

instance ParseText Hash where
  parseText s = Hash . toShort <$> parseB64UrlUnpaddedText s
  {-# INLINE parseText #-}


hashToText :: Hash -> Text
hashToText (Hash h) = toB64UrlUnpaddedText $ fromShort h

hashLength :: Int
hashLength = 32

#if !defined(ghcjs_HOST_OS)

hash :: ByteString -> Hash
hash = Hash . toShort . ByteArray.convert . Crypto.hashWith Crypto.Blake2b_256
{-# INLINE hash #-}

#else

hash :: ByteString -> Hash h
hash bs = case blake2b (hashLength algo) mempty bs of
        Left _ -> error "hashing failed"
        Right h -> toShort h

#endif

verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hashed == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ renderCompactString h ++
              " but our hashing resulted in " ++ renderCompactString hashed
  where hashed = hash b
{-# INLINE verifyHash #-}


initialHash :: Hash
initialHash = hash mempty
