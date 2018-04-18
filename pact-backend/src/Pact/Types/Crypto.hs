{-# LANGUAGE FlexibleContexts #-}
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
  , sign, valid
  , PPKScheme(..)
  , Hash(..), initialHash, hashLengthAsBS, hashLengthAsBase16, hashToB16Text
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.DeepSeq

import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), importPublic, importPrivate, exportPublic, exportPrivate)
import qualified Crypto.Ed25519.Pure as Ed25519

import Data.Aeson as A
import Data.Aeson.Types (toJSONKeyText)

import Data.ByteString (ByteString)
import Data.Serialize as SZ
import qualified Data.Serialize as S
import Data.Maybe
import Data.Text.Encoding

import GHC.Generics
import Prelude

import Pact.Types.Util
import Pact.Types.Hash

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
