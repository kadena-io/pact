{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( PublicKey, importPublic, exportPublic, fromTextPublic
  , PrivateKey, importPrivate, exportPrivate, fromTextPrivate
  , Signature(..), exportSignature
  , genKeyPair, KeyPair, importKeyPair, exportKeyPair
  , hashTx, verifyHashTx, initialHashTx, sign, valid
  , PPKScheme(..), SignatureAlgo(..), HashAlgo(..), AddressFormat(..)
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

import Data.Default (Default, def)
import Data.ByteString (ByteString)
import qualified Data.ByteArray as B
import Data.Serialize as SZ
import qualified Data.Serialize as S
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding

import GHC.Generics
import Prelude

import Pact.Types.Util


newtype Signature = Signature ByteString
  deriving (Show, Eq, Ord)
instance Serialize Signature where
  put (Signature s) = S.put s
  get = Signature <$> (S.get >>= S.getByteString)
instance ToJSON Signature where
  toJSON (Signature s) = toB16JSON s
instance FromJSON Signature where
  parseJSON = withText "Signature" parseText
  {-# INLINE parseJSON #-}
instance ParseText Signature where
  parseText s = Signature <$> parseB16Text s
  {-# INLINE parseText #-}

exportSignature :: Signature -> ByteString
exportSignature (Signature s) = s


{-- A PPK Scheme indicates
      1. How to format blockchain addresses
      2. The hash algorithm used to hash the transaction payload. This hash algorithm
         is also used in the signing/verification phase of certain signature
         algorithms (i.e. ECDSA).
      3. The signature algorithm used in signing transactions (i.e. ED25519, ECDSA)
--}
newtype PPKScheme = PPKScheme (AddressFormat, HashAlgo, SignatureAlgo)
  deriving (Show, Eq, Ord, Generic)

instance NFData PPKScheme
-- default instance with only one value is empty array!!
instance ToJSON PPKScheme where
  toJSON (PPKScheme (addrFormat, hashAlgo, sigAlgo)) =
    A.object ["address" .= (toJSON addrFormat),
              "hash" .= (toJSON hashAlgo),
              "signature" .= (toJSON sigAlgo)]
instance FromJSON PPKScheme where
  parseJSON = A.withObject "PPKScheme" $ \o -> do
    af <- (o .: "address") >>= parseJSON
    ha <- (o .: "hash") >>= parseJSON
    sa <- (o .: "signature") >>= parseJSON
    return (PPKScheme (af, ha, sa))
instance Serialize PPKScheme
instance Default PPKScheme where
  def = PPKScheme (def, def, def)


data AddressFormat = Chainweb
  deriving (Show, Eq, Ord, Generic)

instance NFData AddressFormat
-- default instance with only one value is empty array!!
instance ToJSON AddressFormat where
  toJSON Chainweb = "Chainweb"
instance FromJSON AddressFormat where
  parseJSON = withText "AddressFormat" $ \s -> case s of
    "Chainweb" -> return Chainweb
    _ -> fail $ "Unsupported Blockchain Address Format: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize AddressFormat
instance Default AddressFormat where  def = Chainweb


data HashAlgo = Blake2b_512
  deriving (Show, Eq, Ord, Generic)

instance NFData HashAlgo
-- default instance with only one value is empty array!!
instance ToJSON HashAlgo where
  toJSON Blake2b_512 = "Blake2b_512"
instance FromJSON HashAlgo where
  parseJSON = withText "HashAlgo" $ \s -> case s of
    "Blake2b_512" -> return Blake2b_512
    _ -> fail $ "Unsupported Hash Algorithm: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize HashAlgo
instance Default HashAlgo where  def = Blake2b_512


data SignatureAlgo = ED25519
  deriving (Show, Eq, Ord, Generic)

instance NFData SignatureAlgo
-- default instance with only one value is empty array!!
instance ToJSON SignatureAlgo where
  toJSON ED25519 = "ED25519"
instance FromJSON SignatureAlgo where
  parseJSON = withText "SignatureAlgo" $ \s -> case s of
    "ED25519" -> return ED25519
    _ -> fail $ "Unsupported Signature Algorithm: " ++ show s
  {-# INLINE parseJSON #-}
instance Serialize SignatureAlgo
instance Default SignatureAlgo where  def = ED25519



hashTx :: HashAlgo -> ByteString -> Hash
hashTx algo b = case algo of
  Blake2b_512 -> (Hash . B.convert . H.hashWith H.Blake2b_512) b

verifyHashTx :: HashAlgo -> Hash -> ByteString -> Either String Hash
verifyHashTx algo h b = if hashTx algo b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h
       ++ " but our hashing resulted in " ++ show (hashTx algo b)
{-# INLINE verifyHashTx #-}

initialHashTx :: HashAlgo -> Hash
initialHashTx algo = hashTx algo mempty

sign :: ByteString -> KeyPair -> Maybe Signature
sign msg keys =
  let (scheme,priv,pub) = exportKeyPair keys
      (PPKScheme (_, hashAlgo, sigAlgo)) = scheme
      (Hash hsh) = hashTx hashAlgo msg
  in case (pub, priv, sigAlgo) of
    (PubEd edPub, PrivEd edPriv, ED25519) ->
      let (Ed25519.Sig b) = Ed25519.sign hsh edPriv edPub in
      Just (Signature b)
    _ -> Nothing -- TODO

valid :: PPKScheme -> ByteString -> PublicKey -> Signature -> Bool
valid (PPKScheme (_, hashAlgo, sigAlgo)) msg pubKey sig =
  let (Hash hsh) = hashTx hashAlgo msg in
  case (pubKey, sigAlgo) of
    (PubEd edPub, ED25519) -> Ed25519.valid hsh edPub (Ed25519.Sig $ exportSignature sig)
    _ -> False -- TODO


data PublicKey = PubEd Ed25519.PublicKey
  deriving (Show)
data PrivateKey = PrivEd Ed25519.PrivateKey
  deriving (Show)

data KeyPair = MakeKeyPair PPKScheme PrivateKey PublicKey
  deriving (Show)

importPublic :: SignatureAlgo -> ByteString -> Maybe PublicKey
importPublic algo b = case algo of
  ED25519 -> PubEd <$> (Ed25519.importPublic b)

importPrivate :: SignatureAlgo -> ByteString -> Maybe PrivateKey
importPrivate algo b = case algo of
  ED25519 -> PrivEd <$> (Ed25519.importPrivate b)

importKeyPair :: PPKScheme -> PublicKey -> PrivateKey -> Maybe KeyPair
importKeyPair scheme pub priv =
  let (sigPub, _) = exportPublic pub
      (sigPriv, _) = exportPrivate priv
      (PPKScheme (_, _, sigAlgo)) = scheme
  in if (sigPub == sigPriv) && (sigPub == sigAlgo)
     then Just $ MakeKeyPair scheme priv pub
     else Nothing


exportPublic :: PublicKey -> (SignatureAlgo, ByteString)
exportPublic pub = case pub of
  PubEd pe -> (ED25519, Ed25519.exportPublic pe)

exportPrivate :: PrivateKey -> (SignatureAlgo, ByteString)
exportPrivate priv = case priv of
  PrivEd pe -> (ED25519, Ed25519.exportPrivate pe)

exportKeyPair :: KeyPair -> (PPKScheme, PrivateKey, PublicKey)
exportKeyPair pair = case pair of
  MakeKeyPair edScheme edPriv edPub  -> (edScheme, edPriv, edPub)


fromTextPublic :: SignatureAlgo -> Text -> Either String PublicKey
fromTextPublic sigAlgo t = do
  bs <- fromTextWith' parseB16Text t
  failMaybe (show sigAlgo ++ " Public key import failed: " ++ show bs)
              (importPublic sigAlgo bs)

fromTextPrivate :: SignatureAlgo -> Text -> Either String PrivateKey
fromTextPrivate sigAlgo t = do
  bs <- fromTextWith' parseB16Text t
  failMaybe (show sigAlgo ++ " Private key import failed: " ++ show bs)
              (importPrivate sigAlgo bs)


genKeyPair :: SignatureAlgo -> IO (PrivateKey, PublicKey)
genKeyPair sigAlgo = case sigAlgo of
  ED25519 -> do
    g :: SystemRandom <- newGenIO
    case Ed25519.generateKeyPair g of
      Left _ -> error "Something went wrong in genKeyPairs"
      Right (s,p,_) -> return $ ((PrivEd s), (PubEd p))


instance Eq PublicKey where
  b == b' = exportPublic b == exportPublic b'
instance Ord PublicKey where
  b <= b' = exportPublic b <= exportPublic b'
instance ToJSON PublicKey where
  toJSON pub = A.object ["type" .= (toJSON typ), "key" .= (toB16JSON key)]
    where (typ, key) = exportPublic pub
instance FromJSON PublicKey where
  parseJSON = A.withObject "PublicKey" $ \o -> do
    typ <- (o .: "type") >>= parseJSON
    key <- (o .: "key")
    failEither $ fromTextPublic typ key
instance ToJSONKey PublicKey
instance FromJSONKey PublicKey

instance Eq PrivateKey where
  b == b' = exportPrivate b == exportPrivate b'
instance Ord PrivateKey where
  b <= b' = exportPrivate b <= exportPrivate b'
instance ToJSON PrivateKey where
  toJSON priv = A.object ["type" .= (toJSON typ), "key" .= (toB16JSON key)]
    where (typ, key) = exportPrivate priv
instance FromJSON PrivateKey where
  parseJSON = A.withObject "PrivateKey" $ \o -> do
    typ <- (o .: "type") >>= parseJSON
    key <- (o .: "key")
    failEither $ fromTextPrivate typ key


instance Serialize PublicKey where
  put s = S.put algo >> S.putByteString b
    where (algo, b) = exportPublic s
  get = maybe (fail "Invalid Public Key") return =<< (mkPub <$> pubTuple)
    where pubTuple = getTwoOf get (S.getByteString 32)
          mkPub (a', b') = importPublic a' b'
instance Serialize PrivateKey where
  put s =  S.put algo >> S.putByteString b
    where (algo, b) = exportPrivate s
  get = maybe (fail "Invalid Private Key") return =<< (mkPriv <$> privTuple)
    where privTuple = getTwoOf get (S.getByteString 32)
          mkPriv (a', b') = importPrivate a' b'


instance ToJSON ByteString where
  toJSON = String . decodeUtf8
instance FromJSON ByteString where
  parseJSON = withText "ByteString" (return . encodeUtf8)
instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText decodeUtf8
instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8
