{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}


module Pact.Types.Scheme
  ( PPKScheme(..)
  , SPPKScheme(..)
  , ConvertBS(..)
  , Scheme(..)
  ) where



import GHC.Generics
import Control.DeepSeq
import Data.Serialize
import Data.ByteString  (ByteString)
import Data.Aeson



--------- PPKSCHEME DATA TYPE ---------

data PPKScheme = ED25519 | ETH
  deriving (Show, Eq, Ord, Generic)


instance NFData PPKScheme
instance Serialize PPKScheme
instance ToJSON PPKScheme where
  toJSON ED25519 = "ED25519"
  toJSON ETH = "ETH"
instance FromJSON PPKScheme where
  parseJSON = withText "PPKScheme" $ \s -> case s of
    "ED25519" -> return ED25519
    "ETH" -> return ETH
    _ -> fail $ "Unsupported PPKScheme: " ++ show s
  {-# INLINE parseJSON #-}




-- Run-time witness to PPKScheme kind.

data SPPKScheme :: PPKScheme -> * where
  SED25519 :: SPPKScheme 'ED25519
  SETH :: SPPKScheme 'ETH




--------- INTERNAL SCHEME CLASS ---------

class ConvertBS a where
  toBS :: a -> ByteString
  fromBS :: ByteString -> Either String a




-- Scheme class created to enforce at the type level that Public Key,
-- Private Key, and Signature are of the same scheme when signing
-- and validating.

-- Also ensures that each scheme specifies its payload hash and
-- how it will transform its public keys to Pact Runtime public keys.

class (ConvertBS (PublicKey a),
       ConvertBS (PrivateKey a),
       ConvertBS (Signature a))  =>
      Scheme a where

  type PublicKey a
  type PrivateKey a
  type Signature a

  _hashPayload :: a -> ByteString -> ByteString
  _sign :: a -> ByteString -> PublicKey a -> PrivateKey a -> IO (Signature a)
  _valid :: a -> ByteString -> PublicKey a -> Signature a -> Bool
  _genKeyPair :: a -> IO (PublicKey a, PrivateKey a)


  -- Converts Cryptographic public keys to Pact Runtime public keys depending on the scheme.
  -- Pact uses Cryptographic PKs to sign/validate transactions.
  -- While it uses Runtime PKs during keyset enforcement to denote ownership and permissions.
  -- Certain schemes (i.e. Ethereum) call Runtime public keys 'addresses'.

  _formatPublicKey :: a -> PublicKey a -> ByteString
