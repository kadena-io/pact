{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  ( hash
  , verifyHash
  , initialHash
  , numericBasedHash
  , binaryHash
  , octalHash
  , hexadecimalHash
  , hexStringToInteger
  ) where


import Prelude hiding (null)

import Control.Lens (prism')

import Data.ByteString (ByteString, null, foldl')
import Data.Digit (integralHexadecimal, charHeXaDeCiMaL)
import Data.Text (Text, append)
import Data.Text.Lens (text)
import Data.Word (Word8)

import Pact.Types.Util


#if !defined(ghcjs_HOST_OS)

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE

hash :: ByteString -> Hash
hash = Hash . BLAKE.hash hashLengthAsBS mempty
{-# INLINE hash #-}

#else

import Crypto.Hash.Blake2Native

hash :: ByteString -> Hash
hash bs = let (Right h) = blake2b hashLengthAsBS mempty bs in Hash h

#endif


verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hash b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash b)
{-# INLINE verifyHash #-}


initialHash :: Hash
initialHash = hash mempty

numericBasedHash
  :: Integer -- ^ base a
  -> ByteString -- ^ ByteString to hash
  -> Either Text Integer -- ^ Left : Err, Right : Success
numericBasedHash base =
    hashAsBasedInteger base toBase 
  where
    toBase :: Word8 -> Integer
    toBase = (`mod` base) . fromIntegral 
{-# INLINE numericBasedHash #-}


-- | Compute the octal value of a BLAKE2 hashed bytestring
octalHash
  :: ByteString -- ^ value to hash and compute in octal
  -> Either Text Integer -- ^ Left if given malformed input, Right if success
octalHash = numericBasedHash 8
{-# INLINE octalHash #-}

-- | Compute the binary value of BLAKE2 hashed bytestring
binaryHash
  :: ByteString -- ^ value to hash and compute in binary
  -> Either Text Integer -- ^ Left if malformed input, Right if success
binaryHash = numericBasedHash 2
{-# INLINE binaryHash #-}

-- | Compute the hexidecimal value of a BLAKE2 hashed bytestring
hexadecimalHash
  :: ByteString -- ^ value to hash and compute in hex
  -> Either Text Integer
hexadecimalHash = numericBasedHash 16
{-# INLINE hexadecimalHash #-}

-- | Reads 'Hash' as a non-negative 'Integral' number using the base
-- specified by the first argument, and character representation
-- specified by the second argument
hashAsBasedInteger
  :: Integer -- ^ The base specification
  -> (Word8 -> Integer) -- ^ the a-valued representation for a given character
  -> ByteString -- ^ The string to convert to integral base-a
  -> Either Text Integer
hashAsBasedInteger base k h 
  | base <= 1 = Left $
    "readStringAtBase: applied to unsupported base - " `append` asString base
  | null h = Left $
    "readStringAtBase: applied to empty hash - " `append` asString h
  | otherwise = Right $ foldl' go 0 h
    where
      go :: Integer -> Word8 -> Integer
      go acc w = base * acc + (k w) 
{-# INLINE hashAsBasedInteger #-}

-- | Computes the integer value of a hexadecimal string
hexStringToInteger
  :: Text
  -> Maybe Integer
hexStringToInteger = text
  
    
