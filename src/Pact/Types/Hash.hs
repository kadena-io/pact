{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  (
  -- Bytestring hashing
    hash
  , verifyHash
  , initialHash

  -- Integer hashing
  , hashAsBasedInteger
  , decimalStringToInteger
  , binaryStringToInteger
  , octalStringToInteger
  , hexStringToInteger
  ) where


import Prelude hiding (null)

import Data.Char (digitToInt)
import Data.ByteString (ByteString)
import Data.Text (Text, append, foldl', null)

import Pact.Types.Util


#if !defined(ghcjs_HOST_OS)

import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

hash :: (Crypto.HashAlgorithm a) => a -> ByteString -> Hash
hash algo = Hash . ByteArray.convert . Crypto.hashWith algo
{-# INLINE hash #-}

verifyHash :: (Crypto.HashAlgorithm a) => a -> Hash -> ByteString -> Either String Hash
verifyHash algo h b = if hash algo b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash algo b)
{-# INLINE verifyHash #-}


initialHash :: (Crypto.HashAlgorithm a) => a -> Hash
initialHash algo = hash algo mempty

#else
-- TODO add support for ETH/BTC hashing algorithm
import Crypto.Hash.Blake2Native

hash :: ByteString -> Hash
hash bs = case blake2b hashLengthAsBS mempty bs of
  Left _ -> error "hashing failed"
  Right h -> Hash h

verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hash b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash b)
{-# INLINE verifyHash #-}

initialHash :: Hash
initialHash = hash mempty

#endif


-- | Reads 'Hash' as a non-negative 'Integral' number using the base
-- specified by the first argument, and character representation
-- specified by the second argument
hashAsBasedInteger
  :: Integer -- ^ The base specification
  -> (Char -> Integer) -- ^ the a-valued representation for a given character
  -> Text -- ^ The string to convert to integral base-a
  -> Either Text Integer
hashAsBasedInteger base k h
  | base <= 1 = Left $
    "readStringAtBase: applied to unsupported base - " `append` asString base
  | null h = Left $
    "readStringAtBase: applied to empty hash - " `append` asString h
  | otherwise = Right $ foldl' go 0 h
    where
      go :: Integer -> Char -> Integer
      go acc w = base * acc + (k w) 
{-# INLINE hashAsBasedInteger #-}

-- | Computes the integer value of a hexadecimal string by lensing
-- through text and converting all the chars into their corresponding
-- hexadecimal values. Equivalent to `foldl' (*) 1`, but with the
-- correct mapping of [(a, 10)..(f, 15)]
hexStringToInteger
  :: Text
  -> Either Text Integer
hexStringToInteger =
  hashAsBasedInteger 16 (fromIntegral . digitToInt)
{-# INLINE hexStringToInteger #-}

decimalStringToInteger
  :: Text
  -> Either Text Integer
decimalStringToInteger =
  hashAsBasedInteger 10 (fromIntegral . digitToInt)
{-# INLINE decimalStringToInteger #-}

octalStringToInteger
  :: Text
  -> Either Text Integer
octalStringToInteger =
  hashAsBasedInteger 8 (fromIntegral . digitToInt)
{-# INLINE octalStringToInteger #-}

binaryStringToInteger
  :: Text
  -> Either Text Integer
binaryStringToInteger =
  hashAsBasedInteger 2 (fromIntegral . digitToInt)
{-# INLINE binaryStringToInteger #-}
