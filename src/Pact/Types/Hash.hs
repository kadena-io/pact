{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  (
  -- Bytestring hashing
    hash
  , verifyHash
  , initialHash

  -- Integer hashing
  , basedStringToInteger
  , hexStringToInteger
  ) where


import Prelude hiding (null)

import Data.Char (digitToInt)
import Data.ByteString (ByteString)
import Data.Text (Text, append, foldl', null)

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

-- | Reads 'Hash' as a non-negative 'Integral' number using the base <= 16
-- specified by the first argument
basedStringToInteger
  :: Integer -- ^ The base specification
  -> Text -- ^ The string to convert to integral base-a
  -> Either Text Integer
basedStringToInteger base h 
  | base <= 1 || base >= 16 = Left $
    "readStringAtBase: applied to unsupported base - " `append` asString base
  | null h = Left $
    "readStringAtBase: applied to empty hash - " `append` asString h
  | otherwise = Right $ foldl' go 0 h
    where
      go :: Integer -> Char -> Integer
      go acc w = base * acc + (fromIntegral . digitToInt $ w) 
{-# INLINE basedStringToInteger #-}

-- | Computes the integer value of a hexadecimal string by lensing
-- through text and converting all the chars into their corresponding
-- hexadecimal values. Equivalent to `foldl' (*) 1`, but with the
-- correct mapping of [(a, 10)..(f, 15)]
hexStringToInteger
  :: Text
  -> Either Text Integer
hexStringToInteger =
  basedStringToInteger 16 
{-# INLINE hexStringToInteger #-}

