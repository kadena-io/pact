{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  ( hash
  , verifyHash
  , initialHash
  , numericBasedHash
  , binaryHash
  , octalHash
  , hexidecimalHash
  ) where

import Prelude hiding (null)

import Data.ByteString (ByteString)
import Data.Char (ord, toUpper)
import Data.Text (Text, append, null, foldl') 

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
  :: Integer
  -> ByteString
  -> Either Text Integer
numericBasedHash base =
    readStringAtBase base toBase . asString . hash 
  where
    toBase :: Char -> Integer
    toBase = (`mod` base) . fromIntegral . ord . toUpper
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
hexidecimalHash
  :: ByteString -- ^ value to hash and compute in hex
  -> Either Text Integer
hexidecimalHash = numericBasedHash 16
{-# INLINE hexidecimalHash #-}

-- | Reads 'String' as a non-negative 'Integral' number using the base
-- specified by the first argument, and character representation
-- specified by the second argument
readStringAtBase
  :: Integer -- ^ The base specification
  -> (Char -> Integer) -- ^ the a-valued representation for a given character
  -> Text -- ^ The string to convert to integral base-a
  -> Either Text Integer
readStringAtBase base rChr txt
  | base <= 1 = Left ("readStringAtBase: applied to unsupported base - " `append` asString base)
  | null txt = Left ("readStringAtBase: applied to empty string - " `append`  txt)
  | otherwise = Right $ foldl' go 0 txt
  where
    go :: Integer -> Char -> Integer
    go acc c = base*acc + (rChr c)
{-# INLINE readStringAtBase #-}
