{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  (
  -- Bytestring hashing
    hash
  , verifyHash
  , initialHash

  -- Integer hashing
  , numericBasedHash
  , binaryHash
  , octalHash
  , hexadecimalHash
  , hexStringToInteger
  , decimalStringToInteger
  , octalStringToInteger
  , binaryStringToInteger

  -- optics for numeric text parsing
  , mixedHexDigits
  , decimalDigits
  , octalDigits
  , binaryDigits

  ) where


import Prelude hiding (null)

import Control.Lens (Traversal', Prism', Const, foldMapOf, re, (^?))

import qualified Data.ByteString as BS (ByteString, null, foldl')
import Data.Digit 
import Data.Functor.Compose (Compose(..))
import Data.Monoid (Product(..))
import Data.Text (Text, append)
import Data.Text.Lens (IsText(..), text)
import Data.Validation (Validation(..))
import Data.Word (Word8)

import Pact.Types.Util


#if !defined(ghcjs_HOST_OS)

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE

hash :: BS.ByteString -> Hash
hash = Hash . BLAKE.hash hashLengthAsBS mempty
{-# INLINE hash #-}

#else

import Crypto.Hash.Blake2Native

hash :: BS.ByteString -> Hash
hash bs = let (Right h) = blake2b hashLengthAsBS mempty bs in Hash h

#endif


verifyHash :: Hash -> BS.ByteString -> Either String Hash
verifyHash h b = if hash b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash b)
{-# INLINE verifyHash #-}


initialHash :: Hash
initialHash = hash mempty

numericBasedHash
  :: Integer -- ^ base a
  -> BS.ByteString -- ^ BS.ByteString to hash
  -> Either Text Integer -- ^ Left : Err, Right : Success
numericBasedHash base =
    hashAsBasedInteger base toBase 
  where
    toBase :: Word8 -> Integer
    toBase = (`mod` base) . fromIntegral 
{-# INLINE numericBasedHash #-}


-- | Compute the octal value of a BLAKE2 hashed bytestring
octalHash
  :: BS.ByteString -- ^ value to hash and compute in octal
  -> Either Text Integer -- ^ Left if given malformed input, Right if success
octalHash = numericBasedHash 8
{-# INLINE octalHash #-}

-- | Compute the binary value of BLAKE2 hashed bytestring
binaryHash
  :: BS.ByteString -- ^ value to hash and compute in binary
  -> Either Text Integer -- ^ Left if malformed input, Right if success
binaryHash = numericBasedHash 2
{-# INLINE binaryHash #-}

-- | Compute the hexidecimal value of a BLAKE2 hashed bytestring
hexadecimalHash
  :: BS.ByteString -- ^ value to hash and compute in hex
  -> Either Text Integer
hexadecimalHash = numericBasedHash 16
{-# INLINE hexadecimalHash #-}

-- | Reads 'Hash' as a non-negative 'Integral' number using the base
-- specified by the first argument, and character representation
-- specified by the second argument
hashAsBasedInteger
  :: Integer -- ^ The base specification
  -> (Word8 -> Integer) -- ^ the a-valued representation for a given character
  -> BS.ByteString -- ^ The string to convert to integral base-a
  -> Either Text Integer
hashAsBasedInteger base k h 
  | base <= 1 = Left $
    "readStringAtBase: applied to unsupported base - " `append` asString base
  | BS.null h = Left $
    "readStringAtBase: applied to empty hash - " `append` asString h
  | otherwise = Right $ BS.foldl' go 0 h
    where
      go :: Integer -> Word8 -> Integer
      go acc w = base * acc + (k w) 
{-# INLINE hashAsBasedInteger #-}

-- | Computes the integer value of a hexadecimal string by lensing
-- through text and converting all the chars into their corresponding
-- hexadecimal values. Equivalent to `foldl' (*) 1`, but with the
-- correct mapping of [(a, 10)..(f, 15)]
hexStringToInteger
  :: Text
  -> Integer
hexStringToInteger = getProduct . go
  where
    go :: Text -> Product Integer
    go = foldMapOf (mixedHexDigits . re integralHexadecimal) Product

-- | Computes the integer value of a decimal string by lensing
-- through text and converting all the chars into their corresponding
-- decimal values. Equivalent to `foldl' (*) 1`
decimalStringToInteger
  :: Text
  -> Integer
decimalStringToInteger = getProduct . go
  where
    go :: Text -> Product Integer
    go = foldMapOf (decimalDigits . re integralDecimal) Product

-- | Computes the integer value of a octal string by lensing
-- through text and converting all the chars into their corresponding
-- octal values. Equivalent to `foldl' (*) 1`, but with the
-- correct mapping in Z/8Z
octalStringToInteger
  :: Text
  -> Integer
octalStringToInteger = getProduct . go
  where
    go :: Text -> Product Integer
    go = foldMapOf (octalDigits . re integralOctal) Product

-- | Computes the integer value of a binary string by lensing
-- through text and converting all the chars into their corresponding
-- binary values. Equivalent to `foldl' (*) 1`, but with the
-- correct mapping of Z/2Z
binaryStringToInteger
  :: Text
  -> Integer
binaryStringToInteger = getProduct . go
  where
    go :: Text -> Product Integer
    go = foldMapOf (binaryDigits . re integralBinary) Product

-- | Create the Traversal' optics for converting 'Char' to mixed
-- case hexadecimal digits using prisms over Text
mixedHexDigits
  :: IsText a
  => Traversal' a HeXDigit
mixedHexDigits =
  text . charHeXaDeCiMaL

octalDigits
  :: IsText a
  => Traversal' a OctDigit
octalDigits = 
  text . charOctal

decimalDigits
  :: IsText a
  => Traversal' a DecDigit
decimalDigits =
  text . charDecimal

binaryDigits
  :: IsText a
  => Traversal' a BinDigit
binaryDigits =
  text . charBinary 
  
