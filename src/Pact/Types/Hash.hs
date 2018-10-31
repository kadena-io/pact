{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Types.Hash
  (
  -- Bytestring hashing
    hash
  , verifyHash
  , initialHash
  ) where


import Prelude hiding (null)
import Data.ByteString (ByteString)

import Pact.Types.Util


#if !defined(ghcjs_HOST_OS)

import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

hash :: ByteString -> Hash
hash = Hash . ByteArray.convert . Crypto.hashWith Crypto.Blake2b_512
{-# INLINE hash #-}

#else

import Crypto.Hash.Blake2Native

hash :: ByteString -> Hash
hash bs = case blake2b hashLengthAsBS mempty bs of
  Left _ -> error "hashing failed"
  Right h -> Hash h

#endif


verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hash b == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ show h ++ " but our hashing resulted in " ++ show (hash b)
{-# INLINE verifyHash #-}


initialHash :: Hash
initialHash = hash mempty