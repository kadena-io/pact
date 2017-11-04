{-# LANGUAGE CPP #-}

module Pact.Types.Hash where

import Data.ByteString (ByteString)

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
