{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of the `keccack256` pact native.
--
--   `keccak256` takes as input a Pact object representing a
--   'HyperlaneMessage', and returns a base16-encoded hash of the abi-encoding
--   of the input.
module Crypto.Hash.Keccak256Native (keccak256) where

import Control.Exception (Exception(..), SomeException(..), try)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Catch (throwM)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Short qualified as BSS
import Data.Hash.Class.Mutable (initialize, finalize, updateByteString)
import Data.Hash.Internal.OpenSSL (OpenSslException(..))
import Data.Hash.Keccak (Keccak256(..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Pact.Types.Term (Term, Name, _TLitString)
import System.IO.Unsafe (unsafePerformIO)

keccak256 :: Vector (Term Name) -> Text
keccak256 strings = unsafePerformIO $ do
  e <- try @SomeException @_ $ do
    ctx <- initialize @Keccak256
    forM_ strings $ \string -> do
      let s = string ^. _TLitString
      case Base64.decode (Text.encodeUtf8 s) of
        Left b64Err -> do
          throwM (Base64Exception b64Err)
        Right bytes -> do
          updateByteString @Keccak256 ctx bytes
    Keccak256 hash <- finalize ctx
    pure (BSS.fromShort hash)
  case e of
    Left err
      | Just (OpenSslException _) <- fromException err -> error "keccak256 failed"
      | Just (Base64Exception _) <- fromException err -> error "keccak256 failed: invalid base64 input"
      | otherwise -> error "keccak256 failed"
    Right hash -> pure (Text.decodeUtf8 (Base64.encode hash))
{-# noinline keccak256 #-}

newtype Base64Exception = Base64Exception String
  deriving stock (Show)

instance Exception Base64Exception where
  displayException (Base64Exception err) =
    "base64 decode exception: " ++ err
