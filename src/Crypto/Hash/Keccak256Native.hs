{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of the `keccak256` pact native.
module Crypto.Hash.Keccak256Native (Keccak256Error(..), keccak256) where

import Control.Exception (Exception(..), SomeException(..), try)
import Control.Monad (forM_)
import Control.Monad.Catch (throwM)
import Data.ByteString.Short qualified as BSS
import Data.Hash.Class.Mutable (initialize, finalize, updateByteString)
import Data.Hash.Internal.OpenSSL (OpenSslException(..))
import Data.Hash.Keccak (Keccak256(..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Pact.Types.Util (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)
import System.IO.Unsafe (unsafePerformIO)

data Keccak256Error
  = Keccak256OpenSslException String
  | Keccak256Base64Exception String
  | Keccak256OtherException !SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

keccak256 :: Vector Text -> Either Keccak256Error Text
keccak256 strings = unsafePerformIO $ do
  e <- try @SomeException @_ $ do
    ctx <- initialize @Keccak256
    forM_ strings $ \string -> do
      case decodeBase64UrlUnpadded (Text.encodeUtf8 string) of
        Left b64Err -> do
          throwM (Keccak256Base64Exception b64Err)
        Right bytes -> do
          updateByteString @Keccak256 ctx bytes
    Keccak256 hash <- finalize ctx
    pure (BSS.fromShort hash)
  case e of
    Left err
      | Just (OpenSslException msg) <- fromException err -> pure (Left (Keccak256OpenSslException msg))
      | Just (exc :: Keccak256Error) <- fromException err -> pure (Left exc)
      | otherwise -> pure (Left (Keccak256OtherException err))
    Right hash -> pure (Right (Text.decodeUtf8 (encodeBase64UrlUnpadded hash)))
{-# noinline keccak256 #-}
