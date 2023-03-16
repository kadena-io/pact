{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Native.Crypto where

import Control.Lens
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding

import Pact.Native.Internal
import Pact.Types.Runtime

import Crypto.JOSE.Error
import Crypto.JOSE.JWA.JWS
import Crypto.JOSE.JWK

cryptoDefs :: NativeModule
cryptoDefs =
    ("Crypto",
    [ verifySignatureDef ])
    where
    verifySignatureDef = defRNative "verify-signature-jwk" verifySignature (funType tTyBool [("message", tTyString), ("sig", tTyString), ("pubkey-jwk", tTyString)]) [] ""
    verifySignature _ [TLitString msg,TLitString sig,TLitString pubkeyjwk] = do
        let keyMaterial = maybe (error "invalid JWK") (view jwkMaterial) $ decode' (LBS.fromStrict $ encodeUtf8 pubkeyjwk)
        let decodeBase64UrlUnpaddedFatal = either error id . decodeBase64UrlUnpadded
        isValidOrErr :: Either Error Bool <- runExceptT $ verify ES512 keyMaterial (decodeBase64UrlUnpaddedFatal $ encodeUtf8 msg) (decodeBase64UrlUnpaddedFatal $ encodeUtf8 sig)
        case isValidOrErr of
            Left err -> error ("verify-signature-jwk: " <> show err)
            Right isValid -> return $ toTerm isValid

    verifySignature i as = argsError i as
