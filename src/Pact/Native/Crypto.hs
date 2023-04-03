{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Native.Crypto where

import Control.Lens
import Control.Monad.Except
import Data.Aeson
import Data.Foldable
import Data.Hash.SHA2
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text.Encoding

import Pact.Native.Internal
import Pact.Types.Runtime

import Crypto.JOSE.Error
import Crypto.JOSE.JWA.JWS
import Crypto.JOSE.JWK

decodeBase64UrlUnpaddedFatal :: ByteString -> ByteString
decodeBase64UrlUnpaddedFatal = either error id . decodeBase64UrlUnpadded

cryptoDefs :: NativeModule
cryptoDefs =
    ("Crypto",
    [ verifySignatureDef, sha256Def, base64ConcatDef ])
    where
    verifySignatureDef = defRNative "verify-signature-jwk" verifySignature (funType tTyBool [("message", tTyString), ("sig", tTyString), ("pubkey-jwk", tTyString)]) [] ""
    verifySignature _ [TLitString msg,TLitString sig,TLitString pubkeyjwk] = do
        let keyMaterial = maybe (error "invalid JWK") (view jwkMaterial) $ decode' (LBS.fromStrict $ encodeUtf8 pubkeyjwk)
        isValidOrErr :: Either Error Bool <- runExceptT $ verify ES256 keyMaterial (decodeBase64UrlUnpaddedFatal $ encodeUtf8 msg) (decodeBase64UrlUnpaddedFatal $ encodeUtf8 sig)
        case isValidOrErr of
            Left err -> error ("verify-signature-jwk: " <> show err)
            Right isValid -> return $ toTerm isValid
    verifySignature i as = argsError i as

    sha256Def = defRNative "sha-256" sha256 (funType tTyString [("input", tTyString)]) [] ""
        where
        sha256 _ [TLitString msg] = do
            case hashByteString $ decodeBase64UrlUnpaddedFatal (encodeUtf8 msg) of
                Sha2_256 sb -> return $ tStr $ decodeUtf8 $ encodeBase64UrlUnpadded $ SBS.fromShort sb
        sha256 i as = argsError i as

    base64ConcatDef = defRNative "base64-concat" base64Concat (funType tTyString [("input", TyList tTyString)]) [] ""
        where
        fromTLitString (TLitString m) = Just m
        fromTLitString _ = Nothing
        base64Concat _ [TList (traverse fromTLitString -> Just ins) _ _] = do
            let ins' = decodeBase64UrlUnpaddedFatal . encodeUtf8 <$> ins
            return $ tStr $ decodeUtf8 $ encodeBase64UrlUnpadded $ fold ins'
        base64Concat i as = argsError i as