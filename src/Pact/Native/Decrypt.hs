{-# LANGUAGE CPP #-}
-- |
-- Module      :  Pact.Native.Decrypt
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Built-ins for decrypting commitments.
--


module Pact.Native.Decrypt
  ( decryptDefs ) where

import Data.Text (Text)


import Pact.Native.Internal
import Pact.Types.Runtime
import Pact.Types.Pretty

import Pact.Eval

#if !defined(ghcjs_HOST_OS)
import Crypto.PubKey.Curve25519
import qualified Data.ByteArray as BA
#else

#endif


decryptDefs :: NativeModule
decryptDefs = ("Decryption",
  [ validateKeypairDef
  , decryptDef
  ])

validateKeypairDef :: NativeDef
validateKeypairDef =
  defRNative "validate-keypair"
  validateKeypair
  (funType tTyBool [("public",tTyString),("secret",tTyString)])
  [LitExample "(validate-keypair \"\" \"\")"]
  ("Test that the Curve25519 keypair of (PUBLIC,SECRET) match, and fail if not. " <>
   "Key values are base-16 strings of length 32 each.")
  where
    validateKeypair :: RNativeFun e
    validateKeypair i [TLitString pk, TLitString sk] = toTerm <$> doValidate i pk sk
    validateKeypair i as = argsError i as

decryptDef :: NativeDef
decryptDef =
  defRNative "decrypt"
  decrypt
  (funType tTyString [("ciphertext",tTyString),("public-key",tTyString),("secret-key",tTyString)])
  [LitExample "(decrypt cipher key)"]
  ("Given Curve25519 PUBLIC-KEY and SECRET-KEY, attempt to decrypt CIPHERTEXT " <>
   "to plaintext string, failing on error. " <>
   "PUBLIC-KEY and SECRET-KEY are base-16 strings of length 32 each.")
  where
    decrypt :: RNativeFun e
    decrypt i [TLitString ct,TLitString sk] = toTerm <$> doDecrypt i ct sk
    decrypt i as = argsError i as



doValidate :: FunApp -> Text -> Text -> Eval e Bool
#if !defined(ghcjs_HOST_OS)
doValidate i pk sk = undefined

#else
doValidate = evalError' i "'validate-keyset' unsupported in javascript-backed Pact"

#endif



doDecrypt :: FunApp -> Text -> Text -> Eval e Text
#if !defined(ghcjs_HOST_OS)
doDecrypt i ciphertext secretKey = undefined

#else
doDecrypt i _ _ = evalError' i "'decrypt' unsupported in javascript-backed Pact"

#endif



#if !defined(ghcjs_HOST_OS)


#endif
