{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Pact.Native.Decrypt
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Built-ins for decrypting commitments.
--


module Pact.Native.Decrypt
  ( decryptDefs
#if !defined(ghcjs_HOST_OS)
  , doEncrypt
#endif
  ) where

import Control.Monad
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Text (Text)
import Data.Text.Encoding

import Pact.Native.Internal
import Pact.Types.Pretty
import Pact.Types.Runtime hiding (PublicKey)
#if !defined(ghcjs_HOST_OS)
import Crypto.PubKey.Curve25519
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305
import Crypto.MAC.Poly1305 (Auth(..),authTag)
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
  [LitExample "(validate-keypair pubkey privkey)"]
  ("Enforce that the Curve25519 keypair of (PUBLIC,SECRET) match. " <>
   "Key values are base-16 strings of length 32.")
  where
    validateKeypair :: RNativeFun e
    validateKeypair i [p@(TLitString p'), s@(TLitString s')] = do
      public <- doPublic p p'
      secret <- doSecret s s'
      v <- doValidate i public secret
      if v then return $ toTerm v
        else evalError' i $ "validate-keypair: mismatch"
    validateKeypair i as = argsError i as

doValidate :: HasInfo i => i -> ByteString -> ByteString -> Eval e Bool
#if defined(ghcjs_HOST_OS)
doValidate i _ _ = evalError' i "'validate-keyset' unsupported in javascript-backed Pact"
#else

doValidate i public secret = do
  pk <- importPublic i public
  sk <- importSecret i secret
  return $ toPublic sk == pk

#endif



decryptDef :: NativeDef
decryptDef =
  defRNative "decrypt-cc20p1305"
  decrypt'
  (funType tTyString [("ciphertext",tTyString),("nonce",tTyString),("aad",tTyString),
                      ("public-key",tTyString),("secret-key",tTyString)])
  [LitExample "(decrypt-cc20p1305 ciphertext nonce aad pubkey privkey)"]
  ("Decrypt a Curve25519/ChaCha20/Poly1305 CIPHERTEXT using NONCE, AAD, PUBLIC-KEY and SECRET-KEY. " <>
   "CIPHERTEXT is an unpadded base64url value with the 16-byte authentication tag output of the encryption. " <>
   "PUBLIC-KEY and SECRET-KEY are base-16 strings of length 32. " <>
   "NONCE is an unpadded base64URL value of a 24-byte bytestring. " <>
   "AAD is additional authentication data of an unpadded base-16 string of any length. " <>
   "Result is unpadded base64URL.")
  where
    decrypt' :: RNativeFun e
    decrypt' i [c@(TLitString c'),n@(TLitString n'),a@(TLitString a'),p@(TLitString p'),s@(TLitString s')] = do
      ciphertext <- doBase64Url c c'
      aad <- doBase16 a a'
      nonce <- ofLength n "nonce" 12 =<< doBase64Url n n'
      public <- doPublic p p'
      secret <- doSecret s s'
      (toTerm . toB64UrlUnpaddedText) <$> doDecrypt i ciphertext nonce aad public secret
    decrypt' i as = argsError i as


doDecrypt :: HasInfo i => i -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Eval e ByteString
#if defined(ghcjs_HOST_OS)
doDecrypt i _ _ _ _ _ = evalError' i "'decrypt' unsupported in javascript-backed Pact"
#else

doDecrypt i ciphertext nonce aad public secret = do
  let authPfx = BS.take 16 ciphertext
  auth <- liftCrypto i "auth prefix" $ authTag authPfx
  let ct = BS.drop 16 ciphertext
  n <- liftCrypto i "nonce" $ nonce12 nonce
  pk <- importPublic i public
  sk <- importSecret i secret
  let k = dh pk sk
  ini <- liftCrypto i "initialize failed" $ initialize k n
  let afterAAD = finalizeAAD (appendAAD aad ini)
      (out,afterDecrypt) = decrypt ct afterAAD
      outtag = finalize afterDecrypt
  unless (outtag == auth) $
    evalError' i $ "AEAD Authentication failed: " <> pretty (show $ authToBS outtag)
  return out

#endif



doPublic :: HasInfo i => i -> Text -> Eval e ByteString
doPublic p p' = ofLength p "public key" 32 =<< doBase16 p p'

doSecret :: HasInfo i => i -> Text -> Eval e ByteString
doSecret s s' = ofLength s "secret key" 32 =<< doBase16 s s'

doBase64Url :: HasInfo a => a -> Text -> Eval e ByteString
doBase64Url i = either (evalError' i . prettyString) return .
                decodeBase64UrlUnpadded . encodeUtf8

doBase16 :: HasInfo i => i -> Text -> Eval e ByteString
doBase16 i = either (evalError' i . prettyString) return .
             parseB16TextOnly

ofLength :: HasInfo i => i -> Text -> Int -> ByteString -> Eval e ByteString
ofLength i m n b | BS.length b == n = return b
                 | otherwise = evalError' i $ pretty m <> " must be bytestring of length " <> pretty n




#if !defined(ghcjs_HOST_OS)

liftCrypto
  :: HasInfo i => i -> [Char] -> CryptoFailable a -> Eval e a
liftCrypto _ _ (CryptoPassed a) = return a
liftCrypto i msg (CryptoFailed e) = evalError' i $ prettyString $ msg ++ ": " ++ show e

importPublic
  :: (HasInfo i, BA.ByteArrayAccess bs) =>
     i -> bs -> Eval e PublicKey
importPublic i = liftCrypto i "Invalid public key" . publicKey

importSecret
  :: (HasInfo i, BA.ByteArrayAccess bs) => i -> bs -> Eval e SecretKey
importSecret i = liftCrypto i "Invalid secret key" . secretKey

authToBS :: Auth -> ByteString
authToBS (Auth b) = BS.pack $ BA.unpack b

-- | Exposed for testing
doEncrypt :: Info -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Eval e ByteString
doEncrypt i plaintext nonce aad public secret = do
  n <- liftCrypto i "nonce" $ nonce12 nonce
  pk <- importPublic i public
  sk <- importSecret i secret
  let k = dh pk sk
  ini <- liftCrypto i "initialize failed" $ initialize k n
  let afterAAD = finalizeAAD (appendAAD aad ini)
      (out,afterEncrypt) = encrypt plaintext afterAAD
      outtag = finalize afterEncrypt
  return $ authToBS outtag <> out

------ TESTING ------

_pk :: ByteString
_pk = either error id $ parseB16TextOnly "a236ee764da1bde16f3452df78ed0d46d94c0763042330a8341511cfe537da6d"
_sk :: ByteString
_sk = either error id $ parseB16TextOnly "92702bbc9026d489f0ecefe266d694b9ad321965f3345758d1f937fba8817184"
_nonce :: ByteString
_nonce = either error id $ decodeBase64UrlUnpadded "Ka96AovW2JFVrgOK"
_plaintext :: ByteString
_plaintext = either error id $ decodeBase64UrlUnpadded "bWVzc2FnZQ" -- base64url of "message"
_ciphertext :: ByteString
_ciphertext = either error id $ decodeBase64UrlUnpadded "xvhAzzyGWuWdlA2XW7irq2sEAC6JiHk" -- encrypt of "message"
_aad :: ByteString
_aad = either error id $ parseB16TextOnly "616164" -- "aad"

_i :: Info
_i = def

_jankyRunEval :: Eval e b -> IO b
_jankyRunEval = fmap fst . runEval undefined undefined

_testEncrypt :: ByteString -> IO ByteString
_testEncrypt pt = _jankyRunEval $ doEncrypt def pt _nonce _aad _pk _sk

_testDecrypt :: ByteString -> IO ByteString
_testDecrypt ct = _jankyRunEval $ doDecrypt (def :: Info) ct _nonce _aad _pk _sk

_testRoundtrip :: ByteString -> IO ByteString
_testRoundtrip pt = _testEncrypt pt >>= _testDecrypt

_testValidate :: ByteString -> ByteString -> IO Bool
_testValidate pk sk = _jankyRunEval $ doValidate (def :: Info) pk sk

_testValidate' :: IO Bool
_testValidate' = _testValidate _pk _sk

{- from https://tools.ietf.org/html/rfc7748#section-6.1 -}

_importSK :: Text -> IO SecretKey
_importSK b = _jankyRunEval (importSecret _i =<< doBase16 _i b)
_importPK :: Text -> IO Crypto.PubKey.Curve25519.PublicKey
_importPK b = _jankyRunEval (importPublic _i =<< doBase16 _i b)
_importDh :: Text -> IO DhSecret
_importDh b = _jankyRunEval ((liftCrypto _i "Dh" . dhSecret) =<< doBase16 _i b)

--Alice's private key, a:
_aliceSK :: IO (ByteString,SecretKey)
_aliceSK = _wBsArg _importSK "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
--Alice's public key, X25519(a, 9):
_alicePK :: IO (ByteString,PublicKey)
_alicePK = _wBsArg _importPK "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
--Bob's private key, b:
_bobSK :: IO (ByteString,SecretKey)
_bobSK = _wBsArg _importSK "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
--Bob's public key, X25519(b, 9):
_bobPK :: IO (ByteString,PublicKey)
_bobPK = _wBsArg _importPK "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
--Their shared secret, K:
_K :: IO DhSecret
_K = _importDh "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"

_checkRFC7748_6_1
  :: IO ((PublicKey, SecretKey), (PublicKey, SecretKey), DhSecret)
_checkRFC7748_6_1 = do
  ask <- snd <$> _aliceSK
  apk <- snd <$> _alicePK
  bsk <- snd <$> _bobSK
  bpk <- snd <$> _bobPK
  k <- _K
  -- test valid keypairs
  unless (apk == toPublic ask) $ error $ "bad alice keypair: " ++ show (apk, ask)
  unless (bpk == toPublic bsk) $ error $ "bad bob keypair: " ++ show (bpk, bsk)
  unless (dh apk bsk == k) $ error $ "bad dh result for apk/bsk"
  unless (dh bpk ask == k) $ error $ "bad dh result for bpk/ask"
  return ((apk,ask),(bpk,bsk),k)

_wBsArg :: (Text -> IO a) -> Text -> IO (ByteString, a)
_wBsArg f a = (,) <$> _jankyRunEval (doBase16 _i a) <*> f a

_encryptAB :: IO Text
_encryptAB = do
  a <- fst <$> _alicePK
  b <- fst <$> _bobSK
  toB64UrlUnpaddedText <$> _jankyRunEval (doEncrypt _i _plaintext _nonce _aad a b)

_encryptBA :: IO Text
_encryptBA = do
  a <- fst <$> _aliceSK
  b <- fst <$> _bobPK
  toB64UrlUnpaddedText <$> _jankyRunEval (doEncrypt _i _plaintext _nonce _aad b a)

_decryptAB :: Text -> IO Text
_decryptAB ct = do
  a <- fst <$> _alicePK
  b <- fst <$> _bobSK
  ct' <- _jankyRunEval $ doBase64Url _i ct
  toB64UrlUnpaddedText <$> _jankyRunEval (doDecrypt _i ct' _nonce _aad a b)

_decryptBA :: Text -> IO Text
_decryptBA ct = do
  a <- fst <$> _aliceSK
  b <- fst <$> _bobPK
  ct' <- _jankyRunEval $ doBase64Url _i ct
  toB64UrlUnpaddedText <$> _jankyRunEval (doDecrypt _i ct' _nonce _aad b a)



#endif
