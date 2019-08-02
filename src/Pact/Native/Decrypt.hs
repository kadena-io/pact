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
  -- ( decryptDefs
-- #if !defined(ghcjs_HOST_OS)
--   , doEncrypt
-- #endif
--   )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Text (Text)
import Data.Text.Encoding

import Pact.Native.Internal
import Pact.Types.Pretty
import Pact.Types.Runtime hiding (PublicKey)
#if !defined(ghcjs_HOST_OS)
import qualified Data.ByteArray as BA
import Crypto.PubKey.Curve25519
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305
import Crypto.MAC.Poly1305 (Auth(..),authTag)
#else

#endif


decryptDefs :: NativeModule
decryptDefs = ("Commitments",
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
doValidate i _ _ = evalError' i "'validate-keypair' unsupported in javascript-backed Pact"
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
  (funType tTyString
   [("ciphertext",tTyString)
   ,("nonce",tTyString)
   ,("aad",tTyString)
   ,("mac",tTyString)
   ,("public-key",tTyString)
   ,("secret-key",tTyString)
   ])
  [LitExample "(decrypt-cc20p1305 ciphertext nonce aad mac pubkey privkey)"]
  ("Perform decryption of CIPHERTEXT using the CHACHA20-POLY1305 Authenticated " <>
   "Encryption with Associated Data (AEAD) construction described in IETF RFC 7539. " <>
    "CIPHERTEXT is an unpadded base64url string. " <>
    "NONCE is a 12-byte base64 string. " <>
    "AAD is base64 additional authentication data of any length. " <>
    "MAC is the \"detached\" base64 tag value for validating POLY1305 authentication. " <>
    "PUBLIC-KEY and SECRET-KEY are base-16 Curve25519 values to form the DH symmetric key." <>
    "Result is unpadded base64URL.")
  where
    decrypt' :: RNativeFun e
    decrypt' i [c@(TLitString c'),n@(TLitString n'),a@(TLitString a'),
                m@(TLitString m'),p@(TLitString p'),s@(TLitString s')] = do
      ciphertext <- doBase64Url c c'
      aad <- doBase64Url a a'
      nonce <- ofLength n "nonce" 12 =<< doBase64Url n n'
      mac <- doBase64Url m m'
      public <- doPublic p p'
      secret <- doSecret s s'
      (toTerm . toB64UrlUnpaddedText) <$> doDecrypt i ciphertext nonce aad mac public secret
    decrypt' i as = argsError i as


-- | Implementation of CHACHA20-POLY1305 decryption.
doDecrypt ::
  HasInfo i => i
  -> ByteString
  -- ^ Ciphertext
  -> ByteString
  -- ^ nonce
  -> ByteString
  -- ^ aad
  -> ByteString
  -- ^ mac
  -> ByteString
  -- ^ Curve25519 pubKey DH input
  -> ByteString
  -- ^ Curve25519 secretKey DH input
  -> Eval e ByteString
#if defined(ghcjs_HOST_OS)
doDecrypt i _ _ _ _ _ _ = evalError' i "'decrypt' unsupported in javascript-backed Pact"
#else

doDecrypt i ciphertext nonce aad auth public secret = do
  at <- liftCrypto i "auth prefix" $ authTag auth
  n <- liftCrypto i "nonce" $ nonce12 nonce
  pk <- importPublic i public
  sk <- importSecret i secret
  let k = dh pk sk
  ini <- liftCrypto i "initialize failed" $ initialize k n
  let afterAAD = finalizeAAD (appendAAD aad ini)
      (out,afterDecrypt) = decrypt ciphertext afterAAD
      outtag = finalize afterDecrypt
  unless (outtag == at) $
    evalError' i $ "AEAD Authentication failed: " <> pretty (toB16Text $ authToBS outtag)
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
doEncrypt :: Info -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> Eval e (ByteString,ByteString)
doEncrypt i plaintext nonce aad public secret = do
  n <- liftCrypto i "nonce" $ nonce12 nonce
  pk <- importPublic i public
  sk <- importSecret i secret
  let k = dh pk sk
  ini <- liftCrypto i "initialize failed" $ initialize k n
  let afterAAD = finalizeAAD (appendAAD aad ini)
      (out,afterEncrypt) = encrypt plaintext afterAAD
      outtag = finalize afterEncrypt
  return $ (out,authToBS outtag)

------ TESTING ------

_pk :: ByteString
_pk = either error id $ parseB16TextOnly "a236ee764da1bde16f3452df78ed0d46d94c0763042330a8341511cfe537da6d"
_sk :: ByteString
_sk = either error id $ parseB16TextOnly "92702bbc9026d489f0ecefe266d694b9ad321965f3345758d1f937fba8817184"
_nonce :: ByteString
_nonce = either error id $ decodeBase64UrlUnpadded "AAAAAAECAwQFBgcI"
_plaintext :: ByteString
_plaintext = either error id $ decodeBase64UrlUnpadded "bWVzc2FnZQ" -- base64url of "message"
_aad :: ByteString
_aad = either error id $ decodeBase64UrlUnpadded "YWFk" -- "aad"

_i :: Info
_i = def

_jankyRunEval :: Eval e b -> IO b
_jankyRunEval = fmap fst . runEval undefined undefined

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

_encrypt :: IO (ByteString,PublicKey) -> IO (ByteString,SecretKey) -> IO (ByteString,ByteString)
_encrypt p s = do
  p' <- p
  s' <- s
  _jankyRunEval $ doEncrypt _i _plaintext _nonce _aad (fst p') (fst s')

_decrypt :: IO (ByteString,PublicKey) -> IO (ByteString,SecretKey) ->
              (ByteString,ByteString) -> IO Text
_decrypt p s (ct,tag) = do
  p' <- p
  s' <- s
  toB64UrlUnpaddedText <$> _jankyRunEval (doDecrypt _i ct _nonce _aad tag (fst p') (fst s'))

-- _roundtrip _alicePK _bobSK --> ("message","message")
-- _roundtrip _bobPK _aliceSK --> ("message","message")
_roundtrip
  :: IO (ByteString, PublicKey)
     -> IO (ByteString, SecretKey) -> IO (ByteString, ByteString)
_roundtrip p s = do
  r <- _encrypt p s >>= _decrypt p s >>= _jankyRunEval . doBase64Url _i
  return (_plaintext,r)
#endif
