{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stability: experimental
-- This module contains a partial implementation of the
-- [COSE_Key](https://datatracker.ietf.org/doc/html/rfc8152#section-7) format,
-- limited to what is needed for Webauthn, and in a structured way.
module Pact.Crypto.WebAuthn.Cose.PublicKeyWithSignAlg
  ( -- * COSE public Key
    PublicKeyWithSignAlg (PublicKeyWithSignAlgInternal, PublicKeyWithSignAlg, Pact.Crypto.WebAuthn.Cose.PublicKeyWithSignAlg.publicKey, signAlg),
    CosePublicKey
  )
where

import Codec.CBOR.Decoding (Decoder, TokenType (TypeBool, TypeBytes), decodeBytesCanonical, decodeMapLenCanonical, peekTokenType)
import Codec.CBOR.Encoding (Encoding, encodeBytes, encodeMapLen)
import Codec.Serialise (Serialise (decode, encode))
import Control.Monad (unless)
import Crypto.Number.Serialize (i2osp, i2ospOf_, os2ip)
import qualified Pact.Crypto.WebAuthn.Cose.Registry as R
import qualified Pact.Crypto.WebAuthn.Cose.PublicKey as P
import qualified Pact.Crypto.WebAuthn.Cose.SignAlg as A
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | A combination of a t'P.PublicKey' holding the public key data and a
-- 'A.CoseSignAlg' holding the exact signature algorithm that should be used.
-- This type can only be constructed with 'makePublicKeyWithSignAlg', which
-- ensures that the signature scheme matches between 'P.PublicKey' and
-- 'A.CoseSignAlg'. This type is equivalent to a COSE public key, which holds
-- the same information, see 'CosePublicKey'
data PublicKeyWithSignAlg = PublicKeyWithSignAlgInternal
  { publicKeyInternal :: P.PublicKey,
    signAlgInternal :: A.CoseSignAlg
    -- TODO: Consider adding a RawField here to replace
    -- acdCredentialPublicKeyBytes. This would then require parametrizing
    -- 'PublicKeyWithSignAlg' with 'raw :: Bool'
  }
  deriving (Eq, Show, Generic)

-- | [(spec)](https://www.w3.org/TR/webauthn-2/#credentialpublickey)
-- A structured and checked representation of a
-- [COSE_Key](https://datatracker.ietf.org/doc/html/rfc8152#section-7), limited
-- to what is know to be necessary for Webauthn public keys for the
-- [credentialPublicKey](https://www.w3.org/TR/webauthn-2/#credentialpublickey)
-- field.
type CosePublicKey = PublicKeyWithSignAlg

-- | Deconstructs a 'makePublicKeyWithSignAlg' into its t'P.PublicKey' and
-- 'A.CoseSignAlg'. Since 'makePublicKeyWithSignAlg' can only be constructed
-- using 'makePublicKeyWithSignAlg', we can be sure that the signature scheme
-- of t'P.PublicKey' and 'A.CoseSignAlg' matches.
pattern PublicKeyWithSignAlg :: P.PublicKey -> A.CoseSignAlg -> PublicKeyWithSignAlg
pattern PublicKeyWithSignAlg {publicKey, signAlg} <- PublicKeyWithSignAlgInternal {publicKeyInternal = publicKey, signAlgInternal = signAlg}

{-# COMPLETE PublicKeyWithSignAlg #-}

-- | CBOR encoding as a [COSE_Key](https://tools.ietf.org/html/rfc8152#section-7)
-- using the [CTAP2 canonical CBOR encoding form](https://fidoalliance.org/specs/fido-v2.0-ps-20190130/fido-client-to-authenticator-protocol-v2.0-ps-20190130.html#ctap2-canonical-cbor-encoding-form)
instance Serialise CosePublicKey where
  encode PublicKeyWithSignAlg {..} = case publicKey of
    P.PublicKey P.PublicKeyEdDSA {..} ->
      common R.CoseKeyTypeOKP
        <> encode R.CoseKeyTypeParameterOKPCrv
        <> encode (fromCurveEdDSA eddsaCurve)
        <> encode R.CoseKeyTypeParameterOKPX
        <> encodeBytes eddsaX
    P.PublicKey P.PublicKeyECDSA {..} ->
      common R.CoseKeyTypeEC2
        <> encode R.CoseKeyTypeParameterEC2Crv
        <> encode (fromCurveECDSA ecdsaCurve)
        -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1
        -- > Leading zero octets MUST be preserved.
        <> encode R.CoseKeyTypeParameterEC2X
        -- This version of i2ospOf_ throws if the bytestring is larger than
        -- size, but this can't happen due to the PublicKey invariants
        <> encodeBytes (i2ospOf_ size ecdsaX)
        <> encode R.CoseKeyTypeParameterEC2Y
        <> encodeBytes (i2ospOf_ size ecdsaY)
      where
        size = P.coordinateSizeECDSA ecdsaCurve
    P.PublicKey P.PublicKeyRSA {..} ->
      common R.CoseKeyTypeRSA
        -- https://www.rfc-editor.org/rfc/rfc8230.html#section-4
        -- > The octet sequence MUST utilize the minimum
        -- number of octets needed to represent the value.
        <> encode R.CoseKeyTypeParameterRSAN
        <> encodeBytes (i2osp rsaN)
        <> encode R.CoseKeyTypeParameterRSAE
        <> encodeBytes (i2osp rsaE)
    where
      common :: R.CoseKeyType -> Encoding
      common kty =
        encodeMapLen (R.parameterCount kty)
          <> encode R.CoseKeyCommonParameterKty
          <> encode kty
          <> encode R.CoseKeyCommonParameterAlg
          <> encode signAlg

  -- NOTE: CBOR itself doesn't give an ordering of map keys, but the CTAP2 canonical CBOR encoding form does:
  -- > The keys in every map must be sorted lowest value to highest. The sorting rules are:
  -- >
  -- > * If the major types are different, the one with the lower value in numerical order sorts earlier.
  -- > * If two keys have different lengths, the shorter one sorts earlier;
  -- > * If two keys have the same length, the one with the lower value in (byte-wise) lexical order sorts earlier.
  --
  -- This has the effect that numeric keys are sorted like 1, 2, 3, ..., -1, -2, -3, ...
  -- Which aligns very nicely with the fact that common parameters use positive
  -- values and can therefore be decoded first, while key type specific
  -- parameters use negative values
  decode = do
    n <- fromIntegral <$> decodeMapLenCanonical
    -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-15#section-7.1
    -- This parameter MUST be present in a key object.
    decodeExpected R.CoseKeyCommonParameterKty
    kty <- decode
    -- https://www.w3.org/TR/webauthn-2/#credentialpublickey
    -- The COSE_Key-encoded credential public key MUST contain the "alg"
    -- parameter and MUST NOT contain any other OPTIONAL parameters.
    decodeExpected R.CoseKeyCommonParameterAlg
    alg <- decode

    uncheckedKey <- decodeKey n kty alg
    case P.checkPublicKey uncheckedKey of
      Left err -> fail $ "Key check failed: " <> Text.unpack err
      Right result ->
        pure $
          PublicKeyWithSignAlgInternal
            { publicKeyInternal = result,
              signAlgInternal = alg
            }
    where
      decodeKey :: Word -> R.CoseKeyType -> A.CoseSignAlg -> Decoder s P.UncheckedPublicKey
      decodeKey n kty alg = case alg of
        A.CoseSignAlgEdDSA -> decodeEdDSAKey
        A.CoseSignAlgECDSA _ -> decodeECDSAKey
        A.CoseSignAlgRSA _ -> decodeRSAKey
        where
          -- [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-15#section-7.1)
          -- Implementations MUST verify that the key type is appropriate for
          -- the algorithm being processed.
          checkKty :: R.CoseKeyType -> Decoder s ()
          checkKty expectedKty = do
            unless (expectedKty == kty) $
              fail $
                "Expected COSE key type "
                  <> show expectedKty
                  <> " for COSE algorithm "
                  <> show alg
                  <> " but got COSE key type "
                  <> show kty
                  <> " instead"
            unless (R.parameterCount kty == n) $
              fail $
                "Expected CBOR map to contain "
                  <> show (R.parameterCount kty)
                  <> " parameters for COSE key type "
                  <> show kty
                  <> " but got "
                  <> show n
                  <> " parameters instead"

          decodeEdDSAKey :: Decoder s P.UncheckedPublicKey
          decodeEdDSAKey = do
            -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.2
            -- > The 'kty' field MUST be present, and it MUST be 'OKP' (Octet Key Pair).
            checkKty R.CoseKeyTypeOKP
            -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.2
            decodeExpected R.CoseKeyTypeParameterOKPCrv
            eddsaCurve <- toCurveEdDSA <$> decode
            decodeExpected R.CoseKeyTypeParameterOKPX
            eddsaX <- decodeBytesCanonical
            pure P.PublicKeyEdDSA {..}

          decodeECDSAKey :: Decoder s P.UncheckedPublicKey
          decodeECDSAKey = do
            -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1
            -- > The 'kty' field MUST be present, and it MUST be 'EC2'.
            checkKty R.CoseKeyTypeEC2
            -- https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1
            decodeExpected R.CoseKeyTypeParameterEC2Crv
            ecdsaCurve <- toCurveECDSA <$> decode
            let size = P.coordinateSizeECDSA ecdsaCurve
            decodeExpected R.CoseKeyTypeParameterEC2X
            ecdsaX <- os2ipWithSize size =<< decodeBytesCanonical

            decodeExpected R.CoseKeyTypeParameterEC2Y
            ecdsaY <-
              peekTokenType >>= \case
                TypeBytes -> os2ipWithSize size =<< decodeBytesCanonical
                TypeBool -> fail "Compressed EC2 y coordinate not yet supported"
                typ -> fail $ "Unexpected type in EC2 y parameter: " <> show typ

            pure P.PublicKeyECDSA {..}

          decodeRSAKey :: Decoder s P.UncheckedPublicKey
          decodeRSAKey = do
            -- https://www.rfc-editor.org/rfc/rfc8812.html#section-2
            -- > Implementations need to check that the key type is 'RSA' when creating or verifying a signature.
            checkKty R.CoseKeyTypeRSA
            -- https://www.rfc-editor.org/rfc/rfc8230.html#section-4
            decodeExpected R.CoseKeyTypeParameterRSAN
            -- > The octet sequence MUST utilize the minimum number of octets needed to represent the value.
            rsaN <- os2ipNoLeading =<< decodeBytesCanonical
            decodeExpected R.CoseKeyTypeParameterRSAE
            rsaE <- os2ipNoLeading =<< decodeBytesCanonical
            pure P.PublicKeyRSA {..}

-- | Same as 'os2ip', but throws an error if there are not exactly as many bytes as expected. Thus any successful result of this function will give the same 'BS.ByteString' back if encoded with @'i2ospOf_' size@.
os2ipWithSize :: MonadFail m => Int -> BS.ByteString -> m Integer
os2ipWithSize size bytes
  | BS.length bytes == size = pure $ os2ip bytes
  | otherwise =
      fail $
        "bytes have length "
          <> show (BS.length bytes)
          <> " when length "
          <> show size
          <> " was expected"

-- | Same as 'os2ip', but throws an error if there are leading zero bytes. Thus any successful result of this function will give the same 'BS.ByteString' back if encoded with 'i2osp'.
os2ipNoLeading :: MonadFail m => BS.ByteString -> m Integer
os2ipNoLeading bytes
  | leadingZeroCount == 0 = pure $ os2ip bytes
  | otherwise =
      fail $
        "bytes of length "
          <> show (BS.length bytes)
          <> " has "
          <> show leadingZeroCount
          <> " leading zero bytes when none were expected"
  where
    leadingZeroCount = BS.length (BS.takeWhile (== 0) bytes)

-- | Decode a value and ensure it's the same as the value that was given
decodeExpected :: (Show a, Eq a, Serialise a) => a -> Decoder s ()
decodeExpected expected = do
  actual <- decode
  unless (expected == actual) $
    fail $
      "Expected " <> show expected <> " but got " <> show actual

fromCurveEdDSA :: P.CoseCurveEdDSA -> R.CoseEllipticCurveOKP
fromCurveEdDSA P.CoseCurveEd25519 = R.CoseEllipticCurveEd25519

toCurveEdDSA :: R.CoseEllipticCurveOKP -> P.CoseCurveEdDSA
toCurveEdDSA R.CoseEllipticCurveEd25519 = P.CoseCurveEd25519

fromCurveECDSA :: P.CoseCurveECDSA -> R.CoseEllipticCurveEC2
fromCurveECDSA P.CoseCurveP256 = R.CoseEllipticCurveEC2P256
fromCurveECDSA P.CoseCurveP384 = R.CoseEllipticCurveEC2P384
fromCurveECDSA P.CoseCurveP521 = R.CoseEllipticCurveEC2P521

toCurveECDSA :: R.CoseEllipticCurveEC2 -> P.CoseCurveECDSA
toCurveECDSA R.CoseEllipticCurveEC2P256 = P.CoseCurveP256
toCurveECDSA R.CoseEllipticCurveEC2P384 = P.CoseCurveP384
toCurveECDSA R.CoseEllipticCurveEC2P521 = P.CoseCurveP521
