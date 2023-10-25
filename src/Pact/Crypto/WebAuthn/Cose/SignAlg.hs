{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stability: experimental
-- This module contains definitions for [COSE registry](https://www.iana.org/assignments/cose/cose.xhtml)
-- entries that are relevant for Webauthn COSE public keys. All the types in
-- this module implement the 'Serialise' class, mapping them to the respective
-- CBOR values/labels.
--
-- This modules sometimes uses this
-- [CBOR Grammar](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-13#section-1.4)
-- to describe CBOR value types corresponding to CBOR parameters
module Pact.Crypto.WebAuthn.Cose.SignAlg
  ( -- * COSE Algorithms
    CoseSignAlg
      ( ..,
        CoseAlgorithmEdDSA,
        CoseAlgorithmES256,
        CoseAlgorithmES384,
        CoseAlgorithmES512,
        CoseAlgorithmRS256,
        CoseAlgorithmRS384,
        CoseAlgorithmRS512,
        CoseAlgorithmRS1
      ),
    fromCoseSignAlg,
    toCoseSignAlg,

    -- * Hash Algorithms
    CoseHashAlgECDSA (..),
    CoseHashAlgRSA (..),
  )
where

import Codec.CBOR.Decoding (decodeIntCanonical)
import Codec.CBOR.Encoding (encodeInt)
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (decode, encode)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- All the entries from the [COSE Algorithms registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- limited to the ones that are currently needed for Webauthn. Notably we only
-- care about asymmetric signature algorithms
data CoseSignAlg
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.2)
    -- EdDSA
    --
    -- [RFC8032](https://datatracker.ietf.org/doc/html/rfc8032) describes the elliptic curve signature scheme Edwards-curve
    -- Digital Signature Algorithm (EdDSA).  In that document, the signature
    -- algorithm is instantiated using parameters for edwards25519 and
    -- edwards448 curves.  The document additionally describes two variants
    -- of the EdDSA algorithm: Pure EdDSA, where no hash function is applied
    -- to the content before signing, and HashEdDSA, where a hash function
    -- is applied to the content before signing and the result of that hash
    -- function is signed.  For EdDSA, the content to be signed (either the
    -- message or the pre-hash value) is processed twice inside of the
    -- signature algorithm.  For use with COSE, only the pure EdDSA version
    -- is used.
    --
    -- Security considerations are [here](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.2.1)
    CoseSignAlgEdDSA
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1)
    -- ECDSA
    --
    -- ECDSA [DSS] defines a signature algorithm using ECC.  Implementations
    -- SHOULD use a deterministic version of ECDSA such as the one defined
    -- in [RFC6979].
    --
    -- The ECDSA signature algorithm is parameterized with a hash function
    -- (h).  In the event that the length of the hash function output is
    -- greater than the group of the key, the leftmost bytes of the hash
    -- output are used.
    -- ECDSA w/ SHA-256
    --
    -- This document defines ECDSA to work only with the curves P-256,
    -- P-384, and P-521. Future documents may define it to work with other
    -- curves and points in the future.
    --
    -- In order to promote interoperability, it is suggested that SHA-256 be
    -- used only with curve P-256, SHA-384 be used only with curve P-384,
    -- and SHA-512 be used with curve P-521.  This is aligned with the
    -- recommendation in [Section 4 of RFC5480](https://datatracker.ietf.org/doc/html/rfc5480#section-4)
    --
    -- Security considerations are [here](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1.1)
    CoseSignAlgECDSA CoseHashAlgECDSA
  | -- | [(spec)](https://www.rfc-editor.org/rfc/rfc8812.html#section-2)
    -- The RSASSA-PKCS1-v1_5 signature algorithm is defined in
    -- [RFC8017](https://www.rfc-editor.org/rfc/rfc8812.html#RFC8017).
    -- The RSASSA-PKCS1-v1_5 signature algorithm is parameterized with a hash function (h).
    --
    -- A key of size 2048 bits or larger MUST be used with these algorithms.
    --
    -- Security considerations are [here](https://www.rfc-editor.org/rfc/rfc8812.html#section-5)
    CoseSignAlgRSA CoseHashAlgRSA
  deriving (Eq, Show, Ord, Generic)

-- | Hash algorithms that can be used with the ECDSA signature algorithm
data CoseHashAlgECDSA
  = -- | SHA-256
    CoseHashAlgECDSASHA256
  | -- | SHA-384
    CoseHashAlgECDSASHA384
  | -- | SHA-512
    CoseHashAlgECDSASHA512
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Hash algorithms that can be used with the RSA signature algorithm
data CoseHashAlgRSA
  = -- | SHA-1 (deprecated)
    CoseHashAlgRSASHA1
  | -- | SHA-256
    CoseHashAlgRSASHA256
  | -- | SHA-384
    CoseHashAlgRSASHA384
  | -- | SHA-512
    CoseHashAlgRSASHA512
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.2)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @EdDSA@. Alias for 'CoseSignAlgEdDSA'
--
-- * Name: EdDSA
-- * Description: EdDSA
-- * Recommended: Yes
pattern CoseAlgorithmEdDSA :: CoseSignAlg
pattern CoseAlgorithmEdDSA = CoseSignAlgEdDSA

-- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @ES256@. Alias for @'CoseSignAlgECDSA' 'CoseHashAlgECDSASHA256'@
--
-- * Name: ES256
-- * Description: ECDSA w/ SHA-256
-- * Recommended: Yes
pattern CoseAlgorithmES256 :: CoseSignAlg
pattern CoseAlgorithmES256 = CoseSignAlgECDSA CoseHashAlgECDSASHA256

-- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @ES384@. Alias for @'CoseSignAlgECDSA' 'CoseHashAlgECDSASHA384'@
--
-- * Name: ES384
-- * Description: ECDSA w/ SHA-384
-- * Recommended: Yes
pattern CoseAlgorithmES384 :: CoseSignAlg
pattern CoseAlgorithmES384 = CoseSignAlgECDSA CoseHashAlgECDSASHA384

-- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-2.1)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @ES512@. Alias for @'CoseSignAlgECDSA' 'CoseHashAlgECDSASHA512'@
--
-- * Name: ES512
-- * Description: ECDSA w/ SHA-512
-- * Recommended: Yes
pattern CoseAlgorithmES512 :: CoseSignAlg
pattern CoseAlgorithmES512 = CoseSignAlgECDSA CoseHashAlgECDSASHA512

-- | [(spec)](https://www.rfc-editor.org/rfc/rfc8812.html#section-2)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @RS256@. Alias for @'CoseSignAlgRSA' 'CoseHashAlgRSASHA256'@
--
-- * Name: RS256
-- * Description: RSASSA-PKCS1-v1_5 using SHA-256
-- * Recommended: No
pattern CoseAlgorithmRS256 :: CoseSignAlg
pattern CoseAlgorithmRS256 = CoseSignAlgRSA CoseHashAlgRSASHA256

-- | [(spec)](https://www.rfc-editor.org/rfc/rfc8812.html#section-2)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @RS384@. Alias for @'CoseSignAlgRSA' 'CoseHashAlgRSASHA384'@
--
-- * Name: RS384
-- * Description: RSASSA-PKCS1-v1_5 using SHA-384
-- * Recommended: No
pattern CoseAlgorithmRS384 :: CoseSignAlg
pattern CoseAlgorithmRS384 = CoseSignAlgRSA CoseHashAlgRSASHA384

-- | [(spec)](https://www.rfc-editor.org/rfc/rfc8812.html#section-2)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @RS512@. Alias for @'CoseSignAlgRSA' 'CoseHashAlgRSASHA512'@
--
-- * Name: RS512
-- * Description: RSASSA-PKCS1-v1_5 using SHA-512
-- * Recommended: No
pattern CoseAlgorithmRS512 :: CoseSignAlg
pattern CoseAlgorithmRS512 = CoseSignAlgRSA CoseHashAlgRSASHA512

-- | [(spec)](https://www.rfc-editor.org/rfc/rfc8812.html#section-2)
-- [Cose Algorithm registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms)
-- entry @RS1@. Alias for @'CoseSignAlgRSA' 'CoseHashAlgRSASHA1'@
--
-- * Name: RS1
-- * Description: RSASSA-PKCS1-v1_5 using SHA-1
-- * Recommended: Deprecated
pattern CoseAlgorithmRS1 :: CoseSignAlg
pattern CoseAlgorithmRS1 = CoseSignAlgRSA CoseHashAlgRSASHA1

-- | Serialises COSE Algorithms using the @Value@ column from the
-- [COSE Algorithms registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms).
-- This uses the 'fromCoseSignAlg' and 'toCoseSignAlg' functions to do the
-- encoding and decoding respectively.
instance Serialise CoseSignAlg where
  encode = encodeInt . fromCoseSignAlg
  decode = do
    int <- decodeIntCanonical
    case toCoseSignAlg int of
      Right res -> pure res
      Left err -> fail $ Text.unpack err

-- | Converts a 'CoseSignAlg' to the corresponding integer value from the
-- [COSE Algorithms registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms).
-- The inverse operation is 'toCoseSignAlg'
fromCoseSignAlg :: Num p => CoseSignAlg -> p
fromCoseSignAlg (CoseSignAlgRSA CoseHashAlgRSASHA1) = -65535
fromCoseSignAlg (CoseSignAlgRSA CoseHashAlgRSASHA512) = -259
fromCoseSignAlg (CoseSignAlgRSA CoseHashAlgRSASHA384) = -258
fromCoseSignAlg (CoseSignAlgRSA CoseHashAlgRSASHA256) = -257
fromCoseSignAlg (CoseSignAlgECDSA CoseHashAlgECDSASHA512) = -36
fromCoseSignAlg (CoseSignAlgECDSA CoseHashAlgECDSASHA384) = -35
fromCoseSignAlg CoseSignAlgEdDSA = -8
fromCoseSignAlg (CoseSignAlgECDSA CoseHashAlgECDSASHA256) = -7

-- | Converts an integer value to the corresponding 'CoseSignAlg' from the
-- [COSE Algorithms registry](https://www.iana.org/assignments/cose/cose.xhtml#algorithms).
-- Returns an error if the integer doesn't represent a known algorithm.
-- The inverse operation is 'fromCoseSignAlg'
toCoseSignAlg :: (Eq a, Num a, Show a) => a -> Either Text CoseSignAlg
toCoseSignAlg (-65535) = pure (CoseSignAlgRSA CoseHashAlgRSASHA1)
toCoseSignAlg (-259) = pure (CoseSignAlgRSA CoseHashAlgRSASHA512)
toCoseSignAlg (-258) = pure (CoseSignAlgRSA CoseHashAlgRSASHA384)
toCoseSignAlg (-257) = pure (CoseSignAlgRSA CoseHashAlgRSASHA256)
toCoseSignAlg (-36) = pure (CoseSignAlgECDSA CoseHashAlgECDSASHA512)
toCoseSignAlg (-35) = pure (CoseSignAlgECDSA CoseHashAlgECDSASHA384)
toCoseSignAlg (-8) = pure CoseSignAlgEdDSA
toCoseSignAlg (-7) = pure (CoseSignAlgECDSA CoseHashAlgECDSASHA256)
toCoseSignAlg value = Left $ "Unknown COSE algorithm value " <> Text.pack (show value)
