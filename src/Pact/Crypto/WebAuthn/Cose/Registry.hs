{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}

-- | Stability: internal
-- This module contains definitions for [COSE registry](https://www.iana.org/assignments/cose/cose.xhtml)
-- entries that are relevant for Webauthn COSE public keys. All the types in
-- this module implement the 'Serialise' class, mapping them to the respective
-- CBOR values/labels.
--
-- This modules sometimes uses this
-- [CBOR Grammar](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-13#section-1.4)
-- to describe CBOR value types corresponding to CBOR parameters
module Pact.Crypto.WebAuthn.Cose.Registry
  ( -- * COSE Key Types
    CoseKeyType (..),

    -- * COSE Parameters
    CoseKeyCommonParameter (..),
    CoseKeyTypeParameterOKP (..),
    CoseKeyTypeParameterEC2 (..),
    CoseKeyTypeParameterRSA (..),
    parameterCount,

    -- * COSE Elliptic Curves
    CoseEllipticCurveOKP (..),
    CoseEllipticCurveEC2 (..),
  )
where

import Codec.CBOR.Decoding (decodeIntCanonical)
import Codec.CBOR.Encoding (encodeInt)
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (decode, encode)

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#key-common-parameters)
-- All the entries from the [COSE Key Common Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-common-parameters)
-- that are needed for Webauthn public keys
data CoseKeyCommonParameter
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-15#section-7.1)
    --
    -- * COSE value type: tstr / int
    -- * Value registry: 'CoseKeyType'
    -- * Description: Identification of the key type
    --
    -- This parameter is used to identify the family of keys for this
    -- structure and, thus, the set of key-type-specific parameters to be
    -- found. The key type MUST be included as part of the trust decision
    -- process.
    CoseKeyCommonParameterKty
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-struct-15#section-7.1)
    --
    -- * COSE value type: tstr / int
    -- * Value registry: 'Crypto.WebAuthn.Cose.Algorithm.CoseSignAlg'
    -- * Description: Key usage restriction to this algorithm
    --
    -- This parameter is used to restrict the algorithm that is used
    -- with the key.
    CoseKeyCommonParameterAlg
  deriving (Eq, Show, Bounded, Enum)

-- | Serialises the parameters using the @Label@ column from the
-- [COSE Key Common Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-common-parameters)
instance Serialise CoseKeyCommonParameter where
  encode CoseKeyCommonParameterKty = encodeInt 1
  encode CoseKeyCommonParameterAlg = encodeInt 3
  decode =
    decodeIntCanonical >>= \case
      1 -> pure CoseKeyCommonParameterKty
      3 -> pure CoseKeyCommonParameterAlg
      value -> fail $ "Unknown COSE key common parameter " <> show value

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#key-type)
-- All the entries from the [COSE Key Types registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type)
-- that are known to be needed for Webauthn public keys
data CoseKeyType
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.2)
    -- Octet Key Pair.
    -- See 'CoseKeyTypeParameterOKP' for the parameters specific to this key type.
    CoseKeyTypeOKP
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1)
    -- Elliptic Curve Keys w/ x- and y-coordinate pair.
    -- See 'CoseKeyTypeParameterEC2' for the parameters specific to this key type.
    CoseKeyTypeEC2
  | -- | [(spec)](https://www.rfc-editor.org/rfc/rfc8230.html#section-4)
    -- RSA Key.
    -- See 'CoseKeyTypeParameterRSA' for the parameters specific to this key type.
    CoseKeyTypeRSA
  deriving (Eq, Show)

-- | Serialises the key type using the @Value@ column from the
-- [COSE Key Types registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type)
instance Serialise CoseKeyType where
  encode CoseKeyTypeOKP = encodeInt 1
  encode CoseKeyTypeEC2 = encodeInt 2
  encode CoseKeyTypeRSA = encodeInt 3
  decode =
    decodeIntCanonical >>= \case
      1 -> pure CoseKeyTypeOKP
      2 -> pure CoseKeyTypeEC2
      3 -> pure CoseKeyTypeRSA
      value -> fail $ "Unknown COSE key type " <> show value

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- All the entries from the [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- for key type 'CoseKeyTypeOKP' (aka @Key Type@ is @1@) that are required for
-- public keys
data CoseKeyTypeParameterOKP
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.2)
    --
    -- * COSE value type: int / tstr
    -- * Value registry: 'CoseEllipticCurveOKP'
    -- * Description: EC identifier
    --
    -- This contains an identifier of the curve to be used with the key.
    CoseKeyTypeParameterOKPCrv
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.2)
    --
    -- * COSE value type: bstr
    -- * Description: Public Key
    --
    -- This contains the public key. The byte string contains the public key as defined by the algorithm.
    CoseKeyTypeParameterOKPX
  deriving (Eq, Show, Bounded, Enum)

-- | Serialises the parameters using the @Label@ column from the
-- [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
instance Serialise CoseKeyTypeParameterOKP where
  encode CoseKeyTypeParameterOKPCrv = encodeInt (-1)
  encode CoseKeyTypeParameterOKPX = encodeInt (-2)
  decode =
    decodeIntCanonical >>= \case
      -1 -> pure CoseKeyTypeParameterOKPCrv
      -2 -> pure CoseKeyTypeParameterOKPX
      value -> fail $ "Unknown COSE key type parameter " <> show value <> " for key type OKP"

-- | Elliptic curves for key type 'CoseKeyTypeOKP' from the
-- [COSE Elliptic Curves registry](https://www.iana.org/assignments/cose/cose.xhtml#elliptic-curves),
-- limited to the ones that are currently needed for Webauthn
data CoseEllipticCurveOKP
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1)
    -- Ed25519 for use w/ EdDSA only
    CoseEllipticCurveEd25519
  deriving (Eq, Show)

-- | Serialises COSE Elliptic Curves using the @Value@ column from the
-- [COSE Elliptic Curves registry](https://www.iana.org/assignments/cose/cose.xhtml#elliptic-curves).
instance Serialise CoseEllipticCurveOKP where
  encode CoseEllipticCurveEd25519 = encodeInt 6
  decode =
    decodeIntCanonical >>= \case
      6 -> pure CoseEllipticCurveEd25519
      value -> fail $ "Unknown COSE elliptic curve " <> show value <> " for key type OKP"

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- All the entries from the [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- for key type 'CoseKeyTypeEC2' (aka @Key Type@ is @2@) that are required for
-- public keys
data CoseKeyTypeParameterEC2
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1)
    --
    -- * COSE value type: int / tstr
    -- * Value registry: 'CoseEllipticCurveEC2'
    -- * Description: EC identifier
    --
    -- This contains an identifier of the curve to be used with the key.
    CoseKeyTypeParameterEC2Crv
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1)
    --
    -- * COSE value type: bstr
    -- * Description: x-coordinate
    --
    -- This contains the x-coordinate for the EC point. The integer is
    -- converted to a byte string as defined in [SEC1]. Leading zero
    -- octets MUST be preserved.
    CoseKeyTypeParameterEC2X
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1.1)
    --
    -- * COSE value type: bstr / bool
    -- * Description: y-coordinate
    --
    -- This contains either the sign bit or the value of the
    -- y-coordinate for the EC point. When encoding the value y, the
    -- integer is converted to an byte string (as defined in
    -- [SEC1](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#ref-SEC1))
    -- and encoded as a CBOR bstr. Leading zero octets MUST be
    -- preserved. The compressed point encoding is also supported.
    -- Compute the sign bit as laid out in the Elliptic-Curve-Point-to-
    -- Octet-String Conversion function of
    -- [SEC1](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#ref-SEC1).
    -- If the sign bit is zero, then encode y as a CBOR false value;
    -- otherwise, encode y as a CBOR true value.
    -- The encoding of the infinity point is not supported.
    CoseKeyTypeParameterEC2Y
  deriving (Eq, Show, Bounded, Enum)

-- | Serialises the parameters using the @Label@ column from the
-- [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
instance Serialise CoseKeyTypeParameterEC2 where
  encode CoseKeyTypeParameterEC2Crv = encodeInt (-1)
  encode CoseKeyTypeParameterEC2X = encodeInt (-2)
  encode CoseKeyTypeParameterEC2Y = encodeInt (-3)
  decode =
    decodeIntCanonical >>= \case
      -1 -> pure CoseKeyTypeParameterEC2Crv
      -2 -> pure CoseKeyTypeParameterEC2X
      -3 -> pure CoseKeyTypeParameterEC2Y
      value -> fail $ "Unknown COSE key type parameter " <> show value <> " for key type EC2"

-- | Elliptic curves for key type 'CoseKeyTypeEC2' from the
-- [COSE Elliptic Curves registry](https://www.iana.org/assignments/cose/cose.xhtml#elliptic-curves),
-- limited to the ones that are currently needed for Webauthn
data CoseEllipticCurveEC2
  = -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1)
    -- NIST P-256 also known as secp256r1
    CoseEllipticCurveEC2P256
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1)
    -- NIST P-384 also known as secp384r1
    CoseEllipticCurveEC2P384
  | -- | [(spec)](https://datatracker.ietf.org/doc/html/draft-ietf-cose-rfc8152bis-algs-12#section-7.1)
    -- NIST P-521 also known as secp521r1
    CoseEllipticCurveEC2P521
  deriving (Eq, Show)

-- | Serialises COSE Elliptic Curves using the @Value@ column from the
-- [COSE Elliptic Curves registry](https://www.iana.org/assignments/cose/cose.xhtml#elliptic-curves).
instance Serialise CoseEllipticCurveEC2 where
  encode CoseEllipticCurveEC2P256 = encodeInt 1
  encode CoseEllipticCurveEC2P384 = encodeInt 2
  encode CoseEllipticCurveEC2P521 = encodeInt 3
  decode =
    decodeIntCanonical >>= \case
      1 -> pure CoseEllipticCurveEC2P256
      2 -> pure CoseEllipticCurveEC2P384
      3 -> pure CoseEllipticCurveEC2P521
      value -> fail $ "Unknown COSE elliptic curve " <> show value <> " for key type EC2"

-- | [(spec)](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- All the entries from the [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
-- for key type 'CoseKeyTypeRSA' (aka @Key Type@ is @3@) that are required for
-- public keys
data CoseKeyTypeParameterRSA
  = -- | [(spec)](https://www.rfc-editor.org/rfc/rfc8230.html#section-4)
    --
    -- * COSE value type: bstr
    -- * Description: the RSA modulus n
    --
    -- The RSA modulus n is a product of u distinct odd primes
    -- r_i, i = 1, 2, ..., u, where u >= 2
    --
    -- All numeric key parameters are encoded in an unsigned big-endian
    -- representation as an octet sequence using the CBOR byte string
    -- type (major type 2). The octet sequence MUST utilize the minimum
    -- number of octets needed to represent the value. For instance, the
    -- value 32,768 is represented as the CBOR byte sequence 0b010_00010,
    -- 0x80 0x00 (major type 2, additional information 2 for the length).
    CoseKeyTypeParameterRSAN
  | -- | [(spec)](https://www.rfc-editor.org/rfc/rfc8230.html#section-4)
    --
    -- * COSE value type: bstr
    -- * Description: the RSA public exponent e
    --
    -- The RSA public exponent e is an integer between 3 and n - 1 satisfying
    -- GCD(e,\lambda(n)) = 1, where \lambda(n) = LCM(r_1 - 1, ..., r_u - 1)
    --
    -- All numeric key parameters are encoded in an unsigned big-endian
    -- representation as an octet sequence using the CBOR byte string
    -- type (major type 2). The octet sequence MUST utilize the minimum
    -- number of octets needed to represent the value. For instance, the
    -- value 32,768 is represented as the CBOR byte sequence 0b010_00010,
    -- 0x80 0x00 (major type 2, additional information 2 for the length).
    CoseKeyTypeParameterRSAE
  deriving (Eq, Show, Bounded, Enum)

-- | Serialises the parameters using the @Label@ column from the
-- [COSE Key Type Parameters registry](https://www.iana.org/assignments/cose/cose.xhtml#key-type-parameters)
instance Serialise CoseKeyTypeParameterRSA where
  encode CoseKeyTypeParameterRSAN = encodeInt (-1)
  encode CoseKeyTypeParameterRSAE = encodeInt (-2)
  decode =
    decodeIntCanonical >>= \case
      -1 -> pure CoseKeyTypeParameterRSAN
      -2 -> pure CoseKeyTypeParameterRSAE
      value -> fail $ "Unknown COSE key type parameter " <> show value <> " for key type RSA"

-- | The number of parameters for a 'CoseKeyType' relevant for Webauthn public
-- keys
parameterCount :: CoseKeyType -> Word
parameterCount CoseKeyTypeOKP = cardinality @CoseKeyCommonParameter + cardinality @CoseKeyTypeParameterOKP
parameterCount CoseKeyTypeEC2 = cardinality @CoseKeyCommonParameter + cardinality @CoseKeyTypeParameterEC2
parameterCount CoseKeyTypeRSA = cardinality @CoseKeyCommonParameter + cardinality @CoseKeyTypeParameterRSA

-- | A utility function for getting the number of constructors for a type
-- that implements both 'Bounded' and 'Enum'
cardinality :: forall a b. (Bounded a, Enum a, Num b) => b
cardinality = fromIntegral $ 1 + fromEnum @a maxBound - fromEnum @a minBound
