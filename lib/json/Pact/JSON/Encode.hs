{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Pact.JSON.Encode
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- RFC8259 compliant JSON encoding.
--
-- Primitives are provides to generate encodings that are are binary compatible
-- with aeson <2.
--
-- The implementation gives full control of the ordering of object members to
-- the user.
--
module Pact.JSON.Encode
( Encode(..)
, encode
, encodeStrict
, Builder
, KeyValue(..)
, (.=)
, (.?=)
, (.??=)
, ifMaybe
, object
, array
, text
, null
, number
, true
, false

-- Newtype wrappers
, Array(..)
, Object(..)
, Base10(..)
, Base16(..)
, Aeson(..)
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BBP
import qualified Data.ByteString.Builder.Scientific as BBS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Foldable hiding (null)
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.List as L
import qualified Data.List.NonEmpty as LNE
import Data.Maybe
import Data.Monoid
import qualified Data.Scientific as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_base(4,16,0)
import Data.Tuple
#endif
import qualified Data.Vector as V
import Data.Void
import Data.Word

import Numeric.Natural

import Prelude hiding (null)

-- internal modules

-- -------------------------------------------------------------------------- --
-- Priorities

infixr 8 .=, .?=, .??=

-- -------------------------------------------------------------------------- --
-- Json Builder Abstraction
--
-- All functions that are exposed from the module guarantee that the the value
-- of a builder is valid JSON.

newtype Builder = Builder { _unBuilder :: BB.Builder }
  deriving newtype (Semigroup, Monoid)

singleton :: Char -> Builder
singleton = Builder . BB.charUtf8
{-# INLINE singleton #-}

fromText :: T.Text -> Builder
fromText = Builder . BB.byteString . T.encodeUtf8
{-# INLINE fromText #-}

integerDecimal :: Integer -> Builder
integerDecimal = Builder . BB.integerDec
{-# INLINE integerDecimal #-}

bytesHex :: B.ByteString -> Builder
bytesHex = Builder . BB.byteStringHex
{-# INLINE bytesHex #-}

scientificDecimal :: S.Scientific -> Builder
scientificDecimal = Builder . BBS.scientificBuilder
{-# INLINE scientificDecimal #-}

double :: Double -> Builder
double = Builder . BB.doubleDec
{-# INLINE double #-}

float :: Float -> Builder
float = Builder . BB.floatDec
{-# INLINE float #-}

word8Hex :: Word8 -> Builder
word8Hex = Builder . BB.word8Hex
{-# INLINE word8Hex #-}

word16Hex :: Word16 -> Builder
word16Hex = Builder . BB.word16Hex
{-# INLINE word16Hex #-}

word32Hex :: Word32 -> Builder
word32Hex = Builder . BB.word32Hex
{-# INLINE word32Hex #-}

word64Hex :: Word64 -> Builder
word64Hex = Builder . BB.word64Hex
{-# INLINE word64Hex #-}

wordHex :: Word -> Builder
wordHex = Builder . BB.wordHex
{-# INLINE wordHex #-}

-- -------------------------------------------------------------------------- --
--

encodeStrict :: Encode a => a -> B.ByteString
encodeStrict = BL.toStrict . encode
{-# INLINE encodeStrict #-}

encode :: Encode a => a -> BL.ByteString
encode = BB.toLazyByteString . _unBuilder . build
{-# INLINE encode #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 2, Grammar
--
-- Insignificant whitespace is always omitted by this implementation.
--
-- Structural Characters:

-- | Name-separator
--
colon :: Builder
colon = singleton ':'
{-# INLINE colon #-}

-- | Value-separator
--
comma :: Builder
comma = singleton ','
{-# INLINE comma #-}

-- | Begin-object
--
obrace :: Builder
obrace = singleton '{'
{-# INLINE obrace #-}

-- | End-object
--
cbrace :: Builder
cbrace = singleton '}'
{-# INLINE cbrace #-}

-- | Begin-Array
--
obracket :: Builder
obracket = singleton '['
{-# INLINE obracket #-}

-- | End-Array
--
cbracket :: Builder
cbracket = singleton ']'
{-# INLINE cbracket #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 3, Values
--
-- Literal Names:

null :: Builder
null = fromText "null"
{-# INLINE null #-}

true :: Builder
true = fromText "true"
{-# INLINE true #-}

false :: Builder
false = fromText "false"
{-# INLINE false #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 4, Objects

data KeyValue = KeyValue T.Text Builder

instance Encode KeyValue where
  build (KeyValue k v) = text k <> colon <> v
  {-# INLINE build #-}

list :: Encode a => [a] -> Builder
list [] = mempty
list [h] = build h
list (h:t) = build h <> comma <> list t
{-# INLINE list #-}

object :: Foldable f => f (Maybe KeyValue) -> Builder
object a = obrace <> list (catMaybes $ toList a) <> cbrace
{-# INLINE object #-}

(.=) :: Encode a => T.Text -> a -> Maybe KeyValue
k .= v = Just $ KeyValue k $ build v
{-# INLINE (.=) #-}

(.?=) :: Encode a => T.Text -> Maybe a -> Maybe KeyValue
(.?=) k = fmap $ KeyValue k . build
{-# INLINE (.?=) #-}

(.??=) :: Encode a => Eq a => Monoid a => T.Text -> a -> Maybe KeyValue
(.??=) k a
    | a == mempty = Nothing
    | otherwise = Just $ KeyValue k $ build a
{-# INLINE (.??=) #-}

-- | Helper for use with '(.?=)'
--
ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f a = if f a then Just a else Nothing
{-# INLINE ifMaybe #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 5, Arrays

array :: Foldable f => Encode a => f a -> Builder
array a = obracket <> list (toList a) <> cbracket
{-# INLINE array #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 6, Numbers

number :: S.Scientific -> Builder
number = scientific
{-# INLINE number #-}

-- Backward compatible (unsafe) encodings of numbers

scientific :: S.Scientific -> Builder
scientific s
  | e < 0 || e > 1024 = scientificDecimal s
  | otherwise = integerDecimal (S.coefficient s * 10 ^ e)
 where
  e = S.base10Exponent s

-- realFloat :: RealFloat a => a -> Builder
-- realFloat d
--   | isNaN d = null
--   | isInfinite d = if d > 0 then fromText "+inf" else fromText "-inf"
--   | otherwise = scientific $ S.fromFloatDigits d

realFloat :: RealFloat a => (a -> Builder) -> a -> Builder
realFloat e d
  | isNaN d = null
  | isInfinite d = if d > 0 then fromText "\"+inf\"" else fromText "\"-inf\""
  | otherwise = e d
{-# INLINE realFloat #-}

-- -------------------------------------------------------------------------- --
-- RFC 8259, section 7, Strings
--
-- > [...]
-- > All Unicode characters may be placed within the quotation marks, except for
-- > the characters that MUST be escaped: quotation mark, reverse solidus, and the
-- > control characters (U+0000 through U+001F).
-- >
-- > Any character may be escaped.
-- > [...]
--
-- @
--    string = quotation-mark *char quotation-mark
--
--      char = unescaped /
--          escape (
--              %x22 /          ; "    quotation mark  U+0022
--              %x5C /          ; \    reverse solidus U+005C
--              %x2F /          ; /    solidus         U+002F
--              %x62 /          ; b    backspace       U+0008
--              %x66 /          ; f    form feed       U+000C
--              %x6E /          ; n    line feed       U+000A
--              %x72 /          ; r    carriage return U+000D
--              %x74 /          ; t    tab             U+0009
--              %x75 4HEXDIG )  ; uXXXX                U+XXXX
--
--      escape = %x5C              ; \
--
--      quotation-mark = %x22      ; "
--
--      unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
-- @
--
-- The escaping of this implementation this compatible with escaping performed
-- by aeson.

quote :: Builder
quote = singleton '"'
{-# INLINE quote #-}

text :: T.Text -> Builder
text t = quote <> Builder (T.encodeUtf8BuilderEscaped escapeAscii t) <> quote
{-# INLINE text #-}

-- The following code in this section is copy and pasted from aeson.

escapeAscii :: BBP.BoundedPrim Word8
escapeAscii =
  BBP.condB (== c2w '\\') (ascii2 ('\\','\\')) $
  BBP.condB (== c2w '\"') (ascii2 ('\\','"')) $
  BBP.condB (>= c2w '\x20') (BBP.liftFixedToBounded BBP.word8) $
  BBP.condB (== c2w '\n') (ascii2 ('\\','n')) $
  BBP.condB (== c2w '\r') (ascii2 ('\\','r')) $
  BBP.condB (== c2w '\t') (ascii2 ('\\','t')) $
  BBP.liftFixedToBounded hexEscape -- fallback for chars < 0x20
 where
  hexEscape :: BBP.FixedPrim Word8
  hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BBP.>$< BBP.char8 BBP.>*< BBP.char8 BBP.>*< BBP.word16HexFixed
{-# INLINE escapeAscii #-}

c2w :: Char -> Word8
c2w c = fromIntegral (ord c)
{-# INLINE c2w #-}

ascii2 :: (Char, Char) -> BBP.BoundedPrim a
ascii2 cs = BBP.liftFixedToBounded $ const cs BBP.>$< BBP.char7 BBP.>*< BBP.char7
{-# INLINE ascii2 #-}

-- -------------------------------------------------------------------------- --
-- Encode Class

class Encode a where
  build :: a -> Builder

instance Encode Builder where
  build = id
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- Instances

-- We deliberately provide only provide a small set of unopinionated instances.
--
-- PLEASE, DO NOT PROVIDE OPINIONATED INSTANCES.
--
-- Convenience for the programmer isn't a goal of this framefork!
--
-- Instead that goal is that instances are explicit about how values of a given
-- type are serialized. There should be no hidden magic.

instance Encode Bool where
  build True = true
  build False = false
  {-# INLINE build #-}

instance Encode T.Text where
  build = text
  {-# INLINE build #-}

instance Encode Void where
  build = absurd
  {-# INLINE build #-}

instance Encode a => Encode (Maybe a) where
  build Nothing = null
  build (Just x) = build x
  {-# INLINE build #-}

deriving newtype instance Encode Any
deriving newtype instance Encode All

-- | Numbers in textual decimal encoding
--
newtype Base10 n = Base10 n

instance Integral n => Encode (Base10 n) where
  build (Base10 n) = object
    [ "decimal" .= integerDecimal (fromIntegral n) ]
  {-# INLINE build #-}

instance Encode (Base10 Float) where
  build (Base10 n) = object
    [ "decimal" .= (quote <> float n <> quote) ]
  {-# INLINE build #-}

instance Encode (Base10 Double) where
  build (Base10 n) = object
    [ "decimal" .= (quote <> double n <> quote) ]
  {-# INLINE build #-}

instance Encode (Base10 S.Scientific) where
  build (Base10 n) = object
    [ "decimal" .= (quote <> scientificDecimal n <> quote) ]
  {-# INLINE build #-}

-- | Numbers in textual big-endian hexdecimal encoding
--
-- (no leading zeros and no '0x' prefix)
--
newtype Base16 n = Base16 n

instance Encode (Base16 Word8) where
  build (Base16 n) = object
    [ "hexadecimal" .= word8Hex n ]
  {-# INLINE build #-}

instance Encode (Base16 Word16) where
  build (Base16 n) = object
    [ "hexadecimal" .= word16Hex n ]
  {-# INLINE build #-}

instance Encode (Base16 Word32) where
  build (Base16 n) = object
    [ "hexadecimal" .= word32Hex n ]
  {-# INLINE build #-}

instance Encode (Base16 Word64) where
  build (Base16 n) = object
    [ "hexadecimal" .= word64Hex n ]
  {-# INLINE build #-}

instance Encode (Base16 Word) where
  build (Base16 n) = object
    [ "hexadecimal" .= wordHex n ]
  {-# INLINE build #-}

-- TODO
--
-- * Base16Fixed (for bytes)
-- * Base64Url
-- * Decimal
-- * PactTime

-- -------------------------------------------------------------------------- --
-- Array

-- | Values are encoded as JSON Arrays. Missing or Neutral values are encoded
-- as empty Arrays.
--
newtype Array a = Array a

instance Encode a => Encode (Array [a]) where
  build (Array a) = array a
  {-# INLINE build #-}

instance Encode a => Encode (Array (V.Vector a)) where
  build (Array a) = array $ toList a
  {-# INLINE build #-}

instance Encode a => Encode (Array (LNE.NonEmpty a)) where
  build (Array a) = array $ toList a
  {-# INLINE build #-}

instance Encode (Array ()) where
  build (Array ()) = array @_ @Void []
  {-# INLINE build #-}

-- | Uses the Ord instance for the order of the elements
--
instance Encode a => Encode (Array (Set.Set a)) where
  build (Array s) = array $ toList s
  {-# INLINE build #-}

#if MIN_VERSION_base(4,16,0)
instance Encode a => Encode (Array (Solo a)) where
  build (Array a) = array [a]
  {-# INLINE build #-}
#endif

instance (Encode a, Encode b) => Encode (Array (a,b)) where
  build (Array (a,b)) = array [build a, build b]
  {-# INLINE build #-}

instance (Encode a, Encode b, Encode c) => Encode (Array (a,b,c)) where
  build (Array (a,b,c)) = array [build a, build b, build c]
  {-# INLINE build #-}

instance (Encode a, Encode b, Encode c, Encode d) => Encode (Array (a,b,c,d)) where
  build (Array (a,b,c,d)) = array [build a, build b, build c, build d]
  {-# INLINE build #-}

instance (Encode a, Encode b, Encode c, Encode d, Encode e) => Encode (Array (a,b,c,d,e)) where
  build (Array (a,b,c,d,e)) = array [build a, build b, build c, build d, build e]
  {-# INLINE build #-}

instance (Encode a, Encode b, Encode c, Encode d, Encode e, Encode f) => Encode (Array (a,b,c,d,e,f)) where
  build (Array (a,b,c,d,e,f)) = array [build a, build b, build c, build d, build e, build f]
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- Object

-- | Values are encoded as JSON Objects. Missing or Neutral values are encoded
-- as empty Objects.
--
-- Keys are of type 'T.Text'.
--
newtype Object a = Object a

instance Encode (Object [Maybe KeyValue]) where
  build (Object o) = object o
  {-# INLINE build #-}

instance Encode (Object [KeyValue]) where
  build (Object o) = object $ Just <$> o
  {-# INLINE build #-}

instance Encode a => Encode (Object [(T.Text, a)]) where
  build (Object o) = object $ fmap (uncurry (.=)) o
  {-# INLINE build #-}

instance Encode a => Encode (Object (V.Vector (T.Text, a))) where
  build (Object a) = build . Object $ toList a
  {-# INLINE build #-}

instance Encode a => Encode (Object (LNE.NonEmpty (T.Text, a))) where
  build (Object a) = build . Object $ toList a
  {-# INLINE build #-}

instance Encode (Object ()) where
  build (Object ()) = object []
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- Aeson Backward Compatibility

-- | Provide backward compatible encodings for various types.
--
newtype Aeson n = Aeson n

instance Encode (Aeson ()) where
  build _ = build $ Array ()
  {-# INLINE build #-}

instance Encode a => Encode (Aeson [a]) where
  build (Aeson a) = build $ Array a
  {-# INLINE build #-}

-- | Provide backward compatible encoding of scientific numbers.
--
instance Encode (Aeson S.Scientific) where
  build (Aeson n) = scientific n
  {-# INLINE build #-}

-- | Provide backward compatible encoding of floating point numbers.
--
instance Encode (Aeson Double) where
  build (Aeson n) = realFloat double n
  {-# INLINE build #-}

-- | Provide backward compatible encoding of floating point numbers.
--
instance Encode (Aeson Float) where
  build (Aeson n) = realFloat float n
  {-# INLINE build #-}

-- Provide Backward compatible (unsafe) encoding and decoding of integral
-- numbers. It is unsafe because the precision of the input may be beyond what
-- most JSON parser can represent.

instance Encode (Aeson Word8) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Word16) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Word32) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Word64) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Word) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}

instance Encode (Aeson Int8) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Int16) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Int32) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Int64) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Int) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}

instance Encode (Aeson Integer) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}
instance Encode (Aeson Natural) where
  build (Aeson n) = integerDecimal $ fromIntegral n
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- Encoding Aeson Value

-- | Encode an aeson 'Value'. For aeson version 2 onwoards, properties are sort
-- lexicographically by the textual representaiton of the key values in
-- ascending order.
--
instance Encode A.Value where
  build (A.Object o) = build . Object . fmap (first A.toText) $ A.toAscList o
  build (A.Array a) = build $ Array a
  build (A.String s) = build s
  build (A.Bool b) = build b
  build (A.Number n) = build $ Aeson n
  build A.Null = null
  {-# INLINE build #-}

