{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Pact.Types.Util
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Utility types and functions.
--
module Pact.Types.Util
  ( -- | Text parsing
    ParseText(..)
  , fromText, fromText'
  -- | JSON helpers
  , satisfiesRoundtripJSON, roundtripJSONToEither
  , lensyToJSON, lensyParseJSON, lensyOptions, lensyConstructorToNiceJson
  , unsafeFromJSON, outputJSON
  , fromJSON'
  , enableToJSON
  -- | Base 16 helpers
  , parseB16JSON, parseB16Text, parseB16TextOnly
  , toB16Text
  , B16JsonBytes(..)
  -- | Base64Url helpers
  , encodeBase64UrlUnpadded, decodeBase64UrlUnpadded
  , parseB64UrlUnpaddedText, parseB64UrlUnpaddedText'
  , toB64UrlUnpaddedText, fromB64UrlUnpaddedText
  , B64JsonBytes(..)
  -- | AsString
  , AsString(..), asString'
  -- | MVar-as-state utils
  , modifyMVar', modifyingMVar, useMVar
  -- | Miscellany
  , tShow, maybeDelim
  , maybeToEither
  -- | Wrapping utilities
  , rewrap, rewrapping, wrap, unwrap
  -- | Arbitrary generator utils
  , genBareText
  , arbitraryIdent
  ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SB
import qualified Text.Trifecta as Trifecta
import Data.Char
import Data.Either (isRight)
import Data.Hashable (Hashable)
import Data.Word
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Lens hiding (Empty, elements, (.=))
import Test.QuickCheck hiding (Result, Success)

import Pact.Types.Parser (style, symbols)

import GHC.Stack (HasCallStack)

import qualified Pact.JSON.Encode as J

class ParseText a where
  parseText :: Text -> Parser a



fromText :: ParseText a => Text -> Result a
fromText = parse parseText
{-# INLINE fromText #-}

resultToEither :: Result a -> Either String a
resultToEither (Success s) = Right s
resultToEither (Error s) = Left s

fromText' :: ParseText a => Text -> Either String a
fromText' = resultToEither . fromText

satisfiesRoundtripJSON :: (Eq a, FromJSON a, J.Encode a) => a -> Bool
satisfiesRoundtripJSON i = case roundtripJSONToEither i of
  Left _ -> False
  Right j -> i == j

roundtripJSONToEither :: (FromJSON a, J.Encode a) => a -> Either String a
roundtripJSONToEither = eitherDecode . J.encode

fromJSON' :: FromJSON a => Value -> Either String a
fromJSON' = resultToEither . fromJSON

lensyToJSON
  :: (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
lensyToJSON n = genericToJSON (lensyOptions n)

lensyParseJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
lensyParseJSON n = genericParseJSON (lensyOptions n)

lensyOptions :: Int -> Options
lensyOptions n = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson n }

lensyConstructorToNiceJson :: HasCallStack => Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error $ "lensyConstructorToNiceJson: bad arguments: " ++ show (n,fieldName)

encodeBase64UrlUnpadded :: ByteString -> ByteString
encodeBase64UrlUnpadded = fst . B.spanEnd (== equalWord8) . B64URL.encode

decodeBase64UrlUnpadded :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded = B64URL.decode . pad
  where pad t = let s = B.length t `mod` 4 in t <> B.replicate ((4 - s) `mod` 4) equalWord8

equalWord8 :: Word8
equalWord8 = toEnum $ fromEnum '='

parseB64UrlUnpaddedText :: Text -> Parser ByteString
parseB64UrlUnpaddedText t = case decodeBase64UrlUnpadded (encodeUtf8 t) of
  Right s -> return s
  Left e -> fail $ "Base64URL decode failed: " ++ e
{-# INLINE parseB64UrlUnpaddedText #-}

parseB64UrlUnpaddedText' :: Text -> Either String ByteString
parseB64UrlUnpaddedText' = resultToEither . parse parseB64UrlUnpaddedText

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText s = decodeUtf8 $ encodeBase64UrlUnpadded s

fromB64UrlUnpaddedText :: ByteString -> Either String Text
fromB64UrlUnpaddedText bs = case decodeBase64UrlUnpadded bs of
  Right bs' -> case decodeUtf8' bs' of
    Left _ -> Left "Base64URL decode failed: invalid unicode"
    Right t -> Right t
  Left e -> Left $ "Base64URL decode failed: " ++ e

parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text
{-# INLINE parseB16JSON #-}

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
                 (s,leftovers) | leftovers == B.empty -> return s
                               | otherwise -> fail $ "Base16 decode failed: " ++ show t
{-# INLINE parseB16Text #-}

parseB16TextOnly :: Text -> Either String ByteString
parseB16TextOnly t = resultToEither $ parse parseB16Text t

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a)  = Right a


-- | Support for 'toJSON' is required for YAML encodings, which is required by
-- most Pact data types.
--
-- From verison 1.4 of the hashable package onward, 'toJSON' will result in
-- encodings that are not backward compatible. Similarly, for aeson >= 2
-- encodings will change.
--
-- The only option to keep the ordering of properties stable with aeson-2 would
-- be to define a custom type for property names, which would allow to define
-- backward compatile 'Ord' instances.
--
enableToJSON
    :: HasCallStack
    => String
    -> Value
    -> Value
enableToJSON t _ = error $ t <> ": encoding to Data.Aeson.Value is unstable and therefore not supported"
{-# INLINE enableToJSON #-}

-- | Utility for unsafe parse of JSON
unsafeFromJSON :: HasCallStack => FromJSON a => Value -> a
unsafeFromJSON v = case fromJSON v of Success a -> a; Error e -> error ("JSON parse failed: " ++ show e)

-- | Utility for GHCI output of JSON
outputJSON :: J.Encode a => a -> IO ()
outputJSON a = BSL8.putStrLn $ J.encode a

-- | Tagging ByteStrings (and isomorphic types) that are JSON encoded as
-- Hex strings
newtype B16JsonBytes = B16JsonBytes { _b16JsonBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Hashable, Generic)

instance FromJSON B16JsonBytes where
    parseJSON = fmap B16JsonBytes . parseB16JSON
    {-# INLINE parseJSON #-}
instance FromJSONKey B16JsonBytes where
    fromJSONKey = FromJSONKeyTextParser (fmap B16JsonBytes . parseB16Text)
    {-# INLINE fromJSONKey #-}
instance AsString B16JsonBytes where
    asString = toB16Text . _b16JsonBytes
    {-# INLINE asString #-}

-- | Tagging ByteStrings (and isomorphic types) that are JSON encoded as
-- Base64Url (without padding) strings
newtype B64JsonBytes = B64JsonBytes { _b64JsonBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Hashable, Generic)

instance FromJSON B64JsonBytes where
    parseJSON = fmap B64JsonBytes . withText "Base64" parseB64UrlUnpaddedText
    {-# INLINE parseJSON #-}
instance FromJSONKey B64JsonBytes where
    fromJSONKey = FromJSONKeyTextParser (fmap B64JsonBytes . parseB64UrlUnpaddedText)
    {-# INLINE fromJSONKey #-}
instance AsString B64JsonBytes where
    asString = toB64UrlUnpaddedText . _b64JsonBytes
    {-# INLINE asString #-}

-- | Provide unquoted string representation.
class AsString a where asString :: a -> Text

instance AsString String where asString = pack
instance AsString Text where asString = id
instance AsString B.ByteString where asString = asString . decodeUtf8
instance AsString SB.ShortByteString where asString = asString . decodeUtf8 . SB.fromShort
instance AsString BSL8.ByteString where asString = asString . BSL8.toStrict
instance AsString Integer where asString = pack . show
instance AsString a => AsString (Maybe a) where asString = maybe "" asString

asString' :: AsString a => a -> String
asString' = unpack . asString

-- | Pure version of 'modifyMVar_'
modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' mv f = modifyMVar_ mv (pure . f)
{-# INLINE modifyMVar' #-}

-- | Modify the target of a lens.
modifyingMVar :: MVar s -> Lens' s a -> (a -> IO a) -> IO ()
modifyingMVar mv l f = modifyMVar_ mv $ \ps -> (\b -> set l b ps) <$> f (view l ps)
{-# INLINE modifyingMVar #-}

-- | Lens view into mvar.
useMVar :: MVar s -> Getting a s a -> IO a
useMVar e l = view l <$> readMVar e
{-# INLINE useMVar #-}

-- | Text-y show
tShow :: Show a => a -> Text
tShow = pack . show

-- | Show with prepended delimter if not 'Nothing'
maybeDelim :: AsString a => Text -> Maybe a -> Text
maybeDelim d t = maybe "" ((d <>) . asString) t

-- | Re-wrapping lens.
rewrapping :: (Profunctor p, Functor f, Wrapped b, Wrapped a,
               Unwrapped a ~ Unwrapped b) =>
              p a (f a) -> p b (f b)
rewrapping = _Wrapped' . _Unwrapped'

-- | Rewrap utility between isomorphic wrappings.
rewrap :: (Wrapped a, Wrapped b, Unwrapped b ~ Unwrapped a) => a -> b
rewrap = view rewrapping

-- | Utility to construct a 'Wrapped a'
wrap :: Wrapped a => Unwrapped a -> a
wrap = view _Unwrapped'

-- | Utility to destruct a 'Wrapped a'
unwrap :: Wrapped a => a -> Unwrapped a
unwrap = view _Wrapped'

-- | Utility to generate arbitrary non empty text
-- restricted to alphabetic unicode, numbers, and some symbols.
-- Will not generate strings "true" and "false", but could generate
-- other native function names.
genBareText :: Gen Text
genBareText = genText
  where genText :: Gen Text
        genText = do
          start <- genStartChar
          rest <- genRestOfString
          pure $ pack ([start] <> rest)

        genStartChar :: Gen Char
        genStartChar = suchThat arbitrary (isValidBareChar bareStartCharParser)

        genRestOfString :: Gen [Char]
        genRestOfString = listOf (suchThat arbitrary (isValidBareChar bareRestCharParser))

        isValidBareChar :: (Text -> Either String Text) -> Char -> Bool
        isValidBareChar parser c = isRight (parser (pack [c]))

        -- Checks that Text provided starts with letter or
        -- set of approved symbols.
        -- Similar parser used with BareName.
        bareStartCharParser :: Text -> Either String Text
        bareStartCharParser = AP.parseOnly $
          Trifecta.ident style <* Trifecta.eof

        -- Checks that Text provided starts with letter,
        -- set of approved symbols, or digits.
        bareRestCharParser :: Text -> Either String Text
        bareRestCharParser = AP.parseOnly $
          Trifecta.ident style' <* Trifecta.eof

        -- Uses same character set for start of text as for rest of it.
        -- Useful when randomly generating texts character by character.
        style' = style { Trifecta._styleStart = Trifecta.letter <|> Trifecta.digit <|> symbols }

-- | Less general than 'genBarText', but probably faster
--
arbitraryIdent :: Gen Text
arbitraryIdent = cons
  <$> elements (syms <> letters)
  <*> (pack <$> listOf (elements (syms <> letters <> digits)))
 where
  syms = "%#+-_&$@<>=^?*!|/~"
  letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZñûüùúūÛÜÙÚŪß"
  digits = "0123456789"

