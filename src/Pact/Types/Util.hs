{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Pact.Types.Util
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Utility types and functions.
--
module Pact.Types.Util
  ( ParseText(..)
  , fromText, fromText'
  , lensyToJSON, lensyParseJSON
  , unsafeFromJSON, outputJSON
  , fromJSON'
  , Hash(..), hashToText
  , parseB16JSON, parseB16Text, parseB16TextOnly
  , toB16JSON, toB16Text
  , encodeBase64UrlUnpadded, decodeBase64UrlUnpadded
  , AsString(..), asString'
  , tShow, maybeDelim
  , modifyMVar', modifyingMVar, useMVar
  , failMaybe, maybeToEither
  ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Word
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding
import Control.Concurrent
import Control.Lens hiding (Empty)
import Control.DeepSeq
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Pact.Types.Pretty



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

lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error $ "lensyConstructorToNiceJson: bad arguments: " ++ show (n,fieldName)

newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic, Hashable)
instance Show Hash where
  show (Hash h) = show $ encodeBase64UrlUnpadded h
instance Pretty Hash where
  pretty = pretty . asString
instance AsString Hash where asString (Hash h) = decodeUtf8 (encodeBase64UrlUnpadded h)
instance NFData Hash

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

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText s = decodeUtf8 $ encodeBase64UrlUnpadded s


-- | All pact hashes are Blake2b_256, thus length 32.
hashLengthAsBS :: Int
hashLengthAsBS = 32


instance Serialize Hash where
  put (Hash h) = S.put h
  get = do
    raw <- S.get >>= S.getByteString
    if hashLengthAsBS == B.length raw
      then return $ Hash raw
      else fail $ "Unable to decode hash, wrong length: "
                ++ show (B.length raw)
                ++ " from original bytestring " ++ show raw

hashToText :: Hash -> Text
hashToText (Hash h) = toB64UrlUnpaddedText h

instance ToJSON Hash where
  toJSON = String . hashToText
instance FromJSON Hash where
  parseJSON = withText "Hash" parseText
  {-# INLINE parseJSON #-}
instance ParseText Hash where
  parseText s = Hash <$> parseB64UrlUnpaddedText s
  {-# INLINE parseText #-}


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

toB16JSON :: ByteString -> Value
toB16JSON s = String $ toB16Text s

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

failMaybe :: Monad m => String -> Maybe a -> m a
failMaybe err m = maybe (fail err) return m

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a)  = Right a


-- | Utility for unsafe parse of JSON
unsafeFromJSON :: FromJSON a => Value -> a
unsafeFromJSON v = case fromJSON v of Success a -> a; Error e -> error ("JSON parse failed: " ++ show e)

-- | Utility for GHCI output of JSON
outputJSON :: ToJSON a => a -> IO ()
outputJSON a = BSL8.putStrLn $ encode a

-- | Provide unquoted string representation.
class AsString a where asString :: a -> Text

instance AsString String where asString = pack
instance AsString Text where asString = id
instance AsString B.ByteString where asString = asString . decodeUtf8
instance AsString BSL8.ByteString where asString = asString . BSL8.toStrict
instance AsString Integer where asString = pack . show

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
maybeDelim :: Show a => String -> Maybe a -> String
maybeDelim d t = maybe "" ((d ++) . show) t
