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
module Pact.Types.Util where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Concurrent
import Control.Lens
import Control.DeepSeq
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S



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

lensyOptions :: Int -> Options
lensyOptions n = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson n }

lensyToJSON
  :: (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
lensyToJSON n = genericToJSON (lensyOptions n)

lensyParseJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
lensyParseJSON n = genericParseJSON (lensyOptions n)

newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic, Hashable)
instance Show Hash where
  show (Hash h) = show $ B16.encode h
instance AsString Hash where asString (Hash h) = decodeUtf8 (B16.encode h)
instance NFData Hash

-- NB: this hash is also used for the bloom filter, which needs 32bit keys
-- if you want to change this, you need to retool the bloom filter as well
-- So long as this is divisible by 4 you're fine
hashLengthAsBS :: Int
hashLengthAsBS = 64

hashLengthAsBase16 :: Int
hashLengthAsBase16 = hashLengthAsBS * 2

instance Serialize Hash where
  put (Hash h) = S.put h
  get = do
    raw <- S.get >>= S.getByteString
    if hashLengthAsBS == B.length raw
      then return $ Hash raw
      else fail $ "Unable to decode hash, wrong length: "
                ++ show (B.length raw)
                ++ " from original bytestring " ++ show raw

hashToB16Text :: Hash -> Text
hashToB16Text (Hash h) = toB16Text h

instance ToJSON Hash where
  toJSON = String . hashToB16Text
instance FromJSON Hash where
  parseJSON = withText "Hash" parseText
  {-# INLINE parseJSON #-}
instance ParseText Hash where
  parseText s = Hash <$> parseB16Text s
  {-# INLINE parseText #-}

lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error "You've managed to screw up the drop number or the field name"

parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text
{-# INLINE parseB16JSON #-}

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
                 (s,leftovers) | leftovers == B.empty -> return s
                               | otherwise -> fail $ "Base16 decode failed: " ++ show t
{-# INLINE parseB16Text #-}

toB16JSON :: ByteString -> Value
toB16JSON s = String $ toB16Text s

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

failMaybe :: Monad m => String -> Maybe a -> m a
failMaybe err m = maybe (fail err) return m



-- | Utility for unsafe parse of JSON
unsafeFromJSON :: FromJSON a => Value -> a
unsafeFromJSON v = case fromJSON v of Success a -> a; Error e -> error ("JSON parse failed: " ++ show e)

-- | Utility for GHCI output of JSON
outputJSON :: ToJSON a => a -> IO ()
outputJSON a = BSL8.putStrLn $ encode a


data RenderColor = RColor | RPlain

renderString :: Pretty a => (Doc -> SimpleDoc) -> RenderColor -> a -> String
renderString renderf colors p = displayS (renderf ((case colors of RColor -> id; RPlain -> plain) (pretty p))) ""

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString renderCompact RPlain

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString = renderString (renderPretty 0.4 100)


-- | Provide unquoted string representation.
class AsString a where asString :: a -> Text
instance AsString String where asString = pack
instance AsString Text where asString = id
instance AsString B.ByteString where asString = asString . decodeUtf8
instance AsString BSL8.ByteString where asString = asString . BSL8.toStrict
instance AsString Integer where asString = pack . show

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

-- | Prelude-friendly replacement for <$>
infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

-- | Pretty show.
pshow :: Show a => a -> Doc
pshow = text . show

tShow :: Show a => a -> Text
tShow = pack . show


asString' :: AsString a => a -> String
asString' = unpack . asString


maybeDelim :: Show a => String -> Maybe a -> String
maybeDelim d t = maybe "" ((d ++) . show) t
