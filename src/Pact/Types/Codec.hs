{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Pact.Types.Persistence
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- 'Codec' pairs 'ToJSON' and 'FromJSON' marshalling.
--
module Pact.Types.Codec
  ( Codec(..)
  , integerCodec
  , decimalCodec
  , timeCodec
  , valueCodec
  , pactISO8601Format
  , highPrecFormat
  , roundtripCodec
  , withThisText
  ) where


import Control.Applicative
import qualified Data.Aeson as A
import Data.Aeson hiding (Object)
import Data.Aeson.Types (Parser,parse)
import Data.Text (Text,unpack)
import Data.Decimal (Decimal,DecimalRaw(..))
import Data.Thyme.Time.Core
import Text.Read (readMaybe)
import Data.Ratio (denominator)
import System.Locale


-- | Min, max values that Javascript doesn't mess up.
--
--   http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html
--   "The integer part of the Number type in Javascript is safe in [-2^53 .. 2^53] (253 = 9 007 199 254 740 992).
--    Beyond this there will be precision loss on the least significant numbers."
jsIntegerBounds :: (Integer, Integer)
jsIntegerBounds = (-9007199254740991,9007199254740991)

isSafeInteger :: Integer -> Bool
isSafeInteger i = i >= l && i <= h
  where (l,h) = jsIntegerBounds

-- | JSON codec pair.
data Codec a = Codec {
  encoder :: a -> Value,
  decoder :: Value -> Parser a
  }

-- | Integers encode to an object that uses Number if in reasonable JS bounds or String otherwise.
integerCodec :: Codec Integer
integerCodec = Codec encodeInteger decodeInteger
  where
    encodeInteger i
      | isSafeInteger i = object [ field .= i ]
      | otherwise = object [ field .= show i ]
    {-# INLINE encodeInteger #-}
    decodeInteger = withObject "Integer" $ \o -> do
      s <- o .: field
      case s of
        Number n -> return (round n)
        String n -> case readMaybe (unpack n) of
          Just i -> return i
          Nothing -> fail $ "Invalid integer value: " ++ show s
        _ -> fail $ "Invalid integer value: " ++ show s
    {-# INLINE decodeInteger #-}
    field = "int"

-- | Decimals encode to a Scientific, which is encoded as an object + String
-- if mantissa precision exceeds JS.
-- TODO fromRational . toRational may not be the speediest.
decimalCodec :: Codec Decimal
decimalCodec = Codec enc dec
  where
    enc d@(Decimal _places mantissa)
      | isSafeInteger mantissa = Number $ fromRational $ toRational d
      | otherwise = object [ field .= show d ]
    {-# INLINE enc #-}
    dec (Number n) = return $ fromRational $ toRational n
    dec (A.Object o) = o .: field >>= \s -> case readMaybe (unpack s) of
      Just d -> return d
      Nothing -> fail $ "Invalid decimal value: " ++ show s
    dec v = fail $ "Invalid decimal value: " ++ show v
    {-# INLINE dec #-}
    field = "decimal"

-- | default Pact ISO8601 format
pactISO8601Format :: String
pactISO8601Format = "%Y-%m-%dT%H:%M:%SZ"

-- | high-precision format
highPrecFormat :: String
highPrecFormat = "%Y-%m-%dT%H:%M:%S.%vZ"

-- | Time uses
timeCodec :: Codec UTCTime
timeCodec = Codec enc dec
  where
    enc t
      | 1 == denom s = object [ field .= formatTime loc pactISO8601Format t ]
      | otherwise = object [ highprec .= formatTime loc highPrecFormat t ]
      where (UTCTime (ModifiedJulianDay _d) s) = unUTCTime t
            denom :: DiffTime -> Integer
            denom = denominator . toSeconds
    {-# INLINE enc #-}
    dec = withObject "time" $ \o ->
      (o .: field >>= mkTime pactISO8601Format) <|>
      (o .: highprec >>= mkTime highPrecFormat)
      where
        mkTime :: String -> String -> Parser UTCTime
        mkTime fmt v = case parseTime loc fmt v of
              Just t -> return t
              Nothing -> fail $ "Invalid time value, expected " ++ fmt
    {-# INLINE dec #-}
    field = "time"
    highprec = "timep"
    loc = defaultTimeLocale

valueCodec :: Codec Value
valueCodec = Codec enc dec
  where
    enc v = object [field .= v]
    {-# INLINE enc #-}
    dec = withObject "Value" $ \o -> o .: field
    {-# INLINE dec #-}
    field = "_P_val"


roundtripCodec :: Codec t -> t -> Result t
roundtripCodec c t = parse (decoder c) $ encoder c t

withThisText :: String -> Text -> Value -> Parser a -> Parser a
withThisText s t v p = withText s go v
  where
    go tv | tv == t = p
          | otherwise = fail $ s ++ ": Expected " ++ show t
