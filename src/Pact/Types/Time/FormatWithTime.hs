{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Pact.Types.Time.FormatWithTime
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Pact.Types.Time.FormatWithTime
(
-- * Formatting and Parsing
  parseTime
, formatTime
) where

import Data.Aeson
import qualified Data.Time as T

import Text.Read (readPrec)

-- internal modules

import Pact.Types.Time.Internal

-- -------------------------------------------------------------------------- --
-- Internal Utils

toUtcTime :: UTCTime -> T.UTCTime
toUtcTime t = T.UTCTime (T.ModifiedJulianDay (fromIntegral d)) $ realToFrac dt / 1000000
  where
    Julian (ModifiedJulianDay d) (NominalDiffTime dt) = toJulian t

fromUtcTime :: T.UTCTime -> UTCTime
fromUtcTime (T.UTCTime (T.ModifiedJulianDay d) t) = fromJulian
    $ Julian (ModifiedJulianDay (fromIntegral d)) (NominalDiffTime $ round (t * 1000000))

-- -------------------------------------------------------------------------- --
-- Formatting and Parsing

-- | Parse a 'UTCTime' using the supplied format string.
--
-- Please refer to the [Pact Language
-- Reference](https://pact-language.readthedocs.io/en/stable/pact-reference.html#time-formats) for details on the
-- supported format strings.
--
#if MIN_VERSION_time(1,9,0)
parseTime :: MonadFail m => String -> String -> m UTCTime
#else
parseTime :: String -> String -> Maybe UTCTime
#endif
parseTime format = fmap fromUtcTime . parseTime_ T.defaultTimeLocale format
{-# INLINE parseTime #-}

-- | Format a 'UTCTime' value using the supplied format string.
--
-- Please refer to the [Pact Language
-- Reference](https://pact-language.readthedocs.io/en/stable/pact-reference.html#time-formats) for details on the
-- supported format strings.
--
formatTime :: String -> UTCTime -> String
formatTime format = formatTime_ T.defaultTimeLocale format . toUtcTime
{-# INLINE formatTime #-}

-- -------------------------------------------------------------------------- --
-- Inherited instances

-- | The 'Show' instance is inherited from the "official" time package.
--
instance Show NominalDiffTime where
    show = show @T.NominalDiffTime . realToFrac . toSeconds
    {-# INLINE show #-}

-- | The 'Show' instance is inherited from the "official" time package.
--
instance Show UTCTime where
    show = show . toUtcTime
    {-# INLINE show #-}

-- | The 'Read' instance is inherited from the "official" time package.
--
instance Read UTCTime where
    readPrec = fromUtcTime <$> readPrec
    {-# INLINE readPrec #-}

-- | The 'ToJSON' instance is inherited from the instance of 'T.UTCTime' from
-- the "official" time package.
--
instance ToJSON UTCTime where
    toJSON = toJSON . toUtcTime
    {-# INLINE toJSON #-}

-- | The 'FromJSON' instance is inherited from the instance of 'T.UTCTime' from
-- the "official" time package.
--
instance FromJSON UTCTime where
    parseJSON = fmap fromUtcTime . parseJSON
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Ported Implementations from Time

#if MIN_VERSION_time(1,9,0)
parseTime_ :: MonadFail m => T.TimeLocale -> String -> String -> m T.UTCTime
#else
parseTime_ :: T.TimeLocale -> String -> String -> Maybe T.UTCTime
#endif
parseTime_ locale formatStr = T.parseTimeM False locale (mapFormat formatStr)
    where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('.':'%':'v':t) = "%Q" <> mapFormat t
    mapFormat ('%':'N':t) = "%z" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t

formatTime_ :: T.TimeLocale -> String -> T.UTCTime -> String
#if MIN_VERSION_time(1,9,0)
formatTime_ locale formatStr = T.formatTime locale (mapFormat formatStr)
  where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('%':'v':t) = "%6q" <> mapFormat t
    mapFormat ('%':'N':t) = "%Ez" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t
#else
formatTime_ locale formatStr timeValue = concat . snd $ go0 formatStr
  where
    format f = T.formatTime locale f timeValue
    n = let (h,m) = splitAt 3 $ format "%z" in h <> ":" <> m
    v = format "%6q"

    go0 s = let (a, b) = go1 s in ("", format a : b)

    go1 ('%':'%':t) = let (a, b) = go1 t in ("", "%" : format a : b)
    go1 ('%':'v':t) = let (a, b) = go1 t in ("", v : format a : b)
    go1 ('%':'N':t) = let (a, b) = go1 t in ("", n : format a : b)
    go1 (h:t) = let (a, b) = go1 t in (h:a, b)
    go1 "" = ("", [])
#endif

