{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Pact.Types.Time
-- Copyright: Copyright © 2018 Kadena LLC
-- License:  BSD-style (see the file LICENSE)
-- Maintainer:  Stuart Popejoy <stuart@kadena.io>
--
-- A tiny time library for Pact.
--
-- The API and is mostly a subset of the thyme package, which this module
-- intents to replace.
--
-- Formatting and parsing of time values is implemented via the respective
-- functions of the time package.
--
-- The implementatin ignores leap seconds and therefore only uses nominal
-- time differences.
--
module Pact.Types.Time
(
-- * NominalDiffTime
  NominalDiffTime(..)
, microseconds
, toMicroseconds
, fromMicroseconds
, seconds
, toSeconds
, fromSeconds
, nominalDay

-- * UTCTime
, UTCTime
, getCurrentTime
, day
, dayTime
, fromDayAndDayTime

-- ** Formatting and Parsing
, parseTime
, formatTime

-- * Julian Dates
, T.Day(..)
, Julian(..)
, julianDay
, julianDayTime
, julianEpoch
, toJulian
, fromJulian
, julian
, julianEpochUtc

-- * POSIX Time
, POSIXTime
, toTimestampMicros
, fromTimestampMicros
, getPOSIXTime
, posixEpoch
, posixEpochUtc
, posixEpochDay
, fromPosix
, toPosix

-- * Reexports
, AffineSpace(..)
, VectorSpace(..)
) where

import Control.DeepSeq
import Control.Lens

import Data.AdditiveGroup
import Data.Aeson
import Data.AffineSpace
import Data.Decimal
import Data.Serialize
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import Data.VectorSpace

import GHC.Generics hiding (from)
import GHC.Int (Int64)

import Test.QuickCheck.Arbitrary

import Text.Read (readPrec)

-- -------------------------------------------------------------------------- --
-- Orphans

deriving newtype instance Arbitrary T.Day

-- -------------------------------------------------------------------------- --
-- Internal Utils

toUtcTime :: UTCTime -> T.UTCTime
toUtcTime t = T.UTCTime d $ realToFrac dt / 1000000
  where
    Julian d (NominalDiffTime dt) = toJulian t

fromUtcTime :: T.UTCTime -> UTCTime
fromUtcTime (T.UTCTime d t) = fromJulian
    $ Julian d (NominalDiffTime $ round (t * 1000000))

-- -------------------------------------------------------------------------- --
-- Nominal Diff Time

-- | A time interval as measured by UTC, that does not take leap-seconds
-- into account.
--
-- For instance, the difference between @23:59:59@ and @00:00:01@ on the
-- following day is always 2 seconds of 'NominalDiffTime', regardless of
-- whether a leap-second took place.
--
-- 'NominalDiffTime' forms an 'AdditiveGroup'―so can be added using '^+^'
-- (or '^-^' for subtraction), and also an instance of 'VectorSpace'―so can
-- be scaled using '*^', where
--
-- @
-- type 'Scalar' 'NominalDiffTime' = 'Rational'
-- @
--
newtype NominalDiffTime = NominalDiffTime { _microseconds :: Int64 }
    deriving (Eq, Ord, Bounded)
    deriving newtype (NFData, Arbitrary)

-- | Convert between 'NominalDiffTime' and 64-bit representation of
-- microseconds.
--
microseconds :: Iso' NominalDiffTime Int64
microseconds = iso (\(NominalDiffTime m) -> m) NominalDiffTime
{-# INLINE microseconds #-}

-- | Convert from 'NominalDiffTime' to a 64-bit representation of microseconds.
--
toMicroseconds :: NominalDiffTime -> Int64
toMicroseconds = view microseconds
{-# INLINE toMicroseconds #-}

-- | Convert from a 64-bit representation of microseconds to 'NominalDiffTime'.
--
fromMicroseconds :: Int64 -> NominalDiffTime
fromMicroseconds = view (from microseconds)
{-# INLINE fromMicroseconds #-}

instance AdditiveGroup NominalDiffTime where
    zeroV = NominalDiffTime 0
    NominalDiffTime a ^+^ NominalDiffTime b = NominalDiffTime (a + b)
    negateV (NominalDiffTime v) = NominalDiffTime (-v)
    NominalDiffTime a ^-^ NominalDiffTime b = NominalDiffTime (a - b)
    {-# INLINE zeroV #-}
    {-# INLINE (^+^) #-}
    {-# INLINE negateV #-}
    {-# INLINE (^-^) #-}

instance VectorSpace NominalDiffTime where
    type Scalar NominalDiffTime = Rational
    s *^ (NominalDiffTime m) = NominalDiffTime $ round (s * fromIntegral m)
    {-# INLINE (*^) #-}

-- | Serializes 'NominalDiffTime' as 64-bit signed microseconds in little endian
-- encoding.
--
instance Serialize NominalDiffTime where
    put (NominalDiffTime m) = putInt64le m
    get = NominalDiffTime <$> getInt64le
    {-# INLINE put #-}
    {-# INLINE get #-}

-- | Convert between 'NominalDiffTime' and a 'Decimal' representation of
-- seconds.
--
-- The result of the conversion from 'Decimal' to 'NominalDiffTime' is rounded
-- using banker's method, i.e. remainders of 0.5 a rounded to the next even
-- integer.
--
seconds :: Iso' NominalDiffTime Decimal
seconds = iso toSeconds fromSeconds

-- | Convert from 'NominalDiffTime' to a 'Decimal' representation of seconds.
--
toSeconds :: NominalDiffTime -> Decimal
toSeconds (NominalDiffTime m) = realToFrac m / 1000000
{-# INLINE toSeconds #-}

-- | Convert from 'Decimal' representation of seconds to 'NominalDiffTime'.
--
-- The result is rounded using banker's method, i.e. remainders of 0.5 a rounded
-- to the next even integer.
--
fromSeconds :: Decimal -> NominalDiffTime
fromSeconds d = NominalDiffTime $ round $ d * 1000000
{-# INLINE fromSeconds #-}

-- | The nominal length of a day: precisely 86400 SI seconds.
--
nominalDay :: NominalDiffTime
nominalDay = NominalDiffTime $ 86400 * 1000000
{-# INLINE nominalDay #-}

-- -------------------------------------------------------------------------- --
-- POSIX Timestamps

-- | POSIX Time represents time as 64 bit value of microseconds since
-- 'posixEpoch'.
--
-- Internally this is just UTCTime offset by 'posixEpochUtc'.
--
-- Most users will only use the functions 'toTimestampMicros' and
-- 'fromTimestampMicros'.
--
newtype POSIXTime = POSIXTime { _posixTime :: NominalDiffTime }
    deriving (Eq, Ord, Bounded)
    deriving newtype (NFData, Arbitrary)

instance AffineSpace POSIXTime where
    type Diff POSIXTime = NominalDiffTime
    POSIXTime a .-. POSIXTime b = a ^-^ b
    POSIXTime a .+^ b = POSIXTime (a ^+^ b)
    {-# INLINE (.-.) #-}
    {-# INLINE (.+^) #-}

-- | Represent POSIXTime as 64-bit value of microseconds since 'posixEpoch'.
--
toTimestampMicros :: POSIXTime -> Int64
toTimestampMicros = _microseconds . _posixTime
{-# INLINE toTimestampMicros #-}

-- | Create POSIXTime from 64-bit value of microseconds since 'posixEpoch'.
--
fromTimestampMicros :: Int64 -> POSIXTime
fromTimestampMicros = POSIXTime . fromMicroseconds
{-# INLINE fromTimestampMicros #-}

-- | POSIX Epoch
--
posixEpoch :: POSIXTime
posixEpoch = POSIXTime zeroV
{-# INLINE posixEpoch #-}

-- | The POSIX Epoch represented as UTCTime.
--
posixEpochUtc :: UTCTime
posixEpochUtc = UTCTime (fromIntegral d *^ nominalDay)
  where
    T.ModifiedJulianDay d = posixEpochDay
{-# INLINE posixEpochUtc #-}

-- | The day of the epoch of 'SystemTime', 1970-01-01
--
posixEpochDay :: T.Day
posixEpochDay = T.ModifiedJulianDay 40587
{-# INLINE posixEpochDay #-}

-- | Get current POSIX time
--
getPOSIXTime :: IO POSIXTime
getPOSIXTime = POSIXTime . NominalDiffTime . round . (* 1000000)
    <$> T.getPOSIXTime

-- The following conversions between POSIXTime and UTCTime are efficient because
-- all constants are inlined.

-- | Convert from UTCTime to POSIXTime
--
toPosix :: UTCTime -> POSIXTime
toPosix t = POSIXTime $ _utcTime t ^-^ _utcTime posixEpochUtc
{-# INLINE toPosix #-}

-- | Convert from POSIXTime to UTCTime
--
fromPosix :: POSIXTime -> UTCTime
fromPosix p = UTCTime $ _posixTime p ^+^ _utcTime posixEpochUtc
{-# INLINE fromPosix #-}

-- -------------------------------------------------------------------------- --
-- UTCTime

-- UTCTime with microseconds precision.
--
-- Represented as 64 bit count since Microseconds since MJD Epoch.
--
-- This implementation ignores Leap seconds.
--
newtype UTCTime = UTCTime { _utcTime :: NominalDiffTime }
    deriving (Eq, Ord, Bounded)
    deriving (Generic)
    deriving newtype (NFData, Arbitrary, Serialize)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    UTCTime a .-. UTCTime b = a ^-^ b
    UTCTime a .+^ b = UTCTime (a ^+^ b)
    {-# INLINE (.-.) #-}
    {-# INLINE (.+^) #-}

getCurrentTime :: IO UTCTime
getCurrentTime = UTCTime . (^+^ _utcTime posixEpochUtc) . _posixTime
    <$> getPOSIXTime
{-# INLINE getCurrentTime #-}

-- | The date of a UTCTime value represented as modified Julian 'T.Day'.
--
day :: Lens' UTCTime T.Day
day = julian . julianDay
{-# INLINE day #-}

-- | The day time of a 'UTCTime' value represented as 'NominalDiffTime' since
-- @00:00:00@ of that respective day.
--
dayTime :: Lens' UTCTime NominalDiffTime
dayTime = julian . julianDayTime
{-# INLINE dayTime #-}

-- | Create a 'UTCTime' from a date and a daytime. The date is represented
-- as modified Julian 'T.Day' and the day time is represented as
-- 'NominalDiffTime' since '00:00:00' of the respective day.
--
-- Note that this implementation does not support representation of leap
-- seconds.
--
fromDayAndDayTime :: T.Day -> NominalDiffTime -> UTCTime
fromDayAndDayTime d t = fromJulian $ Julian d t
{-# INLINE fromDayAndDayTime #-}

-- -------------------------------------------------------------------------- --
-- Modified Julian Day Representation of UTC

-- | Modified Julian Day Representation of UTC
--
data Julian = Julian
    { _julianDay :: !T.Day
    , _julianDayTime :: !NominalDiffTime
    }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Arbitrary Julian where
    arbitrary = Julian <$> arbitrary <*> arbitrary
    {-# INLINE arbitrary #-}

julianDay :: Lens' Julian T.Day
julianDay = lens _julianDay $ \a b -> a { _julianDay = b }
{-# INLINE julianDay #-}

julianDayTime :: Lens' Julian NominalDiffTime
julianDayTime = lens _julianDayTime $ \a b -> a { _julianDayTime = b }
{-# INLINE julianDayTime #-}

-- | The Epoch of the modified Julian day.
--
julianEpoch :: Julian
julianEpoch = Julian (T.ModifiedJulianDay 0) zeroV
{-# INLINE julianEpoch #-}

-- | The Epoch of the modified Julian day represented as 'UTCTime'.
--
julianEpochUtc :: UTCTime
julianEpochUtc = UTCTime zeroV
{-# INLINE julianEpochUtc #-}

-- | Convert from 'UTCTime' to modified 'Julian' Day time.
--
toJulian :: UTCTime -> Julian
toJulian (UTCTime (NominalDiffTime m)) = Julian
    (T.ModifiedJulianDay (fromIntegral d))
    (NominalDiffTime t)
  where
    (d, t) = divMod m n
    NominalDiffTime n = nominalDay
{-# INLINE toJulian #-}

-- | Convert from modified 'Julian' Day time to 'UTCTime'.
--
fromJulian :: Julian -> UTCTime
fromJulian (Julian (T.ModifiedJulianDay d) t)
    = UTCTime $ (fromIntegral d *^ nominalDay) ^+^ t
{-# INLINE fromJulian #-}

-- | Convert between 'UTCTime' and modified 'Julian' Day time.
--
julian :: Iso' UTCTime Julian
julian = iso toJulian fromJulian
{-# INLINE julian #-}

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

