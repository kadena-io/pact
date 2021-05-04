{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Pact.Types.Time.Internal
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Pact.Types.Time.Internal
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
, toPosixTimestampMicros
, fromPosixTimestampMicros

-- * Julian Dates
, Day(..)
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
import Data.AffineSpace
import Data.Decimal
import Data.Serialize
import Data.VectorSpace

import GHC.Generics hiding (from)
import GHC.Int (Int64)

#ifdef FORMAT_WITH_TIME
import qualified Data.Time.Clock.POSIX (getPOSIXTime)
#else
import System.Clock (getTime, TimeSpec(..), Clock(Realtime))
#endif


import Test.QuickCheck.Arbitrary

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

toPosixTimestampMicros :: UTCTime -> Int64
toPosixTimestampMicros = toTimestampMicros . toPosix
{-# INLINE toPosixTimestampMicros #-}

fromPosixTimestampMicros :: Int64 -> UTCTime
fromPosixTimestampMicros = fromPosix . fromTimestampMicros
{-# INLINE fromPosixTimestampMicros #-}

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
    ModifiedJulianDay d = posixEpochDay
{-# INLINE posixEpochUtc #-}

-- | The day of the epoch of 'SystemTime', 1970-01-01
--
posixEpochDay :: Day
posixEpochDay = ModifiedJulianDay 40587
{-# INLINE posixEpochDay #-}

-- | Get current POSIX time
--
getPOSIXTime :: IO POSIXTime
getPOSIXTime = POSIXTime . NominalDiffTime <$> getSystemTimeMicros
{-# INLINE getPOSIXTime #-}

getSystemTimeMicros :: IO Int64
getSystemTimeMicros = do
#ifdef FORMAT_WITH_TIME
    s <- Data.Time.Clock.POSIX.getPOSIXTime
    return $ round $ s * 1000000
#else
    TimeSpec s ns <- getTime Realtime
    return $ (s * 1000000) + (ns `quot` 1000)
#endif
{-# INLINE getSystemTimeMicros #-}

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

-- | The date of a UTCTime value represented as modified Julian 'Day'.
--
day :: Lens' UTCTime Day
day = julian . julianDay
{-# INLINE day #-}

-- | The day time of a 'UTCTime' value represented as 'NominalDiffTime' since
-- @00:00:00@ of that respective day.
--
dayTime :: Lens' UTCTime NominalDiffTime
dayTime = julian . julianDayTime
{-# INLINE dayTime #-}

-- | Create a 'UTCTime' from a date and a daytime. The date is represented
-- as modified Julian 'Day' and the day time is represented as
-- 'NominalDiffTime' since '00:00:00' of the respective day.
--
-- Note that this implementation does not support representation of leap
-- seconds.
--
fromDayAndDayTime :: Day -> NominalDiffTime -> UTCTime
fromDayAndDayTime d t = fromJulian $ Julian d t
{-# INLINE fromDayAndDayTime #-}

-- -------------------------------------------------------------------------- --
-- Modified Julian Day Representation of UTC

newtype Day = ModifiedJulianDay Int
    deriving newtype (Eq, Ord, NFData, Arbitrary)

-- | Modified Julian Day Representation of UTC
--
data Julian = Julian
    { _julianDay :: !Day
    , _julianDayTime :: !NominalDiffTime
    }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Arbitrary Julian where
    arbitrary = Julian <$> arbitrary <*> arbitrary
    {-# INLINE arbitrary #-}

julianDay :: Lens' Julian Day
julianDay = lens _julianDay $ \a b -> a { _julianDay = b }
{-# INLINE julianDay #-}

julianDayTime :: Lens' Julian NominalDiffTime
julianDayTime = lens _julianDayTime $ \a b -> a { _julianDayTime = b }
{-# INLINE julianDayTime #-}

-- | The Epoch of the modified Julian day.
--
julianEpoch :: Julian
julianEpoch = Julian (ModifiedJulianDay 0) zeroV
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
    (ModifiedJulianDay (fromIntegral d))
    (NominalDiffTime t)
  where
    (d, t) = divMod m n
    NominalDiffTime n = nominalDay
{-# INLINE toJulian #-}

-- | Convert from modified 'Julian' Day time to 'UTCTime'.
--
fromJulian :: Julian -> UTCTime
fromJulian (Julian (ModifiedJulianDay d) t)
    = UTCTime $ (fromIntegral d *^ nominalDay) ^+^ t
{-# INLINE fromJulian #-}

-- | Convert between 'UTCTime' and modified 'Julian' Day time.
--
julian :: Iso' UTCTime Julian
julian = iso toJulian fromJulian
{-# INLINE julian #-}

