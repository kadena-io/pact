{-# LANGUAGE CPP #-}

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
, toPosixTimestampMicros
, fromPosixTimestampMicros

-- ** Formatting and Parsing
, parseTime
, formatTime

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

import Pact.Types.Time.Internal

#ifdef FORMAT_WITH_TIME
import Pact.Types.Time.FormatWithTime
#else
import Pact.Types.Time.Format
#endif
