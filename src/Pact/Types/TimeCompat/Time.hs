{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Pact.Types.Time
-- Copyright: Copyright © 2018 Kadena LLC
-- License:  BSD-style (see the file LICENSE)
-- Maintainer:  Stuart Popejoy <stuart@kadena.io>
--
module Pact.Types.TimeCompat.Time
( module Exported
, getPOSIXTime
, posixSecondsToUTCTime
, utcTimeToPOSIXSeconds

-- * Ported Implementations
, parseTime
, formatTime

-- * Extensions
, UTCTime(..)
, Day(..)
, _utctDay
, _utctDayTime
, utcTimeFromDaysAndDayTime
, toSeconds
, fromSeconds
, toMicroseconds
, fromMicroseconds
, microseconds

-- ** Abstractions over the UTCTime constructor
, unUTCTime
, mkUTCTime
) where

-- -------------------------------------------------------------------------- --
-- Imports

import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Decimal
import Data.Fixed (Fixed(..), Pico)
import Data.Serialize

-- A symbol is exported from a module export only it it is in scope qualified
-- /and/ unqualified.
import Data.Time hiding (parseTime, formatTime)
import qualified Data.Time as Exported

import qualified Data.Time as Internal (formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import GHC.Generics hiding (from)
import GHC.Int (Int64)

-- -------------------------------------------------------------------------- --
-- Orphans

deriving instance Generic Pico
deriving instance Generic Day
deriving instance Generic UTCTime

instance AdditiveGroup NominalDiffTime where
    zeroV = 0
    (^+^) = (+)
    negateV v = -v
    (^-^) = (-)
    {-# INLINE zeroV #-}
    {-# INLINE (^+^) #-}
    {-# INLINE negateV #-}
    {-# INLINE (^-^) #-}

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    (.-.) = diffUTCTime
    (.+^) = flip addUTCTime
    {-# INLINE (.-.) #-}
    {-# INLINE (.+^) #-}

instance Serialize DiffTime where
    put = put . diffTimeToPicoseconds
    get = picosecondsToDiffTime <$> get
    {-# INLINE put #-}
    {-# INLINE get #-}

instance Serialize NominalDiffTime where
    put = put . (realToFrac :: NominalDiffTime -> Pico)
    get = (realToFrac :: Pico -> NominalDiffTime) <$> get
    {-# INLINE put #-}
    {-# INLINE get #-}

instance Serialize Pico
instance Serialize Day
instance Serialize UTCTime

-- -------------------------------------------------------------------------- --
-- Ported Implementations

parseTime :: MonadFail m => TimeLocale -> String -> String -> m UTCTime
parseTime locale formatStr = parseTimeM False locale (mapFormat formatStr)
    where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('.':'%':'v':t) = "%Q" <> mapFormat t
    mapFormat ('%':'N':t) = "%z" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t
{-# INLINE parseTime #-}

formatTime :: TimeLocale -> String -> UTCTime -> String

formatTime locale formatStr = Internal.formatTime locale (mapFormat formatStr)
  where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('%':'v':t) = "%6q" <> mapFormat t
    mapFormat ('%':'N':t) = "%Ez" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t
{-# INLINE formatTime #-}

-- -------------------------------------------------------------------------- --
-- Extensions (from thyme)

class TimeDiff t where
    microseconds :: Iso' t Int64

toMicroseconds :: TimeDiff t => t -> Int64
toMicroseconds = view microseconds
{-# INLINE toMicroseconds #-}

fromMicroseconds :: TimeDiff t => Int64 -> t
fromMicroseconds = view (from microseconds)
{-# INLINE fromMicroseconds #-}

instance TimeDiff DiffTime where
    microseconds = iso
        (\dt -> round (realToFrac dt * 1000000 :: Pico))
        (\ms -> realToFrac ((fromIntegral ms :: Pico) / 1000000))
    {-# INLINE microseconds #-}

instance TimeDiff NominalDiffTime where
    microseconds = iso
        (\dt -> round (realToFrac dt * 1000000 :: Pico))
        (\ms -> realToFrac ((fromIntegral ms :: Pico) / 1000000))
    {-# INLINE microseconds #-}

toSeconds :: NominalDiffTime -> Decimal
toSeconds = realToFrac
{-# INLINE toSeconds #-}

fromSeconds :: Decimal -> NominalDiffTime
fromSeconds = realToFrac
{-# INLINE fromSeconds #-}

_utctDay :: Lens' UTCTime Day
_utctDay = lens utctDay (\t d -> t { utctDay = d })
{-# INLINE _utctDay #-}

_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = lens utctDayTime (\t d -> t { utctDayTime = d })
{-# INLINE _utctDayTime #-}

utcTimeFromDaysAndDayTime :: Day -> DiffTime -> UTCTime
utcTimeFromDaysAndDayTime = UTCTime
{-# INLINE utcTimeFromDaysAndDayTime #-}

unUTCTime :: UTCTime -> UTCTime
unUTCTime = id
{-# INLINE unUTCTime #-}

mkUTCTime :: Day -> DiffTime -> UTCTime
mkUTCTime = UTCTime
{-# INLINE mkUTCTime #-}
