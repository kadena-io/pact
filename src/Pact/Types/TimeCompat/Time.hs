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

-- * Ported Implementations
, parseTime
, formatTime

-- * Extensions
, _utctDay
, _utctDayTime
, utcTimeFromDaysAndDayTime
, toSeconds
, fromSeconds
, toMicroseconds
, fromMicroseconds
) where

-- -------------------------------------------------------------------------- --
-- Imports

import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Decimal
import Data.Fixed (Fixed(..), Pico)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
#if !MIN_VERSION_time(1,9,0)
import Data.Serialize
#endif

-- A symbol is exported from a module export only it it is in scope qualified
-- /and/ unqualified.
import Data.Time hiding (parseTime, formatTime)
import qualified Data.Time as Exported

import qualified Data.Time as Internal (formatTime)

import GHC.Generics
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

parseTime :: Monad m => TimeLocale -> String -> String -> m UTCTime
parseTime locale formatStr = parseTimeM False locale (mapFormat formatStr)
    where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('.':'%':'v':t) = "%Q" <> mapFormat t
    mapFormat ('%':'N':t) = "%z" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t

formatTime :: TimeLocale -> String -> UTCTime -> String

#if MIN_VERSION_time(1,9,0)
formatTime local formatStr = Internal.formatTime locale (mapFormat formatStr)
  where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('%':'v':t) = "%6q" <> mapFormat t
    mapFormat ('%':'N':t) = "%Ez" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t

#else /* time ≥ 1.5 < 1.9 */

formatTime locale formatStr timeValue = concat . snd $ go0 formatStr
  where
    format f = Internal.formatTime locale f timeValue
    n = let (h,m) = splitAt 3 $ format "%z" in h <> ":" <> m
    v = format "%6q"

    go0 s = let (a, b) = go1 s in ("", format a : b)

    go1 ('%':'%':t) = let (a, b) = go1 t in ("", "%" : format a : b)
    go1 ('%':'v':t) = let (a, b) = go1 t in ("", v : format a : b)
    go1 ('%':'N':t) = let (a, b) = go1 t in ("", n : format a : b)
    go1 (h:t) = let (a, b) = go1 t in (h:a, b)
    go1 "" = ("", [])

#endif /* MIN_VERSION_time(1,9,0) */

-- -------------------------------------------------------------------------- --
-- Extensions

toSeconds :: NominalDiffTime -> Decimal
toSeconds = realToFrac
{-# INLINE toSeconds #-}

fromSeconds :: Decimal -> NominalDiffTime
fromSeconds = realToFrac
{-# INLINE fromSeconds #-}

toMicroseconds :: DiffTime -> Int64
toMicroseconds dt = round (realToFrac dt * 1000000 :: Pico)
{-# INLINE toMicroseconds #-}

fromMicroseconds :: Int64 -> DiffTime
fromMicroseconds ms = realToFrac ((fromIntegral ms :: Pico) / 1000000)
{-# INLINE fromMicroseconds #-}

_utctDay :: Lens' UTCTime Day
_utctDay = lens utctDay (\t d -> t { utctDay = d })

_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = lens utctDayTime (\t d -> t { utctDayTime = d })

utcTimeFromDaysAndDayTime :: Day -> DiffTime -> UTCTime
utcTimeFromDaysAndDayTime = UTCTime
{-# INLINE utcTimeFromDaysAndDayTime #-}

