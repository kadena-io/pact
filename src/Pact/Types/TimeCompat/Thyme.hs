{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Pact.Types.Time
-- Copyright: Copyright © 2018 Kadena LLC
-- License:  BSD-style (see the file LICENSE)
-- Maintainer:  Stuart Popejoy <stuart@kadena.io>
--
-- The latest released version of the thyme library is 0.3.5.5 which was
-- released 2014-11-27. Since then the master branch as diverged considerably
-- and includes some breaking changes that are difficult to work around.
-- Development and support of thyme has seems to have stalled since May 2016.
--
-- We support three different version of the thyme library:
--
-- [0.3.5.5]
--     Latest official release on Hackage. Probably not supported with
--     GHCJS.
--
-- [0.3.6]
--     Unofficial fork of the thyme library at
--     https://github.com/kadena-io/thyme. The fork is based of the
--     source code of version 0.3.5.5 and adds support for compilation
--     with GHCJS and add the format modifiers '%N' and '%v% from the
--     master branch.
--     Not available on Hackage.
--
-- [master as of 2018-04-19]
--     Not supported with GHCJS. Not available on Hackage. Requires that
--     the CPP macro @THYME_MASTER@ is defined.
--
-- Additional Notes:
--
-- *   Commit @aaa49dbcb98c0a1122b947c562ebe753ddebcca5@ in the thyme master branch
--     changes the name of the @UTCView@ constructor from @UTCTime@ to @UTCView@.
--     For this reason this module hides @UTCView@. Instead
--     the accessors @utctDay@ and @utctDayTime should be used.
--
-- *   Commit @722eb63a773788812ccc5c8b5ace3109b2dcd8d7@ in the thyme master branch
--     changes the type of mkUTCTime to
--
--     @
--     mkUTCTime :: Year -> Month -> DayOfMonth -> Hour -> Minute -> Double -> UTCTime
--     @
--
--     For this reason this module hides @mkUTCTime@. Instead the extensions
--     function @utcTimeFromDaysAndDayTime@ should be used.
--
-- *   The type of TimeLocale changed in master. Versions based
--     on 0.3.5.5 require TimeLocale from System.Locale which conflicts with the
--     type defined in thyme master. (Data.Time (>= 1.5) defines it's own TimeLocale
--     type.)
--
--     There is currently no way to distinguish between which type of TimeLocale is
--     used when the version is 0.3.5.5 (which is also used by thyme-master).
--     Therefor we require that @THYME_MASTER@ is defined when the master
--     branch is used.
--
module Pact.Types.TimeCompat.Thyme
( module Exported

#ifndef THYME_MASTER
, TimeLocale
, defaultTimeLocale
#endif

-- * Ported Implementations
, parseTime
, formatTime

-- * Extensions
, utcTimeFromDaysAndDayTime
) where

-- -------------------------------------------------------------------------- --
-- Imports

#if !MIN_VERSION_base(4,11,0) && !MIN_VERSION_thyme(0,3,6) && !defined(THYME_MASTER)
import Data.Semigroup ((<>))
#endif
import Data.Serialize
import qualified Data.Time as Time (UTCTime(..))
import Data.Thyme hiding (parseTime, formatTime)
import qualified Data.Thyme as Internal (parseTime, formatTime)
import Data.Thyme.Format.Aeson ()
import Data.Thyme.Internal.Micro

-- A symbol is exported from a module export only it it is in scope qualified
-- /and/ unqualified.
--
import Data.Thyme.Time hiding (parseTime, formatTime)
import qualified Data.Thyme.Time as Exported hiding (UTCView(..), mkUTCTime, unUTCTime)

#ifndef THYME_MASTER
-- required for 0.3.5.5 and thyme-kadena
import System.Locale
#endif

-- -------------------------------------------------------------------------- --
-- Orphans

instance Serialize Micro
instance Serialize NominalDiffTime
instance Serialize UTCTime

-- -------------------------------------------------------------------------- --
-- Ported Implementations

parseTime :: ParseTime t => TimeLocale -> String -> String -> Maybe t
formatTime :: FormatTime t => TimeLocale -> String -> t -> String

#if MIN_VERSION_thyme(0,3,6) || defined(THYME_MASTER)

parseTime = Internal.parseTime
formatTime = Internal.formatTime
{-# INLINE parseTime #-}
{-# INLINE formatTime #-}

#else /* thyme 0.3.5 */

parseTime locale formatStr = Internal.parseTime locale (mapFormat formatStr)
  where
    mapFormat ('%':'%':t) = "%%" <> mapFormat t
    mapFormat ('.':'%':'v':t) = "%Q" <> mapFormat t
    mapFormat ('%':'N':t) = "%z" <> mapFormat t
    mapFormat [] = []
    mapFormat (h:t) = h : mapFormat t

formatTime locale formatStr timeValue = concat . snd $ go0 formatStr
  where
    format f = Internal.formatTime locale f timeValue
    n = let (h,m) = splitAt 3 $ format "%z" in h <> ":" <> m
    v = take 6 $ format "%q"

    go0 s = let (a, b) = go1 s in ("", format a : b)

    go1 ('%':'%':t) = let (a, b) = go1 t in ("", "%" : format a : b)
    go1 ('%':'v':t) = let (a, b) = go1 t in ("", v : format a : b)
    go1 ('%':'N':t) = let (a, b) = go1 t in ("", n : format a : b)
    go1 (h:t) = let (a, b) = go1 t in (h:a, b)
    go1 "" = ("", [])

#endif /* MIN_VERSION_thyme(0,3,6) */

-- -------------------------------------------------------------------------- --
-- Extensions

utcTimeFromDaysAndDayTime :: Day -> DiffTime -> UTCTime
utcTimeFromDaysAndDayTime d s = toThyme $ Time.UTCTime (fromThyme d) (fromThyme s)
{-# INLINE utcTimeFromDaysAndDayTime #-}

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

