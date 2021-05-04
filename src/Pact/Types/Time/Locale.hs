{-# LANGUAGE Safe #-}
-- |
-- Module: Pact.Types.Time.Locale
-- Description : Short description
-- Copyright   : (c) Kadena LLC, 2021
--                   Ashley Yakeley and contributors, 2004-2021
--                   The University of Glasgow 2001
-- License     : MIT
-- Maintainer  : Lars Kuhtz <lars@kadena.io>
-- Stability   : experimental
--
-- The code in this module is derived from time:Data.Time.Format.Locale. The
-- code is included here in order to guarnatee binary stability of formated time
-- values in Pact, even when the upstream code changes.
--
-- The original code has the following Copyright and License:
--
-- @
-- TimeLib is Copyright (c) Ashley Yakeley and contributors, 2004-2021. All rights reserved.
-- Certain sections are Copyright 2004, The University Court of the University of Glasgow. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- - Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
--
-- - Neither name of the copyright holders nor the names of its contributors may
--   be used to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
-- @
--
module Pact.Types.Time.Locale
( TimeLocale(..)
, defaultTimeLocale
) where

data TimeLocale = TimeLocale
    { wDays :: ![(String, String)]
        -- ^ full and abbreviated week days, starting with Sunday
    , months :: ![(String, String)]
        -- ^ full and abbreviated months
    , amPm :: !(String, String)
        -- ^ AM\/PM symbols
    , dateTimeFmt :: !String
    , dateFmt :: !String
    , timeFmt :: !String
    , time12Fmt :: !String
    }
    deriving (Eq, Ord, Show)

-- | Locale representing American usage.
--
defaultTimeLocale :: TimeLocale
defaultTimeLocale = TimeLocale
    { wDays =
          [ ("Sunday", "Sun")
          , ("Monday", "Mon")
          , ("Tuesday", "Tue")
          , ("Wednesday", "Wed")
          , ("Thursday", "Thu")
          , ("Friday", "Fri")
          , ("Saturday", "Sat")
          ]
    , months =
          [ ("January", "Jan")
          , ("February", "Feb")
          , ("March", "Mar")
          , ("April", "Apr")
          , ("May", "May")
          , ("June", "Jun")
          , ("July", "Jul")
          , ("August", "Aug")
          , ("September", "Sep")
          , ("October", "Oct")
          , ("November", "Nov")
          , ("December", "Dec")
          ]
    , amPm = ("AM", "PM")
    , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
    , dateFmt = "%m/%d/%y"
    , timeFmt = "%H:%M:%S"
    , time12Fmt = "%I:%M:%S %p"
    }

