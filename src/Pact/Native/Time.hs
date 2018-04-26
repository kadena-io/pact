{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Native.Time
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Date/time built-ins.
--

module Pact.Native.Time
    (timeDefs)
    where

import Control.Monad
import Prelude
import Data.Decimal
import Data.AffineSpace
import Data.Semigroup

import Pact.Types.Runtime
import Pact.Native.Internal
import Pact.Types.Time


timedoc :: Text
timedoc = "See [\"Time Formats\" docs](#time-formats) for supported formats."

timeDefs :: NativeModule
timeDefs = ("Time", timeDef)
  where
    timeDef =
        [ defRNative "time" time
            (funType tTyTime [("utcval", tTyString)])
            $ "Construct time from UTCVAL using ISO8601 format ("
            <> pack simpleISO8601 <> "). "
            <> "`(time \"2016-07-22T11:26:35Z\")`"
        , defRNative "parse-time" parseTime'
            (funType tTyTime [("format", tTyString),("utcval", tTyString)])
            $ "Construct time from UTCVAL using FORMAT. " <> timedoc
            <> "`(parse-time \"%F\" \"2016-09-12\")`"
        , defRNative "add-time" addTime
            ( funType tTyTime [("time", tTyTime),("seconds", tTyDecimal)]
            <> funType tTyTime [("time", tTyTime),("seconds", tTyInteger)]
            )
            $ "Add SECONDS to TIME; SECONDS can be integer or decimal. "
            <> "`(add-time (time \"2016-07-22T12:00:00Z\") 15)`"
        , defRNative "diff-time" diffTime
            (funType tTyDecimal [("time1", tTyTime),("time2", tTyTime)])
            $ "Compute difference between TIME1 and TIME2 in seconds. "
            <> "`(diff-time (parse-time \"%T\" \"16:00:00\") "
            <> "(parse-time \"%T\" \"09:30:00\"))`"
        , defRNative "minutes" (timeMult 60)
            multType
            $ "N minutes, for use with 'add-time'. "
            <> "`(add-time (time \"2016-07-22T12:00:00Z\") (minutes 1))`"
        , defRNative "hours" (timeMult $ 60 * 60)
            multType
            $ "N hours, for use with 'add-time' "
            <> "`(add-time (time \"2016-07-22T12:00:00Z\") (hours 1))`"
        , defRNative "days" (timeMult $ 60 * 60 * 24)
            multType
            $ "N days, for use with 'add-time' "
            <> "`(add-time (time \"2016-07-22T12:00:00Z\") (days 1))`"
        , defRNative "format-time" formatTime'
            (funType tTyString [("format", tTyString),("time", tTyTime)])
            $ "Format TIME using FORMAT. " <> timedoc
            <> "`(format-time \"%F\" (time \"2016-07-22T12:00:00Z\"))`"
        ]
    multType = funType tTyDecimal [("n", tTyDecimal)]
        <> funType tTyDecimal [("n", tTyInteger)]


time :: RNativeFun e
time i [TLitString s] =
  case parseTime defaultTimeLocale simpleISO8601 (unpack s) of
    Nothing -> evalError' i $ "Invalid time, expecting '" ++ simpleISO8601 ++ "': " ++ unpack s
    Just t -> return (tLit (LTime t))
time i as = argsError i as

parseTime' :: RNativeFun e
parseTime' i [TLitString fmt,TLitString s] =
  case parseTime defaultTimeLocale (unpack fmt) (unpack s) of
    Nothing -> evalError' i $ "Failed to parse time '" ++ unpack s ++ "' with format: " ++ unpack fmt
    Just t -> return (tLit (LTime t))
parseTime' i as = argsError i as

formatTime' :: RNativeFun e
formatTime' _ [TLitString fmt,TLiteral (LTime t) _] =
  return $ toTerm $ pack $ formatTime defaultTimeLocale (unpack fmt) t
formatTime' i as = argsError i as


addTime :: RNativeFun e
addTime _ [TLiteral (LTime t) _,TLiteral (LDecimal r) _] = return $ doTimeAdd t r
addTime _ [TLiteral (LTime t) _,TLitInteger n] = return $ doTimeAdd t (fromIntegral n)
addTime i as = argsError i as

doTimeAdd :: UTCTime -> Decimal -> Term Name
doTimeAdd t r = toTerm (t .+^ fromSeconds r)

diffTime :: RNativeFun e
diffTime _ [TLiteral (LTime t1) _,TLiteral (LTime t2) _] = return $ toTerm (toSeconds (t1 .-. t2) :: Decimal)
diffTime i as = argsError i as

timeMult :: Decimal -> RNativeFun e
timeMult m _ [TLitInteger n] = return $ toTerm (fromIntegral n * m)
timeMult m _ [TLiteral (LDecimal d) _] = return $ toTerm (d * m)
timeMult _ i as = argsError i as
