{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
-- , main1
) where

import Control.DeepSeq
import Control.Exception
import Control.Monad

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.IO as TL

import GHC.Stack

import System.Clock

import Test.QuickCheck

-- internal modules

import Pact.Types.Persistence
import Pact.Types.Term
import Pact.Types.Term.Arbitrary ()

resultsToCsv :: [(T.Text, TimeSpec, Int)] -> TL.Text
resultsToCsv x = TB.toLazyText $ "size,time,source\n" <> go x
  where
    go [] = mempty
    go ((l, a, b):t)
        = TB.decimal b
        <> TB.singleton ','
        <> TB.decimal (toNanoSecs a)
        <> TB.singleton ','
        <> TB.fromText l
        <> TB.singleton '\n'
        <> go t

-- -------------------------------------------------------------------------- --
-- benchmarks

stopWatch :: NFData a => IO a -> IO (TimeSpec, a)
stopWatch a = do
    s <- getTime Monotonic
    !x <- a >>= \x -> return $!! x
    e <- getTime Monotonic
    return (diffTimeSpec e s, x)

benchDecode
    :: forall a
    . HasCallStack
    => FromJSON a
    => NFData a
    => B.ByteString
    -> IO (TimeSpec, Int)
benchDecode bytes = do
    (t, x) <- stopWatch $! evaluate $! eitherDecodeStrict' @a bytes
    case x of
        Left err -> error $ "failed: " <> err
        Right !_ -> return (t, B.length bytes)

benchDecodeArbitrary
    :: forall a
    . HasCallStack
    => Arbitrary a
    => ToJSON a
    => NFData a
    => FromJSON a
    => Int
    -> IO (TimeSpec, Int)
benchDecodeArbitrary = benchDecodeArbitrary_ @a @a

benchDecodeArbitrary_
    :: forall a b
    . HasCallStack
    => Arbitrary a
    => ToJSON a
    => FromJSON b
    => NFData b
    => Int
    -> IO (TimeSpec, Int)
benchDecodeArbitrary_ i = do
    !t <- generate @a $ scale (\_ -> i) arbitrary
    benchDecode @b (BL.toStrict $ encode $! t)

benchDecodeFile
    :: forall a
    . HasCallStack
    => FromJSON a
    => NFData a
    => FilePath
    -> IO (TimeSpec, Int)
benchDecodeFile file = do
    !bytes <- B.readFile file
    benchDecode @a $! bytes

tag :: T.Text -> (TimeSpec, a) -> (T.Text, TimeSpec, a)
tag l (a,b) = (l,a,b)

-- -------------------------------------------------------------------------- --
-- main 2

main :: HasCallStack => IO ()
main = do

    -- benchmark files
    print "======== files:"
    r1 <- benchDecodeFile @(Module (Def (Ref' PersistDirect))) "./bench/wiza.module.json"
    r2 <- benchDecodeFile @(ModuleData ((Ref' PersistDirect))) "./bench/wiza.row.json"
    r3 <- benchDecodeFile @Value "./bench/wiza.row.json"

    -- benchmark arbitrary
    print "========= arbitrary:"
    l1 <- flip foldMap [0..1000 :: Int] $ \i -> do
        when (mod i (50::Int) == 0) $ print i
        !val <- generate $ scale (const i) $ arbitrary @(Term (Ref' PersistDirect))
        !bytes <- evaluate $! BL.toStrict $ encode $! val

        x1 <- tag "arbitrary_term" <$> benchDecode @(Term (Ref' PersistDirect)) bytes
        x2 <- tag "arbitrary_term_value" <$> benchDecode @Value bytes
        return [x1, x2]

    -- print results
    TL.writeFile "out.csv" $ resultsToCsv $
        [ tag "wiza.module" r1
        , tag "wiza.row" r2
        , tag "wiza.row_value" r3
        ]
        <> l1

