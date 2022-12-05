{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Aeson hiding (Result)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.IO as TL

import GHC.Stack

import System.Clock

import Test.QuickCheck hiding (Result)

-- internal modules

import Pact.Types.Persistence
import Pact.Types.Term hiding (Object)
import Pact.Types.Term.Arbitrary ()

-- -------------------------------------------------------------------------- --
-- Tools

resultsToCsv :: [(T.Text, Result)] -> TL.Text
resultsToCsv x = TB.toLazyText $ "source,time,size,maxDepth,minDepth,valueSize\n" <> go x
  where
    go [] = mempty
    go ((l,(a0,a1,a2,a3,a4)):t)
        = TB.fromText l
        <> TB.singleton ','
        <> TB.decimal (toNanoSecs a0)
        <> TB.singleton ','
        <> TB.decimal a1
        <> TB.singleton ','
        <> TB.decimal a2
        <> TB.singleton ','
        <> TB.decimal a3
        <> TB.singleton ','
        <> TB.decimal a4
        <> TB.singleton '\n'
        <> go t

-- -------------------------------------------------------------------------- --
-- Json Tools

valueMetric :: (forall f . Foldable f => f Int -> Int) -> Value -> Int
valueMetric f (Object a)
    | mempty == a = 1
    | otherwise = 1 + f (valueMetric f <$> a)
valueMetric f (Array a)
    | mempty == a = 1
    | otherwise = 1 + f (valueMetric f <$> a)
valueMetric _ _ = 1

valueMaxDepth :: Value -> Int
valueMaxDepth = valueMetric maximum

valueMinDepth :: Value -> Int
valueMinDepth = valueMetric minimum

valueSize :: Value -> Int
valueSize = valueMetric sum

-- -------------------------------------------------------------------------- --
-- benchmarks

type Result = (TimeSpec, Int, Int, Int, Int)

benchDecode
    :: forall a
    . HasCallStack
    => FromJSON a
    => NFData a
    => B.ByteString
    -> IO Result
benchDecode bytes = do
    s <- getTime Monotonic
    !r <- evaluate $! eitherDecodeStrict' @a bytes
    e <- getTime Monotonic
    case r of
        Left err -> error $ "failed: " <> err
        Right !_ -> do
            let Right v = eitherDecodeStrict' @Value bytes
            return
                ( diffTimeSpec e s
                , B.length bytes
                , valueMaxDepth v
                , valueMinDepth v
                , valueSize v
                )

benchDecodeArbitrary
    :: forall a
    . HasCallStack
    => Arbitrary a
    => ToJSON a
    => NFData a
    => FromJSON a
    => Int
    -> IO Result
benchDecodeArbitrary = benchDecodeArbitrary_ @a @a

benchDecodeArbitrary_
    :: forall a b
    . HasCallStack
    => Arbitrary a
    => ToJSON a
    => FromJSON b
    => NFData b
    => Int
    -> IO Result
benchDecodeArbitrary_ i = do
    !t <- generate @a $ scale (\_ -> i) arbitrary
    benchDecode @b (BL.toStrict $ encode $! t)

benchDecodeFile
    :: forall a
    . HasCallStack
    => FromJSON a
    => NFData a
    => FilePath
    -> IO Result
benchDecodeFile file = do
    !bytes <- B.readFile file
    benchDecode @a $! bytes

tag :: T.Text -> Result -> (T.Text, Result)
tag l r = (l, r)

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
    l1 <- flip foldMap [0,100..100000 :: Int] $ \i -> do
        when (mod i (1000::Int) == 0) $ print i
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

