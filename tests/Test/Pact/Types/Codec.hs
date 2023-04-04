{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Test.Pact.Types.Codec
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: BSD-3
-- Stability: experimental
--
module Test.Pact.Types.Codec
( tests
) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Pact.Time

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success)

-- internal modules

import Pact.Types.Codec
import Pact.Types.Orphans ()

tests :: Spec
tests = describe "Codec"
  spec_timeCodec

spec_timeCodec :: Spec
spec_timeCodec = describe "timeCodec" $ do
  prop "roundtrips" $ withMaxSuccess 10000 $ \t ->
    parse (decoder timeCodec) (encoder timeCodec t) === Success t

  prop "% 1000000 roundtrips" $ \t -> do
    let p = toPosixTimestampMicros t
    let t' = fromPosixTimestampMicros $ 1000000 * (p `div` 1000000)
    parse (decoder timeCodec) (encoder timeCodec t') === Success t'

  prop "% 1000 roundtrips" $ \t -> do
    let p = toPosixTimestampMicros t
    let t' = fromPosixTimestampMicros $ 1000 * (p `div` 1000)
    parse (decoder timeCodec) (encoder timeCodec t') === Success t'

  prop "uses correct format" $ \t ->
    let
      isHighRes = toPosixTimestampMicros t `rem` 1000000 /= 0
    in
      case encoder timeCodec t of
        Object o
            | isHighRes -> case HM.lookup "timep" o of
                Just (String s) -> T.elem '.' s
                _ -> error $ "timeCodec failed: " <> show o
            | otherwise -> case HM.lookup "time" o of
                Just (String s) -> not (T.elem '.' s)
                _ -> error $ "timeCodec failed: " <> show o
        x -> error $ "timeCodec failed: " <> show x

