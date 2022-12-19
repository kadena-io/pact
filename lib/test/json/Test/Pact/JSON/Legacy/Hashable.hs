{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Test.Pact.JSON.Legacy.Hashable
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Pact.JSON.Legacy.Hashable
( spec
) where

import Control.Monad

import Data.Hashable
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- internal modules
import Pact.JSON.Legacy.Hashable

-- -------------------------------------------------------------------------- --
-- Arbitray Instance

instance Arbitrary t => Arbitrary (LegacyHashed t) where
  arbitrary = LegacyHashed <$> arbitrary

-- -------------------------------------------------------------------------- --
-- LegacyHashInstances

spec :: Spec
spec = describe "LegacyHashable" $ do
#if !MIN_VERSION_hashable(1,3,1)
  describe "compatible with hashable-1.3.0" $ do
    describe "Text" $ compatTests @T.Text
    describe "Int" $ compatTests @Int
    describe "Word" $ compatTests @Word
    describe "()" $ compatTests @()
    describe "Bool" $ compatTests @Bool
    describe "Ordering" $ compatTests @Ordering
    describe "Char" $ compatTests @Char
#else
  describe "compatible with VERSION_hashable" $ do
    describe "Int" $ compatTests @Int
    describe "Word" $ compatTests @Word
    describe "()" $ compatTests @()
    describe "Bool" $ compatTests @Bool
    describe "Ordering" $ compatTests @Ordering
    describe "Char" $ compatTests @Char
#endif
  describe "text vectors" $ do
    it "legacyHash" $ forM_ textVectors $ \(i,o) ->
      legacyHash i `shouldBe` o
    it "hash(LegacyHashed)" $ forM_ textVectors $ \(i,o) ->
      hash (LegacyHashed i) `shouldBe` o

compatTests
  :: forall a
  . LegacyHashable a
  => Hashable a
  => Arbitrary a
  => Show a
  => Spec
compatTests = do
  it "legacyHash" $ property $ prop_legacyHashCompat @a
  it "hash(LegacyHashed)" $ property $ prop_legacyHashInstance @a

prop_legacyHashCompat :: Hashable a => LegacyHashable a => a -> Property
prop_legacyHashCompat a = legacyHash a === hash a

prop_legacyHashInstance :: Hashable a => LegacyHashable a => LegacyHashed a -> Property
prop_legacyHashInstance t = hash (_getLegacyHashed t) === hash t

textVectors :: [(T.Text, Int)]
textVectors = zip arg expected
 where
  arg = "" : "a" : [ T.pack (show @[Int] [1..i]) | i <- [0,100..1000] ]
  expected =
    [ -2578643520546668380,-7234408896080994505,-6997694561424077354,4219119192155343941
    , 4490691819985819348,-2633759961541750241,-7390682183909917138,-4349854433322841223
    , -6002586313296005752,2857719091733616083,3758582501661966194,6453168825425917325
    , 8933568572060804037
    ]

