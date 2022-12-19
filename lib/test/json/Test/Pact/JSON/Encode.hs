{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Pact.JSON.Encode
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Pact.JSON.Encode
( tests
) where

import qualified Data.Aeson as A
import Data.Int
import Data.Scientific
import qualified Data.Text as T
import Data.Void
import Data.Word

import Numeric.Natural

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- internal modules

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- Tests

tests :: Spec
tests = describe "JSON Encode" $ do
  objectTests
  aesonCompat

-- -------------------------------------------------------------------------- --
--

objectTests :: Spec
objectTests = describe "object" $ do
    it "mempty" $
      J.encode (J.object @[] mempty) === "{}"
    it "Int" $
      J.encode (J.object ["a" J..= J.Aeson (1::Int)]) === "{\"a\":1}"
    it ".= Just 1" $
      J.encode (J.object ["a" J..= Just (J.Aeson (1::Int))]) === "{\"a\":1}"
    it ".= Nothing" $
      J.encode (J.object ["a" J..= Nothing @Void]) === "{\"a\":null}"
    it ".?= Just 1" $
      J.encode (J.object ["a" J..?= Just (J.Aeson (1::Int))]) == "{\"a\":1}"
    it ".?= Nothing" $
      J.encode (J.object ["a" J..?= Nothing @Void]) == "{}"

-- -------------------------------------------------------------------------- --
-- Aeson Compatibility

aesonCompat :: Spec
aesonCompat = describe "Aeson Compatibility" $ do

  -- Number
  aesonCompat_integral
  aesonCompat_float
  prop "Scientific" $ \a -> A.encode a === J.encode (J.Aeson @Scientific a)

  -- Text
  prop "Text" $ \(a :: T.Text) -> A.encode a === J.encode a
  prop "String" $ \(a :: String) -> A.encode a === J.encode (T.pack a)

  -- Bool
  prop "Bool" $ \(a :: Bool) -> A.encode a === J.encode a

  -- Array
  describe "Array" $ do
    prop "Array (Int, Int)" $ \((a@(a0, a1)) :: (Int, Int)) ->
      A.encode a === J.encode (J.Array (J.Aeson a0, J.Aeson a1))
    prop "Array (Int, Int, Text)" $ \((a@(a0, a1, a2)) :: (Int, Int, T.Text)) ->
      A.encode a === J.encode (J.Array (J.Aeson a0, J.Aeson a1, a2))
    it "Array ()" $ A.encode @[Void] [] === J.encode (J.Array ())
    prop "Array [Int]" $ \(l :: [Int]) -> A.encode l === J.encode (J.Array $ J.Aeson <$> l)

  -- Object
  describe "Object" $ do
    it "object []" $
      A.encode (A.object mempty) === J.encode (J.object @[] mempty)
    it "object" $
      A.encode (A.object ["a" A..= (1::Int)]) === J.encode (J.object ["a" J..= J.Aeson (1::Int)])
    it "object" $
      A.encode (A.object ["a" A..= Just (1::Int)]) === J.encode (J.object ["a" J..?= Just (J.Aeson (1::Int))])
    it "object" $
      A.encode (A.object ["a" A..= Just (1::Int)]) === J.encode (J.object ["a" J..?= Just (J.Aeson (1::Int))])

-- Numbers

aesonCompat_integral :: Spec
aesonCompat_integral = describe "integral" $ do
  prop "Int" $ \(a :: Int) -> A.encode a === J.encode (J.Aeson a)
  prop "Int8" $ \(a :: Int8) -> A.encode a === J.encode (J.Aeson a)
  prop "Int16" $ \(a :: Int16) -> A.encode a === J.encode (J.Aeson a)
  prop "Int32" $ \(a :: Int32) -> A.encode a === J.encode (J.Aeson a)
  prop "Int64" $ \(a :: Int64) -> A.encode a === J.encode (J.Aeson a)
  prop "Word" $ \(a :: Word) -> A.encode a === J.encode (J.Aeson a)
  prop "Word8" $ \(a :: Word8) -> A.encode a === J.encode (J.Aeson a)
  prop "Word16" $ \(a :: Word16) -> A.encode a === J.encode (J.Aeson a)
  prop "Word32" $ \(a :: Word32) -> A.encode a === J.encode (J.Aeson a)
  prop "Word64" $ \(a :: Word64) -> A.encode a === J.encode (J.Aeson a)
  prop "Integer" $ \(a :: Integer) -> A.encode a === J.encode (J.Aeson a)
  prop "Natural" $ \(a :: Natural) -> A.encode a === J.encode (J.Aeson a)

aesonCompat_float :: Spec
aesonCompat_float = describe "float" $ do
  prop "Float" $ \(a :: Float) -> A.encode a === J.encode (J.Aeson a)
  prop "Double" $ \(a :: Double) -> A.encode a === J.encode (J.Aeson a)

