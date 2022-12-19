{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Pact.JSON.Legacy.HashMap
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: BSD-3
--
module Test.Pact.JSON.Legacy.HashMap
( spec
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- internal modules

import qualified Pact.JSON.Legacy.HashMap as LHM

spec :: Spec
spec = describe "LegacyHashMap" $ do

  it "null" $ shouldSatisfy @(LHM.HashMap () ()) LHM.empty LHM.null

  it "does not store duplicate keys" $ property $ \(m :: [((), Int)]) -> do
    length (LHM.fromList m) `shouldSatisfy` (<= 1)

  it "insert of existing key updates the value" $ do
    LHM.toList (LHM.insert () (1 :: Int) (LHM.singleton () 0)) `shouldBe` [((),1)]
  it "fromList associates from the left" $ do
    LHM.toList (LHM.fromList [((),0 :: Int), ((),1)]) `shouldBe` [((),1)]

  it "has same set semantics as HashMap" $ property $ \(m :: [(T.Text, Int)]) ->
    HM.fromList m === HM.fromList (LHM.toList (LHM.fromList m))

  it "has same set semantics as HashMap2" $ property $ \(m :: HM.HashMap T.Text Int) ->
    m === HM.fromList (LHM.toList (LHM.fromList (HM.toList m)))

  it "collisions do not affect order" $ property $ \(_m :: [(Int, Int)]) ->
    pendingWith "not yet implemented (hint: use fromListWithHash)"

