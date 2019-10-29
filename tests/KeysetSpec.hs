{-# LANGUAGE OverloadedStrings #-}
module KeysetSpec (spec) where

import Test.Hspec

import Data.Aeson

import Pact.Types.Runtime


spec :: Spec
spec = describe "fromJSON" testFromJSON

testFromJSON :: Spec
testFromJSON = do
  let ks = mkKeySet ["a","b"] "keys-all"
  it "full read from JSON" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"], \"pred\": \"keys-all\" }" `shouldBe` Right ks
  it "object no pred" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"]}" `shouldBe` Right ks
  it "just list" $
    eitherDecode "[\"a\",\"b\"]" `shouldBe` Right ks
