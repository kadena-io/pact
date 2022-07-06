{-# LANGUAGE OverloadedStrings #-}
module KeysetSpec (spec) where

import Test.Hspec

import Data.Aeson

import Pact.Types.Runtime


spec :: Spec
spec = describe "fromJSON" testFromJSON

testFromJSON :: Spec
testFromJSON = do
  let ks = mkKeySet ["a","b"] "keys-all" Nothing
      ksn = mkKeySet ["a","b"] "keys-all" (Just "ns")
  it "full read from JSON, no namespace" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"], \"pred\": \"keys-all\" }" `shouldBe` Right ks
  it "object no pred, no namespace" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"]}" `shouldBe` Right ks
  it "just list, no namespace" $
    eitherDecode "[\"a\",\"b\"]" `shouldBe` Right ks
  it "full read from JSON, namespaced" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"], \"pred\": \"keys-all\" }" `shouldBe` Right ksn
  it "object no pred, namespaced" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"]}" `shouldBe` Right ksn
  it "just list, namespaced" $
    eitherDecode "[\"a\",\"b\"]" `shouldBe` Right ksn
