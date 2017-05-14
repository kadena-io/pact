{-# LANGUAGE OverloadedStrings #-}
module KeysetSpec where

import Test.Hspec

import Pact.Types.Runtime
import Data.Aeson

spec :: Spec
spec = do
  describe "fromJSON" testFromJSON

testFromJSON :: Spec
testFromJSON = do
  let ks = KeySet ["a","b"] "keys-all"
  it "full read from JSON" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"], \"pred\": \"keys-all\" }" `shouldBe` Right ks
  it "object no pred" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"]}" `shouldBe` Right ks
  it "just list" $
    eitherDecode "[\"a\",\"b\"]" `shouldBe` Right ks
