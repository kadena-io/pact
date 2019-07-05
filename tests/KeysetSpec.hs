{-# LANGUAGE OverloadedStrings #-}
module KeysetSpec (spec) where

import Test.Hspec

import Data.Aeson
import Data.Default (def)

import Pact.Types.Term

spec :: Spec
spec = describe "fromJSON" testFromJSON

testFromJSON :: Spec
testFromJSON = do
  let ks = KeySet ["a","b"] (Name "keys-all" def)
  it "full read from JSON" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"], \"pred\": \"keys-all\" }" `shouldBe` Right ks
  it "object no pred" $
    eitherDecode "{ \"keys\": [\"a\",\"b\"]}" `shouldBe` Right ks
  it "just list" $
    eitherDecode "[\"a\",\"b\"]" `shouldBe` Right ks
