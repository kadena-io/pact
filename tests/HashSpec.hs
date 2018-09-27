 {-# LANGUAGE OverloadedStrings #-}
module HashSpec
  ( spec
  )
where       

import Data.Either (isRight)
import Data.Text (pack)
import Pact.Types.Hash
import Test.Hspec

spec :: Spec
spec = do
  hexadecimalSpec
  changeOfBaseSpec
  
hexadecimalSpec :: Spec
hexadecimalSpec = describe "hexadecimal hashing" $ do
  let hex = "123456abcdef"
  it "should be able to produce a base-16 integral representation of a given hex string" $ do
    hexStringToInteger hex `shouldSatisfy` isRight
  it "should be able to produce a base-10 integral representation of a given hex string" $ do
    basedStringToInteger 10 hex `shouldSatisfy` isRight
  it "should be able to produce a base-8 integral representation of a given hex string" $ do
    basedStringToInteger 8 hex `shouldSatisfy` isRight
  it "should yield the same result as change-of-base hashing" $ do
    let s = "ffffff"
    basedStringToInteger 16 s `shouldBe` hexStringToInteger s

changeOfBaseSpec :: Spec
changeOfBaseSpec = describe "change-of-base hashing" $ do
  let hex = "123456abcdef"
  let dec = 20016001699311
  let oct = 443212652746757
  let octStr = pack (show oct)
  let decStr = pack (show dec)
  it "should be able to produce a base-10 integral representation of a given hex string" $ do
    basedStringToInteger 10 hex `shouldBe` Right dec
  it "should be able to produce a base-8 integral representation of a given hex string" $ do
    basedStringToInteger 8 hex `shouldBe` Right oct
  it "should be able to produce a base-10 integral representation of a given octal string" $ do
    basedStringToInteger 10 octStr  `shouldBe` Right dec 
  it "should be able to produce a base-16 integral representation of a given octal string" $ do
    basedStringToInteger 16 octStr `shouldBe` hexStringToInteger hex
  it "should be able to produce a base-16 integral representation of a given decimal string" $ do
    basedStringToInteger 16 decStr `shouldBe` hexStringToInteger hex
  it "should be able to produce a base-8 integral representation of a given decimal string" $ do
    basedStringToInteger 8 decStr `shouldBe` Right oct
