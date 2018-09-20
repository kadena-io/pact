 {-# LANGUAGE OverloadedStrings #-}
module HashSpec
  ( spec
  )
where       


import Data.Char (digitToInt)
import Data.Either (isRight)
import Data.Text (pack)
import Pact.Types.Hash
import Test.Hspec

spec :: Spec
spec = describe "hexadecimal hashing" $ do
  it "should be able to produce a base-16 integral representation of a given hex string" $ do
    let s = pack "ffffff"
    hexStringToInteger s `shouldSatisfy` isRight
  it "should have sane base-16 behavior" $ do
    let s = pack "ffffff"
    hexStringToInteger s `shouldBe` hashAsBasedInteger 16 (fromIntegral . digitToInt) s
  it "should have correct behavior for base-16 numbers" $ do
    let d = 20016001699311
    let h = pack "123456abcdef" 
    hexStringToInteger h `shouldBe` Right d
  

