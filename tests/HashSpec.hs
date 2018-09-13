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
spec = do
  hashSpec
  sanitySpec
  unitSpec
  
hashSpec :: Spec
hashSpec = describe "hashing functions" $ do
  let s = pack "ffffff"
  it "should produce a base-2 integral representation of a given hash" $ do
    binaryStringToInteger s `shouldSatisfy` isRight
  it "should produce a base-8 integral representation of a given hash" $ do
    octalStringToInteger s `shouldSatisfy` isRight
  it "should produce a base-10 integral representation of a given hash" $ do
    octalStringToInteger s `shouldSatisfy` isRight
  it "should produce a base-16 integral representation of a given hash" $ do
    hexStringToInteger s `shouldSatisfy` isRight

sanitySpec :: Spec
sanitySpec = describe "hashing functions" $ do
  let s = pack "ffffff"
  it "should have sane base-2 behavior" $ do
    binaryStringToInteger s `shouldBe` hashAsBasedInteger 2 (fromIntegral . digitToInt) s
  it "should have sane base-8 behavior" $ do
    octalStringToInteger s `shouldBe` hashAsBasedInteger 8 (fromIntegral . digitToInt) s
  it "should have sane base-10 behavior" $ do
    decimalStringToInteger s `shouldBe` hashAsBasedInteger 10 (fromIntegral . digitToInt) s
  it "should have sane base-16 behavior" $ do
    hexStringToInteger s `shouldBe`  hashAsBasedInteger 16 (fromIntegral . digitToInt) s

unitSpec :: Spec
unitSpec = describe "hashing functions" $ do
  -- these are all the same value
  let b = pack "100100011010001010110101010111100110111101111"
  let o = pack "443212652746757"
  let d = pack "20016001699311"
  let h = pack "123456abcdef" 
  
  it "should have correct behavior for base-2 numbers" $ do
    binaryStringToInteger b `shouldBe` Right 20016001699311
  it "should have correct behavior for base-8 numbers" $ do
    octalStringToInteger o `shouldBe` Right 20016001699311
  it "should have correct behavior for base-10 numbers" $ do
    decimalStringToInteger d `shouldBe` Right 20016001699311 
  it "should have correct behavior for base-16 numbers" $ do
    hexStringToInteger h `shouldBe` Right 20016001699311
 

 
