 {-# LANGUAGE OverloadedStrings #-}
module HashSpec
  ( spec
  )
where       


import Data.Either (isRight)
import Data.ByteString.Char8 (pack)
import Pact.Types.Hash
import Test.Hspec

spec :: Spec
spec = do
  hashSpec
  sanitySpec

hashSpec :: Spec
hashSpec = describe "hashing functions" $ do
  let s = pack "ffffff"
  it "should produce a base-2 integral representation of a given hash" $ do
    binaryHash s `shouldSatisfy` isRight
  it "should produce a base-8 integral representation of a given hash" $ do
    octalHash s `shouldSatisfy` isRight
  it "should produce a base-16 integral representation of a given hash" $ do
    hexadecimalHash s `shouldSatisfy` isRight
  it "should produce a base-n integral representation of a given hash" $ do
    numericBasedHash 20 s `shouldSatisfy` isRight

sanitySpec :: Spec
sanitySpec = describe "hashing functions" $ do
  let s = pack "ffffff"
  it "should have sane base-2 behavior" $ do
    binaryHash s `shouldBe` numericBasedHash 2 s
  it "should have sane base-2 behavior" $ do
    octalHash s `shouldBe` (numericBasedHash 8 s)
  it "should have sane base-2 behavior" $ do
    hexadecimalHash s `shouldBe` numericBasedHash 16 s
