-- |
-- Module: Main
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Musl.Trans

import Test.Hspec

main :: IO ()
main = hspec $ describe "Musl.Trans" $ do
    spec_trans_ln
    spec_trans_pow

spec_trans_ln :: Spec
spec_trans_ln = describe "trans_ln" $
    it "ln 60" $ trans_ln 60 `shouldBe` 4.09434456222210041431708305026404559612274169921875

spec_trans_pow :: Spec
spec_trans_pow = describe "trans_pow" $ do
    it "pow 2 0" $ trans_pow 2 0 `shouldBe` 1
    it "pow 2 1" $ trans_pow 2 1 `shouldBe` 2
    it "pow 2 2" $ trans_pow 2 2 `shouldBe` 4
    it "pow 2 3" $ trans_pow 2 3 `shouldBe` 8
    it "pow 2 4" $ trans_pow 2 4 `shouldBe` 16
    it "pow 2 10" $ trans_pow 2 10 `shouldBe` 1024
