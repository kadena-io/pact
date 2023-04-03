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
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ describe "Musl.Trans" $ do
    spec_trans_ln
    spec_trans_exp
    spec_trans_pow
    spec_trans_log
    spec_trans_sqrt
    spec_ln_exp
    spec_log_pow

-- -------------------------------------------------------------------------- --
-- Specs

spec_trans_ln :: Spec
spec_trans_ln = describe "trans_ln" $ do
    it "ln 60" $ trans_ln 60 `shouldBe` 4.09434456222210041431708305026404559612274169921875

spec_trans_exp :: Spec
spec_trans_exp = describe "trans_exp" $ do
    it "e" $ trans_exp 1 `shouldBe` 2.718281828459045

spec_ln_exp :: Spec
spec_ln_exp = describe "trans_ln . trans_exp" $ do
    prop "ln(exp x)" $ \x -> trans_ln (trans_exp x) `roughlyEq` x

spec_trans_pow :: Spec
spec_trans_pow = describe "trans_pow" $ do
    it "pow 2 0" $ trans_pow 2 0 `shouldBe` 1
    it "pow 2 1" $ trans_pow 2 1 `shouldBe` 2
    it "pow 2 2" $ trans_pow 2 2 `shouldBe` 4
    it "pow 2 3" $ trans_pow 2 3 `shouldBe` 8
    it "pow 2 4" $ trans_pow 2 4 `shouldBe` 16
    it "pow 2 10" $ trans_pow 2 10 `shouldBe` 1024
    it "pow 8.7 85.7" $ trans_pow 8.7 85.7 `shouldBe` 3.28700185683462e80
    it "pow 8.7 856739.34857" $ trans_pow 8.7 856739.34857 `shouldBe` infty
    prop "pow x 0" $ \x -> trans_pow x 0 === 1
    prop "pow x 1" $ \x -> trans_pow x 1 === x
    prop "pow x 2" $ \x -> trans_pow x 2 === x * x

spec_trans_log :: Spec
spec_trans_log = describe "trans_log" $ do
    it "log 2 1" $ trans_log 2 1 `shouldBe` 0
    it "log 2 2" $ trans_log 2 2 `shouldBe` 1
    it "log 2 4" $ trans_log 2 4 `shouldBe` 2
    it "log 2 8" $ trans_log 2 8 `shouldBe` 3
    it "log 2 16" $ trans_log 2 16 `shouldBe` 4
    it "log 2 256" $ trans_log 2 256 `shouldBe` 8
    it "log 2 1024" $ trans_log 2 1024 `shouldBe` 10
    it "log 2 5" $ trans_log 2 5 `shouldBe` 2.321928094887362
    it "log 13.7 721.8" $ trans_log 13.7 721.8 `shouldBe` 2.5146170134618364

spec_log_pow :: Spec
spec_log_pow = describe "trans_log . trans_pow" $ do
    prop "log 2 (pow 2 x)" $ \x -> trans_log 2 (trans_pow 2 x) `roughlyEq` x

spec_trans_sqrt :: Spec
spec_trans_sqrt = describe "trans_sqrt" $ do
    it "sqrt 1" $ trans_sqrt 1 `shouldBe` 1
    it "sqrt 4" $ trans_sqrt 4 `shouldBe` 2
    it "sqrt 9" $ trans_sqrt 9 `shouldBe` 3
    it "sqrt 16" $ trans_sqrt 16 `shouldBe` 4
    it "sqrt 25" $ trans_sqrt 25 `shouldBe` 5
    it "sqrt 36" $ trans_sqrt 36 `shouldBe` 6
    it "sqrt 49" $ trans_sqrt 49 `shouldBe` 7
    it "sqrt 2" $ trans_sqrt 2 `shouldBe` 1.4142135623730951
    prop "sqrt (x * x)" $ \x -> trans_sqrt (x * x) === abs x

-- -------------------------------------------------------------------------- --
-- Utils

infty :: Double
infty = read "Infinity"

roughlyEq :: Double -> Double -> Bool
roughlyEq a b
    | a == b = True
    | abs (a - b) < 0.00000001 = True
    | otherwise = False
