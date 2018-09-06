{-# LANGUAGE OverloadedStrings #-}
module Hashpec
  ( spec
  )
where


import           Data.ByteString                ( ByteString )

import           Pact.Types.Hash                ( hash
                                                , verifyHash
                                                , initialHash
                                                , numericBasedHash
                                                , binaryHash
                                                , octalHash
                                                , hexidecimalHash
                                                )

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , expectationFailure
                                                , shouldBe
                                                , SpecWith(..)
                                                , Expectation
                                                )

-- TODO
spec :: Spec
spec = undefined
