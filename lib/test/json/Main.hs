{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Test.Hspec

-- internal modules

import qualified Test.Pact.JSON.Encode
import qualified Test.Pact.JSON.Legacy.Hashable
import qualified Test.Pact.JSON.Legacy.HashMap

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = hspec $ describe "json" $ do
  Test.Pact.JSON.Encode.tests
  describe "Test.Pact.JSON.Legacy.Hashable" Test.Pact.JSON.Legacy.Hashable.spec
  describe "Test.Pact.JSON.Legacy.HashMap" Test.Pact.JSON.Legacy.HashMap.spec
