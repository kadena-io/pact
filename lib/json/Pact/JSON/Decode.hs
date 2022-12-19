{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Pact.JSON.Decode
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.JSON.Decode
( A.decode
, A.decode'
, A.decodeStrict
, A.decodeFileStrict
, A.decodeStrict'
, A.decodeFileStrict'
, A.eitherDecode
, A.eitherDecode'
, A.eitherDecodeStrict
, A.eitherDecodeFileStrict
, A.eitherDecodeStrict'
, A.eitherDecodeFileStrict'

-- * FromJSON
, A.FromJSON(..)
, A.FromJSON1(..)
, A.Value(..)
, A.KeyValue(..)
, A.Array
, A.Object
, (A..:)
, (A..:?)
, (A..:!)
, (A..!=)
, (A.<?>)
, A.Result(..)
, A.withObject
, A.withArray
, A.withText
, A.withScientific
, A.withBool

-- * KeyMap
, A.FromJSONKey(..)
, A.FromJSONKeyFunction(..)
, A.KeyMap
, A.Key
, A.fromText
, A.toText
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A

