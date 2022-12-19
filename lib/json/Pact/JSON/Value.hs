-- |
-- Module: Pact.JSON.Value
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module only exposes what is needed to create 'Value' values. It hides
-- all functionality that would allow encoding of 'Value' values via aeson's
-- encoding function.
--
module Pact.JSON.Value
( A.ToJSON(toJSON)
, (A..=)
, A.Value(..)
, A.KeyMap
, A.Key
, A.fromText
, A.toText
, A.object
, A.pairs
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A

