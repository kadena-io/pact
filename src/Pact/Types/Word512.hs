{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Pact.Types.Word512
  ( Word512(..)
  ) where

import Data.Data
import Data.DoubleWord (Word256(..), Int256(..))
import Data.DoubleWord.TH (mkUnpackedDoubleWord)
import GHC.Generics

-- | This provides strict (low and high halves are unpacked)
--   signed and unsigned binary word data types of size 512b.
--   The order of data is as follows:
--        Unsigned type
--     -> Unsigned higher half type
--     -> Signed variant type
--     -> signed variant higher half type
--     -> lower-half type
--     -> [List of instances to autoderive]
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]


