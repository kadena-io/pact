{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module      :  Pact.Types.ChainId
-- Copyright   :  (C) 2019 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>
--
-- ChainId data
--
module Pact.Types.ChainId
( -- * types
  ChainId(..)
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize)
import Data.String (IsString)
import Data.Text

import Pact.Types.Term (ToTerm(..))

-- | Expresses unique platform-specific chain identifier.
--
newtype ChainId = ChainId { _chainId :: Text }
  deriving (Eq, Show, Generic, IsString, ToJSON, FromJSON, Serialize)
  deriving newtype (NFData)

instance ToTerm ChainId where toTerm (ChainId i) = toTerm i

instance Wrapped ChainId
