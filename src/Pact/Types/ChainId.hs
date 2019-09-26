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
-- Chain and Network Identifiers
--
module Pact.Types.ChainId
( -- * Chain Id
  ChainId(..)
, chainId

  -- * Network Id
, NetworkId(..)
, networkId
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize)
import Data.String (IsString)
import Data.Text

import Pact.Types.Term (ToTerm(..))
import Pact.Types.Pretty

-- | Expresses unique platform-specific chain identifier.
--
newtype ChainId = ChainId { _chainId :: Text }
  deriving stock (Eq, Generic)
  deriving newtype
    ( Show, Pretty, IsString
    , ToJSON, FromJSON
    , Serialize, ToTerm, NFData
    )

instance Wrapped ChainId

chainId :: Lens' ChainId Text
chainId =  lens _chainId (\t b -> t { _chainId = b })

-- | Network Ids are blockchain-specific network identifiers
--
newtype NetworkId = NetworkId { _networkId :: Text }
  deriving stock (Eq, Generic)
  deriving newtype
    ( Show, Pretty, IsString
    , ToJSON, FromJSON
    , Serialize, ToTerm, NFData
    )

instance Wrapped NetworkId

networkId :: Lens' NetworkId Text
networkId =  lens _networkId (\t b -> t { _networkId = b })
