{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Serialize (Serialize)
import Data.String (IsString)
import Data.Text
import Test.QuickCheck (Arbitrary(..))

import Servant.API (ToHttpApiData(..))

import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Types.Term (ToTerm(..))
import Pact.Types.Util (genBareText)

-- | Expresses unique platform-specific chain identifier.
--
newtype ChainId = ChainId { _chainId :: Text }
  deriving stock (Eq, Generic)
  deriving newtype
    ( Show, Pretty, IsString
    , ToJSON, FromJSON
    , Serialize, ToTerm, NFData
    , SizeOf
    )

instance Arbitrary ChainId where
  arbitrary = ChainId <$> genBareText

instance Wrapped ChainId

instance ToHttpApiData ChainId where
  toUrlPiece = _chainId

-- | Lens into the text value of a 'ChainId'
--
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

-- | Lens into the value of 'NetworkId'
--
networkId :: Lens' NetworkId Text
networkId =  lens _networkId (\t b -> t { _networkId = b })
