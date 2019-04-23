{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Meta data and its types
--
module Pact.Types.ChainMeta
  ( -- * types
    Address(..)
  , PrivateMeta(..)
  , ChainId(..)
  , PublicMeta(..)
  , HasPlafMeta(..)
  , PublicData(..)
  , EntityName(..)
    -- * optics
  , aFrom, aTo
  , pmAddress, pmChainId, pmSender, pmGasLimit, pmGasPrice
  , pdPublicMeta, pdBlockHeight, pdBlockTime
  ) where


import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)

import Data.Aeson
import Data.Default (Default, def)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Serialize (Serialize)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text
import Data.Word (Word64)

-- internal pact modules

import Pact.Parse
import Pact.Types.Util (AsString, lensyToJSON, lensyParseJSON)
import Pact.Types.Term (ToTerm(..))


newtype EntityName = EntityName Text
  deriving (Eq, Ord, NFData, ToJSON, FromJSON, Default, Generic, IsString, Serialize, Hashable, AsString)

instance Show EntityName where show (EntityName t) = show t


-- | Confidential/Encrypted addressing info, for use in metadata on privacy-supporting platforms.
data Address = Address
  { _aFrom :: EntityName
  , _aTo :: Set EntityName
  }
  deriving (Eq,Show,Ord,Generic)

instance NFData Address
instance Serialize Address
instance ToJSON Address where toJSON = lensyToJSON 2
instance FromJSON Address where parseJSON = lensyParseJSON 2
makeLenses ''Address

newtype PrivateMeta = PrivateMeta { _pmAddress :: Maybe Address }
  deriving (Eq,Show,Generic)
makeLenses ''PrivateMeta

instance Default PrivateMeta where def = PrivateMeta def
instance ToJSON PrivateMeta where toJSON = lensyToJSON 3
instance FromJSON PrivateMeta where parseJSON = lensyParseJSON 3
instance NFData PrivateMeta
instance Serialize PrivateMeta


-- | Expresses unique platform-specific chain identifier.
newtype ChainId = ChainId Text
  deriving (Eq, Show, Generic, IsString, ToJSON, FromJSON, Serialize, NFData)
instance ToTerm ChainId where toTerm (ChainId i) = toTerm i


-- | Contains all necessary metadata for a Chainweb-style public chain.
data PublicMeta = PublicMeta
  { _pmChainId :: ChainId
  , _pmSender :: Text
  , _pmGasLimit :: ParsedInteger
  , _pmGasPrice :: ParsedDecimal
  } deriving (Eq, Show, Generic)
makeLenses ''PublicMeta

instance Default PublicMeta where def = PublicMeta "" "" 0 0
instance ToJSON PublicMeta where toJSON = lensyToJSON 3
instance FromJSON PublicMeta where parseJSON = lensyParseJSON 3
instance NFData PublicMeta
instance Serialize PublicMeta

class HasPlafMeta a where
  getPrivateMeta :: a -> PrivateMeta
  getPublicMeta :: a -> PublicMeta

instance HasPlafMeta PrivateMeta where
  getPrivateMeta = id
  getPublicMeta = const def

instance HasPlafMeta PublicMeta where
  getPrivateMeta = const def
  getPublicMeta = id

instance HasPlafMeta () where
  getPrivateMeta = const def
  getPublicMeta = const def

data PublicData = PublicData
  { _pdPublicMeta :: PublicMeta
  , _pdBlockHeight :: Word64
  , _pdBlockTime :: Int64
  }
  deriving (Show, Eq, Generic)
makeLenses ''PublicData

instance ToJSON PublicData where toJSON = lensyToJSON 3
instance FromJSON PublicData where parseJSON = lensyParseJSON 3
instance Default PublicData where def = PublicData def def def
