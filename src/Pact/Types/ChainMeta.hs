{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Meta data and its types
--
module Pact.Types.ChainMeta
  ( -- * types
    Address(..)
  , PrivateMeta(..)
  , PublicMeta(..)
  , HasPlafMeta(..)
  , PublicData(..)
  , EntityName(..)
  , TTLSeconds(..)
  , TxCreationTime(..)
    -- * optics
  , aFrom, aTo
  , pmAddress, pmChainId, pmSender, pmGasLimit, pmGasPrice, pmTTL, pmCreationTime
  , pdPublicMeta, pdBlockHeight, pdBlockTime, pdPrevBlockHash
  ) where


import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Control.Monad

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
import Pact.Types.ChainId (ChainId)
import Pact.Types.Gas
import Pact.Types.Util (AsString, lensyToJSON, lensyParseJSON)


newtype EntityName = EntityName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, NFData, Hashable, Serialize, Default, ToJSON, FromJSON, IsString, AsString)

-- | Wrapper for 'PublicMeta' ttl field in seconds since offset
--
newtype TTLSeconds = TTLSeconds ParsedInteger
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num, NFData, ToJSON, FromJSON, Serialize)

-- | Wrapper for 'PublicMeta' creation time field in seconds since POSIX epoch
--
newtype TxCreationTime = TxCreationTime ParsedInteger
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num, NFData, ToJSON, FromJSON, Serialize)

-- | Confidential/Encrypted addressing info, for use in metadata on privacy-supporting platforms.
data Address = Address
  { _aFrom :: EntityName
  , _aTo :: Set EntityName
  } deriving (Eq,Show,Ord,Generic)

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

-- | Contains all necessary metadata for a Chainweb-style public chain.
data PublicMeta = PublicMeta
  { _pmChainId :: !ChainId
  , _pmSender :: !Text
  , _pmGasLimit :: !GasLimit
  , _pmGasPrice :: {-# UNPACK #-} !GasPrice
  , _pmTTL :: !TTLSeconds
  , _pmCreationTime :: !TxCreationTime
  } deriving (Eq, Show, Generic)
makeLenses ''PublicMeta

instance Default PublicMeta where def = PublicMeta "" "" 0 0 0 0

instance ToJSON PublicMeta where
  toJSON (PublicMeta cid s gl gp ttl ct) = object
    [ "chainId" .= cid
    , "sender" .= s
    , "gasLimit" .= gl
    , "gasPrice" .= gp
    , "ttl" .= ttl
    , "creationTime" .= ct
    ]

instance FromJSON PublicMeta where
  parseJSON = withObject "PublicMeta" $ \o -> PublicMeta
    <$!> o .: "chainId"
    <*> o .: "sender"
    <*> o .: "gasLimit"
    <*> o .: "gasPrice"
    <*> o .: "ttl"
    <*> o .: "creationTime"

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
  { _pdPublicMeta :: {-# UNPACK #-} !PublicMeta
  , _pdBlockHeight :: {-# UNPACK #-} !Word64
  , _pdBlockTime :: {-# UNPACK #-} !Int64
  , _pdPrevBlockHash :: !Text
  }
  deriving (Show, Eq, Generic)
makeLenses ''PublicData

instance ToJSON PublicData where toJSON = lensyToJSON 3
instance FromJSON PublicData where parseJSON = lensyParseJSON 3
instance Default PublicData where def = PublicData def def def def
