{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , getCurrentCreationTime
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
import Pact.Time (getCurrentTime, toPosixTimestampMicros)
import Data.Word (Word64)

-- internal pact modules

import Pact.Parse
import Pact.Types.ChainId (ChainId)
import Pact.Types.Gas
import Pact.Types.Util (AsString, lensyToJSON, lensyParseJSON)

-- | Name of "entity", ie confidential counterparty in an encrypted exchange, in privacy-supporting platforms.
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

-- | Get current time as TxCreationTime
getCurrentCreationTime :: IO TxCreationTime
getCurrentCreationTime = TxCreationTime
    . fromIntegral
    . (`div` 1000000)
    . toPosixTimestampMicros
    <$> getCurrentTime

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

-- | Private-blockchain specific metadata.
newtype PrivateMeta = PrivateMeta { _pmAddress :: Maybe Address }
  deriving (Eq,Show,Generic)
makeLenses ''PrivateMeta

instance Default PrivateMeta where def = PrivateMeta def
instance ToJSON PrivateMeta where toJSON = lensyToJSON 3
instance FromJSON PrivateMeta where parseJSON = lensyParseJSON 3
instance NFData PrivateMeta
instance Serialize PrivateMeta

-- | Allows user to specify execution parameters specific to public-chain
-- execution, namely gas parameters, TTL, creation time, chain identifier.
data PublicMeta = PublicMeta
  { _pmChainId :: !ChainId
    -- ^ platform-specific chain identifier, e.g. "0"
  , _pmSender :: !Text
    -- ^ sender gas account key
  , _pmGasLimit :: !GasLimit
    -- ^ gas limit (maximum acceptable gas units for tx)
  , _pmGasPrice :: !GasPrice
    -- ^ per-unit gas price
  , _pmTTL :: !TTLSeconds
    -- ^ TTL in seconds
  , _pmCreationTime :: !TxCreationTime
    -- ^ Creation time in seconds since UNIX epoch
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
    <$> o .: "chainId"
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

-- | "Public chain" data with immutable block data
-- height, hash, creation time
data PublicData = PublicData
  { _pdPublicMeta :: !PublicMeta
    -- ^ 'PublicMeta' data from request
  , _pdBlockHeight :: !Word64
    -- ^ block height as specified by platform.
  , _pdBlockTime :: !Int64
    -- ^ block creation time, micros since UNIX epoch
  , _pdPrevBlockHash :: !Text
    -- ^ block hash of preceding block
  }
  deriving (Show, Eq, Generic)
makeLenses ''PublicData

instance ToJSON PublicData where toJSON = lensyToJSON 3
instance FromJSON PublicData where parseJSON = lensyParseJSON 3
instance Default PublicData where def = PublicData def def def def
