{-# LANGUAGE CPP #-}
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

import Test.QuickCheck

-- internal pact modules

import Pact.Parse
import Pact.Types.ChainId (ChainId)
import Pact.Types.Gas
import Pact.Types.Util (AsString, lensyParseJSON)
#ifdef PACT_TOJSON
import Pact.Types.Util (lensyToJSON, JsonProperties, enableToJSON)
#endif

import qualified Pact.JSON.Encode as J

-- | Name of "entity", ie confidential counterparty in an encrypted exchange, in privacy-supporting platforms.
newtype EntityName = EntityName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, NFData, Hashable, Serialize, Default, FromJSON, IsString, AsString, J.Encode)
#ifdef PACT_TOJSON
  deriving newtype (ToJSON)
#endif

instance Arbitrary EntityName where
  arbitrary = EntityName <$> arbitrary

-- | Wrapper for 'PublicMeta' ttl field in seconds since offset
--
newtype TTLSeconds = TTLSeconds ParsedInteger
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num, NFData, FromJSON, Serialize, J.Encode)
#ifdef PACT_TOJSON
  deriving newtype (ToJSON)
#endif

instance Arbitrary TTLSeconds where
  arbitrary = TTLSeconds <$> arbitrary

-- | Wrapper for 'PublicMeta' creation time field in seconds since POSIX epoch
--
newtype TxCreationTime = TxCreationTime ParsedInteger
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num, NFData, FromJSON, Serialize, J.Encode)
#ifdef PACT_TOJSON
  deriving newtype (ToJSON)
#endif

instance Arbitrary TxCreationTime where
  arbitrary = TxCreationTime <$> arbitrary

-- | Get current time as TxCreationTime
getCurrentCreationTime :: IO TxCreationTime
getCurrentCreationTime = TxCreationTime
    . fromIntegral
    . (`div` 1000000)
    . toPosixTimestampMicros
    <$> getCurrentTime

-- | Confidential/Encrypted addressing info, for use in metadata on privacy-supporting platforms.
data Address = Address
  { _aFrom :: !EntityName
  , _aTo :: !(Set EntityName)
  } deriving (Eq,Show,Ord,Generic)

instance NFData Address
instance Serialize Address

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary

#ifdef PACT_TOJSON
addressProperties :: JsonProperties Address
addressProperties o =
  [ "to" .= _aTo o
  , "from" .= _aFrom o
  ]
{-# INLINE addressProperties #-}

instance ToJSON Address where
  toJSON = enableToJSON "Pact.Types.ChainMeta.Address" . lensyToJSON 2
  toEncoding = pairs . mconcat . addressProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode Address where
  build o = J.object
    [ "to" J..= J.Array (_aTo o)
    , "from" J..= _aFrom o
    ]
  {-# INLINE build #-}

instance FromJSON Address where parseJSON = lensyParseJSON 2
makeLenses ''Address

-- | Private-blockchain specific metadata.
newtype PrivateMeta = PrivateMeta { _pmAddress :: Maybe Address }
  deriving (Eq,Show,Generic)
makeLenses ''PrivateMeta

instance Default PrivateMeta where def = PrivateMeta def

instance Arbitrary PrivateMeta where
  arbitrary = PrivateMeta <$> arbitrary

#ifdef PACT_TOJSON
privateMetaProperties :: JsonProperties PrivateMeta
privateMetaProperties o = [ "address" .= _pmAddress o ]
{-# INLINE privateMetaProperties #-}

instance ToJSON PrivateMeta where
  toJSON = enableToJSON "Pact.Types.ChainMeta.PrivateMeta" . lensyToJSON 3
  toEncoding = pairs . mconcat . privateMetaProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode PrivateMeta where
  build o = J.object [ "address" J..= _pmAddress o ]
  {-# INLINE build #-}

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

instance Arbitrary PublicMeta where
  arbitrary = PublicMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

#ifdef PACT_TOJSON
publicMetaProperties :: JsonProperties PublicMeta
publicMetaProperties o =
  [ "creationTime" .= _pmCreationTime o
  , "ttl" .= _pmTTL o
  , "gasLimit" .= _pmGasLimit o
  , "chainId" .= _pmChainId o
  , "gasPrice" .= _pmGasPrice o
  , "sender" .= _pmSender o
  ]
{-# INLINE publicMetaProperties #-}

instance ToJSON PublicMeta where
  toJSON = enableToJSON "Pact.Types.ChainMeta.PublicMeta" . object . publicMetaProperties
  toEncoding = pairs . mconcat . publicMetaProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode PublicMeta where
  build o = J.object
    [ "creationTime" J..= _pmCreationTime o
    , "ttl" J..= _pmTTL o
    , "gasLimit" J..= _pmGasLimit o
    , "chainId" J..= _pmChainId o
    , "gasPrice" J..= _pmGasPrice o
    , "sender" J..= _pmSender o
    ]
  {-# INLINE build #-}

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

#ifdef PACT_TOJSON
publicDataProperties :: JsonProperties PublicData
publicDataProperties o =
  [ "publicMeta" .= _pdPublicMeta o
  , "blockTime" .= _pdBlockTime o
  , "prevBlockHash" .= _pdPrevBlockHash o
  , "blockHeight" .= _pdBlockHeight o
  ]
{-# INLINE publicDataProperties #-}

instance ToJSON PublicData where
  toJSON = enableToJSON "Pact.Types.ChainMeta.PublicData" . lensyToJSON 3
  toEncoding = pairs. mconcat . publicDataProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode PublicData where
  build o = J.object
    [ "publicMeta" J..= _pdPublicMeta o
    , "blockTime" J..= J.Aeson (_pdBlockTime o)
    , "prevBlockHash" J..= _pdPrevBlockHash o
    , "blockHeight" J..= J.Aeson (_pdBlockHeight o)
    ]
  {-# INLINE build #-}

instance FromJSON PublicData where parseJSON = lensyParseJSON 3
instance Default PublicData where def = PublicData def def def def

instance Arbitrary PublicData where
  arbitrary = PublicData
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
