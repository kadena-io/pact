{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Pact.JSON.Legacy.Value
( -- LegacyValue
  LegacyValue(..)
, toLegacyJson

, legacyKeyMap
, legacyMap
, legacyMap_
, legacyHashMap
, legacyHashMap_

-- , legacyJsonPropertySort
-- , legacyHashMapToEncoding
-- , legacyMapToEncoding
-- , legacyHashMap

-- * Tools
, legacyJsonPropertySort
, legacyJsonPropertySortPairs
, legacyHashSetToList
) where

import Control.DeepSeq

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import Data.Reflection
import qualified Data.Text as T
import Data.Typeable

import GHC.Stack

import Pact.JSON.Legacy.Hashable
import qualified Pact.JSON.Legacy.HashMap as LHM

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- Aeson-2 Migration Plan
--
-- All places where JSON data is hashed:
--
-- * PaylaodWithOutputs
-- * RequestKey
-- * Signatures
-- * TxLogs?
--
-- 1. Phase
--
-- * Replace /all/ direct use of 'Value' by 'LegacyValue'.
-- * Ban the direct use of 'toJSON' and replace by `toLegacyJson`
-- *
--
-- 2. Phase
--
-- * Create pact-json package that
--   * use aeson for parsing
--   * custom builder (replace toEncoding)
--   * legacy property ordering
--
-- * Fork
--   * order all properties alphabetically
--   * Don't hash json that hasn't been encoded the builder
--

-- -------------------------------------------------------------------------- --
-- Legacy JSON Value

-- | Legacy JSON Value
--
-- Use this type instead of 'Value' whenever a Value needs to be encoded. Do
-- not encode 'Value' values directly with 'encode' and similar functions.
--
-- Values of this types are stored as aeson 'Value' values in memory. Only the
-- encoding behavior is changed as follows:
--
-- * Calling 'toEncoding' orders object properties using the legacy order from
--   hashable-1.3.0 and unordered-containers-0.2.15.0
-- * Calling 'toJSON' is an 'error'.
--
newtype LegacyValue = LegacyValue { _getLegacyValue :: Value }
  deriving (Show, Eq, NFData)

instance ToJSON LegacyValue where
  toJSON :: LegacyValue -> Value
  toJSON =  _getLegacyValue
        -- this case is only safe when 'LegacyValues' are created. It allows to
        -- nest 'LegacyValue' values in inner datastructers into a new top-level
        -- 'LegacyValue'.
  {-# INLINE toJSON #-}

instance FromJSON LegacyValue where
  parseJSON = fmap LegacyValue . parseJSON
  {-# INLINE parseJSON #-}

-- | Convert a value with 'ToJSON' instance into 'LegacyValue'. The resulting
-- value must not be encoded with 'toJSON'. Instead the 'toEncoding' code path
-- must be used, which guarantees correct legacy ordering of properties.
--
-- In the context of toLegacyJson, we allow the use of toJSON on LegacyValue.
-- This allows to create aeson 'Value' values from types that have nested
-- 'LegacyValue' values. (This happens for instance in chainweb in pact service
-- when TxLog values are embedded in pact error messages.) It is safe to do so,
-- becaues by wrapping the overall result in 'LegacyValue' it is guaranteed that
-- the correct legacy serialization is preserved for all nested values. It is
-- also guaranteed that 'toJSON' can not be called in on the result to encode
-- the value to JSON.
--
toLegacyJson :: ToJSON a => a -> LegacyValue
toLegacyJson = LegacyValue . toJSON
{-# INLINE toLegacyJson #-}

-- Encode an aeson 'Value' in a way that is compatible with aeson <2
--
-- TODO: we should probalby create a dedicated value data type 'LegacyValue' for
-- it instead of just newtype wrapping 'Value'
--
instance J.Encode LegacyValue where
  build (LegacyValue (Object o)) = J.build $ LegacyValue <$> legacyKeyMap o
  build (LegacyValue (Array a)) = J.build $ J.Array $ LegacyValue <$> a
  build (LegacyValue v) = J.build v

  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- Utils

-- | Transform a list of key value pairs with a textual JSON key encoding into a
-- list of pairs with text keys.
--
-- It is an error to call this function on pairs with non-textual keys.
--
-- Often, instead of forcing Text Keys we would also just require
-- 'LegacyHashable' for the key type, which would avoid the 'error'. However, we
-- think (and have evidence), that a non-textual encoding is almost always
-- uninteded and enforcing a textual encoding catches those issues.
--
enforceTextKeys
  :: forall k v
  . HasCallStack
  => Typeable k
  => ToJSONKey k
  => [(k, v)]
  -> [(T.Text, v)]
enforceTextKeys = case toJSONKey of
  ToJSONKeyText f _ -> fmap (first (AK.toText . f))
  _ -> error $ "Pact.Utils.Json: failed to JSON encode non-textual legacy key of type " <> show (typeRep (Proxy @k))
{-# INLINE enforceTextKeys #-}

-- -------------------------------------------------------------------------- --
-- Aeson KeyMap Conversion

-- | Convert a 'AKM.KeyMap' that has a textual JSON key into LegacyHashMap which
-- perserve the legacy ordering of keys.
--
legacyKeyMap :: HasCallStack => AKM.KeyMap v -> LHM.HashMap T.Text v
legacyKeyMap = LHM.fromList . fmap (first AK.toText) . AKM.toList
{-# INLINE legacyKeyMap #-}

-- -------------------------------------------------------------------------- --
-- Strict Map Conversion

-- | Convert a 'M.Map' that has a textual JSON key into HashMap with a Legacy
-- Key.
--
-- It is an error if the key type is not encoded as JSON text.
--
-- Instead of forcing text Keys we would also just require 'LegacyHashable' for
-- the key type, which would avoid the 'error'. However, we think (and have
-- evidence), that a non-textual encoding is almost always uninteded and
-- enforcing a textual encoding catches those issues.
--
legacyMap
  :: HasCallStack
  => Typeable a
  => ToJSONKey a
  => M.Map a b
  -> LHM.HashMap T.Text b
legacyMap = LHM.fromList . enforceTextKeys . M.toList
{-# INLINE legacyMap #-}

-- | A version of legacyMap that does not enforce textual keys but instead
-- requires a 'LegacyHashable' constraint.
--
legacyMap_ :: LegacyHashable a => M.Map a b -> LHM.HashMap a b
legacyMap_ = LHM.fromList . M.toList
{-# INLINE legacyMap_ #-}

-- -------------------------------------------------------------------------- --
-- Strict HashMap Conversion

-- | Convert a 'HM.HashMap.Strict' that has a textual JSON key into HashMap with
-- a Legacy Key.
--
-- It is an error if the key type is not encoded as JSON text.
--
-- Instead of forcing text Keys we would also just require 'LegacyHashable' for
-- the key type, which would avoid the 'error'. However, we think (and have
-- evidence), that a non-textual encoding is almost always uninteded and
-- enforcing a textual encoding catches those issues.
--
legacyHashMap
  :: HasCallStack
  => Typeable a
  => ToJSONKey a
  => HM.HashMap a b
  -> LHM.HashMap T.Text b
legacyHashMap = LHM.fromList . enforceTextKeys . HM.toList
{-# INLINE legacyHashMap #-}

-- | A version of legacyMap that does not enforce textual keys but instead
-- requires a 'LegacyHashable' constraint.
--
legacyHashMap_ :: LegacyHashable a => HM.HashMap a b -> LHM.HashMap a b
legacyHashMap_ = LHM.fromList . HM.toList
{-# INLINE legacyHashMap_ #-}

-- -------------------------------------------------------------------------- --
-- Tools

-- | Sort a list of 'T.Text' using the legacy JSON property sorting.
--
legacyJsonPropertySort :: [T.Text] -> [T.Text]
legacyJsonPropertySort = LHM.sort
{-# INLINE legacyJsonPropertySort #-}

-- | Sort a list of 'T.Text' using the legacy JSON property sorting.
--
legacyJsonPropertySortPairs :: [T.Text] -> [T.Text]
legacyJsonPropertySortPairs = LHM.sort
{-# INLINE legacyJsonPropertySortPairs #-}

legacyHashSetToList :: HS.HashSet T.Text -> [T.Text]
legacyHashSetToList = legacyJsonPropertySort . HS.toList
{-# INLINE legacyHashSetToList #-}

