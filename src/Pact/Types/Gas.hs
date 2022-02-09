{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Pact.Types.Gas
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Gas (compute and space cost calculation) types.
--
module Pact.Types.Gas
  ( -- * types
    Gas(..)
  , GasPrice(..)
  , GasEnv(..)
  , ReadValue(..)
  , WriteValue(..)
  , GasModel(..)
  , GasArgs(..)
  , GasLimit(..)
    -- * optics
  , geGasLimit
  , geGasPrice
  , geGasModel
  ) where

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses,Wrapped)

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Aeson.Types (Parser)
import Data.Serialize

import GHC.Generics

import Pact.Types.Continuation
import Pact.Types.Info
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.PactValue
import Pact.Types.RowData
import Pact.Types.Term
import Pact.Parse
import Pact.Types.SizeOf(Bytes)


parseGT0 :: (FromJSON a,Num a,Ord a) => Value -> Parser a
parseGT0 v = parseJSON v >>= \a ->
  if a >= 0 then return a else
    fail "value must be greater than 0"
{-# INLINABLE parseGT0 #-}

-- | API Price value, basically a newtype over `Decimal`
newtype GasPrice = GasPrice ParsedDecimal
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,NFData,Serialize,Generic,ToTerm,ToJSON,Pretty)
instance Show GasPrice where
  show (GasPrice (ParsedDecimal d)) = show d

instance FromJSON GasPrice where
  parseJSON = fmap GasPrice . parseGT0

instance Wrapped GasPrice

-- | DB Read value for per-row gas costing.
-- Data is included if variable-size.
data ReadValue
  = ReadData !RowData
  | ReadKey !RowKey
  | ReadTxId
  | ReadModule !ModuleName !Code
  | ReadInterface !ModuleName !Code
  | ReadNamespace !(Namespace PactValue)
  | ReadKeySet !KeySetName !KeySet
  | ReadYield !Yield

instance Pretty ReadValue where
  pretty g = case g of
    ReadData {} -> "ReadData"
    ReadKey {} -> "ReadKey"
    ReadTxId -> "ReadTxId"
    ReadModule {} -> "ReadModule"
    ReadInterface {} -> "ReadInterface"
    ReadNamespace {} -> "ReadNamespace"
    ReadKeySet {} -> "ReadKeySet"
    ReadYield {} -> "ReadYield"


data WriteValue
  = WriteData !WriteType !Text !(ObjectMap PactValue)
  | WriteTable !Text
  | WriteModule !ModuleName !Code
  | WriteInterface !ModuleName !Code
  | WriteNamespace !(Namespace PactValue)
  | WriteKeySet !KeySetName !KeySet
  | WriteYield !Yield

instance Pretty WriteValue where
  pretty g = case g of
    WriteData ty _ _ -> "WriteData:" <> pretty ty
    WriteTable {} -> "WriteTable"
    WriteModule {} -> "WriteModule"
    WriteInterface {} -> "WriteInterface"
    WriteNamespace {} -> "WriteNamespace"
    WriteKeySet {} -> "WriteKeySet"
    WriteYield {} -> "WriteYield"


data GasArgs
  = GSelect !(Maybe [(Info,FieldKey)])
  -- ^ Cost of selecting columns from a user table
  | GDistinct !Int
  -- ^ Cost of deduping any list
  | GSort !Int
  -- ^ Cost of performing sort on any list
  | GSortFieldLookup !Int
  -- ^ Cost of sorting by lookup fields
  | GConcatenation !Int !Int
  -- ^ Cost of concatenating two strings, lists, and objects
  | GUnreduced ![Term Ref]
  -- ^ Cost of using a native function
  | GPostRead !ReadValue
  -- ^ Cost for reading from database
  | GPreWrite !WriteValue
  -- ^ Cost of writing to the database
  | GModuleMember !(ModuleDef (Term Name))
  -- ^ TODO documentation
  | GModuleDecl !ModuleName !Code
  -- ^ Cost of creating a module
  | GUse !ModuleName !(Maybe ModuleHash)
  -- ^ Cost of using a (potentially blessed) module
  | GInterfaceDecl !ModuleName !Code
  -- ^ Cost of creating an interface
  | GUserApp !DefType
  -- ^ Cost of using a function, capability, or defpact
  | GMakeList !Integer
  -- ^ Cost of make-list
  | GFoldDB
  -- ^ Cost of the fold-db call
  | GModuleMemory Bytes
  -- ^ The cost of the in-memory representation of the module

instance Pretty GasArgs where
  pretty g = case g of
    GSelect {} -> "GSelect"
    GSortFieldLookup i -> "GSortFieldLookup:" <> pretty i
    GConcatenation i j -> "GConcatenation:" <> pretty i <> colon <> pretty j
    GUnreduced {} -> "GUnreduced"
    GPostRead rv -> "GPostRead:" <> pretty rv
    GPreWrite wv -> "GWrite:" <> pretty wv
    GModuleMember {} -> "GModuleMember"
    GModuleDecl {} -> "GModuleDecl"
    GUse {} -> "GUse"
    GInterfaceDecl {} -> "GInterfaceDecl"
    GUserApp {} -> "GUserApp"
    GMakeList i -> "GMakeList:" <> pretty i
    GSort i -> "GSort:" <> pretty i
    GDistinct i -> "GDistinct:" <> pretty i
    GFoldDB -> "GFoldDB"
    GModuleMemory i -> "GModuleMemory: " <> pretty i

newtype GasLimit = GasLimit ParsedInteger
  deriving (Eq,Ord,Num,Real,Integral,Enum,Serialize,NFData,Generic,ToTerm,ToJSON,Pretty)

instance Show GasLimit where
  show (GasLimit (ParsedInteger i)) = show i

instance FromJSON GasLimit where
  parseJSON = fmap GasLimit . parseGT0

instance Wrapped GasLimit

data GasModel = GasModel
  { gasModelName :: !Text
  , gasModelDesc :: !Text
  , runGasModel :: !(Text -> GasArgs -> Gas)
  }

instance Show GasModel where
  show m = "[GasModel: " <> unpack (gasModelName m) <> "]"

instance Pretty GasModel where
  pretty m = viaShow m

data GasEnv = GasEnv
  { _geGasLimit :: !GasLimit
  , _geGasPrice :: !GasPrice
  , _geGasModel :: !GasModel
  }
makeLenses ''GasEnv
