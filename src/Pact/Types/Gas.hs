{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Gas
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Gas (compute and space cost calculation) types.
--
module Pact.Types.Gas
  ( Gas(..),GasPrice(..),
    GasEnv(..),geGasLimit,geGasPrice,geGasModel,
    ReadValue(..),GasModel(..),GasArgs(..),GasLimit(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Decimal (Decimal)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics
import Data.Serialize

import Pact.Types.Lang
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.PactValue
import Pact.Parse


-- | Price per 'Gas' unit.
newtype GasPrice = GasPrice Decimal
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,NFData,Enum,Show,Serialize,Generic,ToTerm)
instance Pretty GasPrice where
  pretty (GasPrice p) = viaShow p

instance ToJSON GasPrice where
  toJSON (GasPrice d) = toJSON (ParsedDecimal d)
instance FromJSON GasPrice where
  parseJSON v = parseJSON v >>= \(ParsedDecimal d) -> return (GasPrice d)

-- | DB Read value for per-row gas costing.
-- Data is included if variable-size.
data ReadValue
  = ReadData (ObjectMap PactValue)
  | ReadKey RowKey
  | ReadTxId


data GasArgs
  = GPostRead ReadValue
  | GSelect (Maybe [(Info,FieldKey)]) (Term Ref) (Term Name)
  | GSortFieldLookup Int
  | GUnreduced [Term Ref]
  | GWrite WriteType (Term Name) (Term Name)
  | GUse ModuleName (Maybe Hash)
  | GModuleDecl (Module (Term Name))
  | GInterfaceDecl Interface
  | GModuleMember (ModuleDef (Term Name))
  | GUserApp

newtype GasLimit = GasLimit Word64
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Serialize,NFData,Generic,ToTerm)
instance Pretty GasLimit where
  pretty (GasLimit g) = viaShow g

instance ToJSON GasLimit where
  toJSON (GasLimit d) = toJSON (ParsedInteger (fromIntegral d))
instance FromJSON GasLimit where
  parseJSON v = parseJSON v >>= \(ParsedInteger d) -> return (GasLimit (fromIntegral d))

data GasModel = GasModel
  { gasModelName :: Text
  , gasModelDesc :: Text
  , runGasModel :: Text -> GasArgs -> Gas
  }

instance Show GasModel where
  show m = "[GasModel: " <> T.unpack (gasModelName m) <> "]"

instance Pretty GasModel where
  pretty m = viaShow m

data GasEnv = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel
  }
makeLenses ''GasEnv
