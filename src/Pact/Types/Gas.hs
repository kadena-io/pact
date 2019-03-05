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
import Data.Decimal (Decimal)
import qualified Data.Text as T
import Data.Word (Word64)

import Pact.Types.Lang
import Pact.Types.Persistence
import Pact.Types.Pretty


-- | Price per 'Gas' unit.
newtype GasPrice = GasPrice Decimal
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,NFData,Enum,Show)
instance Pretty GasPrice where
  pretty (GasPrice p) = viaShow p

-- | DB Read value for per-row gas costing.
-- Data is included if variable-size.
data ReadValue
  = ReadData (Columns Persistable)
  | ReadKey RowKey
  | ReadTxId


data GasArgs
  = GPostRead ReadValue
  | GSelect (Maybe [(Info,ColumnId)]) (Term Ref) (Term Name)
  | GSortFieldLookup Int
  | GUnreduced [Term Ref]
  | GWrite WriteType (Term Name) (Term Name)
  | GUse ModuleName (Maybe Hash)
  | GModuleDecl (Module (Term Name))
  | GInterfaceDecl Interface
  | GModuleMember (ModuleDef (Term Name))
  | GUserApp

newtype GasLimit = GasLimit Word64
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show)
instance Pretty GasLimit where
  pretty (GasLimit g) = viaShow g

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
