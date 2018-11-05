{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Lens (makeLenses)
import Data.Word (Word64)
import Data.Decimal (Decimal)
import Control.DeepSeq (NFData)

import Pact.Types.Lang
import Pact.Types.Persistence


-- | Price per 'Gas' unit.
newtype GasPrice = GasPrice Decimal
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,NFData,Enum)
instance Show GasPrice where show (GasPrice p) = show p

-- | DB Read value for per-row gas costing.
-- Data is included if variable-size.
data ReadValue
  = ReadData (Columns Persistable)
  | ReadKey RowKey
  | ReadTxId


data GasArgs
  = GPostRead ReadValue
  | GSelect (Maybe [(Info,ColumnId)]) (Term Ref) (Term Name)
  | GUnreduced [Term Ref]
  | GReduced [Term Name]
  | GUse ModuleName (Maybe Hash)
  | GModule Module
  | GInterface Module
  | GModuleMember Module
  | GUser


newtype GasLimit = GasLimit Word64
  deriving (Eq,Ord,Num,Real,Integral,Enum)
instance Show GasLimit where show (GasLimit g) = show g


newtype GasModel = GasModel { runGasModel :: Text -> GasArgs -> Gas }
instance Show GasModel where show _ = "[GasModel]"

data GasEnv = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel
  }
makeLenses ''GasEnv
