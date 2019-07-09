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

import Pact.Types.Info
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.PactValue
import Pact.Types.Term
import Pact.Parse


parseGT0 :: (FromJSON a,Num a,Ord a) => Value -> Parser a
parseGT0 v = parseJSON v >>= \a ->
  if a >= 0 then return a else
    fail "value must be greater than 0"
{-# INLINABLE parseGT0 #-}

-- | API Price value, basically a newtype over `Decimal`
newtype GasPrice = GasPrice ParsedDecimal
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,NFData,Serialize,Generic,ToTerm,ToJSON)
instance Show GasPrice where
  show (GasPrice (ParsedDecimal d)) = show d

instance Pretty GasPrice where
  pretty (GasPrice p) = viaShow p

instance FromJSON GasPrice where
  parseJSON = fmap GasPrice . parseGT0

instance Wrapped GasPrice

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
  | GUse ModuleName (Maybe ModuleHash)
  | GModuleDecl (Module (Term Name))
  | GInterfaceDecl Interface
  | GModuleMember (ModuleDef (Term Name))
  | GUserApp

newtype GasLimit = GasLimit ParsedInteger
  deriving (Eq,Ord,Num,Real,Integral,Enum,Serialize,NFData,Generic,ToTerm,ToJSON)

instance Show GasLimit where
  show (GasLimit (ParsedInteger i)) = show i

instance Pretty GasLimit where
  pretty (GasLimit g) = viaShow g

instance FromJSON GasLimit where
  parseJSON = fmap GasLimit . parseGT0

instance Wrapped GasLimit

data GasModel = GasModel
  { gasModelName :: Text
  , gasModelDesc :: Text
  , runGasModel :: Text -> GasArgs -> Gas
  }

instance Show GasModel where
  show m = "[GasModel: " <> unpack (gasModelName m) <> "]"

instance Pretty GasModel where
  pretty m = viaShow m

data GasEnv = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel
  }
makeLenses ''GasEnv
