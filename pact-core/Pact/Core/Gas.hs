{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Gas
 ( Gas(..)
 , GasModel(..)
 , GasEnv(..)
 , gmName
 , gmDesc
 , gmModel
 , geGasLimit
 , geGasPrice
 , geGasModel
 ) where

import Control.Lens
import Data.Word(Word64)
import Data.Monoid(Sum(..))
import Data.Text(Text)
import Data.Semiring(Semiring)

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas Word64
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum, Num, Real, Integral) via Word64

  -- deriving (Num, Real, Integral, Enum, Show) via Word64

type GasLimit = Gas
type GasPrice = Gas

data GasModel b
  = GasModel
  { _gmName :: Text
  , _gmDesc :: Text
  , _gmModel :: b -> Gas
  }
makeLenses ''GasModel

data GasEnv b
  = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel b
  }
makeLenses ''GasEnv
