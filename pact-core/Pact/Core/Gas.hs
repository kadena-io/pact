{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Gas
 ( Gas(..)
 , GasModel(..)
 , GasEnv(..)
 , NodeType(..)
 , gmName
 , gmDesc
 , gmNatives
 , gmNodes
 , geGasLimit
 , geGasPrice
 , geGasModel
 , constantGasModel
 , freeGasModel
 , freeGasEnv
 ) where

import Control.Lens
import Data.Word(Word64)
import Data.Monoid(Sum(..))
import Data.Text(Text)
import qualified Data.Text as T
import Data.Semiring(Semiring)

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas Word64
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum, Num, Real, Integral) via Word64

type GasLimit = Gas
type GasPrice = Rational

data NodeType
  = VarNode
  | LamNode
  | AppNode
  | SeqNode
  | BuiltinNode
  | ConstantNode
  | ListNode
  deriving (Eq, Show)

nodeGas :: NodeType -> Gas
nodeGas = \case
  VarNode -> 1
  LamNode -> 1
  AppNode -> 1
  SeqNode -> 1
  BuiltinNode -> 1
  ConstantNode -> 1
  ListNode -> 1


data GasModel b
  = GasModel
  { _gmName :: Text
  , _gmDesc :: Text
  , _gmNatives :: b -> Gas
  , _gmNodes :: NodeType -> Gas
  }
makeLenses ''GasModel


data GasEnv b
  = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel b
  }
makeLenses ''GasEnv

constantGasModel :: Gas -> GasModel b
constantGasModel unitPrice =
  GasModel
  { _gmName = "unitGasModel"
  , _gmDesc = "GasModel with constant cost " <> T.pack (show unitPrice)
  , _gmNatives = const unitPrice
  , _gmNodes = if unitPrice > 0 then nodeGas else const 0
  }

freeGasModel :: GasModel b
freeGasModel = constantGasModel 0

freeGasEnv :: GasEnv b
freeGasEnv =
  GasEnv 1 1 freeGasModel
