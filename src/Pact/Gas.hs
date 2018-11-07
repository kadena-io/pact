{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Pact.Gas where

import Data.Text
import Pact.Types.Runtime
import Control.Lens
import Control.Arrow
import Data.Word

-- | Compute gas for some application or evaluation.
computeGas :: Either (Info,Text) FunApp -> GasArgs -> Eval e Gas
computeGas i args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- use evalGas
  let
    (info,name) = either id (_faInfo &&& _faName) i
    g1 = g0 + runGasModel _geGasModel name args
  evalGas .= g1
  if g1 > fromIntegral _geGasLimit then
    throwErr GasError info $ "Gas limit (" <> tShow _geGasLimit <> ") exceeded: " <> tShow g1
    else return g1


-- | Pre-compute gas for some application before some action.
computeGas' :: FunApp -> GasArgs -> Eval e a -> Eval e (Gas,a)
computeGas' i gs action = computeGas (Right i) gs >>= \g -> (g,) <$> action

-- | Pre-compute gas for some application with unreduced args before some action.
gasUnreduced :: FunApp -> [Term Ref] -> Eval e a -> Eval e (Gas,a)
gasUnreduced i as = computeGas' i (GUnreduced as)

-- | GasEnv for suppressing gas charging.
freeGasEnv :: GasEnv
freeGasEnv = GasEnv 0 0.0 (constGasModel 0)

-- | Gas model that charges a fixed (positive) rate per tracked operation.
constGasModel :: Word64 -> GasModel
constGasModel r = GasModel $ \_ _ -> fromIntegral r
