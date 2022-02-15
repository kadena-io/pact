{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Pact.Gas
 ( computeGas
 , computeGas'
 , computeGasNonCommit
 , recordGas
 , logGas
 , freeGasEnv
 , gasUnreduced
 , constGasModel )
 where

import Data.Text
import Pact.Types.Runtime
import Control.Lens
import Control.Arrow
import Data.Word
import Pact.Types.Pretty

-- | Compute gas for some application or evaluation.
computeGas :: Either (Info,Text) FunApp -> GasArgs -> Eval e Gas
computeGas i args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- use evalGas
  let
    (info,name) = either id (_faInfo &&& _faName) i
    g1 = runGasModel _geGasModel name args
  logGas name args g1
  let gUsed = g0 + g1
  recordGas gUsed
  if gUsed > fromIntegral _geGasLimit then
    throwErr GasError info $ "Gas limit (" <> pretty _geGasLimit <> ") exceeded: " <> pretty gUsed
    else return gUsed
{-# INLINABLE computeGas #-}

-- | Gas calculation for incremental computations, that does not emit a gas log,
--   nor records it, but does check it against the gas limit.
computeGasNonCommit :: Info -> Text -> GasArgs -> Eval e Gas
computeGasNonCommit info name args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- use evalGas
  let !gUsed = g0 + runGasModel _geGasModel name args
  if gUsed > fromIntegral _geGasLimit then
    throwErr GasError info $ "Gas limit (" <> pretty _geGasLimit <> ") exceeded: " <> pretty gUsed
    else return gUsed

logGas :: Text -> GasArgs -> Gas -> Eval e ()
logGas name args gas =
  evalLogGas %= fmap ((renderCompactText' (pretty name <> ":" <> pretty args),gas):)
{-# INLINABLE logGas #-}

recordGas :: Gas -> Eval e ()
recordGas = (evalGas .=)
{-# INLINABLE recordGas #-}

-- | Pre-compute gas for some application before some action.
computeGas' :: Gas -> FunApp -> GasArgs -> Eval e a -> Eval e (Gas,a)
computeGas' g0 i gs action = computeGas (Right i) gs >>= \g -> (g0 + g,) <$> action

-- | Pre-compute gas for some application with unreduced args before some action.
gasUnreduced :: FunApp -> [Term Ref] -> Eval e a -> Eval e (Gas,a)
gasUnreduced i as = computeGas' 0 i (GUnreduced as)

-- | GasEnv for suppressing gas charging.
freeGasEnv :: GasEnv
freeGasEnv = GasEnv 0 0.0 (constGasModel 0)

-- | Gas model that charges a fixed (positive) rate per tracked operation.
constGasModel :: Word64 -> GasModel
constGasModel r = GasModel
  { gasModelName = "fixed " <> tShow r
  , gasModelDesc = "constant rate gas model with fixed rate " <> tShow r
  , runGasModel = \_ _ -> fromIntegral r }
