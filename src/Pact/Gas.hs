{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Pact.Gas
 ( computeGas
 , computeGas'
 , computeGasNonCommit
 , computeGasCommit
 , freeGasEnv
 , gasUnreduced
 , constGasModel
 , getGas
 , putGas)
 where

import Data.Text
import Data.IORef
import Pact.Types.Gas
import Pact.Types.Runtime
import Control.Lens
import Control.Arrow
import Data.Word
import Pact.Types.Pretty

-- | Compute gas for some application or evaluation.
computeGas :: Either (Info,Text) FunApp -> GasArgs -> Eval e MicroGas
computeGas i args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let
    (info,name) = either id (_faInfo &&& _faName) i
    g1 = runGasModel _geGasModel name args
  let gUsed = g0 <> g1
      gUsed' = microGasToGas gUsed
  evalLogGas %= fmap ((renderCompactText' (pretty name <> ":" <> pretty args <> ":currTotalGas=" <> pretty gUsed'),microGasToGas g1):)
  putGas gUsed
  let (MicroGasLimit gasLimit) = _geGasLimit
  if gUsed > gasLimit then
    throwErr GasError info $ "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty gUsed
    else return gUsed
{-# INLINABLE computeGas #-}

-- | Performs gas calculation for incremental computations with some caveats:
--   - Checks the gas calculations against the gas limit
--   - Does not emit gas logs
--   - Commit gas calc depending on `commit`
computeGasNoLog :: (MicroGas -> Eval e ()) -> Info -> Text -> GasArgs -> Eval e MicroGas
computeGasNoLog commit info name args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let !gUsed = g0 <> runGasModel _geGasModel name args
  commit gUsed
  let (MicroGasLimit gl) = _geGasLimit
  if gUsed > gl then
    throwErr GasError info $ "Gas limit (" <> pretty gl <> ") exceeded: " <> pretty gUsed
    else return gUsed

putGas :: MicroGas -> Eval e ()
putGas !g = do
  gasRef <- view eeGas
  liftIO (writeIORef gasRef g)

getGas :: Eval e MicroGas
getGas = view eeGas >>= liftIO . readIORef

-- | See: ComputeGasNoLog, does not commit gas calculation.
computeGasNonCommit :: Info -> Text -> GasArgs -> Eval e MicroGas
computeGasNonCommit = computeGasNoLog (const (pure ()))

-- | See: ComputeGasNoLog, save currently used `evalGas`
computeGasCommit :: Info -> Text -> GasArgs -> Eval e MicroGas
computeGasCommit info name args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let !g1 = runGasModel _geGasModel name args
      !gUsed = g0 <> g1
      gUsed' = microGasToGas gUsed
  evalLogGas %= fmap ((renderCompactText' (pretty name <> ":" <> pretty args <> ":currTotalGas=" <> pretty gUsed'),microGasToGas g1):)
  putGas gUsed
  let (MicroGasLimit gl) = _geGasLimit
  if gUsed > gl then
    throwErr GasError info $ "Gas limit (" <> pretty gl <> ") exceeded: " <> pretty gUsed
    else return gUsed


-- | Pre-compute gas for some application before some action.
computeGas' :: Gas -> FunApp -> GasArgs -> Eval e a -> Eval e (MicroGas,a)
computeGas' g0 i gs action = computeGas (Right i) gs >>= \g -> (gasToMicroGas g0 <> g,) <$> action

-- | Pre-compute gas for some application with unreduced args before some action.
gasUnreduced :: FunApp -> [Term Ref] -> Eval e a -> Eval e (MicroGas,a)
gasUnreduced i as = computeGas' 0 i (GUnreduced as)

-- | GasEnv for suppressing gas charging.
freeGasEnv :: GasEnv
freeGasEnv = GasEnv (MicroGasLimit mempty) 0.0 (constGasModel 0)

-- | Gas model that charges a fixed (positive) rate per tracked operation.
constGasModel :: Word64 -> GasModel
constGasModel r = GasModel
  { gasModelName = "fixed " <> tShow r
  , gasModelDesc = "constant rate gas model with fixed rate " <> tShow r
  , runGasModel = \_ _ -> MicroGas (fromIntegral r) }
