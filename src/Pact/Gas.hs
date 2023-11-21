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

import Control.Monad(when)
import Control.Monad.State.Strict
import Data.Text
import Data.IORef
import Pact.Types.Gas
import Pact.Types.Runtime
import Control.Lens
import Control.Arrow
import Data.Word
import Pact.Types.Pretty

-- | Compute gas for some application or evaluation.
computeGas :: Either (Info,Text) FunApp -> GasArgs -> Eval e ()
computeGas i args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let
    (info,name) = either id (_faInfo &&& _faName) i
    g1 = runGasModel _geGasModel name args
  let gMillisUsed = g0 <> g1
      gUsed = milliGasToGas gMillisUsed
  evalLogGas %= fmap ((gasLogMsg name args gUsed, milliGasToGas g1):)
  putGas gMillisUsed
  let (MilliGasLimit gasLimit) = _geGasLimit
  when (gMillisUsed > gasLimit) $ do
    let oldGasLimit = milliGasToGas gasLimit
    throwErr GasError info $ "Gas limit (" <> pretty oldGasLimit <> ") exceeded: " <> pretty gUsed
    -- else return gUsed
{-# INLINABLE computeGas #-}

gasLogMsg
  :: Text
  -> GasArgs
  -> Gas
  -> Text
gasLogMsg name args used =
  renderCompactText' (pretty name <> ":" <> pretty args <> ":currTotalGas=" <> pretty used)

-- | Performs gas calculation for incremental computations with some caveats:
--   - Checks the gas calculations against the gas limit
--   - Does not emit gas logs
--   - Commit gas calc depending on `commit`
computeGasNoLog :: (MilliGas -> Eval e ()) -> Info -> Text -> GasArgs -> Eval e MilliGas
computeGasNoLog commit info name args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let !gMillisUsed = g0 <> runGasModel _geGasModel name args
  commit gMillisUsed
  let (MilliGasLimit gasLimit) = _geGasLimit
  if gMillisUsed > gasLimit then do
    let oldGasLimit = milliGasToGas gasLimit
        gUsed = milliGasToGas gMillisUsed
    throwErr GasError info $ "Gas limit (" <> pretty oldGasLimit <> ") exceeded: " <> pretty gUsed
    else return gMillisUsed

putGas :: MilliGas -> Eval e ()
putGas !g = do
  gasRef <- view eeGas
  liftIO (writeIORef gasRef g)

getGas :: Eval e MilliGas
getGas = view eeGas >>= liftIO . readIORef

-- | See: ComputeGasNoLog, does not commit gas calculation.
computeGasNonCommit :: Info -> Text -> GasArgs -> Eval e MilliGas
computeGasNonCommit = computeGasNoLog (const (pure ()))

-- | See: ComputeGasNoLog, save currently used `evalGas`
computeGasCommit :: Info -> Text -> GasArgs -> Eval e ()
computeGasCommit info name args = do
  GasEnv {..} <- view eeGasEnv
  g0 <- getGas
  let !g1 = runGasModel _geGasModel name args
      !gMillisUsed = g0 <> g1
      gUsed = milliGasToGas gMillisUsed
  evalLogGas %= fmap ((gasLogMsg name args gUsed,milliGasToGas g1):)
  putGas gMillisUsed
  let (MilliGasLimit milliGasLimit) = _geGasLimit
  when (gMillisUsed > milliGasLimit) $ do
    let gasLimit = milliGasToGas milliGasLimit
    throwErr GasError info $ "Gas limit (" <> pretty gasLimit <> ") exceeded: " <> pretty gUsed
    -- else return gUsed


-- | Pre-compute gas for some application before some action.
computeGas' :: FunApp -> GasArgs -> Eval e a -> Eval e a
computeGas' i gs action = computeGas (Right i) gs *> action

-- | Pre-compute gas for some application with unreduced args before some action.
gasUnreduced :: FunApp -> [Term Ref] -> Eval e a -> Eval e a
gasUnreduced i as = computeGas' i (GUnreduced as)

-- | GasEnv for suppressing gas charging.
freeGasEnv :: GasEnv
freeGasEnv = GasEnv (MilliGasLimit mempty) 0.0 (constGasModel 0)

-- | Gas model that charges a fixed (positive) rate per tracked operation.
constGasModel :: Word64 -> GasModel
constGasModel r = GasModel
  { gasModelName = "fixed " <> tShow r
  , gasModelType = ConstantGasModel (fromIntegral r)
  , gasModelDesc = "constant rate gas model with fixed rate " <> tShow r
  , runGasModel = \_ _ -> gasToMilliGas (fromIntegral r) }
