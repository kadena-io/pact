{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Server.PactService
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Service to provide Pact interpreter and backend.
--

module Pact.Server.PactService where

import Prelude

import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Data.Default

import Pact.Gas
import Pact.Interpreter
import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Pretty (viaShow)
import Pact.Types.PactValue (PactValue(..))


initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface PublicMeta ParsedCode [TxLog Value])
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      gasRate = fromMaybe 0 _ccGasRate
      gasModel = constGasModel (fromIntegral gasRate)
      blockHeight = 0
      blockTime = 0

  let mkCEI p@PactDbEnv {..} = do
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd ->
              applyCmd logger _ccEntity p gasModel
                blockHeight blockTime eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccEntity p gasModel blockHeight blockTime }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI


applyCmd :: Logger ->
            Maybe EntityName ->
            PactDbEnv p ->
            GasModel ->
            Word64 ->
            Int64 ->
            ExecutionMode ->
            Command a ->
            ProcessedCommand PublicMeta ParsedCode ->
            IO (CommandResult [TxLog Value])
applyCmd _ _ _ _ _ _ _ cmd (ProcFail s) = return $ resultFailure
                                          (cmdToRequestKey cmd)
                                          (PactError TxFailure def def . viaShow $ s)
applyCmd logger conf dbv gasModel bh bt exMode _ (ProcSucc cmd) = do
  let pubMeta = _pMeta $ _cmdPayload cmd
      (ParsedDecimal gasPrice) = _pmGasPrice pubMeta
      gasEnv = GasEnv (fromIntegral $ _pmGasLimit pubMeta) (GasPrice gasPrice) gasModel

  let pd = PublicData pubMeta bh bt

  r <- catchesPactError $ runCommand (CommandEnv conf exMode dbv logger gasEnv pd) $ runPayload cmd
  case r of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left e -> do
      logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show e
      return $ resultFailure (cmdToRequestKey cmd) e


resultFailure :: RequestKey -> PactError -> CommandResult [TxLog Value]
resultFailure rk pe =
  CommandResult rk Nothing (PactResult . Left $ pe) (Gas 0) Nothing Nothing Nothing


resultSuccess :: RequestKey ->
                 Maybe TxId ->
                 PactValue ->
                 Gas ->
                 [TxLog Value] ->
                 Maybe PactExec ->
                 CommandResult [TxLog Value]
resultSuccess rk tx pv g l pe =
  CommandResult rk tx (PactResult . Right $ pv) g (Just l) pe Nothing


runPayload :: Command (Payload PublicMeta ParsedCode) -> CommandM p (CommandResult [TxLog Value])
runPayload c@Command{..} = case (_pPayload _cmdPayload) of
  Exec pm -> applyExec (cmdToRequestKey c) _cmdHash (_pSigners _cmdPayload) pm
  Continuation ym -> applyContinuation (cmdToRequestKey c) _cmdHash (_pSigners _cmdPayload) ym


applyExec :: RequestKey -> PactHash -> [Signer] -> ExecMsg ParsedCode -> CommandM p (CommandResult [TxLog Value])
applyExec rk hsh signers (ExecMsg parsedCode edata) = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  let sigs = userSigsToPactKeySet signers
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode (MsgData sigs edata Nothing (toUntypedHash hsh))
                initRefStore _ceGasEnv permissiveNamespacePolicy noSPVSupport _cePublicData
  EvalResult{..} <- liftIO $ evalExec def evalEnv parsedCode
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) _erExec
  return $ resultSuccess rk _erTxId (last _erOutput) _erGas _erLogs _erExec


applyContinuation :: RequestKey -> PactHash -> [Signer] -> ContMsg -> CommandM p (CommandResult [TxLog Value])
applyContinuation rk hsh signers ContMsg{..} = do
  CommandEnv{..} <- ask
  -- Setup environment and get result
  let sigs = userSigsToPactKeySet signers
      pactStep = Just $ PactStep _cmStep _cmRollback _cmPactId Nothing
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs _cmData pactStep (toUntypedHash hsh)) initRefStore
                _ceGasEnv permissiveNamespacePolicy noSPVSupport _cePublicData
  EvalResult{..} <- liftIO $ evalContinuation def evalEnv Nothing
  return $ resultSuccess rk _erTxId (last _erOutput) _erGas _erLogs _erExec
