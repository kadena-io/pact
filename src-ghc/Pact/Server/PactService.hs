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

import Control.Monad.Except
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Data.Default
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy  as BSL
import Data.Text (Text)

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Pretty (viaShow)
import Pact.Types.PactValue (PactValue)
import Pact.Types.SPV


initPactService
  :: CommandConfig
  -> Loggers
  -> SPVSupport
  -> IO (CommandExecInterface PublicMeta ParsedCode Hash)
initPactService CommandConfig {..} loggers spv = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      gasRate = fromMaybe 0 _ccGasRate
      gasModel = constGasModel (fromIntegral gasRate)
      blockHeight = 0
      blockTime = 0
      blockHash = ""

  let mkCEI p@PactDbEnv {..} = do
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd ->
              applyCmd logger _ccEntity p gasModel
                blockHeight blockTime blockHash spv eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccEntity p gasModel
                             blockHeight blockTime blockHash spv }
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
            Text ->
            SPVSupport ->
            ExecutionMode ->
            Command a ->
            ProcessedCommand PublicMeta ParsedCode ->
            IO (CommandResult Hash)
applyCmd _ _ _ _ _ _ _ _ _ cmd (ProcFail s) =
  return $ resultFailure
           Nothing
           (cmdToRequestKey cmd)
           (PactError TxFailure def def . viaShow $ s)
applyCmd logger conf dbv gasModel bhe bt bh spv exMode _ (ProcSucc cmd) = do
  let pubMeta = _pMeta $ _cmdPayload cmd
      gasEnv = GasEnv (_pmGasLimit pubMeta) (_pmGasPrice pubMeta) gasModel
      pd = PublicData pubMeta bhe bt bh

  res <- catchesPactError $ runCommand
                            (CommandEnv conf exMode dbv logger gasEnv pd spv)
                            (runPayload cmd)
  case res of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left pactErr -> do
      logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show pactErr
      return $ resultFailure Nothing (cmdToRequestKey cmd) pactErr


resultFailure :: Maybe TxId ->
                 RequestKey ->
                 PactError ->
                 CommandResult Hash
resultFailure tx cmd a = CommandResult cmd tx (PactResult . Left $ a) (Gas 0) Nothing Nothing Nothing

resultSuccess :: Maybe TxId ->
                 RequestKey ->
                 Gas ->
                 PactValue ->
                 Maybe PactExec ->
                 [TxLog Value] ->
                 CommandResult Hash
resultSuccess tx cmd gas a pe l = CommandResult cmd tx (PactResult . Right $ a) gas (Just hshLog) pe Nothing
  where hshLog = fullToHashLogCr l

fullToHashLogCr :: [TxLog Value] -> Hash
fullToHashLogCr full = (pactHash . BSL.toStrict . encode) full


runPayload :: Command (Payload PublicMeta ParsedCode) -> CommandM p (CommandResult Hash)
runPayload c@Command{..} = case (_pPayload _cmdPayload) of
  Exec pm -> applyExec (cmdToRequestKey c) _cmdHash (_pSigners _cmdPayload) pm
  Continuation ym -> applyContinuation (cmdToRequestKey c) _cmdHash (_pSigners _cmdPayload) ym


applyExec :: RequestKey -> PactHash -> [Signer] -> ExecMsg ParsedCode -> CommandM p (CommandResult Hash)
applyExec rk hsh signers (ExecMsg parsedCode edata) = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  let sigs = userSigsToPactKeySet signers
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode (MsgData sigs edata Nothing (toUntypedHash hsh))
                initRefStore _ceGasEnv permissiveNamespacePolicy _ceSPVSupport _cePublicData
  EvalResult{..} <- liftIO $ evalExec def evalEnv parsedCode
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) _erExec
  return $ resultSuccess _erTxId rk _erGas (last _erOutput) _erExec _erLogs


applyContinuation :: RequestKey -> PactHash -> [Signer] -> ContMsg -> CommandM p (CommandResult Hash)
applyContinuation rk hsh signers cm = do
  CommandEnv{..} <- ask
  -- Setup environment and get result
  let sigs = userSigsToPactKeySet signers
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs (_cmData cm) Nothing (toUntypedHash hsh)) initRefStore
                _ceGasEnv permissiveNamespacePolicy _ceSPVSupport _cePublicData
  EvalResult{..} <- liftIO $ evalContinuation def evalEnv cm
  return $ resultSuccess _erTxId rk _erGas (last _erOutput) _erExec _erLogs
