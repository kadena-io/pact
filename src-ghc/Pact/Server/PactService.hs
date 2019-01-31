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

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M

import Data.Aeson as A
import Data.Maybe (fromMaybe)

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger
import Pact.Gas
import Pact.Parse (ParsedDecimal(..))

import Pact.Interpreter

initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface PublicMeta ParsedCode)
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      gasRate = fromMaybe 0 _ccGasRate
      gasModel = constGasModel (fromIntegral gasRate)
      mkCEI p@PactDbEnv {..} = do
        cmdVar <- newMVar (CommandState initRefStore M.empty)
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> applyCmd logger _ccEntity p cmdVar gasModel eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccEntity p cmdVar gasModel }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI


applyCmd :: Logger -> Maybe EntityName -> PactDbEnv p -> MVar CommandState ->
            GasModel -> ExecutionMode -> Command a ->
            ProcessedCommand PublicMeta ParsedCode -> IO CommandResult
applyCmd _ _ _ _ _ ex cmd (ProcFail s) = return $ jsonResult ex (cmdToRequestKey cmd) (Gas 0) s
applyCmd logger conf dbv cv gasModel exMode _ (ProcSucc cmd) = do
  let pubMeta = _pMeta $ _cmdPayload cmd
      (ParsedDecimal gasPrice) = _pmGasPrice pubMeta
      gasEnv = GasEnv (fromIntegral $ _pmGasLimit pubMeta) (GasPrice gasPrice) gasModel
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv logger gasEnv) $ runPayload cmd
  case r of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left e -> do
      logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show e
      let
        cValue :: CommandValue (Term Name)
        cValue = CommandFailure $
          CommandError "Command execution failed" (Just $ show e)
      return $ jsonResult exMode (cmdToRequestKey cmd) (Gas 0) cValue

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> Gas -> a -> CommandResult
jsonResult ex cmd gas a = CommandResult cmd (exToTx ex) (toJSON a) gas

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload :: Command (Payload PublicMeta ParsedCode) -> CommandM p CommandResult
runPayload c@Command{..} = case (_pPayload _cmdPayload) of
  Exec pm -> applyExec (cmdToRequestKey c) pm c
  Continuation ym -> applyContinuation (cmdToRequestKey c) ym c


applyExec :: RequestKey -> ExecMsg ParsedCode -> Command a -> CommandM p CommandResult
applyExec rk (ExecMsg parsedCode edata) Command{..} = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore pacts) <- liftIO $ readMVar _ceState
  let sigs = userSigsToPactKeySet _cmdSigs
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs edata Nothing _cmdHash) refStore _ceGasEnv permissiveNamespacePolicy
  EvalResult{..} <- liftIO $ evalExec evalEnv parsedCode
  newCmdPact <- join <$> mapM (handlePactExec _erInput) _erExec
  let newPacts = case newCmdPact of
        Nothing -> pacts
        Just cmdPact -> M.insert (_cpTxId cmdPact) cmdPact pacts
  void $ liftIO $ swapMVar _ceState $ CommandState _erRefStore newPacts
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) newCmdPact
  return $ jsonResult _ceMode rk _erGas $ CommandSuccess (last _erOutput)

handlePactExec :: [Term Name] -> PactExec -> CommandM p (Maybe CommandPact)
handlePactExec em PactExec{..} = do
  CommandEnv{..} <- ask
  unless (length em == 1) $
    throwCmdEx $ "handlePactExec: defpact execution must occur as a single command: " ++ show em
  case _ceMode of
    Local -> return Nothing
    Transactional tid -> do
      return $ Just $ CommandPact tid (head em) _peStepCount _peStep _peYield


applyContinuation :: RequestKey -> ContMsg -> Command a -> CommandM p CommandResult
applyContinuation rk msg@ContMsg{..} Command{..} = do
  env@CommandEnv{..} <- ask
  case _ceMode of
    Local -> throwCmdEx "Local continuation exec not supported"
    Transactional _ -> do
      state@CommandState{..} <- liftIO $ readMVar _ceState
      case M.lookup _cmTxId _csPacts of
        Nothing -> throwCmdEx $ "applyContinuation: txid not found: " ++ show _cmTxId
        Just pact@CommandPact{..} -> do
          -- Verify valid ContMsg Step
          when (_cmStep < 0 || _cmStep >= _cpStepCount) $ throwCmdEx $ "Invalid step value: " ++ show _cmStep
          if _cmRollback
            then when (_cmStep /= _cpStep) $ throwCmdEx $ "Invalid rollback step value: Received "
                 ++ show _cmStep ++ " but expected " ++ show _cpStep
            else when (_cmStep /= (_cpStep + 1)) $ throwCmdEx $ "Invalid continuation step value: Received "
                 ++ show _cmStep ++ " but expected " ++ show (_cpStep + 1)

          -- Setup environement and get result
          let sigs = userSigsToPactKeySet _cmdSigs
              pactStep = Just $ PactStep _cmStep _cmRollback (PactId $ pack $ show _cmTxId) _cpYield
              evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                        (MsgData sigs _cmData pactStep _cmdHash) _csRefStore
                        _ceGasEnv permissiveNamespacePolicy
          res <- tryAny (liftIO  $ evalContinuation evalEnv _cpContinuation)

          -- Update pacts state
          case res of
            Left (SomeException ex) -> throwM ex
            Right EvalResult{..} -> do
              exec@PactExec{..} <- maybe (throwCmdEx "No pact execution in continuation exec!")
                                   return _erExec
              if _cmRollback
                then rollbackUpdate env msg state
                else continuationUpdate env msg state pact exec
              return $ jsonResult _ceMode rk _erGas $ CommandSuccess (last _erOutput)

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> CommandM p ()
rollbackUpdate CommandEnv{..} ContMsg{..} CommandState{..} = do
  -- if step doesn't have a rollback function, no error thrown. Therefore, pact will be deleted
  -- from state.
  let newState = CommandState _csRefStore $ M.delete _cmTxId _csPacts
  liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: rollbackUpdate: reaping pact "
    ++ show _cmTxId
  void $ liftIO $ swapMVar _ceState newState

continuationUpdate :: CommandEnv p -> ContMsg -> CommandState -> CommandPact -> PactExec -> CommandM p ()
continuationUpdate CommandEnv{..} ContMsg{..} CommandState{..} CommandPact{..} PactExec{..} = do
  let nextStep = _cmStep + 1
      isLast = nextStep >= _cpStepCount
      updateState pacts = CommandState _csRefStore pacts -- never loading modules during continuations

  if isLast
    then do
      liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: continuationUpdate: reaping pact: "
        ++ show _cmTxId
      void $ liftIO $ swapMVar _ceState $ updateState $ M.delete _cmTxId _csPacts
    else do
      let newPact = CommandPact _cpTxId _cpContinuation _cpStepCount _cmStep _peYield
      liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: updated state of pact "
        ++ show _cmTxId ++ ": " ++ show newPact
      void $ liftIO $ swapMVar _ceState $ updateState $ M.insert _cmTxId newPact _csPacts
