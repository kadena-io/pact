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
import qualified Data.Set as S
import Control.Lens (view)

import Data.Aeson as A
import Data.Maybe (fromMaybe)

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger
import Pact.Gas

import Pact.Interpreter

initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface (PactRPC ParsedCode))
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      gasLimit = fromMaybe 0 _ccGasLimit
      gasRate = fromMaybe 0 _ccGasRate
      gasEnv = (GasEnv (fromIntegral gasLimit) 0.0 (constGasModel (fromIntegral gasRate)))
      mkCEI p@PactDbEnv {..} = do
        cmdVar <- newMVar (CommandState initRefStore M.empty)
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> applyCmd logger _ccEntity p cmdVar gasEnv eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccEntity p cmdVar gasEnv }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI


applyCmd :: Logger -> Maybe EntityName -> PactDbEnv p -> MVar CommandState -> GasEnv -> ExecutionMode -> Command a ->
            ProcessedCommand (PactRPC ParsedCode) -> IO CommandResult
applyCmd _ _ _ _ _ ex cmd (ProcFail s) = return $ jsonResult ex (cmdToRequestKey cmd) s
applyCmd logger conf dbv cv gasEnv exMode _ (ProcSucc cmd) = do
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv logger gasEnv) $ runPayload cmd
  case r of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left e -> do
      logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show e
      return $ jsonResult exMode (cmdToRequestKey cmd) $
               CommandError "Command execution failed" (Just $ show e)

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> a -> CommandResult
jsonResult ex cmd a = CommandResult cmd (exToTx ex) (toJSON a)

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload :: Command (Payload (PactRPC ParsedCode)) -> CommandM p CommandResult
runPayload c@Command{..} = do
  let runRpc (Exec pm) = applyExec (cmdToRequestKey c) pm c
      runRpc (Continuation ym) = applyContinuation (cmdToRequestKey c) ym c
      Payload{..} = _cmdPayload
  case _pAddress of
    Just Address{..} -> do
      -- simulate fake blinding if not addressed to this entity or no entity specified
      ent <- view ceEntity
      mode <- view ceMode
      case ent of
        Just entName | entName == _aFrom || (entName `S.member` _aTo) -> runRpc _pPayload
        _ -> return $ jsonResult mode (cmdToRequestKey c) $ CommandError "Private" Nothing
    Nothing -> runRpc _pPayload

applyExec :: RequestKey -> ExecMsg ParsedCode -> Command a -> CommandM p CommandResult
applyExec rk (ExecMsg parsedCode edata) Command{..} = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore pacts) <- liftIO $ readMVar _ceState
  let sigs = userSigsToPactKeySet _cmdSigs
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs edata Nothing _cmdHash) refStore _ceGasEnv
  pr <- liftIO $ evalExec evalEnv parsedCode
  newCmdPact <- join <$> mapM (handlePactExec (erInput pr)) (erExec pr)
  let newPacts = case newCmdPact of
        Nothing -> pacts
        Just cmdPact -> M.insert (_cpTxId cmdPact) cmdPact pacts
  void $ liftIO $ swapMVar _ceState $ CommandState (erRefStore pr) newPacts
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) newCmdPact
  return $ jsonResult _ceMode rk $ CommandSuccess (last (erOutput pr))

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
                        _ceGasEnv
          res <- tryAny (liftIO  $ evalContinuation evalEnv _cpContinuation)

          -- Update pacts state
          case res of
            Left (SomeException ex) -> throwM ex
            Right EvalResult{..} -> do
              exec@PactExec{..} <- maybe (throwCmdEx "No pact execution in continuation exec!")
                                   return erExec         
              if _cmRollback
                then rollbackUpdate env msg state
                else continuationUpdate env msg state pact exec
              return $ jsonResult _ceMode rk $ CommandSuccess (last erOutput)

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
