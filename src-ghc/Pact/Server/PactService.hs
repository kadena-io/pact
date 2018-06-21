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

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger

import Pact.Interpreter

initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface (PactRPC ParsedCode))
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      mkCEI p@PactDbEnv {..} = do
        cmdVar <- newMVar (CommandState initRefStore M.empty)
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> applyCmd logger _ccEntity p cmdVar eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccEntity p cmdVar }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI


applyCmd :: Logger -> Maybe EntityName -> PactDbEnv p -> MVar CommandState -> ExecutionMode -> Command a ->
            ProcessedCommand (PactRPC ParsedCode) -> IO CommandResult
applyCmd _ _ _ _ ex cmd (ProcFail s) = return $ jsonResult ex (cmdToRequestKey cmd) s
applyCmd logger conf dbv cv exMode _ (ProcSucc cmd) = do
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv logger) $ runPayload cmd
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
      runRpc (Continuation ym) = applyContinuation (cmdToRequestKey c) ym _cmdSigs _cmdHash
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
  CommandState {..} <- liftIO $ readMVar _ceState
  let sigs = userSigsToPactKeySet _cmdSigs
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs edata Nothing _cmdHash) _csRefStore
  EvalResult{..} <- liftIO $ evalExec evalEnv parsedCode
  newPact <- join <$> mapM (handleYield erInput) erExec
  let newState = CommandState erRefStore $ case newPact of
        Nothing -> _csPacts
        Just (cp@CommandPact{..}) -> M.insert _cpTxId cp _csPacts
  void $ liftIO $ swapMVar _ceState newState
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) newPact
  return $ jsonResult _ceMode rk $ CommandSuccess (last erOutput)

-- Better name?
handleYield :: [Term Name] -> PactExec -> CommandM p (Maybe CommandPact)
handleYield em PactExec{..} = do
  CommandEnv{..} <- ask
  --TODO: handle entity?
  unless (length em == 1) $
    throwCmdEx $ "handleYield: defpact execution must occur as a single command: " ++ show em
  case _ceMode of
    Local -> return Nothing
    Transactional tid -> do
      --TODO: handle yielded objects
      return $ Just $ CommandPact tid (head em) _peStepCount _peStep


applyContinuation :: RequestKey -> ContMsg -> [UserSig] -> Hash -> CommandM p CommandResult
applyContinuation rk msg@ContMsg{..} cmdSigs cmdHash = do
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
          let sigs = userSigsToPactKeySet cmdSigs
              msgData = case _cmData of
                Nothing -> Null
                Just d -> d
              resume = Nothing -- TODO decode yielded object
              pactStep = Just $ PactStep _cmStep _cmRollback (PactId $ pack $ show $ _cmTxId) resume
              evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                        (MsgData sigs msgData pactStep cmdHash) _csRefStore
          res <- tryAny (liftIO  $ evalContinuation evalEnv _cpContinuation)

          -- Update pacts state
          case res of
            Left (SomeException ex) -> throwM ex
            Right EvalResult{..}    -> do
              PactExec{..} <- maybe (throwCmdEx "No pact execution in continuation exec!")
                                   return erExec         
              if _cmRollback
                then rollbackUpdate env msg state
                else continuationUpdate env msg state pact
              return $ jsonResult _ceMode rk $ CommandSuccess (last erOutput)

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> CommandM p ()
rollbackUpdate CommandEnv{..} ContMsg{..} CommandState{..} = do
  let newState = CommandState _csRefStore $ M.delete _cmTxId _csPacts
  liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: rollbackUpdate: reaping pact "
    ++ show _cmTxId
  void $ liftIO $ swapMVar _ceState newState

continuationUpdate :: CommandEnv p -> ContMsg -> CommandState -> CommandPact -> CommandM p ()
continuationUpdate CommandEnv{..} ContMsg{..} CommandState{..} CommandPact{..} = do
  let nextStep = _cmStep + 1
      isLast = nextStep >= _cpStepCount
      updatePact step = CommandPact _cpTxId _cpContinuation _cpStepCount step
      updateState pacts = CommandState _csRefStore pacts

  if isLast
    then do
      liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: continuationUpdate: reaping pact: "
        ++ show _cmTxId
      void $ liftIO $ swapMVar _ceState $ updateState $ M.delete _cmTxId _csPacts
    else do
      let newPact = updatePact _cmStep
      -- TODO get resume from yield
      liftIO $ logLog _ceLogger "DEBUG" $ "applyContinuation: updated state of pact "
        ++ show _cpTxId ++ ": " ++ show newPact
      void $ liftIO $ swapMVar _ceState $ CommandState _csRefStore $ M.insert _cmTxId newPact _csPacts 
