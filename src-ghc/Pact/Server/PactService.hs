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
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv) $ runPayload cmd
  case r of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      -- FOR TESTING
      s <- liftIO $ readMVar cv
      let pactState = (_csPacts s)
      logLog logger "DEBUG" $ "testing that commandstate saved: " ++ show pactState
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
  newPact <- join <$> mapM (handleYield erInput _cmdSigs) erExec
  let newState = CommandState erRefStore $ case newPact of
        Nothing -> _csPacts
        Just (cp@CommandPact{..}) -> M.insert _cpTxId cp _csPacts
  void $ liftIO $ swapMVar _ceState newState
  return $ jsonResult _ceMode rk $ CommandSuccess (last erOutput)

-- Better name?
handleYield :: [Term Name] -> [UserSig] -> PactExec -> CommandM p (Maybe CommandPact)
handleYield em cmdSigs PactExec{..} = do
  CommandEnv{..} <- ask
  --TODO: handle entity?
  unless (length em == 1) $
    throwCmdEx $ "handleYield: defpact execution must occur as a single command: " ++ show em
  case _ceMode of
    Local -> return Nothing
    Transactional tid -> do
      --TODO: handle yielded objects
      let sigs = userSigsToPactKeySet cmdSigs
      return $ Just $ CommandPact tid (head em) sigs _peStepCount _peStep


applyContinuation :: RequestKey -> ContMsg -> [UserSig] -> Hash -> CommandM p CommandResult
applyContinuation rk msg@ContMsg{..} cmdSigs cmdHash = do
  CommandEnv{..} <- ask
  case _ceMode of
    Local -> throwCmdEx "Local continuation exec not supported"
    Transactional _ -> do
      CommandState{..} <- liftIO $ readMVar _ceState
      case M.lookup _cmTxId _csPacts of
        Nothing -> throwCmdEx $ "applyContinuation: txid not found: " ++ show _cmTxId
        Just pact@CommandPact{..} -> do
          let hasErr = if _cmRollback
                       then checkRollbackStepErr msg _cpStep _cpStepCount
                       else checkNextStepErr msg (_cpStep + 1) _cpStepCount
          maybe (return ()) throwCmdEx hasErr
          
          let sigs = userSigsToPactKeySet cmdSigs
              msgData = Null -- TODO data from msg
              resume = Nothing -- TODO decode yielded object
              setupStep step = Just $ PactStep step _cmRollback (PactId $ pack $ show $ _cmTxId) resume
              pactStep = setupStep (if _cmRollback then _cpStep else (_cpStep + 1))
              evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode (MsgData sigs msgData pactStep cmdHash) _csRefStore
          res <- tryAny (liftIO  $ evalContinuation evalEnv _cpContinuation)
          case res of
            Left (SomeException ex)   -> throwM ex
            Right EvalResult{..} -> do
              PactExec{..} <- maybe (throwCmdEx "No pact execution in continuation exec!")
                                   return erExec -- TODO better err msg?
              let newPactState = updatePactState _cmRollback _peStep pact _csPacts
                  newState = CommandState erRefStore newPactState

              void $ liftIO $ swapMVar _ceState newState
              return $ jsonResult _ceMode rk $ CommandSuccess (last erOutput)


checkNextStepErr :: ContMsg -> Int -> Int -> Maybe String
checkNextStepErr ContMsg{..} nextStep stepCount
  | nextStep >= stepCount     = Just $ "applyContinuation: pact continuation [disabled]: "
                                ++ show _cmTxId  -- is last step
  | (_cmStep < 0 ||
     _cmStep >= stepCount)    = Just $ "Invalid step value: " ++ show _cmStep
  | _cmStep /= nextStep       = Just $ "Invalid continuation step value: Received " ++ show _cmStep
                                ++ " but expected " ++ show nextStep
  | otherwise                 = Nothing

checkRollbackStepErr :: ContMsg -> Int -> Int -> Maybe String
checkRollbackStepErr ContMsg{..} currStep stepCount
  | (_cmStep < 0 ||
     _cmStep >= stepCount)    = Just $ "Invalid step value: " ++ show _cmStep
  | _cmStep /= currStep       = Just $ "Invalid rollback step value: Received " ++ show _cmStep
                                ++ " but expected " ++ show currStep
  | otherwise                 = Nothing


updatePactState :: Bool -> Int -> CommandPact -> M.Map TxId CommandPact -> M.Map TxId CommandPact
updatePactState isRollback currStep CommandPact{..} oldPacts  = do
  let updatePact step = CommandPact _cpTxId _cpContinuation _cpSigs _cpStepCount step
      prevStep = currStep - 1
      done = prevStep < 0
      -- TODO encode yielded object

  case (isRollback, done) of
    (True, True)  -> M.delete _cpTxId oldPacts
    (True, False) -> M.insert _cpTxId (updatePact prevStep) oldPacts
    (False, _)    -> M.insert _cpTxId (updatePact currStep) oldPacts
