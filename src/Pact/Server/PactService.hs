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

module Pact.Server.PactService
  (
    -- * Initialization
    initPactService
    -- * Commands 
    , applyCmd
    , applyExec
    , applyContinuation    
    -- * Serialization of result.
    , jsonResult
    -- * 'ExecutionMode' to 'Transactional'
    , exToTx
    , runPayload
  )
where

import Prelude
import Control.Concurrent.STM(newTVarIO, readTVar, writeTVar, TVar, atomically)
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Set as S
import Control.Lens

import Data.Aeson as A

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger
import Pact.Interpreter

-- | Init pact service based on 'CommandConfig'.
initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface (PactRPC ParsedCode))
initPactService config loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      mkCEI p = do
        cmdVar <- newTVarIO (CommandState initRefStore)
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> 
                              applyCmd 
                                logger 
                                (config^.ccEntity) 
                                p cmdVar eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger (config^.ccEntity) p cmdVar }
  case (config^.ccSqlite) of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI

-- | Apply a command for an 'EntityName' using a DB. 
-- | Synchronization alert. Clients will need to 
-- | handle asynchronous exceptions to cleanup resources. 
-- | For example, see here, for an example of canonical form
-- | of calling this method.
applyCmd :: Logger -> Maybe EntityName -> PactDbEnv p -> TVar CommandState -> ExecutionMode -> Command a ->
            ProcessedCommand (PactRPC ParsedCode) -> IO CommandResult
applyCmd _ _ _ _ ex cmd (ProcFail s) = return $ jsonResult ex (cmdToRequestKey cmd) s
applyCmd logger conf dbv cv exMode _ (ProcSucc cmd) = do
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv) $ runPayload cmd
  case r of
    Right cr -> do
      logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left e -> do
      logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show e
      return $ jsonResult exMode (cmdToRequestKey cmd) $
               CommandError "Command execution failed" (Just $ show e)

-- | Converts an 'ExecutionMode' to a 'RequestKey'
jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> a -> CommandResult
jsonResult ex cmd a = CommandResult cmd (exToTx ex) (toJSON a)

-- | Execution Mode to Tranasaction
exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

{-| Run the payload command 'Command'. 
-}
runPayload :: Command (Payload (PactRPC ParsedCode)) -> CommandM p CommandResult
runPayload c@Command{..} = do
  let runRpc (Exec pm) = applyExec (cmdToRequestKey c) pm c
      runRpc (Continuation ym) = applyContinuation ym _cmdSigs
      Payload{..} = _cmdPayload
  case _pAddress of
    Just Address{..} -> do
      -- simulate fake blinding if not addressed to this entity or no entity specified
      ent <- view ceEntity
      mode <- view ceMode
      case ent of
        Just entName | entName == (_aFrom) || (entName `S.member` (_aTo)) -> 
          runRpc _pPayload
        _ -> 
          return $ jsonResult mode (cmdToRequestKey c) $ CommandError "Private" Nothing
    Nothing -> runRpc _pPayload


-- | Synchronize the read of the CommandState and evaluate the 
-- | expression setting up the current environment and return, 
-- | with some additional book-keeping.
applyExec :: RequestKey -> ExecMsg ParsedCode -> Command a -> CommandM p CommandResult
applyExec rk (ExecMsg parsedCode edata) Command{..} = do
  env <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore) <- liftIO $ atomically $ readTVar $ env^.ceState
  let evalEnv = setupEvalEnv (env^.ceDbEnv) (env^.ceEntity) (env^.ceMode)
                (MsgData 
                    (userSigsToPactKeySet (_cmdSigs)) 
                    edata Nothing 
                    (_cmdHash)) refStore
  pr <- liftIO $ evalExec evalEnv parsedCode
  void $ liftIO $ atomically $ writeTVar (env^.ceState) $ CommandState (erRefStore pr)
  return $ jsonResult (env^.ceMode) rk $ CommandSuccess (last (erOutput pr))

-- | Unsupported function, raises an error. 
applyContinuation :: ContMsg -> [UserSig] -> CommandM p CommandResult
applyContinuation _ _ = throwCmdEx "Continuation not supported"
