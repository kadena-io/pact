{-# LANGUAGE TupleSections #-}
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

import Prelude hiding (log,exp)

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Set as S
import Control.Lens (view)

import Data.Aeson as A

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger

import Pact.Interpreter


initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface (Envelope (PactRPC ParsedCode)))
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      mkCEI p@PactDbEnv {..} = do
        cmdVar <- newMVar (CommandState initRefStore)
        klog "Creating Pact Schema"
        initSchema p
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> applyCmd logger _ccPact p cmdVar eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccPact p cmdVar }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI




applyCmd :: Logger -> PactConfig -> PactDbEnv p -> MVar CommandState -> ExecutionMode -> Command a ->
            ProcessedCommand (Envelope (PactRPC ParsedCode)) -> IO CommandResult
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

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> a -> CommandResult
jsonResult ex cmd a = CommandResult cmd (exToTx ex) (toJSON a)

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload :: Command (Payload (Envelope (PactRPC ParsedCode))) -> CommandM p CommandResult
runPayload c@Command{..} = case _pPayload _cmdPayload of
  PublicEnvelope rpc -> runRpc rpc
  PrivateEnvelope from to rpc -> do
    entName <- view (ceConfig . pactEntity)
    mode <- view ceMode
    if entName == from || (entName `S.member` to)
      then runRpc rpc
      else return $ jsonResult mode (cmdToRequestKey c) $ CommandError "Private" Nothing
  where
    runRpc (Exec pm) = applyExec (cmdToRequestKey c) pm _cmdSigs
    runRpc (Continuation ym) = applyContinuation ym _cmdSigs


applyExec :: RequestKey -> ExecMsg ParsedCode -> [UserSig] -> CommandM p CommandResult
applyExec rk (ExecMsg parsedCode edata) ks = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore) <- liftIO $ readMVar _ceState
  let evalEnv = setupEvalEnv _ceDbEnv _ceConfig _ceMode
                (MsgData (userSigsToPactKeySet ks) edata Nothing) refStore
  pr <- liftIO $ evalExec evalEnv parsedCode
  void $ liftIO $ swapMVar _ceState $ CommandState (erRefStore pr)
  return $ jsonResult _ceMode rk $ CommandSuccess (last (erTerms pr))

applyContinuation :: ContMsg -> [UserSig] -> CommandM p CommandResult
applyContinuation _ _ = throwCmdEx "Continuation not supported"
