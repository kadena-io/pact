{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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

import Data.Aeson as A
import Data.Maybe
import System.Directory
import Data.ByteString (ByteString)

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Crypto
import Pact.Types.Logger

import Pact.Parse (parseExprs)
import Pact.PersistPactDb
import Pact.Native (initEvalEnv)
import Pact.Interpreter

import qualified Pact.Persist.SQLite as PSL
import qualified Pact.Persist.Pure as Pure

mkSQLiteEnv :: Logger -> Bool -> PSL.SQLiteConfig -> InitDbEnv PSL.SQLite
mkSQLiteEnv initLog deleteOldFile c loggers = do
  when deleteOldFile $ do
    dbExists <- doesFileExist (PSL.dbFile c)
    when dbExists $ do
      logLog initLog "INIT" "Deleting Existing Pact DB File"
      removeFile (PSL.dbFile c)
  initDbEnv loggers PSL.persister <$> PSL.initSQLite c loggers

mkPureEnv :: InitDbEnv Pure.PureDb
mkPureEnv loggers = return $ initDbEnv loggers Pure.persister Pure.initPureDb


initPactService :: CommandConfig -> Loggers -> IO (CommandExecInterface (PactRPC ParsedCode))
initPactService CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
      klog s = logLog logger "INIT" s
      mkCEI :: DbEnv a -> IO (CommandExecInterface (PactRPC ParsedCode))
      mkCEI p = do
        ee <- initEvalEnv p pactdb
        cmdVar <- newMVar (CommandState $ _eeRefStore ee)
        let dbVar = _eePactDbVar ee
        klog "Creating Pact Schema"
        createSchema dbVar
        return CommandExecInterface
          { _ceiApplyCmd = \eMode cmd -> applyCmd logger _ccPact dbVar cmdVar eMode cmd (verifyCommand cmd)
          , _ceiApplyPPCmd = applyCmd logger _ccPact dbVar cmdVar }
  case _ccSqlite of
    Nothing -> do
      klog "Initializing pure pact"
      mkPureEnv loggers >>= mkCEI
    Just sqlc -> do
      klog "Initializing pact SQLLite"
      mkSQLiteEnv logger True sqlc loggers >>= mkCEI




verifyCommand :: Command ByteString -> ProcessedCommand (PactRPC ParsedCode)
verifyCommand orig@PublicCommand{..} = case (ppcmdPayload', ppcmdHash', mSigIssue) of
      (Right env', Right _, Nothing) -> ProcSucc $ orig { _cmdPayload = env' }
      (e, h, s) -> ProcFail $ "Invalid command: " ++ toErrStr e ++ toErrStr h ++ fromMaybe "" s
  where
    ppcmdPayload' = traverse (traverse parsePact) =<< A.eitherDecodeStrict' _cmdPayload
    parsePact :: Text -> Either String ParsedCode
    parsePact code = ParsedCode code <$> parseExprs code
    (ppcmdSigs' :: [(UserSig,Bool)]) = (\u -> (u,verifyUserSig _cmdHash u)) <$> _cmdSigs
    ppcmdHash' = verifyHash _cmdHash _cmdPayload
    mSigIssue = if all snd ppcmdSigs' then Nothing
      else Just $ "Invalid sig(s) found: " ++ show ((A.encode . fst) <$> filter (not.snd) ppcmdSigs')
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}




applyCmd :: Logger -> PactConfig -> MVar (DbEnv p) -> MVar CommandState -> ExecutionMode -> Command a ->
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

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> a -> CommandResult
jsonResult ex cmd a = CommandResult cmd (exToTx ex) (toJSON a)

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload :: Command (Payload (PactRPC ParsedCode)) -> CommandM p CommandResult
runPayload c@PublicCommand{..} =
  case _pPayload _cmdPayload of
    (Exec pm) -> applyExec (cmdToRequestKey c) pm _cmdSigs
    (Continuation ym) -> applyContinuation ym _cmdSigs


applyExec :: RequestKey -> ExecMsg ParsedCode -> [UserSig] -> CommandM p CommandResult
applyExec rk (ExecMsg parsedCode edata) ks = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore) <- liftIO $ readMVar _ceState
  let evalEnv = setupEvalEnv (PactDbEnv pactdb _ceDBVar) _ceConfig _ceMode
                (MsgData (userSigsToPactKeySet ks) edata Nothing) refStore
  pr <- liftIO $ evalExec evalEnv parsedCode
  void $ liftIO $ swapMVar _ceState $ CommandState (erRefStore pr)
  return $ jsonResult _ceMode rk $ CommandSuccess (last (erTerms pr))

applyContinuation :: ContMsg -> [UserSig] -> CommandM p CommandResult
applyContinuation _ _ = throwCmdEx "Continuation not supported"
