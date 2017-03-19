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
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Reader

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Aeson as A
import qualified Data.Attoparsec.Text as AP
import Data.Maybe
import System.Directory
import Data.ByteString (ByteString)

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Crypto

import Pact.Compile as Pact
import Pact.PersistPactDb
import Pact.Eval
import Pact.Native (initEvalEnv)

import qualified Pact.Persist.SQLite as PSL
import qualified Pact.Persist.Pure as Pure


initPactService :: CommandConfig -> IO (CommandExecInterface (PactRPC ParsedCode))
initPactService config@CommandConfig {..} = do
  let klog s = _ccDebugFn ("[PactService] " ++ s)
      mkCEI :: MVar (DbEnv a) -> MVar CommandState -> CommandExecInterface (PactRPC ParsedCode)
      mkCEI dbVar cmdVar = CommandExecInterface
        { _ceiApplyCmd = \eMode cmd -> applyCmd config dbVar cmdVar eMode cmd (verifyCommand cmd)
        , _ceiApplyPPCmd = applyCmd config dbVar cmdVar }
  case _ccDbFile of
    Nothing -> do
      klog "Initializing pure pact"
      ee <- initEvalEnv (initDbEnv _ccDebugFn Pure.persister Pure.initPureDb) pactdb
      rv <- newMVar (CommandState $ _eeRefStore ee)
      klog "Creating Pact Schema"
      createSchema (_eePactDbVar ee)
      return $ mkCEI (_eePactDbVar ee) rv
    Just f -> do
      klog "Initializing pact SQLLite"
      dbExists <- doesFileExist f
      when dbExists $ klog "Deleting Existing Pact DB File" >> removeFile f
      p <- initDbEnv _ccDebugFn PSL.persister <$> PSL.initSQLite _ccPragmas (\s -> _ccDebugFn $ "[Pact SQLite] " ++ s) f
      ee <- initEvalEnv p pactdb
      rv <- newMVar (CommandState $ _eeRefStore ee)
      let v = _eePactDbVar ee
      klog "Creating Pact Schema"
      createSchema v
      return $ mkCEI v rv



verifyCommand :: Command ByteString -> ProcessedCommand (PactRPC ParsedCode)
verifyCommand orig@PublicCommand{..} = case (ppcmdPayload', ppcmdHash', mSigIssue) of
      (Right env', Right _, Nothing) -> ProcSucc $ orig { _cmdPayload = env' }
      (e, h, s) -> ProcFail $ "Invalid command: " ++ toErrStr e ++ toErrStr h ++ fromMaybe "" s
  where
    ppcmdPayload' = traverse (traverse parsePact) =<< A.eitherDecodeStrict' _cmdPayload
    parsePact :: Text -> Either String ParsedCode
    parsePact code = ParsedCode code <$> AP.parseOnly Pact.exprs code
    (ppcmdSigs' :: [(UserSig,Bool)]) = (\u -> (u,verifyUserSig _cmdHash u)) <$> _cmdSigs
    ppcmdHash' = verifyHash _cmdHash _cmdPayload
    mSigIssue = if all snd ppcmdSigs' then Nothing
      else Just $ "Invalid sig(s) found: " ++ show ((A.encode . fst) <$> filter (not.snd) ppcmdSigs')
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}




applyCmd :: CommandConfig -> MVar (DbEnv p) -> MVar CommandState -> ExecutionMode -> Command a ->
            ProcessedCommand (PactRPC ParsedCode) -> IO CommandResult
applyCmd _ _ _ ex cmd (ProcFail s) = return $ jsonResult ex (cmdToRequestKey cmd) s
applyCmd conf@CommandConfig {..} dbv cv exMode _ (ProcSucc cmd) = do
  r <- tryAny $ runCommand (CommandEnv conf exMode dbv cv) $ runPayload cmd
  case r of
    Right cr -> do
      _ccDebugFn $ "[PactService]: success for requestKey: " ++ show (cmdToRequestKey cmd)
      return cr
    Left e -> do
      _ccDebugFn $ "[PactService]: tx failure for requestKey: " ++ show (cmdToRequestKey cmd) ++ ": " ++ show e
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
applyExec rk (ExecMsg (ParsedCode code exps) edata) ks = do
  CommandEnv {..} <- ask
  when (null exps) $ throwCmdEx "No expressions found"
  terms <- forM exps $ \exp -> case compile (mkTextInfo code) exp of
            Right r -> return r
            Left err -> throwCmdEx $ show err
  (CommandState refStore) <- liftIO $ readMVar _ceState
  let tid = exToTx _ceMode
      evalEnv :: PactDb e -> MVar e -> EvalEnv e
      evalEnv pdb mv = EvalEnv {
                  _eeRefStore = refStore
                , _eeMsgSigs = userSigsToPactKeySet ks
                , _eeMsgBody = edata
                , _eeTxId = tid
                , _eeEntity = _ccEntity _ceConfig
                , _eePactStep = Nothing
                , _eePactDb = pdb
                , _eePactDbVar = mv
                }
  (r,rEvalState') <- liftIO $ runEval def (evalEnv pactdb _ceDBVar) (execTerms _ceMode terms)
  case r of
    Right t -> do
           when (_ceMode /= Local) $ liftIO $ modifyMVar_ _ceState $ \rs ->
             return $ over (csRefStore.rsModules)
                      (HM.union (HM.fromList (_rsNew (_evalRefs rEvalState')))) rs
           return $ jsonResult _ceMode rk $ CommandSuccess t -- TODO Yield handling
    Left e -> throwCmdEx $ "Exec failed: " ++ show e

execTerms :: ExecutionMode -> [Term Name] -> Eval e (Term Name)
execTerms mode terms = do
  evalBeginTx def
  er <- catchError
        (last <$> mapM eval terms)
        (\e -> evalRollbackTx def >> throwError e)
  case mode of
    Transactional _ -> void $ evalCommitTx def
    Local -> evalRollbackTx def
  return er

applyContinuation :: ContMsg -> [UserSig] -> CommandM p CommandResult
applyContinuation _ _ = throwCmdEx "Continuation not supported"
