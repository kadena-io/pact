{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
-- | "Production" interpreter for Pact, as opposed to the REPL.
-- Mainly, this imposes transaction boundaries around evaluation.
-- However, this is also designed to be stateless for eventual
-- use in FFI.
module Pact.Interpreter where

import Control.Concurrent
import qualified Data.Set as S
import Data.Aeson
import Data.Default
import Control.Monad.Except
import Control.Monad.Catch
import Data.Maybe

import Pact.Types.Runtime
import Pact.Compile
import Pact.Eval
import Pact.Types.RPC
import Pact.Types.Command

data PactDbEnv e = PactDbEnv {
  pdPactDb :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

data MsgData = MsgData {
  mdSigs :: !(S.Set PublicKey),
  mdData :: !Value,
  mdStep :: !(Maybe PactStep)
  }

data EvalResult = EvalResult {
  erTerms :: [Term Name],
  erLogs :: [TxLog],
  erRefStore :: RefStore,
  erYield :: Maybe PactYield
  }


evalExec :: EvalEnv e -> ParsedCode -> IO EvalResult
evalExec evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret evalEnv terms


evalContinuation :: EvalEnv e -> Term Name -> IO EvalResult
evalContinuation ee pact = interpret ee [pact]


setupEvalEnv :: PactDbEnv e -> PactConfig -> ExecutionMode ->
                MsgData -> RefStore -> EvalEnv e
setupEvalEnv dbEnv pactConfig mode msgData refStore =
  EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mdSigs msgData
  , _eeMsgBody = mdData msgData
  , _eeTxId = modeToTx mode
  , _eeEntity = pactEntity pactConfig
  , _eePactStep = mdStep msgData
  , _eePactDb = pdPactDb dbEnv
  , _eePactDbVar = pdPactDbVar dbEnv
  }
  where modeToTx (Transactional t) = Just t
        modeToTx Local = Nothing


interpret :: EvalEnv e -> [Term Name] -> IO EvalResult
interpret evalEnv terms = do
  let tx = _eeTxId evalEnv
  (r,state) <-
    runEval def evalEnv $ evalTerms tx terms
  (rs,logs) <- throwEither r
  let newRefs oldStore | isNothing tx = oldStore
                       | otherwise = updateRefStore (_evalRefs state) oldStore
  return $ EvalResult rs logs (newRefs $ _eeRefStore evalEnv) (_evalYield state)

evalTerms :: Maybe TxId -> [Term Name] -> Eval e ([Term Name],[TxLog])
evalTerms tx terms = do
  let safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))
  handle (\(e :: SomeException) -> safeRollback >> throwM e) $
    flip catchError (\e -> safeRollback >> throwError e) $ do
        evalBeginTx def
        rs <- mapM eval terms
        logs <- case tx of
          Just _ -> evalCommitTx def
          Nothing -> evalRollbackTx def >> return []
        return (rs,logs)
