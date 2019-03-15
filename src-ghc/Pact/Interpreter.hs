{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
-- | "Production" interpreter for Pact, as opposed to the REPL.
--
-- Includes initializers for RefStore and test DB environments.
--
-- Mainly, this imposes transaction boundaries around evaluation.
-- However, this is also designed to be stateless for eventual
-- use in FFI.
--
-- Note that all exceptions are not caught, except insofar as to
-- enforce transaction rollback (and then re-thrown). It is
-- the responsibility of the calling context to catch exceptions.
--
module Pact.Interpreter
  ( PactDbEnv(..)
  , MsgData(..)
  , EvalResult(..)
  , initMsgData
  , evalExec
  , evalExecState
  , evalContinuation
  , evalContinuationState
  , setupEvalEnv
  , initRefStore
  , mkSQLiteEnv
  , mkPureEnv
  , mkPactDbEnv
  , initSchema
  , interpret
  ) where

import Control.Concurrent
import qualified Data.Set as S
import Data.Aeson
import Data.Default
import Control.Monad.Except
import Control.Monad.Catch
import Data.Maybe
import qualified Data.HashMap.Strict as HM

import Pact.Types.Term
import Pact.Types.Runtime
import Pact.Compile
import Pact.Eval hiding (evalContinuation)
import qualified Pact.Eval as Eval (evalContinuation)
import Pact.Types.Command
import Pact.Native (nativeDefs)
import Pact.PersistPactDb
import qualified Pact.Persist.SQLite as PSL
import System.Directory
import Pact.Types.Logger
import qualified Pact.Persist.Pure as Pure

data PactDbEnv e = PactDbEnv {
  pdPactDb :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

data MsgData = MsgData {
  mdSigs :: !(S.Set PublicKey),
  mdData :: !Value,
  mdStep :: !(Maybe PactStep),
  mdHash :: !Hash
  }

initMsgData :: Hash -> MsgData
initMsgData = MsgData def Null def

data EvalResult = EvalResult
  { _erInput :: !(Either PactContinuation [Term Name])
  , _erOutput :: ![Term Name]
  , _erLogs :: ![TxLog Value]
  , _erRefStore :: !RefStore
  , _erExec :: !(Maybe PactExec)
  , _erGas :: Gas
  } deriving (Eq,Show)


evalExec :: EvalEnv e -> ParsedCode -> IO EvalResult
evalExec env pc = evalExecState def env pc

evalExecState :: EvalState -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExecState initState evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret initState evalEnv (Right terms)


evalContinuation :: EvalEnv e -> PactContinuation -> IO EvalResult
evalContinuation ee pact = evalContinuationState def ee pact

evalContinuationState :: EvalState -> EvalEnv e -> PactContinuation -> IO EvalResult
evalContinuationState initState ee pact = interpret initState ee (Left pact)

setupEvalEnv
  :: PactDbEnv e
  -> Maybe EntityName
  -> ExecutionMode
  -> MsgData
  -> RefStore
  -> GasEnv
  -> NamespacePolicy
  -> SPVSupport
  -> EvalEnv e
setupEvalEnv dbEnv ent mode msgData refStore gasEnv np spv =
  EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mdSigs msgData
  , _eeMsgBody = mdData msgData
  , _eeTxId = modeToTx mode
  , _eeEntity = ent
  , _eePactStep = mdStep msgData
  , _eePactDb = pdPactDb dbEnv
  , _eePactDbVar = pdPactDbVar dbEnv
  , _eePurity = PImpure
  , _eeHash = mdHash msgData
  , _eeGasEnv = gasEnv
  , _eeNamespacePolicy = np
  , _eeSPVSupport = spv
  }
  where modeToTx (Transactional t) = Just t
        modeToTx Local = Nothing

initRefStore :: RefStore
initRefStore = RefStore nativeDefs HM.empty

mkSQLiteEnv :: Logger -> Bool -> PSL.SQLiteConfig -> Loggers -> IO (PactDbEnv (DbEnv PSL.SQLite))
mkSQLiteEnv initLog deleteOldFile c loggers = do
  when deleteOldFile $ do
    dbExists <- doesFileExist (PSL._dbFile c)
    when dbExists $ do
      logLog initLog "INIT" "Deleting Existing Pact DB File"
      removeFile (PSL._dbFile c)
  dbe <- initDbEnv loggers PSL.persister <$> PSL.initSQLite c loggers
  mkPactDbEnv pactdb dbe

mkPureEnv :: Loggers -> IO (PactDbEnv (DbEnv Pure.PureDb))
mkPureEnv loggers = mkPactDbEnv pactdb $ initDbEnv loggers Pure.persister Pure.initPureDb

mkPactDbEnv :: PactDb e -> e -> IO (PactDbEnv e)
mkPactDbEnv pdb p = PactDbEnv pdb <$> newMVar p

initSchema :: PactDbEnv (DbEnv p) -> IO ()
initSchema PactDbEnv {..} = createSchema pdPactDbVar


interpret :: EvalState -> EvalEnv e -> Either PactContinuation [Term Name] -> IO EvalResult
interpret initState evalEnv terms = do
  let tx = _eeTxId evalEnv
  ((rs,logs),state) <-
    runEval initState evalEnv $ evalTerms tx terms
  let gas = _evalGas state
      refStore = newRefs . _eeRefStore $ evalEnv
      pactExec = _evalPactExec state
      newRefs oldStore | isNothing tx = oldStore
                       | otherwise = updateRefStore (_evalRefs state) oldStore
  return $! EvalResult terms rs logs refStore pactExec gas

evalTerms :: Maybe TxId -> Either PactContinuation [Term Name] -> Eval e ([Term Name],[TxLog Value])
evalTerms tx terms = do
  let safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))
  handle (\(e :: SomeException) -> safeRollback >> throwM e) $ do
        evalBeginTx def
        rs <- case terms of
          Right ts -> mapM eval ts
          Left pc -> (:[]) <$> Eval.evalContinuation pc
        logs <- case tx of
          Just _ -> evalCommitTx def
          Nothing -> evalRollbackTx def >> return []
        return (rs,logs)
{-# INLINE evalTerms #-}
