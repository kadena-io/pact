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
module Pact.Interpreter where

import Control.Concurrent
import qualified Data.Set as S
import Data.Aeson
import Data.Default
import Control.Monad.Except
import Control.Monad.Catch
import Data.Maybe
import qualified Data.HashMap.Strict as HM

import Pact.Types.Runtime
import Pact.Compile
import Pact.Eval
import Pact.Types.RPC
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
  mdStep :: !(Maybe PactStep)
  }
instance Default MsgData where def = MsgData def Null def

data EvalResult = EvalResult {
  erTerms :: ![Term Name],
  erLogs :: ![TxLog],
  erRefStore :: !RefStore,
  erYield :: !(Maybe PactYield)
  } deriving (Eq,Show)


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

initRefStore :: RefStore
initRefStore = RefStore nativeDefs HM.empty

mkSQLiteEnv :: Logger -> Bool -> PSL.SQLiteConfig -> Loggers -> IO (PactDbEnv (DbEnv PSL.SQLite))
mkSQLiteEnv initLog deleteOldFile c loggers = do
  when deleteOldFile $ do
    dbExists <- doesFileExist (PSL.dbFile c)
    when dbExists $ do
      logLog initLog "INIT" "Deleting Existing Pact DB File"
      removeFile (PSL.dbFile c)
  dbe <- initDbEnv loggers PSL.persister <$> PSL.initSQLite c loggers
  mkPactDbEnv pactdb dbe

mkPureEnv :: Loggers -> IO (PactDbEnv (DbEnv Pure.PureDb))
mkPureEnv loggers = mkPactDbEnv pactdb $ initDbEnv loggers Pure.persister Pure.initPureDb

mkPactDbEnv :: PactDb e -> e -> IO (PactDbEnv e)
mkPactDbEnv pdb p = PactDbEnv pdb <$> newMVar p

initSchema :: PactDbEnv (DbEnv p) -> IO ()
initSchema PactDbEnv {..} = createSchema pdPactDbVar


interpret :: EvalEnv e -> [Term Name] -> IO EvalResult
interpret evalEnv terms = do
  let tx = _eeTxId evalEnv
  ((rs,logs),state) <-
    runEval def evalEnv $ evalTerms tx terms
  let newRefs oldStore | isNothing tx = oldStore
                       | otherwise = updateRefStore (_evalRefs state) oldStore
  return $! EvalResult rs logs (newRefs $ _eeRefStore evalEnv) (_evalYield state)

evalTerms :: Maybe TxId -> [Term Name] -> Eval e ([Term Name],[TxLog])
evalTerms tx terms = do
  let safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))
  handle (\(e :: SomeException) -> safeRollback >> throwM e) $ do
        evalBeginTx def
        rs <- mapM eval terms
        logs <- case tx of
          Just _ -> evalCommitTx def
          Nothing -> evalRollbackTx def >> return []
        return (rs,logs)
{-# INLINE evalTerms #-}
