{-# LANGUAGE TupleSections #-}
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
  , initStateModules
  , evalExec
  , evalContinuation
  , setupEvalEnv
  , initRefStore
  , mkSQLiteEnv
  , mkPureEnv
  , mkPactDbEnv
  , initSchema
  , interpret
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Except
import Control.Lens

import Data.Aeson
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.Set as S
import System.Directory


import Pact.Compile
import Pact.Eval
import Pact.Native (nativeDefs)
import qualified Pact.Persist.Pure as Pure
import qualified Pact.Persist.SQLite as PSL
import Pact.PersistPactDb
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.Types.Term

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
  { _erInput :: !(Either (Maybe PactExec) [Term Name])
  , _erOutput :: ![PactValue]
  , _erLogs :: ![TxLog Value]
  , _erExec :: !(Maybe PactExec)
  , _erGas :: Gas
  , _erLoadedModules :: HashMap ModuleName (ModuleData Ref,Bool)
  , _erTxId :: !(Maybe TxId)
  } deriving (Eq,Show)


-- | Execute pact statements.
evalExec :: EvalState -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExec initState evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret initState evalEnv (Right terms)

-- | For pre-installing modules into state.
initStateModules :: HashMap ModuleName (ModuleData Ref) -> EvalState
initStateModules modules = set (evalRefs . rsLoadedModules) (fmap (,False) modules) def

-- | Resume a defpact execution, with optional PactExec.
evalContinuation :: EvalState -> EvalEnv e -> ContMsg -> IO EvalResult
evalContinuation initState ee cm = case (_cmProof cm) of
  Nothing ->
    interpret initState (setStep Nothing) (Left Nothing)
  Just p -> do
    etpe <- (_spvVerifyContinuation . _eeSPVSupport $ ee) p
    pe <- throwEither . over _Left (userError . show) $ etpe
    interpret initState (setStep (_peYield pe)) (Left $ Just pe)
  where
    setStep y = set eePactStep (Just $ PactStep (_cmStep cm) (_cmRollback cm) (_cmPactId cm) y) ee

setupEvalEnv
  :: PactDbEnv e
  -> Maybe EntityName
  -> ExecutionMode
  -> MsgData
  -> RefStore
  -> GasEnv
  -> NamespacePolicy
  -> SPVSupport
  -> PublicData
  -> EvalEnv e
setupEvalEnv dbEnv ent mode msgData refStore gasEnv np spv pd =
  EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mdSigs msgData
  , _eeMsgBody = mdData msgData
  , _eeMode = mode
  , _eeEntity = ent
  , _eePactStep = mdStep msgData
  , _eePactDb = pdPactDb dbEnv
  , _eePactDbVar = pdPactDbVar dbEnv
  , _eePurity = PImpure
  , _eeHash = mdHash msgData
  , _eeGasEnv = gasEnv
  , _eeNamespacePolicy = np
  , _eeSPVSupport = spv
  , _eePublicData = pd
  }

initRefStore :: RefStore
initRefStore = RefStore nativeDefs

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


interpret :: EvalState -> EvalEnv e -> Either (Maybe PactExec) [Term Name] -> IO EvalResult
interpret initState evalEnv terms = do
  ((rs,logs,txid),state) <-
    runEval initState evalEnv $ evalTerms terms
  let gas = _evalGas state
      pactExec = _evalPactExec state
      modules = _rsLoadedModules $ _evalRefs state
  -- output uses lenient conversion
  return $! EvalResult terms (map toPactValueLenient rs) logs pactExec gas modules txid

evalTerms :: Either (Maybe PactExec) [Term Name] -> Eval e ([Term Name],[TxLog Value],Maybe TxId)
evalTerms terms = do
  let safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))
  handle (\(e :: SomeException) -> safeRollback >> throwM e) $ do
        txid <- evalBeginTx def
        rs <- case terms of
          Right ts -> mapM eval ts
          Left pe -> (:[]) <$> resumePact def pe
        logs <- evalCommitTx def
        return (rs,logs,txid)
{-# INLINE evalTerms #-}
