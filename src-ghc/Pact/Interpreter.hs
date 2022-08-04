{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , Interpreter (..)
  , defaultInterpreter
  , defaultInterpreterState
  , ExecutionConfig (..)
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State (modify)
import Control.Lens

import Data.Aeson
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.Directory

import Pact.Compile
import Pact.Eval
import Pact.Native (nativeDefs)
import qualified Pact.Persist.Pure as Pure
import qualified Pact.Persist.SQLite as PSL
import Pact.PersistPactDb
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV

-- | 'PactDb'-related environment
data PactDbEnv e = PactDbEnv {
  pdPactDb :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

-- | Transaction-payload related environment data.
data MsgData = MsgData {
  mdData :: !Value,
  mdStep :: !(Maybe PactStep),
  mdHash :: !Hash,
  mdSigners :: [Signer]
  }


initMsgData :: Hash -> MsgData
initMsgData h = MsgData Null def h def

-- | Describes either a ContMsg or ExecMsg.
-- ContMsg is represented as a 'Maybe PactExec'
-- where the PactExec represents a provided SPV continuation,
-- or Nothing for same-chain pacts.
-- ExecMsg is represented as a list of compiled expressions.
type EvalInput = Either (Maybe PactExec) [Term Name]

-- | Captures results of execution
type EvalOutput = ([Term Name],[TxLog Value],Maybe TxId)

-- | Interpreter indirection for executing user action.
newtype Interpreter e = Interpreter
  { interpreter :: Eval e [Term Name] -> Eval e [Term Name] }

-- | Default interpreter performs no indirection.
defaultInterpreter :: Interpreter e
defaultInterpreter = Interpreter id

-- | 'defaultInterpreter' that modifies state before execution.
defaultInterpreterState :: (EvalState -> EvalState) -> Interpreter e
defaultInterpreterState stateF = Interpreter $ \runInput ->
  modify stateF >> runInput

-- | Results of evaluation.
data EvalResult = EvalResult
  { _erInput :: !EvalInput
    -- ^ compiled user input
  , _erOutput :: ![PactValue]
    -- ^ Output values
  , _erLogs :: ![TxLog Value]
    -- ^ Transaction logs
  , _erExec :: !(Maybe PactExec)
    -- ^ Result of defpact execution if any
  , _erGas :: Gas
    -- ^ Gas consumed/charged
  , _erLoadedModules :: HashMap ModuleName (ModuleData Ref,Bool)
    -- ^ Modules loaded, with flag indicating "newly loaded"
  , _erTxId :: !(Maybe TxId)
    -- ^ Transaction id, if executed transactionally
  , _erLogGas :: Maybe [(Text, Gas)]
    -- ^ Details on each gas consumed/charged
  , _erEvents :: [PactEvent]
    -- ^ emitted events
  } deriving (Eq,Show)

-- | Execute pact statements.
evalExec :: Interpreter e -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExec runner evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret runner evalEnv (Right terms)

-- | For pre-installing modules into state.
initStateModules :: HashMap ModuleName (ModuleData Ref) -> EvalState
initStateModules modules =
  set (evalRefs . rsQualifiedDeps) (foldMap allModuleExports modules) $ set (evalRefs . rsLoadedModules) (fmap (,False) modules) def

-- | Resume a defpact execution, with optional PactExec.
evalContinuation :: Interpreter e -> EvalEnv e -> ContMsg -> IO EvalResult
evalContinuation runner ee cm = case (_cmProof cm) of
  Nothing ->
    interpret runner (setStep Nothing) (Left Nothing)
  Just p -> do
    etpe <- (_spvVerifyContinuation . _eeSPVSupport $ ee) p
    pe <- throwEither . over _Left (userError . show) $ etpe
    interpret runner (setStep (_peYield pe)) (Left $ Just pe)
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
  -> ExecutionConfig
  -> EvalEnv e
setupEvalEnv dbEnv ent mode msgData refStore gasEnv np spv pd ec =
  EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mkMsgSigs $ mdSigners msgData
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
  , _eeExecutionConfig = ec
  , _eeAdvice = def
  , _eeInRepl = False
  }
  where
    mkMsgSigs ss = M.fromList $ map toPair ss
      where
        toPair Signer{..} = (pk,S.fromList _siCapList)
          where
            pk = PublicKey $ encodeUtf8 $ fromMaybe _siPubKey _siAddress


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


interpret :: Interpreter e -> EvalEnv e -> EvalInput -> IO EvalResult
interpret runner evalEnv terms = do
  ((rs,logs,txid),state) <-
    runEval def evalEnv $ evalTerms runner terms
  let gas = _evalGas state
      gasLogs = _evalLogGas state
      pactExec = _evalPactExec state
      modules = _rsLoadedModules $ _evalRefs state
  -- output uses lenient conversion
  return $! EvalResult
    terms
    (map (elideModRefInfo . toPactValueLenient) rs)
    logs pactExec gas modules txid gasLogs (_evalEvents state)

evalTerms :: Interpreter e -> EvalInput -> Eval e EvalOutput
evalTerms interp input = withRollback (start (interpreter interp runInput) >>= end)

  where

    withRollback act = handle (\(e :: SomeException) -> safeRollback >> throwM e) act

    safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))

    start act = do
      txid <- evalBeginTx def
      (,txid) <$> act

    end (rs,txid) = do
      logs <- evalCommitTx def
      return (rs,logs,txid)

    runInput = case input of
      Right ts -> mapM eval ts
      Left pe -> (:[]) <$> resumePact def pe


{-# INLINE evalTerms #-}
