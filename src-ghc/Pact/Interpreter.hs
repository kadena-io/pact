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
  , EvalInput
  , EvalOutput
  , RunEval
  , BeginTx
  , CommitTx
  , WithRollback
  , Interpreter (..)
  , defaultInterpreter
  , defaultInterpreterState
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (modify)
import Control.Lens

import Data.Aeson
import Data.Default
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text.Encoding (encodeUtf8)
import System.Directory


import Pact.Compile
import Pact.Eval
import Pact.Native (nativeDefs)
import Pact.Native.Capabilities (resolveCapInstallMaybe)
import qualified Pact.Persist.Pure as Pure
import qualified Pact.Persist.SQLite as PSL
import Pact.PersistPactDb
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV

data PactDbEnv e = PactDbEnv {
  pdPactDb :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

data MsgData = MsgData {
  mdData :: !Value,
  mdStep :: !(Maybe PactStep),
  mdHash :: !Hash
  }

initMsgData :: Hash -> MsgData
initMsgData = MsgData Null def

-- | Describes either a ContMsg or ExecMsg.
-- ContMsg is represented as a 'Maybe PactExec'
-- where the PactExec represents a provided SPV continuation,
-- or Nothing for same-chain pacts.
-- ExecMsg is represented as a list of compiled expressions.
type EvalInput = Either (Maybe PactExec) [Term Name]

-- | Captures effects of execution not represented in 'EvalState'.
type EvalOutput = ([Term Name],[TxLog Value],Maybe TxId)

-- | Run some Pact input, get output
type RunEval e = Eval e [Term Name]
-- | Begin a tx and run some Pact Input, get output and txid
type BeginTx e = RunEval e -> Eval e ([Term Name], Maybe TxId)
-- | Given output and txid, commit the tx
type CommitTx e = ([Term Name], Maybe TxId) -> Eval e EvalOutput
-- | Bracket some complete action with tx rollback
type WithRollback e = Eval e EvalOutput -> Eval e EvalOutput

-- | Fully general evaluator for some input.
data Interpreter e = Interpreter
  { interpreter
    :: BeginTx e
    -> CommitTx e
    -> WithRollback e
    -> RunEval e
    -- ^ input runner
    -> Eval e EvalOutput }


-- | Standard runner starts a tx, runs input and commits, with rollback.
defaultInterpreter :: Interpreter e
defaultInterpreter = Interpreter $ \start end withRollback runInput ->
  withRollback $ (start runInput >>= end)

  -- | Standard runner starts a tx, runs input and commits, with rollback.
defaultInterpreterState :: (EvalState -> EvalState) -> Interpreter e
defaultInterpreterState stateF = Interpreter $ \start end withRollback runInput ->
  withRollback $ (start (modify stateF >> runInput) >>= end)

data EvalResult = EvalResult
  { _erInput :: !EvalInput
  , _erOutput :: ![PactValue]
  , _erLogs :: ![TxLog Value]
  , _erExec :: !(Maybe PactExec)
  , _erGas :: Gas
  , _erLoadedModules :: HashMap ModuleName (ModuleData Ref,Bool)
  , _erTxId :: !(Maybe TxId)
  } deriving (Eq,Show)

-- | Execute pact statements.
evalExec :: [Signer] -> Interpreter e -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExec ss runner evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret ss runner evalEnv (Right terms)

-- | For pre-installing modules into state.
initStateModules :: HashMap ModuleName (ModuleData Ref) -> EvalState
initStateModules modules = set (evalRefs . rsLoadedModules) (fmap (,False) modules) def

-- | Resume a defpact execution, with optional PactExec.
evalContinuation :: [Signer] -> Interpreter e -> EvalEnv e -> ContMsg -> IO EvalResult
evalContinuation ss runner ee cm = case (_cmProof cm) of
  Nothing ->
    interpret ss runner (setStep Nothing) (Left Nothing)
  Just p -> do
    etpe <- (_spvVerifyContinuation . _eeSPVSupport $ ee) p
    pe <- throwEither . over _Left (userError . show) $ etpe
    interpret ss runner (setStep (_peYield pe)) (Left $ Just pe)
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
  , _eeMsgSigs = mempty
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


interpret :: [Signer] -> Interpreter e -> EvalEnv e -> EvalInput -> IO EvalResult
interpret ss runner evalEnv terms = do
  ((rs,logs,txid),state) <-
    runEval def evalEnv $ evalTerms runner ss terms
  let gas = _evalGas state
      pactExec = _evalPactExec state
      modules = _rsLoadedModules $ _evalRefs state
  -- output uses lenient conversion
  return $! EvalResult terms (map toPactValueLenient rs) logs pactExec gas modules txid

evalTerms :: Interpreter e -> [Signer] -> EvalInput -> Eval e EvalOutput
evalTerms interp ss input = interpreter interp start end withRollback runInput

  where
    withRollback :: WithRollback e
    withRollback act = handle (\(e :: SomeException) -> safeRollback >> throwM e) act

    safeRollback :: Eval e ()
    safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))

    start :: BeginTx e
    start act = do
      txid <- evalBeginTx def
      sigsAndInstallers <- resolveSignerCaps ss
      -- install sigs into local environment
      local (set eeMsgSigs (toSigs sigsAndInstallers)) $ do
        -- install any caps
        traverse_ (traverse_ (traverse_ $ \i -> i)) sigsAndInstallers
        (,txid) <$> act

    end :: CommitTx e
    end (rs,txid) = do
      logs <- evalCommitTx def
      return (rs,logs,txid)

    toSigs = fmap (S.fromList . M.keys)
    runInput = case input of
      Right ts -> mapM eval ts
      Left pe -> (:[]) <$> resumePact def pe


{-# INLINE evalTerms #-}

-- | Resolves capabilities and returns a datastructure allowing for
-- installing signature caps, and then running installs inside configured environment.
resolveSignerCaps
  :: [Signer]
  -> Eval e (M.Map PublicKey (M.Map Capability (Maybe (Eval e CapAcquireResult))))
resolveSignerCaps ss = M.fromList <$> mapM toPair ss
  where
    toPair Signer{..} = (pk,) . M.fromList <$> mapM resolveCapInstallMaybe _siCapList
      where
        pk = PublicKey $ encodeUtf8 $ fromMaybe _siPubKey _siAddress
