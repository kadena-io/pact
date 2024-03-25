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
  , versionedNativesRefStore
  , ExecutionConfig (..)
  , pact40Natives
  , pact42Natives
  , pact43Natives
  , pact431Natives
  , pact46Natives
  , pact47Natives
  ) where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.State (modify)
import Control.Lens

import Data.Aeson
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.Monoid(Endo(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Foldable(foldl')
import Data.IORef
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import System.Directory

import Pact.Compile
import Pact.Eval
import Pact.Native (nativeDefs)
import qualified Pact.Persist.Pure as Pure
import qualified Pact.Persist.SQLite as PSL
import Pact.PersistPactDb
import Pact.Types.Command
import Pact.Types.ExpParser
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.Types.Verifier

import Pact.JSON.Legacy.Value

-- | 'PactDb'-related environment
data PactDbEnv e = PactDbEnv {
  pdPactDb :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

-- | Transaction-payload related environment data.
data MsgData = MsgData {
  mdData :: !LegacyValue,
  mdStep :: !(Maybe PactStep),
  mdHash :: !Hash,
  mdSigners :: [Signer],
  mdVerifiers :: [Verifier ()]
  }


initMsgData :: Hash -> MsgData
initMsgData h = MsgData (toLegacyJson Null) def h def def

-- | Describes either a ContMsg or ExecMsg.
-- ContMsg is represented as a 'Maybe PactExec'
-- where the PactExec represents a provided SPV continuation,
-- or Nothing for same-chain pacts.
-- ExecMsg is represented as a list of compiled expressions.
type EvalInput = Either (Maybe PactExec) [Term Name]

-- | Captures results of execution
type EvalOutput = ([Term Name],[TxLogJson],Maybe TxId)

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
  , _erLogs :: ![TxLogJson]
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
  , _erWarnings :: S.Set PactWarning
    -- ^ emitted warning
  } deriving (Eq,Show)

-- | Execute pact statements.
evalExec :: Interpreter e -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExec runner evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (ParseEnv isNarrowTry) (mkTextInfo _pcCode) _pcExps
  interpret runner evalEnv (Right terms)
  where
    isNarrowTry = not $ S.member FlagDisablePact44 $ _ecFlags $ _eeExecutionConfig evalEnv

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
    pe <- either contError return etpe
    interpret runner (setStep (_peYield pe)) (Left $ Just pe)
  where
    contError spvErr =
      if S.member FlagDisablePact47 (_ecFlags $ _eeExecutionConfig ee)
      then throw $ userError (show spvErr)
      else throw $ PactError ContinuationError def def (pretty spvErr)
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
  -> IO (EvalEnv e)
setupEvalEnv dbEnv ent mode msgData refStore gasEnv np spv pd ec = do
  gasRef <- newIORef mempty
  warnRef <- newIORef mempty
  pure EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mkMsgSigs $ mdSigners msgData
  , _eeMsgVerifiers = mkMsgVerifiers $ mdVerifiers msgData
  , _eeMsgBody = mdData msgData
  , _eeMode = mode
  , _eeEntity = ent
  , _eePactStep = mdStep msgData
  , _eePactDb = pdPactDb dbEnv
  , _eePactDbVar = pdPactDbVar dbEnv
  , _eePurity = PImpure
  , _eeHash = mdHash msgData
  , _eeGasEnv = gasEnv
  , _eeGas = gasRef
  , _eeNamespacePolicy = np
  , _eeSPVSupport = spv
  , _eePublicData = pd
  , _eeExecutionConfig = ec
  , _eeAdvice = def
  , _eeInRepl = False
  , _eeWarnings = warnRef
  }
  where
    mkMsgSigs ss = M.fromList $ map toPair ss
      where
        toPair Signer{..} = (pk,S.fromList _siCapList)
          where
            pk = PublicKeyText $ fromMaybe _siPubKey _siAddress
    mkMsgVerifiers vs = M.fromListWith S.union $ map toPair vs
      where
        toPair Verifier{..} = (_verifierName, S.fromList _verifierCaps)


disablePactNatives :: [Text] -> ExecutionFlag -> ExecutionConfig -> Endo RefStore
disablePactNatives bannedNatives flag (ExecutionConfig ec) = Endo $
  if S.member flag ec then over rsNatives (\k -> foldl' (flip HM.delete) k bannedNatives)
  else id

disablePact40Natives :: ExecutionConfig -> Endo RefStore
disablePact40Natives =
  disablePactNatives pact40Natives FlagDisablePact40

disablePact42Natives :: ExecutionConfig -> Endo RefStore
disablePact42Natives = disablePactNatives pact42Natives FlagDisablePact42

disablePact43Natives :: ExecutionConfig -> Endo RefStore
disablePact43Natives = disablePactNatives pact43Natives FlagDisablePact43

disablePact431Natives :: ExecutionConfig -> Endo RefStore
disablePact431Natives = disablePactNatives pact431Natives FlagDisablePact431

disablePact46Natives :: ExecutionConfig -> Endo RefStore
disablePact46Natives = disablePactNatives pact46Natives FlagDisablePact46

disablePact47Natives :: ExecutionConfig -> Endo RefStore
disablePact47Natives = disablePactNatives pact47Natives FlagDisablePact47

disablePact410Natives :: ExecutionConfig -> Endo RefStore
disablePact410Natives = disablePactNatives pact410Natives FlagDisablePact410

disablePact411Natives :: ExecutionConfig -> Endo RefStore
disablePact411Natives = disablePactNatives pact411Natives FlagDisablePact411

disablePact412Natives :: ExecutionConfig -> Endo RefStore
disablePact412Natives = disablePactNatives pact412Natives FlagDisablePact412

pact40Natives :: [Text]
pact40Natives = ["enumerate" , "distinct" , "emit-event" , "concat" , "str-to-list"]

pact42Natives :: [Text]
pact42Natives = ["zip", "fold-db"]

pact43Natives :: [Text]
pact43Natives = ["create-principal", "validate-principal", "continue"]

pact431Natives :: [Text]
pact431Natives = ["is-principal", "typeof-principal"]

pact46Natives :: [Text]
pact46Natives = ["point-add", "scalar-mult", "pairing-check"]

pact47Natives :: [Text]
pact47Natives = ["dec"]

pact410Natives :: [Text]
pact410Natives = ["poseidon-hash-hack-a-chain"]

pact411Natives :: [Text]
pact411Natives = ["enforce-verifier", "hyperlane-message-id", "hyperlane-decode-token-message"]

pact412Natives :: [Text]
pact412Natives = ["hash-keccak256"]

initRefStore :: RefStore
initRefStore = RefStore nativeDefs

versionedNativesRefStore :: ExecutionConfig -> RefStore
versionedNativesRefStore ec = versionNatives initRefStore
  where
  versionNatives = appEndo $ mconcat
    [ disablePact40Natives ec
    , disablePact42Natives ec
    , disablePact43Natives ec
    , disablePact431Natives ec
    , disablePact46Natives ec
    , disablePact47Natives ec
    , disablePact410Natives ec
    , disablePact411Natives ec
    , disablePact412Natives ec
    ]

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
  milliGas <- readIORef (_eeGas evalEnv)
  warnings <- readIORef (_eeWarnings evalEnv)
  let pact48Disabled = views (eeExecutionConfig . ecFlags) (S.member FlagDisablePact48) evalEnv
      gasLogs = _evalLogGas state
      pactExec = _evalPactExec state
      modules = _rsLoadedModules $ _evalRefs state
      gasUsed = if pact48Disabled then milliGasToGas milliGas else gasRem milliGas
  -- output uses lenient conversion
  return $! EvalResult
    terms
    (map (elideModRefInfo . toPactValueLenient) rs)
    logs pactExec gasUsed modules txid gasLogs (_evalEvents state) warnings
  where
    -- Round up by 1 if the `MilliGas` amount is in any way fractional.
    gasRem (MilliGas milliGas) =
      let (d, r) = milliGas `quotRem` millisPerGas
      in Gas (if r == 0 then d else d+1)

evalTerms :: Interpreter e -> EvalInput -> Eval e EvalOutput
evalTerms interp input = withRollback (start (interpreter interp runInput) >>= end)

  where

    withRollback act =
      act `onException` safeRollback

    safeRollback =
      void (tryAny (evalRollbackTx def))

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
