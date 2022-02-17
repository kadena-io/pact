{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- 'Eval' monad and utilities.
-- Exports Lang and Util, so this is the "default import" for most pact things.
--

module Pact.Types.Runtime
 ( evalError,evalError',failTx,argsError,argsError',throwDbError,throwEither,throwEitherText,throwErr,
   PactId(..),
   PactEvent(..), eventName, eventParams, eventModule, eventModuleHash,
   RefStore(..),rsNatives,
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeMode,eeEntity,eePactStep,eePactDbVar,
   eePactDb,eePurity,eeHash,eeGasEnv,eeNamespacePolicy,eeSPVSupport,eePublicData,eeExecutionConfig,
   eeAdvice,
   toPactId,
   Purity(..),
   RefState(..),rsLoaded,rsLoadedModules,rsNamespace,
   EvalState(..),evalRefs,evalCallStack,evalPactExec,
   evalGas,evalCapabilities,evalLogGas,evalEvents,
   Eval(..),runEval,runEval',catchesPactError,
   call,method,
   readRow,writeRow,keys,txids,createUserTable,getUserTableInfo,beginTx,commitTx,rollbackTx,getTxLog,
   KeyPredBuiltins(..),keyPredBuiltins,
   NamespacePolicy(..),
   permissiveNamespacePolicy,
   ExecutionConfig(..),ExecutionFlag(..),ecFlags,isExecutionFlagSet,flagRep,flagReps,
   mkExecutionConfig,
   ifExecutionFlagSet,ifExecutionFlagSet',
   whenExecutionFlagSet, unlessExecutionFlagSet,
   module Pact.Types.Lang,
   module Pact.Types.Util,
   module Pact.Types.Persistence,
   module Pact.Types.Gas,
   module Pact.Types.ChainMeta,
   module Pact.Types.PactError,
   liftIO,
   eAdvise
   ) where


import Control.Arrow ((&&&))
import Control.Concurrent.MVar
import Control.Lens hiding ((.=),DefName)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.DeepSeq
import Data.Aeson hiding (Object)
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String
import Data.Text (Text,pack)
import GHC.Generics (Generic)

import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Continuation
import Pact.Types.Gas
import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Advice
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.SPV
import Pact.Types.Util


-- | Governance of namespace use. Policy dictates:
-- 1. Whether a namespace can be created.
-- 2. Whether the default namespace can be used.
data NamespacePolicy =
  SimpleNamespacePolicy (Maybe (Namespace (Term Name)) -> Bool)
  -- ^ if namespace is Nothing/root, govern usage; otherwise govern creation.
  |
  SmartNamespacePolicy Bool QualifiedName
  -- ^ Bool governs root usage, Name governs ns creation.
  -- Def is (defun xxx:bool (ns:string ns-admin:guard))

permissiveNamespacePolicy :: NamespacePolicy
permissiveNamespacePolicy = SimpleNamespacePolicy $ const True

data KeyPredBuiltins = KeysAll|KeysAny|Keys2 deriving (Eq,Show,Enum,Bounded)
instance AsString KeyPredBuiltins where
  asString KeysAll = "keys-all"
  asString KeysAny = "keys-any"
  asString Keys2 = "keys-2"

keyPredBuiltins :: M.Map Name KeyPredBuiltins
keyPredBuiltins = M.fromList $ map (Name . (`BareName` def) . asString &&& id) [minBound .. maxBound]

-- | Storage for natives.
data RefStore = RefStore {
      _rsNatives :: HM.HashMap Name Ref
    } deriving (Eq, Show)
makeLenses ''RefStore
instance Default RefStore where def = RefStore HM.empty

-- | Indicates level of db access offered in current Eval monad.
data Purity =
  -- | Read-only access to systables.
  PSysOnly |
  -- | Read-only access to systables and module tables.
  PReadOnly |
  -- | All database access allowed (normal).
  PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)
instance Default Purity where def = PImpure


-- | Execution flags specify behavior of the runtime environment,
-- with an orientation towards some alteration of a default behavior.
-- Thus, a flag should _not_ describe "normal behavior" (the default),
-- but instead should enable some "unusual" option.
data ExecutionFlag
  -- | Disable user module install
  = FlagDisableModuleInstall
  -- | Disable database history queries in transactional mode (local-only)
  | FlagDisableHistoryInTransactionalMode
  -- | Preserve runReadOnly failing inside of runSysOnly
  | FlagOldReadOnlyBehavior
  -- | Disable table module guard for read operations in local
  | FlagAllowReadInLocal
  -- | Preserve namespace module governance bug
  | FlagPreserveModuleNameBug
  -- | Preserve namespace module acquire gov bug
  | FlagPreserveNsModuleInstallBug
  -- | Disable emission of pact events
  | FlagDisablePactEvents
  -- | Preserve module implemented interface namespacing bug
  | FlagPreserveModuleIfacesBug
  -- | Preserve Show in reduce for Def, Native
  | FlagPreserveShowDefs
  -- | Disable Pact 4.0 features
  | FlagDisablePact40
  -- | Enforce key formats. "Positive" polarity to not break legacy repl tests.
  | FlagEnforceKeyFormats
  -- | Enable Pact 4.2.0 db sorted key guarantees, and row persistence
  | FlagDisablePact420
  -- | Enable memory limit check
  | FlagDisableInlineMemCheck
  deriving (Eq,Ord,Show,Enum,Bounded)

-- | Flag string representation
flagRep :: ExecutionFlag -> Text
flagRep = pack . drop 4 . show

-- | Flag string representations
flagReps :: M.Map Text ExecutionFlag
flagReps = M.fromList $ map go [minBound .. maxBound]
  where go f = (flagRep f,f)

instance Pretty ExecutionFlag where
  pretty = pretty . flagRep
instance ToJSON ExecutionFlag where toJSON = String . flagRep
instance FromJSON ExecutionFlag where
  parseJSON = withText "ExecutionFlag" $ \t -> case M.lookup t flagReps of
    Nothing -> fail "Invalid ExecutionFlag value"
    Just f -> return f

-- | Execution configuration flags, where empty is the "default".
newtype ExecutionConfig = ExecutionConfig
  { _ecFlags :: S.Set ExecutionFlag }
  deriving (Eq,Show,ToJSON,FromJSON)
makeLenses ''ExecutionConfig
instance Default ExecutionConfig where def = ExecutionConfig def
instance Pretty ExecutionConfig where
  pretty = pretty . S.toList . _ecFlags

mkExecutionConfig :: [ExecutionFlag] -> ExecutionConfig
mkExecutionConfig = ExecutionConfig . S.fromList

-- | Interpreter reader environment, parameterized over back-end MVar state type.
data EvalEnv e = EvalEnv {
      -- | Environment references.
      _eeRefStore :: !RefStore
      -- | Verified keys from message.
    , _eeMsgSigs :: !(M.Map PublicKey (S.Set UserCapability))
      -- | JSON body accompanying message.
    , _eeMsgBody :: !Value
      -- | Execution mode
    , _eeMode :: ExecutionMode
      -- | Entity governing private/encrypted 'pact' executions.
    , _eeEntity :: !(Maybe EntityName)
      -- | Step value for 'pact' executions.
    , _eePactStep :: !(Maybe PactStep)
      -- | Back-end state MVar.
    , _eePactDbVar :: MVar e
      -- | Back-end function record.
    , _eePactDb :: PactDb e
      -- | Pure indicator
    , _eePurity :: Purity
      -- | Transaction hash
    , _eeHash :: Hash
      -- | Gas Environment
    , _eeGasEnv :: GasEnv
      -- | Namespace Policy
    , _eeNamespacePolicy :: NamespacePolicy
      -- | SPV backend
    , _eeSPVSupport :: SPVSupport
      -- | Env public data
    , _eePublicData :: PublicData
      -- | Execution configuration flags
    , _eeExecutionConfig :: ExecutionConfig
      -- | Advice bracketer
    , _eeAdvice :: !Advice
    }
makeLenses ''EvalEnv

-- | 'PactId' -> 'Hash' conversion
toPactId :: Hash -> PactId
toPactId = PactId . hashToText

-- | Dynamic storage for loaded names and modules, and current namespace.
data RefState = RefState {
      -- | Imported Module-local defs and natives.
      _rsLoaded :: HM.HashMap Name Ref
      -- | Modules that were loaded, and flag if updated.
    , _rsLoadedModules :: HM.HashMap ModuleName (ModuleData Ref, Bool)
      -- | Current Namespace
    , _rsNamespace :: Maybe (Namespace (Term Name))
    } deriving (Eq,Show,Generic)
makeLenses ''RefState
instance NFData RefState
instance Default RefState where def = RefState HM.empty HM.empty Nothing

data PactEvent = PactEvent
  { _eventName :: !Text
  , _eventParams :: ![PactValue]
  , _eventModule :: !ModuleName
  , _eventModuleHash :: !ModuleHash
  } deriving (Eq, Show, Generic)
instance NFData PactEvent
instance ToJSON PactEvent where toJSON = lensyToJSON 6
instance FromJSON PactEvent where parseJSON = lensyParseJSON 6
makeLenses ''PactEvent


-- | Interpreter mutable state.
data EvalState = EvalState {
      -- | New or imported modules and defs.
      _evalRefs :: !RefState
      -- | Current call stack.
    , _evalCallStack :: ![StackFrame]
      -- | Pact execution trace, if any
    , _evalPactExec :: !(Maybe PactExec)
      -- | Gas tally
    , _evalGas :: Gas
      -- | Capability list
    , _evalCapabilities :: Capabilities
      -- | Tracks gas logs if enabled (i.e. Just)
    , _evalLogGas :: Maybe [(Text,Gas)]
      -- | Accumulate events
    , _evalEvents :: ![PactEvent]
    } deriving (Show, Generic)
makeLenses ''EvalState
instance NFData EvalState
instance Default EvalState where def = EvalState def def def 0 def def def

-- | Interpreter monad, parameterized over back-end MVar state type.
newtype Eval e a =
    Eval { unEval :: ReaderT (EvalEnv e) (StateT EvalState IO) a }
    deriving (Functor,Applicative,Monad,MonadState EvalState,
                     MonadReader (EvalEnv e),MonadThrow,MonadCatch,MonadIO)

-- | "Production" runEval throws exceptions, meaning the state can be lost,
-- which is useful for reporting stack traces in the REPL.
runEval :: EvalState -> EvalEnv e -> Eval e a -> IO (a,EvalState)
runEval s env act = runStateT (runReaderT (unEval act) env) s
{-# INLINE runEval #-}

-- | "Dev" runEval' is the old version that always returns the state
-- along with the Either.
runEval' :: EvalState -> EvalEnv e -> Eval e a ->
           IO (Either PactError a,EvalState)
runEval' s env act =
  runStateT (catchesPactError $ runReaderT (unEval act) env) s

catchesPactError :: (MonadCatch m) => m a -> m (Either PactError a)
catchesPactError action =
  catches (Right <$> action)
  [ Handler (\(e :: PactError) -> return $ Left e)
   ,Handler (\(e :: SomeException) -> return $ Left . PactError EvalError def def . viaShow $ e)
  ]

isExecutionFlagSet :: ExecutionFlag -> Eval e Bool
isExecutionFlagSet f = S.member f <$> view (eeExecutionConfig . ecFlags)

ifExecutionFlagSet :: ExecutionFlag -> Eval e a -> Eval e a -> Eval e a
ifExecutionFlagSet f onTrue onFalse = do
  b <- isExecutionFlagSet f
  if b then onTrue else onFalse

ifExecutionFlagSet' :: ExecutionFlag -> a -> a -> Eval e a
ifExecutionFlagSet' f onTrue onFalse =
  ifExecutionFlagSet f (return onTrue) (return onFalse)

whenExecutionFlagSet :: ExecutionFlag -> Eval e a -> Eval e ()
whenExecutionFlagSet f onTrue =
  ifExecutionFlagSet f (void onTrue) (return ())

unlessExecutionFlagSet :: ExecutionFlag -> Eval e a -> Eval e ()
unlessExecutionFlagSet f onFalse =
  ifExecutionFlagSet f (return ()) (void onFalse)

-- | Bracket interpreter action pushing and popping frame on call stack.
call :: StackFrame -> Eval e (Gas,a) -> Eval e a
call s act = do
  evalCallStack %= (s:)
  (_gas,r) <- act
  evalCallStack %= drop 1
  return r
{-# INLINE call #-}

-- | Invoke a backend method, catching all exceptions as 'DbError'
method :: Info -> (PactDb e -> Method e a) -> Eval e a
method i f = do
  EvalEnv {..} <- ask
  handleAll (throwErr DbError i . viaShow) (liftIO $ f _eePactDb _eePactDbVar)


--
-- Methods for invoking backend function-record function.
--

-- | Invoke '_readRow'
readRow :: (IsString k,FromJSON v) => Info -> Domain k v -> k -> Eval e (Maybe v)
readRow i d k = method i $ \db -> _readRow db d k

-- | Invoke '_writeRow'
writeRow :: (AsString k,ToJSON v) => Info -> WriteType -> Domain k v -> k -> v -> Eval e ()
writeRow i w d k v = method i $ \db -> _writeRow db w d k v

-- | Invoke '_keys'
keys :: (AsString k,IsString k) => Info -> Domain k v -> Eval e [k]
keys i t = method i $ \db -> _keys db t

-- | Invoke '_txids'
txids :: Info -> TableName -> TxId -> Eval e [TxId]
txids i tn tid = method i $ \db -> _txids db tn tid

-- | Invoke '_createUserTable'
createUserTable :: Info -> TableName -> ModuleName -> Eval e ()
createUserTable i t m = method i $ \db -> _createUserTable db t m

-- | Invoke _getUserTableInfo
getUserTableInfo :: Info -> TableName -> Eval e ModuleName
getUserTableInfo i t = method i $ \db -> _getUserTableInfo db t

-- | Invoke _beginTx
beginTx :: Info -> ExecutionMode -> Eval e (Maybe TxId)
beginTx i t = method i $ \db -> _beginTx db t

-- | Invoke _commitTx
commitTx :: Info -> Eval e [TxLog Value]
commitTx i = method i $ \db -> _commitTx db

-- | Invoke _rollbackTx
rollbackTx :: Info -> Eval e ()
rollbackTx i = method i $ \db -> _rollbackTx db

-- | Invoke _getTxLog
getTxLog :: (IsString k,FromJSON v) => Info -> Domain k v -> TxId -> Eval e [TxLog v]
getTxLog i d t = method i $ \db -> _getTxLog db d t


{-# INLINE readRow #-}
{-# INLINE writeRow #-}
{-# INLINE createUserTable #-}
{-# INLINE getUserTableInfo #-}
{-# INLINE commitTx #-}
{-# INLINE beginTx #-}
{-# INLINE rollbackTx #-}
{-# INLINE getTxLog #-}
{-# INLINE keys #-}
{-# INLINE txids #-}



throwArgsError :: FunApp -> [Term Name] -> Text -> Eval e a
throwArgsError FunApp {..} args s = throwErr ArgsError _faInfo $
  pretty s <> ", received " <> bracketsSep (map pretty args) <> " for " <>
            prettyFunTypes _faTypes

throwErr :: PactErrorType -> Info -> Doc -> Eval e a
throwErr ctor i err = get >>= \s -> throwM (PactError ctor i (_evalCallStack s) err)

evalError :: Info -> Doc -> Eval e a
evalError i = throwErr EvalError i

evalError' :: HasInfo i => i -> Doc -> Eval e a
evalError' = evalError . getInfo

failTx :: Info -> Doc -> Eval e a
failTx i = throwErr TxFailure i

throwDbError :: MonadThrow m => Doc -> m a
throwDbError = throwM . PactError DbError def def

-- | Throw an error coming from an Except/Either context.
throwEither :: (MonadThrow m,Exception e) => Either e a -> m a
throwEither = either throwM return

throwEitherText :: PactErrorType -> Info -> Doc -> Either Text a -> Eval e a
throwEitherText typ i d = either (\e -> throwErr typ i (d <> ":" <> pretty e)) return


argsError :: FunApp -> [Term Name] -> Eval e a
argsError i as = throwArgsError i as "Invalid arguments"

argsError' :: FunApp -> [Term Ref] -> Eval e a
argsError' i as = throwArgsError i (map (toTerm.abbrev) as) "Invalid arguments"

eAdvise :: Info -> AdviceContext r -> Eval e (r,a) -> Eval e a
eAdvise i m a = view eeAdvice >>= \adv -> advise i adv m a
