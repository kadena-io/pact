{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
 ( evalError,evalError',
   failTx,failTx',
   argsError,argsError',
   throwDbError,throwEither,throwEitherText,throwErr,
   PactId(..),
   PactEvent(..), eventName, eventParams, eventModule, eventModuleHash,
   RefStore(..),rsNatives,
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeMode,eeEntity,eePactStep,eePactDbVar,eeInRepl,
   eePactDb,eePurity,eeHash,eeGas, eeGasEnv,eeNamespacePolicy,eeSPVSupport,eePublicData,eeExecutionConfig,
   eeAdvice, eeWarnings,
   toPactId,
   Purity(..),
   RefState(..),rsLoaded,rsLoadedModules,rsNamespace,rsQualifiedDeps,
   EvalState(..),evalRefs,evalCallStack,evalPactExec,
   evalCapabilities,evalLogGas,evalEvents,
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
   emitPactWarning,
   PactWarning(..),
   module Pact.Types.Lang,
   module Pact.Types.Util,
   module Pact.Types.Persistence,
   module Pact.Types.Gas,
   module Pact.Types.ChainMeta,
   module Pact.Types.PactError,
   liftIO,
   eAdvise,
   isOffChainForkedError,
   OnChainErrorState(..)
   ) where


import Control.Arrow ((&&&))
import Control.Concurrent.MVar
import Control.Lens hiding ((.=),DefName, elements)
import Control.Monad (void)
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.DeepSeq
import Data.Aeson hiding (Object)
import Data.Default
import Data.IORef(IORef, modifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String
import Data.Text (Text,pack)
import Data.Set(Set)
import GHC.Generics (Generic)
import Test.QuickCheck

import Pact.Types.Term
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
import Pact.Types.RowData
import Pact.Types.SPV
import Pact.Types.Util
import Pact.Types.Namespace

import Pact.JSON.Legacy.Value (LegacyValue(..))

import qualified Pact.JSON.Encode as J

data KeyPredBuiltins = KeysAll|KeysAny|Keys2 deriving (Eq,Show,Enum,Bounded)
instance AsString KeyPredBuiltins where
  asString KeysAll = "keys-all"
  asString KeysAny = "keys-any"
  asString Keys2 = "keys-2"

keyPredBuiltins :: M.Map Name KeyPredBuiltins
keyPredBuiltins = M.fromList $ map (Name . (`BareName` def) . asString &&& id) [minBound .. maxBound]

-- | Storage for natives.
newtype RefStore = RefStore {
      _rsNatives :: HM.HashMap Text Ref
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

-- All warnings pact emits at runtime
data PactWarning
  -- | Deprecated native, with help message
  = DeprecatedNative !NativeDefName !Text
  -- | Deprecated overload with help message
  | DeprecatedOverload !NativeDefName !Text
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PactWarning

instance Pretty PactWarning where
  pretty = \case
    DeprecatedNative ndef msg ->
      "Warning: Using deprecated native" <+> pretty ndef <> ":" <+> pretty msg
    DeprecatedOverload ndef msg ->
      "Warning: using deprecated native overload for" <+> pretty ndef <> ":" <+> pretty msg


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
  -- | Disable Pact 4.2.0 db sorted key guarantees, and row persistence
  | FlagDisablePact420
  -- | Disable memory limit check
  | FlagDisableInlineMemCheck
  -- | Disable new non-inlined modules
  | FlagDisablePact43
  -- | Disable pact 4.3 features
  | FlagDisablePact431
  -- | Disable Pact 4.4 features
  | FlagDisablePact44
  -- | Disable new transcendental impls
  | FlagDisableNewTrans
  -- | Disable Pact 4.5 Features
  | FlagDisablePact45
  -- | Disable Pact 4.6 Features
  | FlagDisablePact46
  -- | Disable Pact 4.7 Features
  | FlagDisablePact47
  -- | Disable runtime return type checking.
  | FlagDisableRuntimeReturnTypeChecking
  -- | Disable Pact 4.8 Features
  | FlagDisablePact48
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

instance J.Encode ExecutionFlag where
  build = J.build . flagRep
  {-# INLINE build #-}

instance FromJSON ExecutionFlag where
  parseJSON = withText "ExecutionFlag" $ \t -> case M.lookup t flagReps of
    Nothing -> fail "Invalid ExecutionFlag value"
    Just f -> return f

instance Arbitrary ExecutionFlag where
  arbitrary = elements [minBound .. maxBound]

-- | Execution configuration flags, where empty is the "default".
newtype ExecutionConfig = ExecutionConfig
  { _ecFlags :: S.Set ExecutionFlag }
  deriving (Eq,Show)
  deriving (FromJSON)

makeLenses ''ExecutionConfig
instance Default ExecutionConfig where def = ExecutionConfig def
instance Pretty ExecutionConfig where
  pretty = pretty . S.toList . _ecFlags

instance Arbitrary ExecutionConfig where
  arbitrary = ExecutionConfig <$> arbitrary

instance J.Encode ExecutionConfig where
  build o = J.build $ J.Array (_ecFlags o)
  {-# INLINE build #-}

mkExecutionConfig :: [ExecutionFlag] -> ExecutionConfig
mkExecutionConfig = ExecutionConfig . S.fromList

-- | Interpreter reader environment, parameterized over back-end MVar state type.
data EvalEnv e = EvalEnv {
      -- | Environment references.
      _eeRefStore :: !RefStore
      -- | Verified keys from message.
    , _eeMsgSigs :: !(M.Map PublicKeyText (S.Set UserCapability))
      -- | JSON body accompanying message.
    , _eeMsgBody :: !LegacyValue
      -- | Execution mode
    , _eeMode :: !ExecutionMode
      -- | Entity governing private/encrypted 'pact' executions.
    , _eeEntity :: !(Maybe EntityName)
      -- | Step value for 'pact' executions.
    , _eePactStep :: !(Maybe PactStep)
      -- | Back-end state MVar.
    , _eePactDbVar :: !(MVar e)
      -- | Back-end function record.
    , _eePactDb :: !(PactDb e)
      -- | Pure indicator
    , _eePurity :: !Purity
      -- | Transaction hash
    , _eeHash :: !Hash
      -- | Gas Environment
    , _eeGasEnv :: !GasEnv
      -- | Tallied gas
    , _eeGas :: !(IORef Gas)
      -- | Namespace Policy
    , _eeNamespacePolicy :: !NamespacePolicy
      -- | SPV backend
    , _eeSPVSupport :: !SPVSupport
      -- | Env public data
    , _eePublicData :: !PublicData
      -- | Execution configuration flags
    , _eeExecutionConfig :: !ExecutionConfig
      -- | Advice bracketer
    , _eeAdvice :: !Advice
      -- | Are we in the repl? If not, ignore info
    , _eeInRepl :: !Bool
      -- | Warnings ref
    , _eeWarnings :: !(IORef (Set PactWarning))
    }
makeLenses ''EvalEnv

-- | 'PactId' -> 'Hash' conversion
toPactId :: Hash -> PactId
toPactId = PactId . hashToText

-- | Dynamic storage for loaded names and modules, and current namespace.
data RefState = RefState {
      -- | Imported Module-local defs and natives.
      _rsLoaded :: !(HM.HashMap Text (Ref, Maybe ModuleHash))
      -- | Modules that were loaded, and flag if updated.
    , _rsLoadedModules :: !(HM.HashMap ModuleName (ModuleData Ref, Bool))
      -- | Current Namespace
    , _rsNamespace :: !(Maybe (Namespace (Term Name)))
      -- | Map of all fully qualified names in scope, including transitive dependencies.
    , _rsQualifiedDeps :: !(HM.HashMap FullyQualifiedName Ref)
    } deriving (Eq,Show,Generic)

makeLenses ''RefState
instance NFData RefState
instance Default RefState where def = RefState HM.empty HM.empty Nothing HM.empty

data PactEvent = PactEvent
  { _eventName :: !Text
  , _eventParams :: ![PactValue]
  , _eventModule :: !ModuleName
  , _eventModuleHash :: !ModuleHash
  } deriving (Eq, Show, Generic)
instance NFData PactEvent

instance J.Encode PactEvent where
  build o = J.object
    [ "params" J..= J.Array (_eventParams o)
    , "name" J..= _eventName o
    , "module" J..= _eventModule o
    , "moduleHash" J..= _eventModuleHash o
    ]
  {-# INLINE build #-}

instance FromJSON PactEvent where parseJSON = lensyParseJSON 6

instance Arbitrary PactEvent where
  arbitrary = PactEvent
    <$> arbitrary
    <*> scale (min 20) arbitrary
    <*> arbitrary
    <*> arbitrary

makeLenses ''PactEvent


-- | Interpreter mutable state.
data EvalState = EvalState {
      -- | New or imported modules and defs.
      _evalRefs :: !RefState
      -- | Current call stack.
    , _evalCallStack :: ![StackFrame]
      -- | Pact execution trace, if any
    , _evalPactExec :: !(Maybe PactExec)
      -- | Capability list
    , _evalCapabilities :: !Capabilities
      -- | Tracks gas logs if enabled (i.e. Just)
    , _evalLogGas :: !(Maybe [(Text,Gas)])
      -- | Accumulate events
    , _evalEvents :: ![PactEvent]
    } deriving (Show, Generic)
makeLenses ''EvalState
instance NFData EvalState
instance Default EvalState where def = EvalState def def def def def def

-- | Interpreter monad, parameterized over back-end MVar state type.
newtype Eval e a =
    Eval { unEval :: ReaderT (EvalEnv e) (StateT EvalState IO) a }
    deriving (Functor,Applicative,Monad,MonadState EvalState,
                     MonadReader (EvalEnv e),MonadThrow,MonadCatch,MonadMask,MonadIO)

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
  handleAny (throwErr DbError i . viaShow) (liftIO $ f _eePactDb _eePactDbVar)

emitPactWarning :: PactWarning -> Eval e ()
emitPactWarning pw =
  view eeWarnings >>= \e -> liftIO (modifyIORef' e (S.insert pw))
--
-- Methods for invoking backend function-record function.
--

-- | Invoke '_readRow'
readRow :: (IsString k,FromJSON v) => Info -> Domain k v -> k -> Eval e (Maybe v)
readRow i d k = method i $ \db -> _readRow db d k

-- | Invoke '_writeRow'
writeRow :: (AsString k,J.Encode v) => Info -> WriteType -> Domain k v -> k -> v -> Eval e ()
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
commitTx :: Info -> Eval e [TxLogJson]
commitTx i = method i $ \db -> _commitTx db

-- | Invoke _rollbackTx
rollbackTx :: Info -> Eval e ()
rollbackTx i = method i $ \db -> _rollbackTx db

-- | Invoke _getTxLog
getTxLog :: IsString k => Info -> Domain k RowData -> TxId -> Eval e [TxLog RowData]
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

throwOnChainArgsError :: Pretty n => FunApp -> [Term n] -> Eval e a
throwOnChainArgsError FunApp{..} args = throwErr ArgsError _faInfo $
  "Invalid arguments in call to"
    <+> pretty _faName
    <> ", received arguments of type "
    <> bracketsSep (map (pretty . typeof') args) <> ", expected "
    <> prettyFunTypes _faTypes

throwErr :: PactErrorType -> Info -> Doc -> Eval e a
throwErr ctor i err = do
  s <- use evalCallStack
  offChainOrPreFork <- isOffChainForkedError' FlagDisablePact47
  throwM (PactError ctor i (if offChainOrPreFork then s else []) err)

evalError :: Info -> Doc -> Eval e a
evalError = throwErr EvalError

evalError' :: HasInfo i => i -> Doc -> Eval e a
evalError' = evalError . getInfo

data OnChainErrorState
  = OnChainError
  | OffChainError
  deriving (Eq, Show)

-- | Function to determine whether we are either pre-errors fork
-- or in a repl environment.
isOffChainForkedError :: ExecutionFlag -> Eval e OnChainErrorState
isOffChainForkedError flag = isOffChainForkedError' flag <&> \p -> if p then OffChainError else OnChainError

isOffChainForkedError' :: ExecutionFlag -> Eval e Bool
isOffChainForkedError' flag =
  isExecutionFlagSet flag >>= \case
    True -> pure True
    False -> view eeInRepl

failTx :: Info -> Doc -> Eval e a
failTx = throwErr TxFailure

failTx' :: HasInfo i => i -> Doc -> Eval e a
failTx' = failTx . getInfo

throwDbError :: MonadThrow m => Doc -> m a
throwDbError = throwM . PactError DbError def def

-- | Throw an error coming from an Except/Either context.
throwEither :: (MonadThrow m,Exception e) => Either e a -> m a
throwEither = either throwM return

throwEitherText :: PactErrorType -> Info -> Doc -> Either Text a -> Eval e a
throwEitherText typ i d = either (\e -> throwErr typ i (d <> ":" <> pretty e)) return


argsError :: FunApp -> [Term Name] -> Eval e a
argsError i as =
  isOffChainForkedError FlagDisablePact47 >>= \case
    OffChainError -> throwArgsError i as "Invalid arguments"
    OnChainError -> throwOnChainArgsError i as

argsError' :: FunApp -> [Term Ref] -> Eval e a
argsError' i as =
  isOffChainForkedError FlagDisablePact47 >>= \case
    OffChainError -> throwArgsError i (map (toTerm.abbrev) as) "Invalid arguments"
    OnChainError -> throwOnChainArgsError i as

eAdvise :: Info -> AdviceContext r -> Eval e (r -> Eval e ())
eAdvise i m = view eeAdvice >>= \adv -> advise i adv m
