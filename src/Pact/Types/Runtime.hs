{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
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
 ( PactError(..),PactErrorType(..),
   evalError,evalError',failTx,argsError,argsError',throwDbError,throwEither,throwErr,
   PactId(..),
   PactStep(..),psStep,psRollback,psPactId,psResume,
   ModuleData(..), mdModule, mdRefMap,
   RefStore(..),rsNatives,rsModules,updateRefStore,
   EntityName(..),
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeTxId,eeEntity,eePactStep,eePactDbVar,
   eePactDb,eePurity,eeHash,eeGasEnv,eeNamespacePolicy,eeSPVSupport,
   Purity(..),PureNoDb,PureSysRead,EnvNoDb(..),EnvReadOnly(..),mkNoDbEnv,mkReadOnlyEnv,
   StackFrame(..),sfName,sfLoc,sfApp,
   PactExec(..),peStepCount,peYield,peExecuted,pePactId,peStep,peContinuation,
   RefState(..),rsLoaded,rsLoadedModules,rsNewModules,rsNamespace,
   EvalState(..),evalRefs,evalCallStack,evalPactExec,evalGas,evalCapabilities,
   Eval(..),runEval,runEval',
   call,method,
   readRow,writeRow,keys,txids,createUserTable,getUserTableInfo,beginTx,commitTx,rollbackTx,getTxLog,
   KeyPredBuiltins(..),keyPredBuiltins,
   Capability(..),CapAcquireResult(..),
   Capabilities(..),capGranted,capComposed,
   NamespacePolicy(..), nsPolicy,
   permissiveNamespacePolicy,
   SPVSupport(..),noSPVSupport,
   PactContinuation(..),
   module Pact.Types.Lang,
   module Pact.Types.Util,
   module Pact.Types.Persistence,
   module Pact.Types.Gas
   ) where

import Control.Arrow ((&&&))
import Control.Lens hiding ((.=),DefName)
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson hiding (Object)
import qualified Data.Set as S
import Data.String
import Data.Default
import Control.Monad.Catch
import Control.Concurrent.MVar
import Data.Serialize (Serialize)
import Data.Hashable

import Pact.Types.Gas
import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.Util


data Capability
  = ModuleAdminCapability ModuleName
  | UserCapability ModuleName DefName [Term Name]
  deriving (Eq,Show)

instance Pretty Capability where
  pretty (ModuleAdminCapability mn) = pretty mn
  pretty (UserCapability mn name tms)  = parensSep (pretty mn <> colon <> pretty name : fmap pretty tms)

data CapAcquireResult = NewlyAcquired|AlreadyAcquired
  deriving (Eq,Show)

newtype NamespacePolicy = NamespacePolicy
  { _nsPolicy :: Maybe Namespace -> Bool
  }
makeLenses ''NamespacePolicy

permissiveNamespacePolicy :: NamespacePolicy
permissiveNamespacePolicy = NamespacePolicy $ const True

data StackFrame = StackFrame {
      _sfName :: !Text
    , _sfLoc :: !Info
    , _sfApp :: Maybe (FunApp,[Text])
    }
instance Show StackFrame where
    show (StackFrame n i app) = renderInfo i ++ ": " ++ case app of
      Nothing -> unpack n
      Just (_,as) -> "(" ++ unpack n ++ concatMap (\a -> " " ++ unpack (asString a)) as ++ ")"
makeLenses ''StackFrame

data PactErrorType
  = EvalError
  | ArgsError
  | DbError
  | TxFailure
  | SyntaxError
  | GasError
  deriving Show

data PactError = PactError
  { peType :: PactErrorType
  , peInfo :: Info
  , peCallStack :: [StackFrame]
  , peDoc :: Doc }

instance Exception PactError

instance Show PactError where
    show (PactError t i _ s) = show i ++ ": Failure: " ++ maybe "" (++ ": ") msg ++ show s
      where msg = case t of
              EvalError -> Nothing
              ArgsError -> Nothing
              TxFailure -> Just "Tx Failed"
              DbError -> Just "Database exception"
              SyntaxError -> Just "Syntax error"
              GasError -> Just "Gas Error"


data KeyPredBuiltins = KeysAll|KeysAny|Keys2 deriving (Eq,Show,Enum,Bounded)
instance AsString KeyPredBuiltins where
  asString KeysAll = "keys-all"
  asString KeysAny = "keys-any"
  asString Keys2 = "keys-2"

keyPredBuiltins :: M.Map Name KeyPredBuiltins
keyPredBuiltins = M.fromList $ map ((`Name` def) . asString &&& id) [minBound .. maxBound]

-- | Environment setup for pact execution.
data PactStep = PactStep {
      _psStep :: !Int
    , _psRollback :: !Bool
    , _psPactId :: !PactId
    , _psResume :: !(Maybe (Term Name))
} deriving (Eq,Show)
makeLenses ''PactStep

-- | Module ref store
data ModuleData = ModuleData
  { _mdModule :: ModuleDef (Def Ref)
  , _mdRefMap :: HM.HashMap Text Ref
  } deriving (Eq, Show)
makeLenses ''ModuleData

-- | Storage for loaded modules, interfaces, and natives.
data RefStore = RefStore {
      _rsNatives :: HM.HashMap Name Ref
    , _rsModules :: HM.HashMap ModuleName ModuleData
    } deriving (Eq, Show)
makeLenses ''RefStore
instance Default RefStore where def = RefStore HM.empty HM.empty

newtype EntityName = EntityName Text
  deriving (IsString,AsString,Eq,Ord,Hashable,Serialize,NFData,ToJSON,FromJSON,Default)
instance Show EntityName where show (EntityName t) = show t

newtype PactContinuation = PactContinuation (App (Term Ref))
  deriving (Eq,Show)

-- | Result of evaluation of a 'defpact'.
data PactExec = PactExec
  { -- | Count of steps in pact (discovered when code is executed)
    _peStepCount :: Int
    -- | Yield value if invoked
  , _peYield :: !(Maybe (Term Name))
    -- | Whether step was executed (in private cases, it can be skipped)
  , _peExecuted :: Bool
    -- | Step that was executed or skipped
  , _peStep :: Int
    -- | Pact id. On a new pact invocation, is copied from tx id.
  , _pePactId :: PactId
    -- | Strict (in arguments) application of pact, for future step invocations.
  , _peContinuation :: PactContinuation
  } deriving (Eq,Show)
makeLenses ''PactExec

-- | Indicates level of db access offered in current Eval monad.
data Purity =
  -- | No database access at all.
  PNoDb |
  -- | Access to read of module, keyset systables.
  PReadOnly |
  -- | All database access allowed (normal).
  PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)
instance Default Purity where def = PImpure

-- | Marker class for 'PNoDb' environments.
class PureNoDb e
-- | Marker class for 'PReadOnly' environments.
-- SysRead supports pure operations as well.
class PureNoDb e => PureSysRead e

-- | Backend for SPV
newtype SPVSupport = SPVSupport {
  -- | Attempt to verify an SPV proof of a given type,
  -- given a payload object. On success, returns the
  -- specific data represented by the proof.
  _spvSupport :: Text -> Object Name -> IO (Either Text (Object Name))
}

noSPVSupport :: SPVSupport
noSPVSupport = SPVSupport $ \_ _ -> return $ Left $ "SPV verify not supported"

-- | Interpreter reader environment, parameterized over back-end MVar state type.
data EvalEnv e = EvalEnv {
      -- | Environment references.
      _eeRefStore :: !RefStore
      -- | Verified keys from message.
    , _eeMsgSigs :: !(S.Set PublicKey)
      -- | JSON body accompanying message.
    , _eeMsgBody :: !Value
      -- | Transaction id. 'Nothing' indicates local execution.
    , _eeTxId :: !(Maybe TxId)
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
    }
makeLenses ''EvalEnv



-- | Dynamic storage for namespace-loaded modules, and new modules compiled in current tx.
data RefState = RefState {
      -- | Imported Module-local defs and natives.
      _rsLoaded :: HM.HashMap Name Ref
      -- | Modules that were loaded.
    , _rsLoadedModules :: HM.HashMap ModuleName (ModuleDef (Def Ref))
      -- | Modules that were compiled and loaded in this tx.
    , _rsNewModules :: HM.HashMap ModuleName ModuleData
      -- | Current Namespace
    , _rsNamespace :: Maybe Namespace
    } deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty HM.empty HM.empty Nothing

-- | Update for newly-loaded modules and interfaces.
updateRefStore :: RefState -> RefStore -> RefStore
updateRefStore RefState {..}
  | HM.null _rsNewModules = id
  | otherwise = over rsModules (HM.union _rsNewModules)

data Capabilities = Capabilities
  { _capGranted :: [Capability]
  , _capComposed :: [Capability]
  } deriving (Show)
instance Default Capabilities where def = Capabilities def def
makeLenses ''Capabilities

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
    } deriving (Show)
makeLenses ''EvalState
instance Default EvalState where def = EvalState def def def 0 def

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
  runStateT (catches (Right <$> runReaderT (unEval act) env)
              [Handler (\(e :: PactError) -> return $ Left e)
              ,Handler (\(e :: SomeException) -> return $ Left . PactError EvalError def def . viaShow $ e)
              ]) s


-- | Bracket interpreter action pushing and popping frame on call stack.
call :: StackFrame -> Eval e (Gas,a) -> Eval e a
call s act = do
  evalCallStack %= (s:)
  (_gas,r) <- act -- TODO opportunity for per-call gas logging here
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
keys :: Info -> TableName -> Eval e [RowKey]
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
beginTx :: Info -> Maybe TxId -> Eval e ()
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

evalError' :: FunApp -> Doc -> Eval e a
evalError' = evalError . _faInfo

failTx :: Info -> Doc -> Eval e a
failTx i = throwErr TxFailure i

throwDbError :: MonadThrow m => Doc -> m a
throwDbError = throwM . PactError DbError def def

-- | Throw an error coming from an Except/Either context.
throwEither :: (MonadThrow m,Exception e) => Either e a -> m a
throwEither = either throwM return


argsError :: FunApp -> [Term Name] -> Eval e a
argsError i as = throwArgsError i as "Invalid arguments"

argsError' :: FunApp -> [Term Ref] -> Eval e a
argsError' i as = throwArgsError i (map (toTerm.abbrev) as) "Invalid arguments"


--
-- Purity stuff.
--

newtype EnvNoDb e = EnvNoDb (EvalEnv e)

instance PureNoDb (EnvNoDb e)

newtype EnvReadOnly e = EnvReadOnly (EvalEnv e)

instance PureSysRead (EnvReadOnly e)
instance PureNoDb (EnvReadOnly e)

diePure :: Method e a
diePure _ = throwM $ PactError EvalError def def "Illegal database access in pure context"

-- | Construct a delegate pure eval environment.
mkPureEnv :: (EvalEnv e -> f) -> Purity ->
             (forall k v . (IsString k,FromJSON v) =>
              Domain k v -> k -> Method f (Maybe v)) ->
             EvalEnv e -> Eval e (EvalEnv f)
mkPureEnv holder purity readRowImpl env@EvalEnv{..} = do
  v <- liftIO $ newMVar (holder env)
  return $ EvalEnv
    _eeRefStore
    _eeMsgSigs
    _eeMsgBody
    _eeTxId
    _eeEntity
    _eePactStep
    v
    PactDb {
      _readRow = readRowImpl
    , _writeRow = \_ _ _ _ -> diePure
    , _keys = const diePure
    , _txids = \_ _ -> diePure
    , _createUserTable = \_ _ -> diePure
    , _getUserTableInfo = const diePure
    , _beginTx = const diePure
    , _commitTx = diePure
    , _rollbackTx = diePure
    , _getTxLog = \_ _ -> diePure
    }
    purity
    _eeHash
    _eeGasEnv
    permissiveNamespacePolicy
    _eeSPVSupport


mkNoDbEnv :: EvalEnv e -> Eval e (EvalEnv (EnvNoDb e))
mkNoDbEnv = mkPureEnv EnvNoDb PNoDb (\_ _ -> diePure)

mkReadOnlyEnv :: EvalEnv e -> Eval e (EvalEnv (EnvReadOnly e))
mkReadOnlyEnv = mkPureEnv EnvReadOnly PReadOnly $ \d k e ->
  withMVar e $ \(EnvReadOnly EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar
