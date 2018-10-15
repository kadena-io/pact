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
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeTxId,eeEntity,eePactStep,eePactDbVar,eePactDb,eePurity,eeHash,eeGasEnv,
   Purity(..),PureNoDb,PureSysRead,EnvNoDb(..),EnvSysRead(..),mkNoDbEnv,mkSysReadEnv,
   StackFrame(..),sfName,sfLoc,sfApp,
   PactExec(..),peStepCount,peYield,peExecuted,pePactId,peStep,
   RefState(..),rsLoaded,rsLoadedModules,rsNewModules,
   EvalState(..),evalRefs,evalCallStack,evalPactExec,evalGas,
   Eval(..),runEval,runEval',
   call,method,
   readRow,writeRow,keys,txids,createUserTable,getUserTableInfo,beginTx,commitTx,rollbackTx,getTxLog,
   KeyPredBuiltins(..),keyPredBuiltins,
   module Pact.Types.Lang,
   module Pact.Types.Util,
   module Pact.Types.Persistence,
   module Pact.Types.Gas
   ) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Control.DeepSeq
import Data.List
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson
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
import Pact.Types.Util


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
  , peText :: Text }

instance Exception PactError

instance Show PactError where
    show (PactError t i _ s) = show i ++ ": Failure: " ++ maybe "" (++ ": ") msg ++ unpack s
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


newtype PactId = PactId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show PactId where show (PactId s) = show s

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
  { _mdModule :: Module
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


-- | Runtime capture of pact execution.
data PactExec = PactExec
  { _peStepCount :: Int
  , _peYield :: !(Maybe (Term Name))
  , _peExecuted :: Bool
  , _peStep :: Int
  , _pePactId :: PactId
  } deriving (Eq,Show)
makeLenses ''PactExec

-- | Indicates level of db access offered in current Eval monad.
data Purity =
  -- | No database access at all.
  PNoDb |
  -- | Access to read of module, keyset systables.
  PSysRead |
  -- | All database access allowed (normal).
  PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)
instance Default Purity where def = PImpure

-- | Marker class for 'PNoDb' environments.
class PureNoDb e
-- | Marker class for 'PSysRead' environments.
-- SysRead supports pure operations as well.
class PureNoDb e => PureSysRead e


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
    } -- deriving (Eq,Show)
makeLenses ''EvalEnv



-- | Dynamic storage for namespace-loaded modules, and new modules compiled in current tx.
data RefState = RefState {
      -- | Namespace-local defs.
      _rsLoaded :: HM.HashMap Name Ref
      -- | Modules that were loaded.
    , _rsLoadedModules :: HM.HashMap ModuleName Module
      -- | Modules that were compiled and loaded in this tx.
    , _rsNewModules :: HM.HashMap ModuleName ModuleData
    } deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty HM.empty HM.empty

-- | Update for newly-loaded modules and interfaces.
updateRefStore :: RefState -> RefStore -> RefStore
updateRefStore RefState {..}
  | HM.null _rsNewModules = id
  | otherwise = over rsModules (HM.union _rsNewModules)

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
    } deriving (Show)
makeLenses ''EvalState
instance Default EvalState where def = EvalState def def def 0

-- | Interpreter monad, parameterized over back-end MVar state type.
newtype Eval e a =
    Eval { unEval :: ReaderT (EvalEnv e) (StateT EvalState IO) a }
    deriving (Functor,Applicative,Monad,MonadState EvalState,
                     MonadReader (EvalEnv e),MonadThrow,MonadCatch,MonadIO)

instance Semigroup a => Semigroup (Eval e a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Eval e a) where
  mempty = pure mempty

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
              ,Handler (\(e :: SomeException) -> return $ Left . PactError EvalError def def . pack . show $ e)
              ]) s


-- | Bracket interpreter action pushing and popping frame on call stack.
call :: StackFrame -> Eval e (Gas,a) -> Eval e a
call s act = do
  evalCallStack %= (s:)
  (_gas,r) <- act -- TODO opportunity for per-call gas logging here
  evalCallStack %= \st -> case st of (_:as) -> as; [] -> []
  return r
{-# INLINE call #-}

-- | Invoke a backend method, catching all exceptions as 'DbError'
method :: Info -> (PactDb e -> Method e a) -> Eval e a
method i f = do
  EvalEnv {..} <- ask
  handleAll (throwErr DbError i . pack . show) (liftIO $ f _eePactDb _eePactDbVar)


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
createUserTable :: Info -> TableName -> ModuleName -> KeySetName -> Eval e ()
createUserTable i t m k = method i $ \db -> _createUserTable db t m k

-- | Invoke _getUserTableInfo
getUserTableInfo :: Info -> TableName -> Eval e (ModuleName,KeySetName)
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
throwArgsError FunApp {..} args s = throwErr ArgsError _faInfo $ pack $
  unpack s ++ ", received [" ++ intercalate "," (map abbrev args) ++ "] for " ++
            showFunTypes _faTypes

throwErr :: PactErrorType -> Info -> Text -> Eval e a
throwErr ctor i err = get >>= \s -> throwM (PactError ctor i (_evalCallStack s) err)

evalError :: Info -> String -> Eval e a
evalError i = throwErr EvalError i . pack

evalError' :: FunApp -> String -> Eval e a
evalError' = evalError . _faInfo

failTx :: Info -> String -> Eval e a
failTx i = throwErr TxFailure i . pack

throwDbError :: MonadThrow m => String -> m a
throwDbError s = throwM $ PactError DbError def def (pack s)

-- | Throw an error coming from an Except/Either context.
throwEither :: (MonadThrow m,Exception e) => Either e a -> m a
throwEither = either throwM return


argsError :: FunApp -> [Term Name] -> Eval e a
argsError i as = throwArgsError i as "Invalid arguments"

argsError' :: FunApp -> [Term Ref] -> Eval e a
argsError' i as = throwArgsError i (map (toTerm.pack.abbrev) as) "Invalid arguments"


--
-- Purity stuff.
--

newtype EnvNoDb e = EnvNoDb (EvalEnv e)

instance PureNoDb (EnvNoDb e)

newtype EnvSysRead e = EnvSysRead (EvalEnv e)

instance PureSysRead (EnvSysRead e)
instance PureNoDb (EnvSysRead e)

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
    , _createUserTable = \_ _ _ -> diePure
    , _getUserTableInfo = const diePure
    , _beginTx = const diePure
    , _commitTx = diePure
    , _rollbackTx = diePure
    , _getTxLog = \_ _ -> diePure
    }
    purity
    _eeHash
    _eeGasEnv


mkNoDbEnv :: EvalEnv e -> Eval e (EvalEnv (EnvNoDb e))
mkNoDbEnv = mkPureEnv EnvNoDb PNoDb (\_ _ -> diePure)

mkSysReadEnv :: EvalEnv e -> Eval e (EvalEnv (EnvSysRead e))
mkSysReadEnv = mkPureEnv EnvSysRead PSysRead $ \d k e -> case d of
  KeySets -> withMVar e $ \(EnvSysRead EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar
  Modules -> withMVar e $ \(EnvSysRead EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar
  _ -> diePure e
