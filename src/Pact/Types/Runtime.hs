{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Runtime types: 'Eval' monad, 'PactDb' backend "funrec", exceptions.
-- Exports Lang and Util, so this is the "default import" for most pact things.
--

module Pact.Types.Runtime
 ( PactError(..),
   evalError,evalError',failTx,argsError,argsError',throwDbError,throwEither,
   Persistable(..),ToPersistable(..),
   ColumnId(..),
   RowKey(..),
   Columns(..),columns,
   Domain(..),
   TxLog(..),txDomain,txKey,txValue,
   WriteType(..),
   Method,
   PactDb(..),
   TxId(..),
   PactStep(..),psStep,psRollback,psTxId,psResume,
   ModuleData,
   RefStore(..),rsNatives,rsModules,updateRefStore,
   EntityName(..),
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeTxId,eeEntity,eePactStep,eePactDbVar,eePactDb,
   StackFrame(..),sfName,sfLoc,sfApp,
   PactYield(..),pyStepCount,pyYield,pyExecuted,
   RefState(..),rsLoaded,rsLoadedModules,rsNew,
   EvalState(..),evalRefs,evalCallStack,evalYield,
   Eval(..),runEval,runEval',
   call,method,
   readRow,writeRow,keys,txids,createUserTable,getUserTableInfo,beginTx,commitTx,rollbackTx,getTxLog,
   KeyPredBuiltins(..),keyPredBuiltins,
   module Pact.Types.Lang,
   module Pact.Types.Util,
   (<>)
   ) where


import Control.Arrow
import Control.Lens hiding (op,(.=))
import Control.Applicative
import Control.DeepSeq
import Data.List
import Control.Monad
import Prelude hiding (exp)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Set as S
import Data.String
import Data.Default
import Data.Thyme
import Data.Thyme.Format.Aeson ()
import Data.Thyme.Time.Core
import Data.Typeable
import Data.Word
import Control.Monad.Catch
import GHC.Generics
import Data.Decimal
import Control.Concurrent.MVar
import Data.Serialize (Serialize)
import Data.Semigroup
import Text.Read (readMaybe)
import Data.Hashable

import Pact.Types.Orphans ()
import Pact.Types.Lang
import Pact.Types.Util


data PactError =
    EvalError { peInfo :: Info, peText :: Text } |
    ArgsError { peInfo :: Info, peText :: Text } |
    DbError { peInfo :: Info, peText :: Text } |
    TxFailure { peInfo :: Info, peText :: Text } |
    SyntaxError { peInfo :: Info, peText :: Text }

instance Exception PactError

instance Show PactError where
    show (EvalError i s) = show i ++ ": Failure: " ++ unpack s
    show (ArgsError i s) = show i ++ ": " ++ show s
    show (TxFailure i s) = show i ++ ": Failure: Tx Failed: " ++ unpack s
    show (DbError i s) = show i ++ ": Failure: Database exception: " ++ unpack s
    show (SyntaxError i s) = show i ++ ": Failure: Syntax error: " ++ unpack s

mkArgsError :: FunApp -> [Term Name] -> Text -> PactError
mkArgsError FunApp {..} args s = ArgsError _faInfo $ pack $
  unpack s ++ ", received [" ++ intercalate "," (map abbrev args) ++ "] for " ++
            showFunTypes _faTypes


evalError :: MonadThrow m => Info -> String -> m a
evalError i = throwM . EvalError i . pack

evalError' :: MonadThrow m => FunApp -> String -> m a
evalError' = evalError . _faInfo

failTx :: MonadThrow m => Info -> String -> m a
failTx i = throwM . TxFailure i . pack

throwDbError :: MonadThrow m => String -> m a
throwDbError s = throwM $ DbError def (pack s)

-- | Throw an error coming from an Except/Either context.
throwEither :: (MonadThrow m,Exception e) => Either e a -> m a
throwEither = either throwM return


argsError :: MonadThrow m => FunApp -> [Term Name] -> m a
argsError i as = throwM $ mkArgsError i as "Invalid arguments"

argsError' :: MonadThrow m => FunApp -> [Term Ref] -> m a
argsError' i as = throwM $ mkArgsError i (map (toTerm.pack.abbrev) as) "Invalid arguments"



data KeyPredBuiltins = KeysAll|KeysAny|Keys2 deriving (Eq,Show,Enum,Bounded)
instance AsString KeyPredBuiltins where
  asString KeysAll = "keys-all"
  asString KeysAny = "keys-any"
  asString Keys2 = "keys-2"
keyPredBuiltins :: M.Map Text KeyPredBuiltins
keyPredBuiltins = M.fromList $ map (asString &&& id) [minBound .. maxBound]



-- | Min, max values that Javascript doesn't mess up.
--
--   http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html
--   "The integer part of the Number type in Javascript is safe in [-253 .. 253] (253 = 9 007 199 254 740 992).
--    Beyond this there will be precision loss on the least significant numbers."
jsIntegerBounds :: (Integer, Integer)
jsIntegerBounds = (-9007199254740991,9007199254740991)

-- | JSON codec pair.
data Codec a = Codec {
  encoder :: a -> Value,
  decoder :: Value -> Parser a
  }

integerCodec :: Codec Integer
integerCodec = Codec encodeInteger decodeInteger
  where
    encodeInteger i = let (l,h) = jsIntegerBounds in
                        if i >= l && i <= h then Number (fromIntegral i)
                        else object [ field .= show i ]
    {-# INLINE encodeInteger #-}
    decodeInteger (Number i) = return (round i)
    decodeInteger (Object o) = do
      s <- o .: field
      case readMaybe (unpack s) of
        Just i -> return i
        Nothing -> fail $ "Invalid integer value: " ++ show o
    decodeInteger v = fail $ "Invalid integer value: " ++ show v
    {-# INLINE decodeInteger #-}
    field = "_P_int"

decimalCodec :: Codec Decimal
decimalCodec = Codec enc dec
  where
    enc (Decimal dp dm) =
      object [ places .= dp,
               mantissa .= encoder integerCodec dm ]
    {-# INLINE enc #-}
    dec = withObject "Decimal" $ \o ->
      Decimal <$> o .: places <*>
      (o .: mantissa >>= decoder integerCodec)
    {-# INLINE dec #-}
    places = "_P_decp"
    mantissa = "_P_decm"

timeCodec :: Codec UTCTime
timeCodec = Codec enc dec
  where
    enc t = object [ day .= d,
                     micros .= encoder integerCodec (fromIntegral (toMicroseconds s)) ]
      where (UTCTime (ModifiedJulianDay d) s) = unUTCTime t
    {-# INLINE enc #-}
    dec = withObject "UTCTime" $ \o ->
      mkUTCTime <$> (ModifiedJulianDay <$> o .: day) <*>
      (fromMicroseconds . fromIntegral <$> (o .: micros >>= decoder integerCodec))
    {-# INLINE dec #-}
    day = "_P_timed"
    micros = "_P_timems"

valueCodec :: Codec Value
valueCodec = Codec enc dec
  where
    enc v = object [field .= v]
    {-# INLINE enc #-}
    dec = withObject "Value" $ \o -> o .: field
    {-# INLINE dec #-}
    field = "_P_val"

keysetCodec :: Codec KeySet
keysetCodec = Codec enc dec
  where
    enc (KeySet ks p) = object [ keyf .= ks, predf .= p ]
    {-# INLINE enc #-}
    dec  = withObject "KeySet" $ \o -> KeySet <$> o .: keyf <*> o .: predf
    {-# INLINE dec #-}
    keyf = "_P_keys"
    predf = "_P_pred"


-- | Represent Pact 'Term' values that can be stored in a database.
data Persistable =
    PLiteral Literal |
    PKeySet KeySet |
    PValue Value
    deriving (Eq,Generic)
instance Serialize Persistable
instance Show Persistable where
    show (PLiteral l) = show l
    show (PKeySet k) = show k
    show (PValue v) = BSL.toString $ encode v
instance ToTerm Persistable where
    toTerm (PLiteral l) = toTerm l
    toTerm (PKeySet k) = toTerm k
    toTerm (PValue v) = toTerm v
instance ToJSON Persistable where
    toJSON (PLiteral (LString s)) = String s
    toJSON (PLiteral (LBool b)) = Bool b
    toJSON (PLiteral (LInteger n)) = encoder integerCodec n
    toJSON (PLiteral (LDecimal d)) = encoder decimalCodec d
    toJSON (PLiteral (LTime t)) = encoder timeCodec t
    toJSON (PKeySet k) = encoder keysetCodec k
    toJSON (PValue v) = encoder valueCodec v
instance FromJSON Persistable where
    parseJSON (String s) = return (PLiteral (LString s))
    parseJSON (Number n) = return (PLiteral (LInteger (round n)))
    parseJSON (Bool b) = return (PLiteral (LBool b))
    parseJSON v@Object {} = (PLiteral . LInteger <$> decoder integerCodec v) <|>
                            (PLiteral . LDecimal <$> decoder decimalCodec v) <|>
                            (PLiteral . LTime <$> decoder timeCodec v) <|>
                            (PValue <$> decoder valueCodec v) <|>
                            (PKeySet <$> decoder keysetCodec v)
    parseJSON Null = return (PValue Null)
    parseJSON va@Array {} = return (PValue va)

class ToPersistable t where
  toPersistable :: t -> Persistable
instance ToPersistable Literal where toPersistable = PLiteral
instance ToPersistable KeySet where toPersistable = PKeySet
instance ToPersistable Value where toPersistable = PValue
instance Show n => ToPersistable (Term n) where
  toPersistable (TLiteral v _) = toPersistable v
  toPersistable (TKeySet ks _) = toPersistable ks
  toPersistable (TValue v _) = toPersistable v
  toPersistable t = toPersistable (toJSON t)

-- | Row key type for user tables.
newtype RowKey = RowKey Text
    deriving (Eq,Ord,IsString,ToTerm,AsString)
instance Show RowKey where show (RowKey s) = show s

-- | Column key type.
newtype ColumnId = ColumnId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show ColumnId where show (ColumnId s) = show s

-- | User table row-value type, mapping column ids to values.
newtype Columns v = Columns { _columns :: M.Map ColumnId v }
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (ToJSON v) => ToJSON (Columns v) where
    toJSON (Columns m) = object . map (\(k,v) -> asString k .= toJSON v) . M.toList $ m
    {-# INLINE toJSON #-}
instance (FromJSON v) => FromJSON (Columns v) where
    parseJSON = withObject "Columns" $ \o ->
                (Columns . M.fromList) <$>
                 forM (HM.toList o)
                  (\(k,v) -> ((,) <$> pure (ColumnId k) <*> parseJSON v))
    {-# INLINE parseJSON #-}

makeLenses ''Columns

-- | Specify key and value types for database domains.
data Domain k v where
  UserTables :: !TableName -> Domain RowKey (Columns Persistable)
  KeySets :: Domain KeySetName KeySet
  Modules :: Domain ModuleName Module
deriving instance Eq (Domain k v)
deriving instance Show (Domain k v)
instance AsString (Domain k v) where
    asString (UserTables t) = asString t
    asString KeySets = "SYS:KeySets"
    asString Modules = "SYS:Modules"

-- | Transaction record.
-- Backends are expected to return "user-visible" values
-- for '_txValue', namely that internal JSON formats for 'Persistable'
-- need to be converted to Term JSON formats.
data TxLog =
    TxLog {
      _txDomain :: !Text
    , _txKey :: !Text
    , _txValue :: !Value
    } deriving (Eq,Show,Typeable,Generic)
makeLenses ''TxLog
instance Hashable TxLog

instance ToJSON TxLog where
    toJSON (TxLog d k v) =
        object ["table" .= d, "key" .= k, "value" .= v]
instance FromJSON TxLog where
    parseJSON = withObject "TxLog" $ \o ->
                TxLog <$> o .: "table" <*> o .: "key" <*> o .: "value"

-- | Instruction for '_writeRow'.
data WriteType =
  -- | Insert a new row, fail if key already found.
  --   Requires complete row value, enforced by pact runtime.
  Insert |
  -- | Update an existing row, fail if key not found.
  --   Allows incomplete row values.
  Update |
  -- | Update an existing row, or insert a new row if not found.
  --   Requires complete row value, enforced by pact runtime.
  Write
  deriving (Eq,Show)

-- | Shape of back-end methods: use MVar for state, run in IO.
type Method e a = MVar e -> IO a

-- | Fun-record type for Pact back-ends.
data PactDb e = PactDb {
    -- | Read a domain value at key, throwing an exception if not found.
    _readRow :: forall k v . (IsString k,FromJSON v) =>
                Domain k v -> k -> Method e (Maybe v)
    -- | Write a domain value at key. WriteType argument governs key behavior.
  , _writeRow :: forall k v . (AsString k,ToJSON v) =>
                 WriteType -> Domain k v -> k -> v -> Method e ()
    -- | Retrieve all keys for user table.
  , _keys ::  TableName -> Method e [RowKey]
    -- | Retrieve all transaction ids greater than supplied txid for table.
  , _txids ::  TableName -> TxId -> Method e [TxId]
    -- | Create a user table.
  , _createUserTable ::  TableName -> ModuleName -> KeySetName -> Method e ()
    -- | Get module, keyset for user table.
  , _getUserTableInfo ::  TableName -> Method e (ModuleName,KeySetName)
    -- | Initiate transaction. If TxId not provided, commit fails/rolls back.
  , _beginTx :: Maybe TxId -> Method e ()
    -- | Commit transaction, if in tx. If not in tx, rollback and throw error.
    -- Return raw TxLogs, for use in checkpointing only (not for transmission to user).
  , _commitTx ::  Method e [TxLog]
    -- | Rollback database transaction.
  , _rollbackTx :: Method e ()
    -- | Get transaction log for table. TxLogs are expected to be user-visible format.
  , _getTxLog :: forall k v . (IsString k,FromJSON v) =>
                 Domain k v -> TxId -> Method e [TxLog]
}


-- | Transaction ids are non-negative 64-bit integers and
--   are expected to be monotonically increasing.
newtype TxId = TxId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON,Generic)

instance NFData TxId
instance Show TxId where show (TxId s) = show s
instance ToTerm TxId where toTerm = tLit . LInteger . fromIntegral

data PactStep = PactStep {
      _psStep :: !Int
    , _psRollback :: !Bool
    , _psTxId :: !TxId
    , _psResume :: !(Maybe (Term Name))
} deriving (Eq,Show)
makeLenses ''PactStep

type ModuleData = (Module,HM.HashMap Text Ref)

-- | Storage for loaded modules and natives.
data RefStore = RefStore {
      _rsNatives :: HM.HashMap Name Ref
    , _rsModules :: HM.HashMap ModuleName ModuleData
    } deriving (Eq,Show)
makeLenses ''RefStore
instance Default RefStore where def = RefStore HM.empty HM.empty


newtype EntityName = EntityName Text
  deriving (IsString,AsString,Eq,Ord,Hashable,Serialize,NFData,ToJSON,FromJSON,Default)
instance Show EntityName where show (EntityName t) = show t



data PactYield = PactYield
  { _pyStepCount :: Int
  , _pyYield :: !(Maybe (Term Name))
  , _pyExecuted :: Bool
  } deriving (Eq,Show)
makeLenses ''PactYield

-- | Interpreter reader environment, parameterized over back-end MVar state type.
data EvalEnv e = EvalEnv {
      -- | Environment references.
      _eeRefStore :: !RefStore
      -- | Verified keys from message.
    , _eeMsgSigs :: !(S.Set PublicKey)
      -- | JSON body accompanying message.
    , _eeMsgBody :: !Value
      -- | Transaction id.
    , _eeTxId :: !(Maybe TxId)
      -- | Entity governing 'pact' executions.
    , _eeEntity :: !EntityName
      -- | Step value for 'pact' executions.
    , _eePactStep :: !(Maybe PactStep)
      -- | Back-end state MVar.
    , _eePactDbVar :: MVar e
      -- | Back-end function record.
    , _eePactDb :: PactDb e
    } -- deriving (Eq,Show)
makeLenses ''EvalEnv

data StackFrame = StackFrame {
      _sfName :: !Text
    , _sfLoc :: !Info
    , _sfApp :: Maybe (FunApp,[Text])
    }
instance Show StackFrame where
    show (StackFrame n i a) = renderInfo i ++ ": " ++ unpack n ++ f a
        where f Nothing = ""
              f (Just (dd,as)) = ", " ++ show dd ++ ", values=" ++ show as
makeLenses ''StackFrame



-- | Dynamic storage for namespace-loaded modules, and new modules compiled in current tx.
data RefState = RefState {
      -- | Namespace-local defs.
      _rsLoaded :: HM.HashMap Name Ref
      -- | Modules that were loaded.
    , _rsLoadedModules :: HM.HashMap ModuleName Module
      -- | Modules that were compiled and loaded in this tx.
    , _rsNew :: [(ModuleName,ModuleData)]
    }
                deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty HM.empty def

-- | Update for newly-loaded modules.
updateRefStore :: RefState -> RefStore -> RefStore
updateRefStore RefState {..}
  | null _rsNew = id
  | otherwise = over rsModules $ HM.union $ HM.fromList _rsNew

-- | Interpreter mutable state.
data EvalState = EvalState {
      -- | New or imported modules and defs.
      _evalRefs :: !RefState
      -- | Current call stack.
    , _evalCallStack :: ![StackFrame]
      -- | Lame implementation of a yield coming back from a 'pact' execution.
    , _evalYield :: !(Maybe PactYield)
    }
makeLenses ''EvalState
instance Show EvalState where
    show (EvalState m y _) = "EvalState " ++ show m ++ " " ++ show y
instance Default EvalState where def = EvalState def def def

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
              [Handler (\(e :: SomeException) -> return $ Left . EvalError def . pack . show $ e),
               Handler (\(e :: PactError) -> return $ Left e)
              ]) s



-- | Bracket interpreter action pushing and popping frame on call stack.
call :: StackFrame -> Eval e a -> Eval e a
call s act = do
  evalCallStack %= (s:)
  r <- act
  evalCallStack %= \st -> case st of (_:as) -> as; [] -> []
  return r
{-# INLINE call #-}

-- | Invoke a backend method, catching all exceptions as 'DbError'
method :: Info -> (PactDb e -> Method e a) -> Eval e a
method i f = do
  EvalEnv {..} <- ask
  handleAll (throwM . DbError i . pack . show) (liftIO $ f _eePactDb _eePactDbVar)


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
commitTx :: Info -> Eval e [TxLog]
commitTx i = method i $ \db -> _commitTx db

-- | Invoke _rollbackTx
rollbackTx :: Info -> Eval e ()
rollbackTx i = method i $ \db -> _rollbackTx db

-- | Invoke _getTxLog
getTxLog :: (IsString k,FromJSON v) => Info -> Domain k v -> TxId -> Eval e [TxLog]
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
