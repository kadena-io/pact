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
   evalError,evalError',failTx,argsError,argsError',
   Persistable(..),
   ColumnId(..),
   RowKey(..),
   Columns(..),columns,
   Domain(..),
   TxLog(..),txDomain,txKey,txValue,
   WriteType(..),
   Method,
   PactDb(..),
   TxId(..),
   PactStep(..),
   ModuleData,
   RefStore(..),rsNatives,rsModules,
   EvalEnv(..),eeRefStore,eeMsgSigs,eeMsgBody,eeTxId,eeEntity,eePactStep,eePactDbVar,eePactDb,
   StackFrame(..),sfName,sfLoc,sfApp,
   PactYield(..),
   RefState(..),rsLoaded,rsLoadedModules,rsNew,
   EvalState(..),evalRefs,evalCallStack,evalYield,
   Eval(..),runEval,
   call,method,
   readRow,writeRow,keys,txids,createUserTable,getUserTableInfo,beginTx,commitTx,rollbackTx,getTxLog,
   PactDbException(..),
   throwDbError,
   module Pact.Types.Lang,
   module Pact.Types.Util,
   (<>)
   ) where


import Control.Lens hiding (op,(.=))
import Control.Applicative
import Data.List
import Control.Monad
import Prelude hiding (exp)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Set as S
import Data.String
import Data.Default
import Data.Thyme
import Data.Thyme.Format.Aeson ()
import Data.Thyme.Time.Core
import Data.Word
import Control.Monad.Catch
import GHC.Generics
import Data.Decimal
import qualified Data.Vector as V
import Control.Concurrent.MVar
import Data.Serialize (Serialize)
import Data.Semigroup

import Pact.Types.Orphans ()
import Pact.Types.Lang
import Pact.Types.Util


data PactError =
    EvalError Info Text |
    ArgsError FunApp [Term Name] Text |
    DbError Text |
    TxFailure Text

instance Show PactError where
    show (EvalError i s) = show i ++ ": " ++ unpack s
    show (ArgsError FunApp {..} args s) =
        show _faInfo ++ ": " ++ unpack s ++ ", received [" ++ intercalate "," (map abbrev args) ++ "] for " ++ showFunTypes _faTypes
    show (TxFailure s) = " Failure: " ++ unpack s
    show (DbError s) = " Failure: Database exception: " ++ unpack s

evalError :: MonadError PactError m => Info -> String -> m a
evalError i = throwError . EvalError i . pack

evalError' :: MonadError PactError m => FunApp -> String -> m a
evalError' = evalError . _faInfo

failTx :: MonadError PactError m => String -> m a
failTx = throwError . TxFailure . pack


argsError :: (MonadError PactError m) => FunApp -> [Term Name] -> m a
argsError i as = throwError $ ArgsError i as "Invalid arguments"

argsError' :: (MonadError PactError m) => FunApp -> [Term Ref] -> m a
argsError' i as = throwError $ ArgsError i (map (toTerm.pack.abbrev) as) "Invalid arguments"


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
    toJSON (PLiteral (LInteger n)) = Number (fromIntegral n)
    toJSON (PLiteral (LDecimal (Decimal dp dm))) =
        Array (V.fromList [Number (fromIntegral dp),Number (fromIntegral dm)])
    toJSON (PLiteral (LTime t)) =
        let (UTCTime (ModifiedJulianDay d) s) = unUTCTime t
        in Array (V.fromList ["t",Number (fromIntegral d),Number (fromIntegral (toMicroseconds s))])
    toJSON (PKeySet k) = toJSON k
    toJSON (PValue v) = Array (V.fromList ["v",v])
instance FromJSON Persistable where
    parseJSON (String s) = return (PLiteral (LString s))
    parseJSON (Number n) = return (PLiteral (LInteger (round n)))
    parseJSON (Bool b) = return (PLiteral (LBool b))
    parseJSON v@(Object _) = PKeySet <$> parseJSON v
    parseJSON Null = return (PValue Null)
    parseJSON va@(Array a) =
        case V.toList a of
          [Number dp,Number dm] -> return (PLiteral (LDecimal (Decimal (truncate dp) (truncate dm))))
          [String typ,Number d,Number s] | typ == "t" -> return $ PLiteral $ LTime $ mkUTCTime
                                                         (ModifiedJulianDay (truncate d))
                                                         (fromMicroseconds (truncate s))
          [String typ,v] | typ == "v" -> return (PValue v)
          _ -> return (PValue va)


newtype ColumnId = ColumnId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default)
instance Show ColumnId where show (ColumnId s) = show s

newtype RowKey = RowKey Text
    deriving (Eq,Ord,IsString,ToTerm,AsString)
instance Show RowKey where show (RowKey s) = show s


newtype Columns v = Columns { _columns :: M.Map ColumnId v }
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (ToJSON v) => ToJSON (Columns v) where
    toJSON (Columns m) = object . map (\(k,v) -> asString k .= toJSON v) . M.toList $ m
instance (FromJSON v) => FromJSON (Columns v) where
    parseJSON = withObject "Columns" $ \o ->
                (Columns . M.fromList) <$>
                 forM (HM.toList o)
                  (\(k,v) -> ((,) <$> pure (ColumnId k) <*> parseJSON v))

makeLenses ''Columns

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

data TxLog =
    TxLog {
      _txDomain :: !Text
    , _txKey :: !Text
    , _txValue :: !Value
    } deriving (Eq,Show)
makeLenses ''TxLog

instance ToJSON TxLog where
    toJSON (TxLog d k v) =
        object ["table" .= d, "key" .= k, "value" .= v]
instance FromJSON TxLog where
    parseJSON = withObject "TxLog" $ \o ->
                TxLog <$> o .: "table" <*> o .: "key" <*> o .: "value"

data WriteType = Insert|Update|Write deriving (Eq,Show)

-- | Shape of back-end methods: use MVar for state, run in IO.
type Method e a = MVar e -> IO a

-- | Fun-record type for Pact back-ends.
data PactDb e = PactDb {
      _readRow :: forall k v . (IsString k,FromJSON v) =>
                  Domain k v -> k -> Method e (Maybe v)
    , _writeRow :: forall k v . (AsString k,ToJSON v) =>
                   WriteType -> Domain k v -> k -> v -> Method e ()
    , _keys ::  TableName -> Method e [RowKey]
    , _txids ::  TableName -> TxId -> Method e [TxId]
    , _createUserTable ::  TableName -> ModuleName -> KeySetName -> Method e ()
    , _getUserTableInfo ::  TableName -> Method e (ModuleName,KeySetName)
    , _beginTx :: Method e ()
    , _commitTx ::  TxId -> Method e ()
    , _rollbackTx :: Method e ()
    , _getTxLog :: forall k v . (IsString k,FromJSON v) =>
                   Domain k v -> TxId -> Method e [TxLog]
}



newtype TxId = TxId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON)
instance Show TxId where show (TxId s) = show s
instance ToTerm TxId where toTerm = tLit . LInteger . fromIntegral

data PactStep = PactStep {
      _psStep :: !Int
    , _psRollback :: !Bool
    , _psTxId :: !TxId
} deriving (Eq,Show)

type ModuleData = (Module,HM.HashMap Text Ref)

data RefStore = RefStore {
      _rsNatives :: HM.HashMap Name Ref
    , _rsModules :: HM.HashMap ModuleName ModuleData
    } deriving (Eq,Show)
makeLenses ''RefStore
instance Default RefStore where def = RefStore HM.empty HM.empty

data EvalEnv e = EvalEnv {
      _eeRefStore :: !RefStore
    , _eeMsgSigs :: !(S.Set PublicKey)
    , _eeMsgBody :: !Value
    , _eeTxId :: !TxId
    , _eeEntity :: !Text
    , _eePactStep :: !(Maybe PactStep)
    , _eePactDbVar :: MVar e
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


data PactYield = PactYield {
      _pyNextStep :: !(Maybe Int)
    , _pyFailStep :: !(Maybe Int)
    } deriving (Eq,Show)
instance Default PactYield where def = PactYield def def


data RefState = RefState {
      _rsLoaded :: HM.HashMap Name Ref
    , _rsLoadedModules :: HM.HashMap ModuleName Module
    , _rsNew :: [(ModuleName,ModuleData)]
    }
                deriving (Eq,Show)
makeLenses ''RefState
instance Default RefState where def = RefState HM.empty HM.empty def

data EvalState = EvalState {
      _evalRefs :: !RefState
    , _evalCallStack :: ![StackFrame]
    , _evalYield :: !(Maybe PactYield)
    }
makeLenses ''EvalState
instance Show EvalState where
    show (EvalState m y _) = "EvalState " ++ show m ++ " " ++ show y
instance Default EvalState where def = EvalState def def def

newtype Eval e a =
    Eval { unEval :: ExceptT PactError (ReaderT (EvalEnv e) (StateT EvalState IO)) a }
    deriving (Functor,Applicative,Monad,MonadError PactError,MonadState EvalState,
                     MonadReader (EvalEnv e),MonadThrow,MonadCatch,MonadIO)

{-# INLINE runEval #-}
runEval :: EvalState -> EvalEnv e -> Eval e a ->
           IO (Either PactError a,EvalState)
runEval s env act = runStateT (runReaderT (runExceptT (unEval act)) env) s


call :: StackFrame -> Eval e a -> Eval e a
call s act = do
  evalCallStack %= (s:)
  r <- act
  evalCallStack %= \st -> case st of (_:as) -> as; [] -> []
  return r
{-# INLINE call #-}

-- | Invoke a backend method, catching all exceptions as 'DbError'
method :: (PactDb e -> Method e a) -> Eval e a
method f = do
  EvalEnv {..} <- ask
  handleAll (throwError . DbError . pack . show) (liftIO $ f _eePactDb _eePactDbVar)


readRow :: (IsString k,FromJSON v) => Domain k v -> k -> Eval e (Maybe v)
readRow d k = method $ \db -> _readRow db d k

writeRow :: (AsString k,ToJSON v) => WriteType -> Domain k v -> k -> v -> Eval e ()
writeRow w d k v = method $ \db -> _writeRow db w d k v

keys :: TableName -> Eval e [RowKey]
keys t = method $ \db -> _keys db t

txids :: TableName -> TxId -> Eval e [TxId]
txids tn tid = method $ \db -> _txids db tn tid

createUserTable :: TableName -> ModuleName -> KeySetName -> Eval e ()
createUserTable t m k = method $ \db -> _createUserTable db t m k

getUserTableInfo :: TableName -> Eval e (ModuleName,KeySetName)
getUserTableInfo t = method $ \db -> _getUserTableInfo db t

beginTx :: Eval e ()
beginTx = method $ \db -> _beginTx db

commitTx :: TxId -> Eval e ()
commitTx t = method $ \db -> _commitTx db t

rollbackTx :: Eval e ()
rollbackTx = method $ \db -> _rollbackTx db

getTxLog :: (IsString k,FromJSON v) => Domain k v -> TxId -> Eval e [TxLog]
getTxLog d t = method $ \db -> _getTxLog db d t

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


newtype PactDbException = PactDbException String deriving (Eq,Show)
instance Exception PactDbException

throwDbError :: MonadThrow m => String -> m a
throwDbError s = throwM $ PactDbException s
