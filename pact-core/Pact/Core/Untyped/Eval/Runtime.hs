{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Untyped.Eval.Runtime
 ( CEKTLEnv
 , CEKEnv
 , CEKRuntimeEnv(..)
 , BuiltinFn(..)
 , EvalM(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , mkBuiltinFn
 , cekBuiltins
 , cekLoaded
 , cekGasModel
 , fromPactValue
 , checkPactValueType
 , CEKErrorHandler(..)
 , MonadCEKEnv(..)
 , MonadCEK
 ) where


import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Data.Void
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Default
-- import Data.Set(Set)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Data.IORef
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Errors
import Pact.Core.Builtin
-- import Pact.Core.Hash
import Pact.Core.Untyped.Term
import Pact.Core.Literal
-- import Pact.Core.Persistence
import Pact.Core.Type
import qualified Pact.Core.Pretty as P

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
type CEKEnv b i m = RAList (CEKValue b i m)

-- | List of builtins
type BuiltinEnv b i m = b -> BuiltinFn b i m

-- | The type of our semantic runtime values
data CEKValue b i m
  = VLiteral !Literal
  | VList !(Vector (CEKValue b i m))
  | VClosure !(EvalTerm b i) !(CEKEnv b i m)
  | VNative !(BuiltinFn b i m)
  | VGuard !(Guard FullyQualifiedName (CEKValue b i m))
  | VError !Text
  deriving (Show)

type MonadCEK b i m = (MonadCEKEnv b i m, MonadError (PactError i) m, Default i)

class (Monad m) => MonadCEKEnv b i m | m -> b, m -> i where
  cekReadEnv :: m (CEKRuntimeEnv b i m)
  cekLogGas :: Text -> Gas -> m ()
  cekChargeGas :: Gas -> m ()

data EvalMEnv b i
  = EvalMEnv
  { _emRuntimeEnv :: CEKRuntimeEnv b i (EvalM b i)
  , _emGas :: IORef Gas
  , _emGasLog :: IORef (Maybe [(Text, Gas)])
  }

-- Todo: are we going to inject state as the reader monad here?
newtype EvalM b i a =
  EvalM (ExceptT (PactError i) (ReaderT (EvalMEnv b i) IO) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (EvalMEnv b i)
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ExceptT (PactError i) (ReaderT (EvalMEnv b i) IO))

runEvalT :: EvalMEnv b i -> EvalM b i a -> IO (Either (PactError i) a)
runEvalT s (EvalM action) = runReaderT (runExceptT action) s

data BuiltinFn b i m
  = BuiltinFn
  { _native :: b
  , _nativeFn :: (MonadCEK b i m) => [CEKValue b i m] -> m (CEKValue b i m)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeAppliedArgs :: [CEKValue b i m]
  }

mkBuiltinFn
  :: (BuiltinArity b)
  => ([CEKValue b i m] -> m (CEKValue b i m))
  -> b
  -> BuiltinFn b i m
mkBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkBuiltinFn #-}

data ExecutionMode
  = Transactional
  | Local
  deriving (Eq, Show, Bounded, Enum)

data Cont b i m
  = Fn (CEKValue b i m) (Cont b i m)
  | Arg (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  | SeqC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  | ListC (CEKEnv b i m) [EvalTerm b i] [CEKValue b i m] (Cont b i m)
  | Mt
  deriving Show


data CEKErrorHandler b i m
  = CEKNoHandler
  | CEKHandler (CEKEnv b i m) (EvalTerm b i) (Cont b i m) (CEKErrorHandler b i m)
  deriving Show

data CEKRuntimeEnv b i m
  = CEKRuntimeEnv
  { _cekBuiltins :: BuiltinEnv b i m
  , _cekLoaded :: CEKTLEnv b i
  , _cekGasModel :: GasEnv b
  --   _cekGas :: IORef Gas
  -- , _cekEvalLog :: IORef (Maybe [(Text, Gas)])
  -- , _ckeData :: EnvData PactValue
  -- , _ckeTxHash :: Hash
  -- , _ckeResolveName :: QualifiedName -> Maybe FullyQualifiedName
  -- , _ckeSigs :: Set PublicKey
  -- , _ckePactDb :: PactDb b i
  }

instance (Show i, Show b) => Show (BuiltinFn b i m) where
  show (BuiltinFn b _ arity args) = unwords
    ["(BuiltinFn"
    , show b
    , "#fn"
    , show arity
    , show args
    , ")"
    ]

instance (Pretty b, Show i, Show b) => Pretty (BuiltinFn b i m) where
  pretty = pretty . show

instance (Show i, Show b, Pretty b) => Pretty (CEKValue b i m) where
  pretty = \case
    VLiteral i ->
      pretty i
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> P.angles "guard#"
    VError e ->
      ("error " <> pretty e)

makeLenses ''CEKRuntimeEnv

fromPactValue :: PactValue -> CEKValue b i m
fromPactValue = \case
  PLiteral lit -> VLiteral lit
  PList vec -> VList (fromPactValue <$> vec)
  PGuard gu ->
    VGuard (fromPactValue <$> gu)

checkPactValueType :: Type Void -> PactValue -> Bool
checkPactValueType ty = \case
  PLiteral lit -> typeOfLit lit == ty
  PList vec -> case ty of
    TyList t -> V.null vec || all (checkPactValueType t) vec
    _ -> False
  PGuard _ -> ty == TyGuard

makeLenses ''EvalMEnv

instance MonadCEKEnv b i (EvalM b i) where
  cekReadEnv = view emRuntimeEnv
  cekLogGas msg g = do
    r <- view emGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))
  cekChargeGas g = do
    r <- view emGas
    liftIO (modifyIORef' r (+ g))
