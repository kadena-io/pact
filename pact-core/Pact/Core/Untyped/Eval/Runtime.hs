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

module Pact.Core.Untyped.Eval.Runtime
 ( CEKTLEnv
 , CEKEnv
 , HasTLEnv
 , HasBuiltinEnv
 , HasRuntimeEnv
 , CEKRuntime
 , BuiltinFn(..)
 , CEKState(..)
 , EvalT(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , RuntimeEnv(..)
--  , ckeData
--  , ckeTxHash
--  , ckeResolveName
--  , ckePactDb
--  , ckeSigs
 , ckeGas
 , ckeEvalLog
 , fromPactValue
 , checkPactValueType
 , Closure(..)
 ) where


import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Void
import Data.Text(Text)
import Data.Map.Strict(Map)
-- import Data.Set(Set)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Data.Primitive(Array)
import Data.IORef(IORef)
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import Pact.Core.PactValue
-- import Pact.Core.Hash
import Pact.Core.Untyped.Term
import Pact.Core.Literal
-- import Pact.Core.Persistence
import Pact.Core.Type
import qualified Pact.Core.Pretty as P

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)
-- | Locally bound variables
type CEKEnv b i = RAList (CEKValue b i)

-- | Top level constraint
type HasTLEnv b i = (?cekLoaded :: CEKTLEnv b i)
-- | List of builtins
type HasBuiltinEnv b i = (?cekBuiltins :: Array (BuiltinFn b i))
-- | runtime env
type HasRuntimeEnv b i = (?cekRuntimeEnv :: RuntimeEnv b i)

type CEKRuntime b i = (HasTLEnv b i, HasBuiltinEnv b i, HasRuntimeEnv b i, Enum b)


data Closure b i
  = Closure !(EvalTerm b i) !(CEKEnv b i)
  deriving Show

-- | The type of our semantic runtime values
data CEKValue b i
  = VLiteral !Literal
  | VList !(Vector (CEKValue b i))
  | VClosure !(EvalTerm b i) !(CEKEnv b i)
  | VNative !(BuiltinFn b i)
  | VGuard !(Guard FullyQualifiedName (CEKValue b i))
  deriving (Show)

data CEKState b
  = CEKState deriving Show
  -- { _cekGas :: IORef Gas
  -- , _cekEvalLog :: IORef (Maybe [Text])
  -- } deriving Show

newtype EvalT b a =
  EvalT (ReaderT (CEKState b) IO a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (CEKState b)
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ReaderT (CEKState b) IO)

runEvalT :: CEKState b -> EvalT b a -> IO a
runEvalT s (EvalT action) = runReaderT action s

data BuiltinFn b i
  = BuiltinFn
  { _native :: b
  , _nativeFn :: CEKRuntime b i => [CEKValue b i] -> EvalT b (CEKValue b i)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeAppliedArgs :: [CEKValue b i]
  }

data ExecutionMode
  = Transactional
  | Local
  deriving (Eq, Show, Bounded, Enum)

data Cont b i
  = Fn (CEKValue b i) (Cont b i)
  | Arg (CEKEnv b i) (EvalTerm b i) (Cont b i)
  | SeqC (CEKEnv b i) (EvalTerm b i) (Cont b i)
  | ListC (CEKEnv b i) [EvalTerm b i] [CEKValue b i] (Cont b i)
  | Mt
  deriving Show

data RuntimeEnv b i
  = RuntimeEnv
  { _ckeGas :: IORef Gas
  , _ckeEvalLog :: IORef (Maybe [(Text, Gas)])
  -- , _ckeData :: EnvData PactValue
  -- , _ckeTxHash :: Hash
  -- , _ckeResolveName :: QualifiedName -> Maybe FullyQualifiedName
  -- , _ckeSigs :: Set PublicKey
  -- , _ckePactDb :: PactDb b i
  }

instance (Pretty b) => Show (BuiltinFn b i) where
  show (BuiltinFn b _ arity args) = unwords
    ["(BuiltinFn"
    , show (pretty b)
    , "#fn"
    , show arity
    , show (pretty args)
    , ")"
    ]

instance (Pretty b) => Pretty (BuiltinFn b i) where
  pretty = pretty . show

instance Pretty b => Pretty (CEKValue b i) where
  pretty = \case
    VLiteral i -> pretty i
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> P.angles "guard#"

makeLenses ''RuntimeEnv


fromPactValue :: PactValue -> CEKValue b i
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
