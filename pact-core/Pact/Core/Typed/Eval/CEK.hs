{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

module Pact.Core.Typed.Eval.CEK where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Typed.Term
import Pact.Core.Typed.Eval.Runtime

import Pact.Types.Gas

data CEKState name b
  = CEKState
  { _cekLoaded :: CEKEnv name b
  , _cekGas :: Gas
  -- , _cekStack :: [StackFrame name]
  , _cekEvalLog :: Maybe [Text]
  } deriving Show

newtype Eval name b a =
  Eval { unEval :: ReaderT (CEKRuntime name b) (StateT (CEKState name b) IO) a }
  deriving (Functor,Applicative,Monad,MonadState (CEKState name b),
           MonadReader (CEKRuntime name b), MonadIO) via (ReaderT (CEKRuntime name b) (StateT (CEKState name b) IO))



type CEKEnv name b = Map name (CEKValue name b)
type BuiltinFn name b = [CEKValue name b] -> Eval name b (CEKValue name b)

data CEKValue name b
  = VLiteral Literal
  | VObject (Map Field (CEKValue name b))
  | VList (Vector (CEKValue name b))
  | VClosure name (NonEmpty name) (Term name () b ()) !(CEKEnv name b)
  | VPartialNative b Int [CEKValue name b]
  | VGuard (Guard name (CEKValue name b))
  | VCap name
  | VModRef
  | VError Text
  deriving (Show)

data Cont name b
  = Fn (CEKValue name b) (Cont name b)
  | Arg (CEKEnv name b) (NonEmpty (Term name () b ())) (Cont name b)
  | BlockC (CEKEnv name b) [Term name () b ()] (Cont name b)
  | Mt
  deriving Show

data CEKRuntime name b
  = CEKRuntime
  { cekBuiltins :: Map b (BuiltinFn name b, Int)
  , cekCallStack :: [StackFrame name]
  }

addFrame :: StackFrame name -> CEKRuntime name b -> CEKRuntime name b
addFrame frame (CEKRuntime bins callstack) =
  CEKRuntime bins (frame:callstack)

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: forall name b .
  (Ord name, Ord b)
  => Term name () b ()
  -> CEKEnv name b
  -> Cont name b
  -> Eval name b (CEKValue name b)
eval = evalCEK
  where
  evalCEK
    :: Term name () b ()
    -> CEKEnv name b
    -> Cont name b
    -> Eval name b (CEKValue name b)
  evalCEK (Var n _) env cont =
    returnCEK cont (env Map.! n)
  evalCEK (Constant l _) _ cont=
    returnCEK cont (VLiteral l)
  evalCEK (App fn arg _) env cont =
    evalCEK fn env (Arg env arg cont)
  evalCEK (Lam n ns _ body _) env cont =
    returnCEK cont (VClosure n ns body env)
  evalCEK (Builtin b _) _ cont = do
    CEKRuntime bEnv _ <- ask
    let (_, arity) =  bEnv Map.! b
    returnCEK cont (VPartialNative b arity [])
  evalCEK (ObjectLit _ obj _) env cont = do
    vs <- traverse (\o -> evalCEK o env Mt) obj
    returnCEK cont (VObject vs)
  evalCEK (Block (t :| ts) _) env cont = do
    evalCEK t env (BlockC env ts cont)
  evalCEK (ListLit _ ts _) env cont = do
    ts' <- traverse (\o -> evalCEK o env Mt) ts
    returnCEK cont (VList ts')
  evalCEK (Error s _ _) _ _= error s -- todo: proper error continuations, we actually have `try`
  evalCEK (TyApp t _ _) env cont =
    evalCEK t env cont
  evalCEK (TyAbs _ t _) env cont =
    evalCEK t env cont
  returnCEK
    :: Cont name b
    -> CEKValue name b
    -> Eval name b (CEKValue name b)
  returnCEK (Arg env args cont) fn =
    evalCEKArgs args env (Fn fn cont)
  returnCEK (Fn fn ctx) arg =
    applyLam fn (arg :| []) ctx
  returnCEK (BlockC env (t:ts) cont) _discarded =
    evalCEK t env (BlockC env ts cont)
  returnCEK (BlockC _ [] cont) v =
    returnCEK cont v
  returnCEK Mt v = return v
  evalCEKArgs args env cont = do
    args' <- traverse (\o -> evalCEK o env Mt) args
    returnCEKArgs args' cont
  returnCEKArgs args (Fn fn cont) =
    applyLam fn args cont
  returnCEKArgs _args _ =
    error "Invalid stack frame"
  applyLam (VClosure n ns body env) args cont =
    applyArgs n (NE.toList ns) (NE.toList args) env body cont
  applyLam (VPartialNative b arity currArgs) incArgs cont =
    if length currArgs + length incArgs >= arity
      then do
        CEKRuntime bEnv _ <- ask
        let (fn, _) = bEnv Map.! b
        fn (currArgs <> NE.toList incArgs) >>= returnCEK cont
      else returnCEK cont (VPartialNative b arity (currArgs <> NE.toList incArgs))
  applyLam _ _ _ = error "applying to non-fun"
  applyArgs lamn (!n : ns') (!arg : args') env body cont =
    applyArgs lamn ns' args' (Map.insert n arg env) body cont
  -- Todo: create stack frame here.
  -- function application is saturated.
  applyArgs lamn [] [] env body cont =
    local (addFrame (StackFrame lamn DTDefun)) $ evalCEK body env cont
  -- Args unsaturated, create a closure and return as an argument
  applyArgs lamn (n:ns') [] env body cont =
    returnCEK cont (VClosure lamn (n :| ns') body env)
  applyArgs _lamn [] (_args:_args') _env _body _cont =
    error "too many arguments in fn application"
    -- evalCEK body (Map.insert n arg env) cont
