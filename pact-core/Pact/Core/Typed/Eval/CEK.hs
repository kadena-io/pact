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

module Pact.Core.Typed.Eval.CEK where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import qualified Data.Map.Strict as Map

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Typed.Term

import Pact.Types.Gas


data CEKState name b
  = CEKState
  { _cekLoaded :: CEKEnv name b
  , _cekGas :: Gas
  -- , _cekPactExec :: Maybe
  , _cekEvalLog :: Maybe []
  }

newtype Eval e name b =
  Eval { unEval :: ReaderT (CEKRuntime name b) (StateT IO CEKState IO) }
  deriving


type CEKEnv name b = Map name (CEKValue name b)
type BuiltinFn name b = [CEKValue name b] -> Eval name b (CEKValue name b)

data CEKValue name b
  = VLiteral Literal
  | VObject (Map Field (CEKValue name b))
  | VList (Vector (CEKValue name b))
  | VClosure name (Term name () b ()) !(CEKEnv name b)
  | VPartialNative b Int [CEKValue name b]
  | VGuard (Guard name (CEKValue name b))
  | VCap name
  | VModRef
  | VError Text
  deriving (Show)

data Cont name b
  = Fn (CEKValue name b) (Cont name b)
  | Arg (CEKEnv name b) (Term name () b ()) (Cont name b)
  | Mt
  deriving Show

newtype CEKRuntime name b
  = CEKRuntime
  { cekBuiltins :: Map b (BuiltinFn name b, Int)
  }

-- Todo: exception handling? do we want labels
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
  evalCEK (Lam n _ body _) env cont =
    returnCEK cont (VClosure n body env)
  evalCEK (Builtin b _) _ cont = do
    CEKRuntime bEnv <- ask
    let (_, arity) =  bEnv Map.! b
    returnCEK cont (VPartialNative b arity [])
  evalCEK (ObjectLit _ obj _) env cont = do
    vs <- traverse (\o -> evalCEK o env cont) obj
    returnCEK cont (VObject vs)
  evalCEK (ListLit _ ts _) env cont = do
    ts' <- traverse (\o -> evalCEK o env cont) ts
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
  returnCEK Mt v = return v
  returnCEK (Arg env arg cont) fn =
    evalCEK arg env (Fn fn cont)
  returnCEK (Fn fn ctx) arg =
    applyLam fn arg ctx
  applyLam (VClosure n body env) arg cont =
    evalCEK body (Map.insert n arg env) cont
  applyLam (VPartialNative b arity args) arg cont =
    if length args + 1 >= arity
      then do
        CEKRuntime bEnv <- ask
        let (fn, _) = bEnv Map.! b
        fn (reverse (arg:args)) >>= returnCEK cont
      else returnCEK cont (VPartialNative b arity (arg:args))
  applyLam _ _ _ = error "applying to non-fun"
