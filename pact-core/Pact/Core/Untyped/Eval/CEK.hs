{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK Evaluator for untyped core.
--

module Pact.Core.Untyped.Eval.CEK
 ( CEKTLEnv
 , CEKEnv
 , CEKValue(..)
 , BuiltinFn(..)
 , CEKState(..)
 , CEKRuntime
 , runCEK
 , eval
 , Cont(..)
 ) where

import Control.Monad.Catch
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Primitive(Array, indexArray)
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Errors

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.Runtime

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: CEKRuntime b i
  => CEKEnv b i
  -> EvalTerm b i
  -> EvalT b (CEKValue b i)
eval = evalCEK Mt
  where
  evalCEK
    :: CEKRuntime b i
    => Cont b i
    -> CEKEnv b i
    -> EvalTerm b i
    -> EvalT b (CEKValue b i)
  evalCEK cont env (Var n _)  =
    case _nKind n of
      NBound i -> case RAList.lookup env i of
        Just v -> returnCEK cont v
        Nothing -> failInvariant $ "unbound identifier" <> T.pack (show n)
        -- returnCEK cont (env RAList.!! i)
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> let
        !fqn = FullyQualifiedName mname (_nName n) mh
        in case Map.lookup fqn ?cekLoaded of
          Just d -> evalCEK cont RAList.Nil (defTerm d)
          Nothing -> failInvariant "top level name not in scope"
  evalCEK cont _env (Constant l _)=
    returnCEK cont (VLiteral l)
  evalCEK cont env (App fn arg _) =
    evalCEK (Arg env arg cont) env fn
  evalCEK cont env (Lam body _) =
    returnCEK cont (VClosure body env)
  evalCEK cont _env (Builtin b _) =
    returnCEK cont (VNative (indexArray ?cekBuiltins (fromEnum b)))
  evalCEK cont env (Block (t :| ts) _) =
    evalCEK (BlockC env ts cont) env t
  evalCEK cont env (ListLit ts _) = case ts of
    [] -> returnCEK cont (VList mempty)
    x:xs -> evalCEK (ListC env xs [] cont) env x
  -- evalCEK _cont _env (ObjectLit _obj _) = undefined
  -- evalCEK _cont _env (ObjectOp _op _) = undefined
  returnCEK
    :: CEKRuntime b i
    => Cont b i
    -> CEKValue b i
    -> EvalT b (CEKValue b i)
  returnCEK (Arg env arg cont) fn =
    evalCEK (Fn fn cont) env arg
  returnCEK (Fn fn ctx) arg =
    applyLam fn arg ctx
  returnCEK (BlockC env (t:ts) cont) _discarded =
    evalCEK (BlockC env ts cont) env t
  returnCEK (BlockC _ [] cont) v =
    returnCEK cont v
  returnCEK (ListC env args vals cont) v = do
    case args of
      [] -> returnCEK cont (VList (V.fromList (reverse (v:vals))))
      e:es ->
        evalCEK (ListC env es (v:vals) cont) env e
  returnCEK Mt v = return v
  applyLam (VClosure body env) arg cont =
    evalCEK cont (RAList.cons arg env) body
  applyLam (VNative (BuiltinFn b fn arity args)) arg cont
    | arity - 1 == 0 = fn (reverse (arg:args)) >>= returnCEK cont
    | otherwise = returnCEK cont (VNative (BuiltinFn b fn (arity - 1) (arg:args)))
  applyLam _ _ _ = failInvariant "Applying value to non-function"

runCEK
  :: Enum b
  => CEKTLEnv b i
  -- ^ Top levels
  -> Array (BuiltinFn b i)
  -- ^ Builtins
  -> RuntimeEnv b i
  -- ^ runtime environment
  -> EvalTerm b i
  -- ^ Term to evaluate
  -> IO (CEKValue b i)
runCEK env builtins renv term = do
  let
    ?cekLoaded = env
    ?cekBuiltins = builtins
    ?cekRuntimeEnv = renv
  runEvalT CEKState (eval RAList.Nil term)

failInvariant :: Text -> EvalT b a
failInvariant b =
  throwM (FatalExecutionError ("invariant failure, native arg failure: " <> b))
