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
 , runCEK
 , eval
 , Cont(..)
 ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Text(Text)
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Gas

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.Runtime

chargeGas :: Gas -> EvalT b i ()
chargeGas g = do
  ref <- view cekGas
  liftIO $ modifyIORef' ref (<> g)

chargeNodeGas :: NodeType -> EvalT b i ()
chargeNodeGas nt = do
  gm <- view (cekGasModel . geGasModel . gmNodes)
  chargeGas (gm nt)

chargeNative :: b -> EvalT b i ()
chargeNative native = do
  gm <- view (cekGasModel . geGasModel . gmNatives)
  chargeGas (gm native)

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: CEKEnv b i
  -> EvalTerm b i
  -> EvalT b i (CEKValue b i)
eval = evalCEK Mt
  where
  evalCEK
    :: Cont b i
    -> CEKEnv b i
    -> EvalTerm b i
    -> EvalT b i (CEKValue b i)
  evalCEK cont env (Var n _)  = do
    chargeNodeGas VarNode
    case _nKind n of
      NBound i -> case RAList.lookup env i of
        Just v -> returnCEK cont v
        Nothing -> failInvariant $ "unbound identifier" <> T.pack (show n)
        -- returnCEK cont (env RAList.!! i)
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> do
        let fqn = FullyQualifiedName mname (_nName n) mh
        views cekLoaded (Map.lookup fqn) >>= \case
          Just d -> evalCEK cont RAList.Nil (defTerm d)
          Nothing -> failInvariant "top level name not in scope"
  evalCEK cont _env (Constant l _) = do
    chargeNodeGas ConstantNode
    returnCEK cont (VLiteral l)
  evalCEK cont env (App fn arg _) = do
    chargeNodeGas AppNode
    evalCEK (Arg env arg cont) env fn
  evalCEK cont env (Lam body _) = do
    chargeNodeGas LamNode
    returnCEK cont (VClosure body env)
  evalCEK cont _env (Builtin b _) = do
    chargeNodeGas BuiltinNode
    builtins <- view cekBuiltins
    returnCEK cont (VNative (builtins b))
  evalCEK cont env (Sequence e1 e2 _) = do
    chargeNodeGas SeqNode
    evalCEK (SeqC env e2 cont) env e1
  evalCEK cont env (ListLit ts _) = do
    chargeNodeGas ListNode
    case ts of
      [] -> returnCEK cont (VList mempty)
      x:xs -> evalCEK (ListC env xs [] cont) env x
  returnCEK
    :: Cont b i
    -> CEKValue b i
    -> EvalT b i (CEKValue b i)
  returnCEK (Arg env arg cont) fn =
    evalCEK (Fn fn cont) env arg
  returnCEK (Fn fn ctx) arg =
    applyLam fn arg ctx
  returnCEK (SeqC env e cont) _ =
    evalCEK cont env e
  returnCEK (ListC env args vals cont) v = do
    case args of
      [] ->
        returnCEK cont (VList (V.fromList (reverse (v:vals))))
      e:es ->
        evalCEK (ListC env es (v:vals) cont) env e
  returnCEK Mt v = return v
  applyLam (VClosure body env) arg cont =
    evalCEK cont (RAList.cons arg env) body
  applyLam (VNative (BuiltinFn b fn arity args)) arg cont
    | arity - 1 == 0 = do
      chargeNative b
      fn (reverse (arg:args)) >>= returnCEK cont
    | otherwise = returnCEK cont (VNative (BuiltinFn b fn (arity - 1) (arg:args)))
  applyLam _ _ _ = failInvariant "Applying value to non-function"

runCEK
  :: CEKRuntimeEnv b i
  -- ^ runtime environment
  -> EvalTerm b i
  -- ^ Term to evaluate
  -> IO (CEKValue b i)
runCEK env term =
  runEvalT env (eval RAList.Nil term)

failInvariant :: Text -> EvalT b i a
failInvariant b =
  throwM (FatalExecutionError ("invariant failure, native arg failure: " <> b))
