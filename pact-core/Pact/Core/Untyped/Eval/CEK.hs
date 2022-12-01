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
import Control.Monad(when)
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
  gCurr <- liftIO (readIORef ref)
  gLimit <- view (cekGasModel . geGasLimit)
  let gUsed = g + gCurr
      msg = "Gas Limit (" <> T.pack (show gLimit) <> ") exceeeded: " <> T.pack (show gUsed)
  when (gUsed > gLimit) $ throwM (GasExceeded msg)

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
eval = evalCEK Mt CEKNoHandler
  where
  evalCEK
    :: Cont b i
    -> CEKErrorHandler b i
    -> CEKEnv b i
    -> EvalTerm b i
    -> EvalT b i (CEKValue b i)
  evalCEK cont handler env (Var n _)  = do
    chargeNodeGas VarNode
    case _nKind n of
      NBound i -> case RAList.lookup env i of
        Just v -> returnCEK cont handler v
        Nothing -> failInvariant $ "unbound identifier" <> T.pack (show n)
        -- returnCEK cont (env RAList.!! i)
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> do
        let fqn = FullyQualifiedName mname (_nName n) mh
        views cekLoaded (Map.lookup fqn) >>= \case
          Just d -> evalCEK cont handler RAList.Nil (defTerm d)
          Nothing -> failInvariant ("top level name " <> T.pack (show fqn) <> " not in scope")
  evalCEK cont handler _env (Constant l _) = do
    chargeNodeGas ConstantNode
    returnCEK cont handler (VLiteral l)
  evalCEK cont handler env (App fn arg _) = do
    chargeNodeGas AppNode
    evalCEK (Arg env arg cont) handler env fn
  evalCEK cont handler env (Lam body _) = do
    chargeNodeGas LamNode
    returnCEK cont handler (VClosure body env)
  evalCEK cont handler _env (Builtin b _) = do
    chargeNodeGas BuiltinNode
    builtins <- view cekBuiltins
    returnCEK cont handler (VNative (builtins b))
  evalCEK cont handler env (Sequence e1 e2 _) = do
    chargeNodeGas SeqNode
    evalCEK (SeqC env e2 cont) handler env e1
  evalCEK cont handler env (ListLit ts _) = do
    chargeNodeGas ListNode
    case ts of
      [] -> returnCEK cont handler (VList mempty)
      x:xs -> evalCEK (ListC env xs [] cont) handler env x
  evalCEK cont handler env (Try e1 rest _) = do
    let handler' = CEKHandler env e1 cont handler
    evalCEK Mt handler' env rest
  -- Error terms ignore the current cont
  evalCEK _ handler _ (Error e _) =
    returnCEK Mt handler (VError e)
  returnCEK
    :: Cont b i
    -> CEKErrorHandler b i
    -> CEKValue b i
    -> EvalT b i (CEKValue b i)
  returnCEK Mt handler v =
    case handler of
      CEKNoHandler -> return v
      CEKHandler env term cont' handler' -> case v of
        VError{} -> evalCEK cont' handler' env term
        _ -> returnCEK cont' handler' v
  -- Error terms that don't simply returnt the empty continuation
  -- "Zero out" the continuation up to the latest handler
  returnCEK _cont handler v@VError{} =
    returnCEK Mt handler v
  returnCEK (Arg env arg cont) handler fn =
    evalCEK (Fn fn cont) handler env arg
  returnCEK (Fn fn cont) handler arg =
    applyLam fn arg cont handler
  returnCEK (SeqC env e cont) handler _ =
    evalCEK cont handler env e
  returnCEK (ListC env args vals cont) handler v = do
    case args of
      [] ->
        returnCEK cont handler (VList (V.fromList (reverse (v:vals))))
      e:es ->
        evalCEK (ListC env es (v:vals) cont) handler env e
  applyLam (VClosure body env) arg cont handler =
    evalCEK cont handler (RAList.cons arg env) body
  applyLam (VNative (BuiltinFn b fn arity args)) arg cont handler
    | arity - 1 == 0 = do
      chargeNative b
      fn (reverse (arg:args)) >>= returnCEK cont handler
    | otherwise = returnCEK cont handler (VNative (BuiltinFn b fn (arity - 1) (arg:args)))
  applyLam _ _ _ _ = failInvariant "Applying value to non-function"

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
