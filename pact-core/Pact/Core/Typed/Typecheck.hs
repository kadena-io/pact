{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


-- |
-- Module      :  Pact.Core.Typed.Typecheck
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Jose Cardona <jose@kadena.io>
--
-- Typed Core Typechecker, which is a simple system F typechecker.
--

module Pact.Core.Typed.Typecheck where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
-- import qualified Data.Map.Strict as Map

import Pact.Core.Typed.Term
import Pact.Core.Type ( typeOfLit, Type(TyForall, TyFun, TyVar) )
import Pact.Core.Names


data TCEnv builtin tyname = TCEnv
  { _tcTypeEnv :: UniqueMap (Type tyname)
  , _tcVarEnv :: UniqueMap (Type tyname)
  , _tcBuiltin :: builtin -> Type tyname }

makeLenses ''TCEnv

applyFunctionType :: (Ord n, MonadError String f) => Type n -> Type n -> f (Type n)
applyFunctionType (TyFun l r) l'
  | l == l' = pure r
  | otherwise  = throwError "function type mismatch"
applyFunctionType _ _ =
  throwError "term application to non-function"

-- tyApp :: (MonadError String m, Ord n, Show n) => Type n -> Type n -> m (Type n)
tyApp :: (MonadError String m, Show n, Show a1) => Type n -> a1 -> m a2
tyApp (TyForall l r _tfty) _ty = case (l, r) of
  ([], []) -> undefined
  (_, _) -> undefined
    -- case ns of
    -- [] -> pure $ substInTy (Map.singleton n ty) tfty
    -- _ -> pure $ TyForall ns $ substInTy (Map.singleton n ty) tfty
tyApp t1 tyr =
  throwError $ "Cannot apply: " <> show t1 <> " to: " <> show tyr


typecheck'
  :: ( HasUnique name, HasUnique tyname,
      MonadReader (TCEnv builtin tyname) m,
      MonadError String m,
      Ord tyname, Show tyname)
  => Term name tyname builtin info
  -> m (Type tyname)
typecheck' = \case
  --   x:t ∈ Γ
  -- ----------- (T-Var)
  --  Γ ⊢ x:t
  Var n _ -> do
    let u = n ^. unique
    view (tcVarEnv . at u) >>= \case
      Nothing -> throwError "Found free var"
      Just k -> pure k
  -- Γ, x:t1 ⊢ e:t2
  -- ------------------- (T-Abs)
  -- Γ ⊢ (λx:t1.e):t1→t2
  Lam n t term _ ->
    TyFun t <$> locally tcVarEnv (set (at (n ^. unique)) (Just t)) (typecheck' term)
  -- Γ ⊢ x:t1→t2     Γ ⊢ y:t1
  -- ------------------------ (T-Abs)
  -- Γ ⊢ (x y):t2
  App app term _ -> do
    tf <- typecheck' app
    targ <- typecheck' term
    applyFunctionType tf targ
  -- Γ ⊢ x:∀X.t1
  -- ------------------------ (T-TApp)
  -- Γ ⊢ x[τ]:[X→τ]t1
  TyApp term ty _ ->
    flip tyApp ty =<< typecheck' term
  -- Γ,X ⊢ x:t1
  -- ------------------------ (T-TAbs)
  -- Γ ⊢ ΛX.x:∀X.t1
  -- BIG TODO: ROW APPS
  TyAbs tn term _ ->
    locally tcTypeEnv (set (at (tn ^. unique)) (Just (TyVar tn))) (typecheck' term) >>= \case
      TyForall xs rs ty -> pure $ TyForall (tn:xs) rs ty
      t -> pure $ TyForall [tn] [] t
  -- ------------------------ (T-Error)
  -- Γ ⊢ (error msg t1) : t1
  Error _ t _ ->
    pure t
  -- b : t ∈ B                (where B = { b : t | for b in Builtins, t in T})
  -- ------------------------ (T-Builtin)
  -- Γ ⊢ b : t1
  Builtin b _ ->
    views tcBuiltin ($ b)
  -- k : p ∈ K                (where K = builtin types, constants)
  -- ------------------------ (T-Const)
  -- Γ ⊢ k : Prim p
  Constant l _ ->
    pure (typeOfLit l)



