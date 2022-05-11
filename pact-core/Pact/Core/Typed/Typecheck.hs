{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


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
import Data.Maybe(catMaybes, fromMaybe)
import Data.Foldable(foldl', foldlM)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

import Pact.Core.Typed.Term
import Pact.Core.Type
import qualified Pact.Core.IR.Term as IR
-- import Pact.Core.Names


data TCEnv name tyname builtin = TCEnv
  { _tcTypeEnv :: Map.Map tyname (Type tyname)
  , _tcVarEnv :: Map.Map name (Type tyname)
  , _tcBuiltin :: builtin -> Type tyname }

makeLenses ''TCEnv

applyFunctionType :: (Ord n, MonadError String f) => Type n -> (Type n) -> f (Type n)
applyFunctionType (TyFun l r) l'
  | l == l' = pure r
  | otherwise  = throwError "function type mismatch"
applyFunctionType _ _ =
  throwError "term application to non-function"

applyType :: (MonadError String m, Ord n, Show n) => Type n -> (Type n, TyVarType) -> m (Type n)
applyType (TyForall tvs rvs tfty) (ty, TyVarType) = case tvs of
  [] -> throwError $ "No tyapps left to apply"
  t:ts -> pure $ TyForall ts rvs $ substInTy (Map.singleton t ty) $ tfty
applyType (TyForall tvs rvs tfty) (ty, RowVarType) = case rvs of
  [] -> throwError $ "No tyapps left to apply"
  r:rs -> pure $ TyForall tvs rs $ substInTy (Map.singleton r ty) tfty
applyType t1 tyr =
  throwError $ "Cannot apply: " <> show t1 <> " to: " <> show tyr

substInTy :: Ord n => Map.Map n (Type n) -> Type n -> Type n
substInTy s (TyVar n) =
  case Map.lookup n s of
    Just ty -> ty
    Nothing -> TyVar n
substInTy s (TyFun l r) =
  TyFun (substInTy s l) (substInTy s r)
substInTy s (TyRow row) = TyRow (substInRow row)
  where
    substInRow (RowVar rv) =
      case Map.lookup rv s of
        Just (TyRow r) -> r
        _ -> row
    substInRow (RowTy obj (Just rv)) =
      case Map.lookup rv s of
        Just (TyRow r) -> case r of
          RowVar rv' -> RowTy obj (Just rv')
          RowTy obj' mrv ->
            RowTy (Map.union obj obj') mrv
          EmptyRow -> RowTy obj Nothing
        _ -> row
    substInRow _ = row
substInTy _ t = t

typecheck'
  :: (MonadReader (TCEnv name tyname builtin) m,
      MonadError String m,
      Ord tyname, Ord name, Show tyname)
  => Term name tyname builtin info
  -> m (Type tyname)
typecheck' = \case
  --   x:t ∈ Γ
  -- ----------- (T-Var)
  --  Γ ⊢ x:t
  Var n _ -> do
    view (tcVarEnv . at n) >>= \case
      Nothing -> throwError "Found free var"
      Just k -> pure k
  -- Γ, x:t1 ⊢ e:t2
  -- ------------------- (T-Abs)
  -- Γ ⊢ (λx:t1.e):t1→t2
  Lam _ ns ts term _ -> do
    let args = NE.zip ns ts
        inEnv e = foldl' (\m (n', t') -> Map.insert n' t' m) e args
    ret <- locally tcVarEnv inEnv (typecheck' term)
    pure $ foldr TyFun ret ts
  -- Γ ⊢ x:t1→t2     Γ ⊢ y:t1
  -- ------------------------ (T-App  )
  -- Γ ⊢ (x y):t2
  App app term _ -> do
    tf <- typecheck' app
    targs <- traverse typecheck' term
    foldlM applyFunctionType tf targs
  -- Γ ⊢ x:∀X.t1
  -- ------------------------ (T-TApp)
  -- Γ ⊢ x[τ]:t1[X→τ]
  TyApp term tyApps _ -> do
    ty <- typecheck' term
    foldlM applyType ty tyApps
  -- Γ,X_1,..,X_n ⊢ e:t1
  -- ------------------------ (T-TAbs)
  -- Γ ⊢ ΛX_1,..,X_n.e : ∀X.t1
  -- BIG TODO: ROW APPS
  TyAbs tn term _ -> do
    typ <- typecheck' term
    let (tvms, rvms) = NE.unzip (tvAbs <$> tn)
        tvs = catMaybes (NE.toList tvms)
        rvs = catMaybes (NE.toList rvms)
    pure (TyForall tvs rvs typ)
    where
    tvAbs (tv, tvt) = case tvt of
      TyVarType -> (Just tv, Nothing)
      RowVarType -> (Nothing, Just tv)

  -- ------------------------ (T-Error)
  -- Γ ⊢ (error msg t1) : t1
  Error _ t _ ->
    pure t
  -- b : t ∈ B                (where B = { b : t | for b in Builtins, t in T})
  -- ------------------------ (T-Builtin)
  -- Γ ⊢ b : t
  Builtin b _ ->
    views tcBuiltin ($ b)
  -- a_1, ..., a_n : t
  -- ------------------------ (T-List)
  -- Γ ⊢ [a_1, ..., a_n] @ t : List t
  ListLit ty ts _ -> do
    tys <- traverse typecheck' ts
    if any (/= ty) tys then throwError "List literal type element mismatch"
    else pure ty
  -- todo: objectlit type ann seems unnecessary
  -- a_1, ..., a_n : t
  -- ------------------------ (T-Builtin)
  -- Γ ⊢ [a_1, ..., a_n] : List t
  ObjectLit rowTy fields _ -> do
    fields' <- traverse typecheck' fields
    if TyRow rowTy /= TyRow (RowTy fields' Nothing) then throwError "Object literal does not match with inferred type"
    else pure $ TyRow (RowTy fields' Nothing)

  -- e_1:t_1, ... , e_n : t_n
  -- ------------------------ (T-Builtin)
  -- Γ ⊢  {e_1:t_1,...,e_n:t_n} : t_n
  Block terms _ ->
    NE.last <$> traverse typecheck' terms
  -- k : p ∈ K                (where K = builtin types, constants)
  -- ------------------------ (T-Const)
  -- Γ ⊢ k : Prim p
  Constant l _ ->
    pure (typeOfLit l)

fromIR
  :: Ord tyname
  => IO name
  -> IR.Term (name, Type tyname) tyname builtin (i, Type tyname)
  -> IO (Term name tyname builtin i)
fromIR mkName = \case
  IR.Var (n, _) (i, _) -> pure (Var n i)
  -- Todo: for generalized nodes, tyAbs inserted here.
  IR.Lam (n, ty) ns _ body (i, _) -> do
    t <- Lam n (fst <$> ns) (snd <$> ns) <$> fromIR mkName body <*> pure i
    pure (mkAbsLam i ty t)
  -- Todo: type apps here.
  IR.App l r (i, _) ->
    mkTyApp l r i
  IR.Let (n, typ) _ e1 e2 (i, _) ->
    collectLets e2 (pure (n, typ)) (pure e1) i
  IR.Constant l (i, _) -> pure (Constant l i)
  IR.Block terms (i, _) ->
    Block <$> traverse (fromIR mkName) terms <*> pure i
  IR.Error errstr (i, ty) ->
    pure (Error errstr ty i)
  IR.ObjectLit objs (i, rty) -> do
    objs' <- traverse (fromIR mkName) objs
    case rty of
      TyRow row ->
        pure (ObjectLit row objs' i)
      _ -> error "internal, fatal error: type inferred for object not a row type"
  IR.ListLit objs (i, ty) ->
    ListLit ty <$> traverse (fromIR mkName) objs <*> pure i
  IR.Builtin b (i, _) -> pure (Builtin b i)
  IR.DynAccess _mod _fn _i -> error "unsupported atm"
  where
  mkAbsLam _ (TyForall [] [] _) lam = lam
  -- It is guaranteed that the concat of both is not empty
  mkAbsLam i (TyForall xs ys _) lam =
    let tyAbs = (,TyVarType) <$> xs
        rowAbs = (,RowVarType) <$> ys
        allAbs = NE.fromList (tyAbs ++ rowAbs)
    in TyAbs allAbs lam i
  mkAbsLam _ _ lam = lam
  collectLets (IR.Let (n, typ) _ e1 e2 _) ns apps i =
    collectLets e2 (NE.cons (n, typ) ns) (NE.cons e1 apps) i
  collectLets t ans appArgs i = do
    name <- mkName
    let (ns, tys) = NE.unzip ans
    lamT <- Lam name ns tys <$> fromIR mkName t <*> pure i
    appArgs' <- traverse (fromIR mkName) appArgs
    let lamAppArgs = uncurry (mkAbsLam i) <$> NE.zip tys appArgs'
    pure (App lamT lamAppArgs i)
  mkTyApp l@(IR.Lam (_, ty) _ _ _ _) appArgs i = case ty of
    TyForall [] [] _ ->
      App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i
    TyForall tvs rvs typ -> case tyFunToArgList typ of
      Just (NE.fromList -> funArgs, _) -> do
        let argTys = view (IR.termInfo._2) <$> appArgs
            substs = foldMap (uncurry tyAppUnify) $ NE.zip funArgs argTys
            tvApps = (\n ->  fromMaybe (TyVar n, TyVarType) (Map.lookup n substs)) <$> tvs
            rvApps = (\n -> fromMaybe (TyRow (RowVar n), RowVarType) (Map.lookup n substs)) <$> rvs
        l' <- fromIR mkName l
        appArgs' <- traverse (fromIR mkName) appArgs
        let tyApp = (TyApp l' (NE.fromList (tvApps ++ rvApps)) i)
        pure (App tyApp appArgs' i)
      Nothing -> error "oop???"
    _ ->
      App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i
  mkTyApp l appArgs i =
    App <$> fromIR mkName l <*> traverse (fromIR mkName) appArgs <*> pure i

-- Left biased unification.
-- It is only to generate type/row applications
tyAppUnify :: (Ord n) => Type n -> Type n -> Map n (Type n, TyVarType)
tyAppUnify (TyVar n) ty = Map.singleton n (ty, TyVarType)
tyAppUnify (TyFun l r) (TyFun l' r') =
  tyAppUnify l l' <> tyAppUnify r r'
tyAppUnify (TyList l) (TyList r) =
  tyAppUnify l r
tyAppUnify (TyRow lrow) (TyRow rrow) = rowUnifies lrow rrow
  where
  rowUnifies (RowVar r) r' = Map.singleton r (TyRow r', RowVarType)
  rowUnifies (RowTy obj (Just r)) (RowTy obj' r') =
    let notInL = Map.difference obj' obj
    in Map.singleton r (TyRow (RowTy notInL r'), RowVarType)
  rowUnifies (RowTy _ (Just r)) EmptyRow =
    Map.singleton r (TyRow EmptyRow, RowVarType)
  rowUnifies (RowTy _ (Just r)) rv =
    Map.singleton r (TyRow rv, RowVarType)
  rowUnifies _ _ = Map.empty
tyAppUnify _ _ = Map.empty
