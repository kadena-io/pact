{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Untyped.Utils where

import Data.Foldable(foldl')

import Pact.Core.Untyped.Term
import qualified Pact.Core.Typed.Term as Typed

fromTypedTerm :: Typed.Term name tyname b i -> Term name b i
fromTypedTerm = \case
  Typed.Var n i -> Var n i
  Typed.Lam args body i ->
    foldr (\_ t -> Lam t i) (fromTypedTerm body) args
  Typed.App fn apps i ->
    foldl' (\f arg -> App f (fromTypedTerm arg) i) (fromTypedTerm fn) apps
  Typed.Let _ e1 e2 i ->
    App (Lam (fromTypedTerm e2) i) (fromTypedTerm e1) i
  Typed.Builtin b i ->
    Builtin b i
  Typed.Constant lit i ->
    Constant lit i
  Typed.TyApp te _ _ ->
    fromTypedTerm te
  Typed.TyAbs _ term _ ->
    fromTypedTerm term
  Typed.Sequence e1 e2 i ->
    Sequence (fromTypedTerm e1) (fromTypedTerm e2) i
  Typed.ListLit _ vec i ->
    ListLit (fromTypedTerm <$> vec) i
  Typed.Try e1 e2 i ->
    Try (fromTypedTerm e1) (fromTypedTerm e2) i
  Typed.Error _ e i -> Error e i
  -- Typed.ObjectLit m i ->
  --   ObjectLit (fromTypedTerm <$> m) i
  -- Typed.ObjectOp oo i ->
  --   ObjectOp (fromTypedTerm <$> oo) i


fromTypedDefun
  :: Typed.Defun name tyname builtin info
  -> Defun name builtin info
fromTypedDefun (Typed.Defun n ty term i) =
  Defun n ty (fromTypedTerm term) i

fromTypedDConst
  :: Typed.DefConst name tyname builtin info
  -> DefConst name builtin info
fromTypedDConst (Typed.DefConst n ty term i) =
  DefConst n ty (fromTypedTerm term) i

-- fromTypedDCap
--   :: Typed.DefCap name NamedDeBruijn builtin info
--   -> DefCap name builtin info
-- fromTypedDCap (Typed.DefCap name args term captype ty info) =
--   DefCap name args (fromTypedTerm term) captype ty info

fromTypedDef
  :: Typed.Def name tyname builtin info
  -> Def name builtin info
fromTypedDef = \case
  Typed.Dfun d -> Dfun (fromTypedDefun d)
  Typed.DConst d -> DConst (fromTypedDConst d)
  -- Typed.DCap d -> DCap (fromTypedDCap d)

fromTypedModule
  :: Typed.Module name tyname builtin info
  -> Module name builtin info
fromTypedModule (Typed.Module mn defs blessed imports implements hs) =
  Module mn (fromTypedDef <$> defs) blessed imports implements hs

fromTypedTopLevel
  :: Typed.TopLevel name tyname builtin info
  -> TopLevel name builtin info
fromTypedTopLevel = \case
  Typed.TLModule m ->
    TLModule (fromTypedModule m)
  Typed.TLInterface _ ->
    error "todo: implement interfaces"
  Typed.TLTerm e ->
    TLTerm (fromTypedTerm e)
