{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}


module Pact.Core.Typed.Overload where

import Data.List.NonEmpty(NonEmpty(..))

import Pact.Core.Type
import Pact.Core.Typed.Term
import Pact.Core.Builtin


pattern TypeApp :: Type n -> (Type n, TyVarType)
pattern TypeApp ty = (ty, TyVarType)

pattern RowApp :: Type n -> (Type n, TyVarType)
pattern RowApp ty = (ty, RowVarType)

pattern Single :: a -> NonEmpty a
pattern Single v = v :| []

resolveOverload :: Monad m => Term name tyname RawBuiltin info -> m (Term name tyname CoreBuiltin info)
resolveOverload = \case
  Var n i ->
    pure (Var n i)
  Lam n ns ts e i ->
    Lam n ns ts <$> resolveOverload e <*> pure i
  App f args i ->
    App <$> resolveOverload f <*> traverse resolveOverload args <*> pure i
  TyApp (Builtin b i1) ts i2 ->
    TyApp <$> fmap (`Builtin` i1) (resolveBuiltin b ts)  <*> pure ts <*> pure i2
  TyApp l rs i ->
    TyApp <$> resolveOverload l <*> pure rs <*> pure i
  TyAbs nel t i ->
    TyAbs nel <$> resolveOverload t <*> pure i
  Block nel i ->
    Block <$> traverse resolveOverload nel <*> pure i
  ObjectLit r ms i ->
    ObjectLit r <$> traverse resolveOverload ms <*> pure i
  ListLit tn ts i ->
    ListLit tn <$> traverse resolveOverload ts <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  Builtin b i ->
    Builtin <$> solveNonOverloaded b <*> pure i
  Error t1 t2 i ->
    pure (Error t1 t2 i)
  where
  solveNonOverloaded = undefined
  resolveBuiltin RawAdd (Single (TypeApp TyInt)) = pure AddInt
  resolveBuiltin _ _ = undefined


