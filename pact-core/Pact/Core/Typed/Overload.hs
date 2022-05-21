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
  resolveBuiltin RawAdd ts = case ts of
    Single (TypeApp TyInt) -> pure AddInt
    Single (TypeApp TyDecimal) -> pure AddDec
    Single (TypeApp TyString) -> pure AddStr
    Single (TypeApp (TyList _t)) -> pure AddList
    _ -> error "unable to resolve add"
  resolveBuiltin RawSub ts = case ts of
    Single (TypeApp TyInt) -> pure SubInt
    Single (TypeApp TyDecimal) -> pure SubDec
    _ -> error "unable to resolve sub"
  resolveBuiltin RawMultiply ts = case ts of
    Single (TypeApp TyInt) -> pure MulInt
    Single (TypeApp TyDecimal) -> pure MulDec
    _ -> error "unable to resolve multiply"
  resolveBuiltin RawDivide ts = case ts of
    Single (TypeApp TyInt) -> pure MulInt
    Single (TypeApp TyDecimal) -> pure MulDec
    _ -> error "unable to resolve divide"
  resolveBuiltin RawNegate ts = case ts of
    Single (TypeApp TyInt) -> pure NegateInt
    Single (TypeApp TyDecimal) -> pure NegateDec
    _ -> error "unable to resolve negate"
  resolveBuiltin RawAbs ts = case ts of
    Single (TypeApp TyInt) -> pure AbsInt
    Single (TypeApp TyDecimal) -> pure AbsDec
    _ -> error "unable to resolve negate"
  resolveBuiltin RawEq ts = case ts of
    Single (TypeApp TyInt) -> pure EqInt
    Single (TypeApp TyDecimal) -> pure EqDec
    Single (TypeApp TyString) -> pure EqStr
    Single (TypeApp (TyList _)) -> pure EqList
    Single (TypeApp (TyRow _)) -> pure EqObj
    _ -> error "unable to resolve eq"
  resolveBuiltin RawGT ts = case ts of
    Single (TypeApp TyInt) -> pure GTInt
    Single (TypeApp TyDecimal) -> pure GTDec
    Single (TypeApp TyString) -> pure GTStr
    _ -> error "unable to resolve gt"
  resolveBuiltin RawGEQ ts = case ts of
    Single (TypeApp TyInt) -> pure GEQInt
    Single (TypeApp TyDecimal) -> pure GEQDec
    Single (TypeApp TyString) -> pure GEQStr
    _ -> error "unable to resolve geq"
  resolveBuiltin RawLT ts = case ts of
    Single (TypeApp TyInt) -> pure LTInt
    Single (TypeApp TyDecimal) -> pure LTDec
    Single (TypeApp TyString) -> pure LTStr
    _ -> error "unable to resolve lt"
  resolveBuiltin RawLEQ ts = case ts of
    Single (TypeApp TyInt) -> pure LEQInt
    Single (TypeApp TyDecimal) -> pure LEQDec
    Single (TypeApp TyString) -> pure LEQStr
    _ -> error "unable to resolve leq"
  resolveBuiltin RawLn ts = case ts of
    Single (TypeApp TyInt) -> pure LnInt
    Single (TypeApp TyDecimal) -> pure LnDec
    _ -> error "unable to resolve ln"
  resolveBuiltin RawExp ts = case ts of
    Single (TypeApp TyInt) -> pure ExpInt
    Single (TypeApp TyDecimal) -> pure ExpDec
    _ -> error "unable to resolve ln"
  resolveBuiltin RawLength ts = case ts of
    Single (TypeApp TyString) -> pure LengthStr
    Single (TypeApp (TyList _t)) -> pure LengthList
    _ -> error "unable to resolve lengthj"
