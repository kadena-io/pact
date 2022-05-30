{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}


module Pact.Core.Typed.Overload where

import Pact.Core.Typed.Term
import Pact.Core.Builtin

resolveOverload :: Monad m => Term name tyname RawBuiltin info -> m (Term name tyname CoreBuiltin info)
resolveOverload = \case
  Var n i ->
    pure (Var n i)
  Lam n nts e i ->
    Lam n nts <$> resolveOverload e <*> pure i
  Let n e1 e2 i ->
    Let n <$> resolveOverload e1 <*> resolveOverload e2 <*> pure i
  App f args i ->
    App <$> resolveOverload f <*> traverse resolveOverload args <*> pure i
  TyApp l rs i ->
    TyApp <$> resolveOverload l <*> pure rs <*> pure i
  TyAbs nel t i ->
    TyAbs nel <$> resolveOverload t <*> pure i
  Block nel i ->
    Block <$> traverse resolveOverload nel <*> pure i
  ObjectLit ms i ->
    ObjectLit <$> traverse resolveOverload ms <*> pure i
  ListLit tn ts i ->
    ListLit tn <$> traverse resolveOverload ts <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  Builtin b i ->
    Builtin <$> pure (solveOverload b) <*> pure i
  ObjectOp o i ->
    ObjectOp <$> traverse resolveOverload o <*> pure i
  Error t1 t2 i ->
    pure (Error t1 t2 i)
  where
  solveOverload = \case
    RawAdd -> AddInt
    RawSub -> SubInt
    RawMultiply -> MulInt
    RawDivide -> DivInt
    RawNegate -> NegateInt
    RawAnd -> AndBool
    RawOr -> OrBool
    RawNot -> NotBool
    RawEq -> EqInt
    RawNeq -> NeqInt
    RawGT -> GTInt
    RawGEQ -> GEQInt
    RawLT -> LTInt
    RawLEQ -> LEQInt
    RawBitwiseAnd -> BitAndInt
    RawBitwiseOr -> BitOrInt
    RawBitwiseXor -> BitXorInt
    RawBitwiseFlip -> BitComplementInt
    RawBitShift -> BitShiftInt
    RawAbs -> AbsInt
    RawRound -> RoundDec
    RawCeiling -> CeilingDec
    RawExp -> ExpDec
    RawFloor -> FloorDec
    RawLn -> LnDec
    RawLogBase -> LogBaseDec
    RawMod -> ModInt
    RawMap -> MapList
    RawFilter -> FilterList
    RawIf -> IfElse
    RawIntToStr -> undefined
    RawConcat -> ConcatStr
    RawStrToInt -> undefined
    RawTake -> TakeList
    RawDrop -> DropList
    RawLength -> LengthList
    RawFold -> FoldList
    RawDistinct -> DistinctList
    RawEnforce -> Enforce
    RawEnforceOne -> EnforceOne
    RawEnumerate -> Enumerate
    RawEnumerateStepN -> EnumerateStepN
    RawDummy -> Dummy
