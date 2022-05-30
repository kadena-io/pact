{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Pact.Core.Typed.Eval.Builtin(coreBuiltinRuntime) where

import Data.Text(Text)
import Data.Decimal(roundTo', Decimal)
import Data.Bits
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.RAList as RAList
import qualified Data.Vector as V
import qualified Data.Primitive.Array as Array
import qualified Data.Text as T

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Typed.Eval.CEK

applyOne :: CEKRuntime b => ETerm b -> CEKEnv b -> CEKValue b -> EvalT b (CEKValue b)
applyOne body env arg = eval (RAList.cons arg env) body

applyTwo :: CEKRuntime b => ETerm b -> CEKEnv b -> CEKValue b -> CEKValue b -> EvalT b (CEKValue b)
applyTwo body env arg1 arg2 = eval (RAList.cons arg2 (RAList.cons arg1 env)) body

-- Todo: runtime error
unaryIntFn :: (Integer -> Integer) -> BuiltinFn b
unaryIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [] -> pure (VLiteral (LInteger (op i)))
  _ -> fail "impossible"
{-# INLINE unaryIntFn #-}

unaryDecFn :: (Decimal -> Decimal) -> BuiltinFn b
unaryDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [] -> pure (VLiteral (LDecimal (op i)))
  _ -> fail "impossible"
{-# INLINE unaryDecFn #-}

binaryIntFn :: (Integer -> Integer -> Integer) -> BuiltinFn b
binaryIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger i')] -> pure (VLiteral (LInteger (op i i')))
  _ -> fail "impossible"
{-# INLINE binaryIntFn #-}

binaryBoolFn :: (Bool -> Bool -> Bool) -> BuiltinFn b
binaryBoolFn op = BuiltinFn \case
  VLiteral (LBool l) :| [VLiteral (LBool r)] -> pure (VLiteral (LBool (op l r)))
  _ -> fail "impossible"
{-# INLINE binaryBoolFn #-}

compareIntFn :: (Integer -> Integer -> Bool) -> BuiltinFn b
compareIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareIntFn #-}

roundingFn :: (Rational -> Integer) -> BuiltinFn b
roundingFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> fail "impossible"
{-# INLINE roundingFn #-}

addInt :: BuiltinFn b
addInt = binaryIntFn (+)

subInt :: BuiltinFn b
subInt = binaryIntFn (-)

mulInt :: BuiltinFn b
mulInt = binaryIntFn (*)

divInt :: BuiltinFn b
divInt = binaryIntFn quot

negateInt :: BuiltinFn b
negateInt = unaryIntFn negate

modInt :: BuiltinFn b
modInt = binaryIntFn mod

andBool :: BuiltinFn b
andBool = binaryBoolFn (&&)

orBool :: BuiltinFn b
orBool = binaryBoolFn (||)

notBool :: BuiltinFn b
notBool = BuiltinFn \case
  VLiteral (LBool i) :| [] -> pure (VLiteral (LBool (not i)))
  _ -> fail "impossible"

eqInt :: BuiltinFn b
eqInt = compareIntFn (==)

neqInt :: BuiltinFn b
neqInt = compareIntFn (/=)

gtInt :: BuiltinFn b
gtInt = compareIntFn (>)

ltInt :: BuiltinFn b
ltInt = compareIntFn (<)

geqInt :: BuiltinFn b
geqInt = compareIntFn (>=)

leqInt :: BuiltinFn b
leqInt = compareIntFn (<=)

asBool :: CEKValue b -> EvalT b Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = fail "impossible"

asString :: CEKValue b -> EvalT b Text
asString (VLiteral (LString b)) = pure b
asString _ = fail "impossible"

coreMap :: BuiltinFn b
coreMap = BuiltinFn \case
  (VClosure _ _ body env) :| [VList li] -> do
    li' <- traverse (applyOne body env) li
    pure (VList li')
  _ -> fail "impossible"

coreFilter :: BuiltinFn b
coreFilter = BuiltinFn \case
  (VClosure _ _ body env) :| [VList li] -> do
    let applyOne' arg = applyOne body env arg >>= asBool
    li' <- V.filterM applyOne' li
    pure (VList li')
  _ -> fail "impossible"

coreFold :: BuiltinFn b
coreFold = BuiltinFn \case
  (VClosure _ _ body env) :| [initElem, VList li] -> do
    out <- V.foldM' (applyTwo body env) initElem li
    pure out
  _ -> fail "impossible"

bitAndInt :: BuiltinFn b
bitAndInt = binaryIntFn (.&.)

bitOrInt :: BuiltinFn b
bitOrInt = binaryIntFn (.|.)

bitFlipInt :: BuiltinFn b
bitFlipInt = unaryIntFn complement

bitXorInt :: BuiltinFn b
bitXorInt = binaryIntFn xor

bitShiftInt :: BuiltinFn b
bitShiftInt = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger s)] ->
    pure (VLiteral (LInteger (shift i (fromIntegral s))))
  _ -> fail "impossible"

absInt :: BuiltinFn b
absInt = unaryIntFn abs

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec, floorDec, ceilingDec :: BuiltinFn b
roundDec = roundingFn round
floorDec = roundingFn floor
ceilingDec = roundingFn ceiling

expDec :: BuiltinFn b
expDec = unaryDecFn (f2Dec . exp . dec2F)

lnDec :: BuiltinFn b
lnDec = unaryDecFn (f2Dec . log . dec2F)

coreIf :: BuiltinFn b
coreIf = BuiltinFn \case
  VLiteral (LBool b) :| [VClosure _ _ ibody ienv, VClosure _ _ ebody eenv] ->
    if b then applyOne ibody ienv (VLiteral LUnit) else  applyOne ebody eenv (VLiteral LUnit)
  _ -> fail "impossible"

lengthList :: BuiltinFn b
lengthList = BuiltinFn \case
  VList li :| [] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> fail "impossible"

takeList :: BuiltinFn b
takeList = BuiltinFn \case
  VLiteral (LInteger i) :| [VList li] ->
    pure (VList (V.take (fromIntegral i) li))
  _ -> fail "impossible"

dropList :: BuiltinFn b
dropList = BuiltinFn \case
  VLiteral (LInteger i) :| [VList li] ->
    pure (VList (V.drop (fromIntegral i) li))
  _ -> fail "impossible"


coreEnumerate :: BuiltinFn b
coreEnumerate = BuiltinFn \case
  VLiteral (LInteger from) :| [VLiteral (LInteger to)] -> enum' from to
  _ -> fail "impossible"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from to
    | to >= from = pure $ toVecList $ V.enumFromN from (fromIntegral (to - from + 1))
    | otherwise = pure $ toVecList $ V.enumFromStepN from (-1) (fromIntegral (from - to + 1))

coreEnumerateStepN :: BuiltinFn b
coreEnumerateStepN = BuiltinFn \case
  VLiteral (LInteger from) :| [VLiteral (LInteger to), VLiteral (LInteger step)] -> enum' from to step
  _ -> fail "impossible"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from to step
    | to > from && (step > 0) = pure $ toVecList $ V.enumFromStepN from step (fromIntegral ((to - from + 1) `quot` step))
    | from > to && (step < 0) = pure $ toVecList $ V.enumFromStepN from step (fromIntegral ((from - to + 1) `quot` step))
    | from == to && step == 0 = pure $ toVecList $ V.singleton from
    | otherwise = fail "enumerate outside interval bounds"

coreConcat :: BuiltinFn b
coreConcat = BuiltinFn \case
  VList li :| [] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> fail "impossible"

unimplemented :: BuiltinFn b
unimplemented = BuiltinFn \case
  _ -> fail "unimplemented"

coreBuiltinFn :: CoreBuiltin -> BuiltinFn CoreBuiltin
coreBuiltinFn = \case
  AddInt -> addInt
  SubInt -> subInt
  DivInt -> divInt
  MulInt -> mulInt
  NegateInt -> negateInt
  AbsInt -> absInt
  LogBaseInt -> unimplemented
  ModInt -> modInt
  ExpInt -> unimplemented
  LnInt -> unimplemented
  BitAndInt -> bitAndInt
  BitOrInt -> bitOrInt
  BitXorInt ->  bitXorInt
  BitShiftInt -> bitShiftInt
  BitComplementInt -> bitFlipInt
  IfElse -> coreIf
  AddDec -> unimplemented
  SubDec -> unimplemented
  DivDec -> unimplemented
  MulDec -> unimplemented
  NegateDec -> unimplemented
  AbsDec -> unimplemented
  RoundDec -> roundDec
  CeilingDec -> ceilingDec
  ExpDec -> expDec
  FloorDec -> floorDec
  LnDec -> lnDec
  LogBaseDec -> unimplemented
  AndBool -> andBool
  OrBool -> orBool
  NotBool -> notBool
  EqInt -> eqInt
  NeqInt -> neqInt
  GTInt -> gtInt
  GEQInt -> geqInt
  LTInt -> ltInt
  LEQInt -> leqInt
  EqDec -> unimplemented
  NeqDec -> unimplemented
  GTDec -> unimplemented
  GEQDec -> unimplemented
  LTDec -> unimplemented
  LEQDec -> unimplemented
  EqStr -> unimplemented
  NeqStr -> unimplemented
  GTStr -> unimplemented
  GEQStr -> unimplemented
  LTStr -> unimplemented
  LEQStr -> unimplemented
  EqObj -> unimplemented
  NeqObj -> unimplemented
  EqList -> unimplemented
  AddStr -> unimplemented
  ConcatStr -> coreConcat
  DropStr -> unimplemented
  TakeStr -> unimplemented
  LengthStr -> unimplemented
  AddList -> unimplemented
  DistinctList -> unimplemented
  TakeList -> takeList
  DropList -> dropList
  LengthList -> lengthList
  FilterList -> coreFilter
  MapList -> coreMap
  FoldList -> coreFold
  Enforce -> unimplemented
  EnforceOne -> unimplemented
  Enumerate -> coreEnumerate
  EnumerateStepN -> coreEnumerateStepN
  Dummy -> unimplemented

coreBuiltinRuntime :: Array.Array (BuiltinFn CoreBuiltin)
coreBuiltinRuntime = Array.arrayFromList (coreBuiltinFn <$> [minBound .. maxBound])
