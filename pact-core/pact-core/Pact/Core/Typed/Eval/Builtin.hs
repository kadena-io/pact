{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}

module Pact.Core.Typed.Eval.Builtin(coreBuiltinRuntime) where

import Data.Text(Text)
import Data.Decimal(roundTo', Decimal)
import Data.Bits
import Data.List.NonEmpty(NonEmpty(..))
import Data.Vector(Vector)

import qualified Data.RAList as RAList
import qualified Data.Vector as V
import qualified Data.Primitive.Array as Array
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Typed.Eval.CEK

applyOne :: CEKRuntime b => ETerm b -> CEKEnv b -> CEKValue b -> EvalT b (CEKValue b)
applyOne body env arg = eval (RAList.cons arg env) body

applyTwo :: CEKRuntime b => ETerm b -> CEKEnv b -> CEKValue b -> CEKValue b -> EvalT b (CEKValue b)
applyTwo body env arg1 arg2 = eval (RAList.cons arg2 (RAList.cons arg1 env)) body

unsafeApplyOne :: CEKRuntime b => CEKValue b -> CEKValue b -> EvalT b (CEKValue b)
unsafeApplyOne (VClosure (_:ns) body env) arg = case ns of
  [] -> applyOne body env arg
  _ -> pure (VClosure ns body (RAList.cons arg env))
unsafeApplyOne (VNative b) arg = do
  let (BuiltinFn f) = Array.indexArray ?cekBuiltins (fromEnum b)
  f (arg :| [])
unsafeApplyOne _ _ = error "impossible"

unsafeApplyTwo :: CEKRuntime b => CEKValue b -> CEKValue b -> CEKValue b -> EvalT b (CEKValue b)
unsafeApplyTwo (VClosure (_:ns) body env) arg1 arg2 = case ns of
  [] -> error "impossible"
  _:ms -> case ms of
    [] -> applyTwo body env arg1 arg2
    _ ->
      let env' = RAList.cons arg2 (RAList.cons arg1 env)
      in pure (VClosure ms body env')
unsafeApplyTwo (VNative b) arg1 arg2 = do
  let (BuiltinFn f) = Array.indexArray ?cekBuiltins (fromEnum b)
  f (arg1 :| [arg2])
unsafeApplyTwo _ _ _ = error "impossible"


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

binaryDecFn :: (Decimal -> Decimal -> Decimal) -> BuiltinFn b
binaryDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (op i i')))
  _ -> fail "impossible"
{-# INLINE binaryDecFn #-}

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

compareDecFn :: (Decimal -> Decimal -> Bool) -> BuiltinFn b
compareDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [VLiteral (LDecimal i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareDecFn #-}

compareStrFn :: (Text -> Text -> Bool) -> BuiltinFn b
compareStrFn op = BuiltinFn \case
  VLiteral (LString i) :| [VLiteral (LString i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareStrFn #-}

roundingFn :: (Rational -> Integer) -> BuiltinFn b
roundingFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> fail "impossible"
{-# INLINE roundingFn #-}

---------------------------------
-- integer ops
------------------------------
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

bitAndInt :: BuiltinFn b
bitAndInt = binaryIntFn (.&.)

bitOrInt :: BuiltinFn b
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: BuiltinFn b
bitComplementInt = unaryIntFn complement

bitXorInt :: BuiltinFn b
bitXorInt = binaryIntFn xor

bitShiftInt :: BuiltinFn b
bitShiftInt = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger s)] ->
    pure (VLiteral (LInteger (shift i (fromIntegral s))))
  _ -> fail "impossible"

absInt :: BuiltinFn b
absInt = unaryIntFn abs

expInt :: BuiltinFn b
expInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
  _ -> fail "impossible"

lnInt :: BuiltinFn b
lnInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
  _ -> fail "impossible"

sqrtInt :: BuiltinFn b
sqrtInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
  _ -> fail "impossible"

showInt :: BuiltinFn b
showInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> fail "impossible"

---------------------------
-- double ops
---------------------------

addDec :: BuiltinFn b
addDec = binaryDecFn (+)

subDec :: BuiltinFn b
subDec = binaryDecFn (-)

mulDec :: BuiltinFn b
mulDec = binaryDecFn (*)

divDec :: BuiltinFn b
divDec = binaryDecFn (/)

negateDec :: BuiltinFn b
negateDec = unaryDecFn negate

absDec :: BuiltinFn b
absDec = unaryDecFn abs

eqDec :: BuiltinFn b
eqDec = compareDecFn (==)

neqDec :: BuiltinFn b
neqDec = compareDecFn (/=)

gtDec :: BuiltinFn b
gtDec = compareDecFn (>)

geqDec :: BuiltinFn b
geqDec = compareDecFn (>=)

ltDec :: BuiltinFn b
ltDec = compareDecFn (<)

leqDec :: BuiltinFn b
leqDec = compareDecFn (<=)

showDec :: BuiltinFn b
showDec = BuiltinFn \case
  VLiteral (LDecimal i) :| [] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> fail "impossible"

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

sqrtDec :: BuiltinFn b
sqrtDec = unaryDecFn (f2Dec . sqrt . dec2F)

---------------------------
-- bool ops
---------------------------
andBool :: BuiltinFn b
andBool = binaryBoolFn (&&)

orBool :: BuiltinFn b
orBool = binaryBoolFn (||)

notBool :: BuiltinFn b
notBool = BuiltinFn \case
  VLiteral (LBool i) :| [] -> pure (VLiteral (LBool (not i)))
  _ -> fail "impossible"

eqBool :: BuiltinFn b
eqBool = binaryBoolFn (==)

neqBool :: BuiltinFn b
neqBool = binaryBoolFn (/=)

showBool :: BuiltinFn b
showBool = BuiltinFn \case
  VLiteral (LBool i) :| [] -> do
    let out = if i then "true" else "false"
    pure (VLiteral (LString out))
  _ -> fail "impossible"

---------------------------
-- string ops
---------------------------
eqStr :: BuiltinFn b
eqStr = compareStrFn (==)

neqStr :: BuiltinFn b
neqStr = compareStrFn (/=)

gtStr :: BuiltinFn b
gtStr = compareStrFn (>)

geqStr :: BuiltinFn b
geqStr = compareStrFn (>=)

ltStr :: BuiltinFn b
ltStr = compareStrFn (<)

leqStr :: BuiltinFn b
leqStr = compareStrFn (<=)

addStr :: BuiltinFn b
addStr =  BuiltinFn \case
  VLiteral (LString i) :| [VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
  _ -> fail "impossible"

takeStr :: BuiltinFn b
takeStr = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.take (fromIntegral i) t)))
  _ -> fail "impossible"

dropStr :: BuiltinFn b
dropStr = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.drop (fromIntegral i) t)))
  _ -> fail "impossible"

lengthStr :: BuiltinFn b
lengthStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    pure (VLiteral (LInteger (fromIntegral (T.length t))))
  _ -> fail "impossible"

reverseStr :: BuiltinFn b
reverseStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    pure (VLiteral (LString (T.reverse t)))
  _ -> fail "impossible"

showStr :: BuiltinFn b
showStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    let out = "\"" <> t <> "\""
    pure (VLiteral (LString out))
  _ -> fail "impossible"

concatStr :: BuiltinFn b
concatStr = BuiltinFn \case
  VList li :| [] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> fail "impossible"


---------------------------
-- Unit ops
---------------------------

eqUnit :: BuiltinFn b
eqUnit = BuiltinFn \case
  VLiteral LUnit :| [VLiteral LUnit] -> pure (VLiteral (LBool True))
  _ -> fail "impossible"

neqUnit :: BuiltinFn b
neqUnit = BuiltinFn \case
  VLiteral LUnit :| [VLiteral LUnit] -> pure (VLiteral (LBool False))
  _ -> fail "impossible"

showUnit :: BuiltinFn b
showUnit = BuiltinFn \case
  VLiteral LUnit :| [] -> pure (VLiteral (LString "()"))
  _ -> fail "impossible"

---------------------------
-- Object ops
---------------------------

eqObj :: BuiltinFn b
eqObj = BuiltinFn \case
  l@VObject{} :| [r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
  _ -> fail "impossible"

neqObj :: BuiltinFn b
neqObj = BuiltinFn \case
  l@VObject{} :| [r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
  _ -> fail "impossible"


------------------------------
--- conversions + unsafe ops
------------------------------
asBool :: CEKValue b -> EvalT b Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = fail "impossible"

asString :: CEKValue b -> EvalT b Text
asString (VLiteral (LString b)) = pure b
asString _ = fail "impossible"

asList :: CEKValue b -> EvalT b (Vector (CEKValue b))
asList (VList l) = pure l
asList _ = fail "impossible"

unsafeEqLiteral :: Literal -> Literal -> Bool
unsafeEqLiteral (LString i) (LString i') = i == i'
unsafeEqLiteral (LInteger i) (LInteger i') = i == i'
unsafeEqLiteral (LDecimal i) (LDecimal i') = i == i'
unsafeEqLiteral LUnit LUnit = True
unsafeEqLiteral (LBool i) (LBool i') = i == i'
unsafeEqLiteral (LTime i) (LTime i') = i == i'
unsafeEqLiteral _ _ = error "todo: throw invariant failure exception"

-- unsafeNeqLiteral :: Literal -> Literal -> Bool
-- unsafeNeqLiteral a b = not (unsafeEqLiteral a b)

unsafeEqCEKValue :: CEKValue b -> CEKValue b -> Bool
unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
unsafeEqCEKValue (VObject o) (VObject o') = and (Map.intersectionWith unsafeEqCEKValue o o')
unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
unsafeEqCEKValue _ _ = error "todo: throw invariant failure exception"

unsafeNeqCEKValue :: CEKValue b -> CEKValue b -> Bool
unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
---------------------------
eqList :: BuiltinFn b
eqList = BuiltinFn \case
  eqClo :| [VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool False))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo eqClo a b) l r
      pure (VLiteral (LBool (and v')))
  _ -> fail "impossible"

neqList :: BuiltinFn b
neqList = BuiltinFn \case
  neqClo :| [VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool True))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo neqClo a b) l r
      pure (VLiteral (LBool (or v')))
  _ -> fail "impossible"

zipList :: BuiltinFn b
zipList = BuiltinFn \case
  clo :| [VList l, VList r] -> do
    v' <- V.zipWithM (unsafeApplyTwo clo) l r
    pure (VList v')
  _ -> fail "impossible"

addList :: BuiltinFn b
addList = BuiltinFn \case
  VList l :| [VList r] -> pure (VList (l <> r))
  _ -> fail "impossible"

pcShowList :: BuiltinFn b
pcShowList = BuiltinFn \case
  showFn :| [VList l1] -> do
    strli <- traverse ((=<<) asString  . unsafeApplyOne showFn) (V.toList l1)
    let out = "[" <> T.intercalate ", " strli <> "]"
    pure (VLiteral (LString out))
  _ -> fail "impossible"

coreMap :: BuiltinFn b
coreMap = BuiltinFn \case
  fn :| [VList li] -> do
    li' <- traverse (unsafeApplyOne fn) li
    pure (VList li')
  _ -> fail "impossible"

coreFilter :: BuiltinFn b
coreFilter = BuiltinFn \case
  fn :| [VList li] -> do
    let applyOne' arg = unsafeApplyOne fn arg >>= asBool
    li' <- V.filterM applyOne' li
    pure (VList li')
  _ -> fail "impossible"

coreFold :: BuiltinFn b
coreFold = BuiltinFn \case
  fn :| [initElem, VList li] -> do
    out <- V.foldM' (unsafeApplyTwo fn) initElem li
    pure out
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

reverseList :: BuiltinFn b
reverseList = BuiltinFn \case
  VList li :| [] ->
    pure (VList (V.reverse li))
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

concatList :: BuiltinFn b
concatList = BuiltinFn \case
  VList li :| [] -> do
    li' <- traverse asList li
    pure (VList (V.concat (V.toList li')))
  _ -> fail "impossible"

-----------------------------------
-- Other Core forms
---------------------------------

coreIf :: BuiltinFn b
coreIf = BuiltinFn \case
  VLiteral (LBool b) :| [VClosure _ ibody ienv, VClosure _ ebody eenv] ->
    if b then applyOne ibody ienv (VLiteral LUnit) else  applyOne ebody eenv (VLiteral LUnit)
  _ -> fail "impossible"

unimplemented :: BuiltinFn b
unimplemented = BuiltinFn \case
  _ -> fail "unimplemented"

coreBuiltinFn :: CoreBuiltin -> BuiltinFn CoreBuiltin
coreBuiltinFn = \case
  -- Int Add + num ops
  AddInt -> addInt
  SubInt -> subInt
  DivInt -> divInt
  MulInt -> mulInt
  NegateInt -> negateInt
  AbsInt -> absInt
  -- Int fractional
  ExpInt -> expInt
  LnInt -> lnInt
  SqrtInt -> sqrtInt
  LogBaseInt -> unimplemented
  -- Geenral int ops
  ModInt -> modInt
  BitAndInt -> bitAndInt
  BitOrInt -> bitOrInt
  BitXorInt ->  bitXorInt
  BitShiftInt -> bitShiftInt
  BitComplementInt -> bitComplementInt
  -- Int Equality + Ord
  EqInt -> eqInt
  NeqInt -> neqInt
  GTInt -> gtInt
  GEQInt -> geqInt
  LTInt -> ltInt
  LEQInt -> leqInt
  -- IntShow inst
  ShowInt -> showInt
  -- If
  IfElse -> coreIf
  -- Decimal ops
  -- Add + Num
  AddDec -> addDec
  SubDec -> subDec
  DivDec -> divDec
  MulDec -> mulDec
  NegateDec -> negateDec
  AbsDec -> absDec
  -- Decimal rounding ops
  RoundDec -> roundDec
  CeilingDec -> ceilingDec
  FloorDec -> floorDec
  -- Decimal fractional
  ExpDec -> expDec
  LnDec -> lnDec
  LogBaseDec -> unimplemented
  SqrtDec -> sqrtDec
  -- Decimal show
  ShowDec -> showDec
  -- Decimal Equality + Ord
  EqDec -> eqDec
  NeqDec -> neqDec
  GTDec -> gtDec
  GEQDec -> geqDec
  LTDec -> ltDec
  LEQDec -> leqDec
  -- Bool Ops
  AndBool -> andBool
  OrBool -> orBool
  NotBool -> notBool
  -- Bool Equality
  EqBool -> eqBool
  NeqBool -> neqBool
  ShowBool -> showBool
  -- String Equality + Ord
  EqStr -> eqStr
  NeqStr -> neqStr
  GTStr -> gtStr
  GEQStr -> geqStr
  LTStr -> ltStr
  LEQStr -> leqStr
  -- String Ops
  AddStr -> addStr
  -- String listlike
  ConcatStr -> concatStr
  DropStr -> dropStr
  TakeStr -> takeStr
  LengthStr -> lengthStr
  ReverseStr -> reverseStr
  -- String show
  ShowStr -> showStr
  -- Object equality
  EqObj -> eqObj
  NeqObj -> neqObj
  -- List Equality + Ord
  EqList -> eqList
  NeqList -> neqList
  GTList -> unimplemented
  GEQList -> unimplemented
  LTList -> unimplemented
  LEQList -> unimplemented
  -- List Show
  ShowList -> pcShowList
  -- ListAdd
  AddList -> addList
  -- List ListlLike
  TakeList -> takeList
  DropList -> dropList
  LengthList -> lengthList
  ConcatList -> concatList
  ReverseList -> reverseList
  -- misc list ops
  FilterList -> coreFilter
  DistinctList -> unimplemented
  ZipList -> zipList
  MapList -> coreMap
  FoldList -> coreFold
  -- Unit ops
  EqUnit -> eqUnit
  NeqUnit -> neqUnit
  ShowUnit -> showUnit
  Enforce -> unimplemented
  EnforceOne -> unimplemented
  Enumerate -> coreEnumerate
  EnumerateStepN -> coreEnumerateStepN
  Dummy -> unimplemented

coreBuiltinRuntime :: Array.Array (BuiltinFn CoreBuiltin)
coreBuiltinRuntime = Array.arrayFromList (coreBuiltinFn <$> [minBound .. maxBound])
