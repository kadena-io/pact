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
-- CEK Evaluator for untyped core using our RawBuiltins (aka untyped, no typechecking)
--

module Pact.Core.Untyped.Eval.Runtime.CoreBuiltin
  ( coreBuiltinRuntime
  , coreBuiltinLiftedRuntime ) where

import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Vector(Vector)
import qualified Data.Vector as V
-- import qualified Data.Primitive.Array as Array
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK

-- | Run our CEK interpreter
--   for only our core builtins
--   monomorphized version
-- runCoreCEK
  -- :: CEKRuntimeEnv CoreBuiltin i
  -- ^ Runtime environment
  -- -> EvalTerm CoreBuiltin i
  -- ^ Term to evaluate
--   -> IO (CEKValue CoreBuiltin i)
-- runCoreCEK = runCEK
----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (BuiltinArity b, MonadCEK b i m) => (Integer -> Integer) -> b -> BuiltinFn b i m
unaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i)] -> pure (VLiteral (LInteger (op i)))
  _ -> failInvariant "unary int function"
{-# INLINE unaryIntFn #-}

unaryDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal) -> b -> BuiltinFn b i m
unaryDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal (op i)))
  _ -> failInvariant "unary decimal function"
{-# INLINE unaryDecFn #-}

binaryIntFn
  :: (BuiltinArity b, MonadCEK b i m)
  => (Integer -> Integer -> Integer)
  -> b
  -> BuiltinFn b i m
binaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (op i i')))
  _ -> failInvariant "binary int function"
{-# INLINE binaryIntFn #-}

binaryDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal -> Decimal) -> b -> BuiltinFn b i m
binaryDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (op i i')))
  _ -> failInvariant "binary decimal function"
{-# INLINE binaryDecFn #-}

binaryBoolFn :: (BuiltinArity b, MonadCEK b i m) => (Bool -> Bool -> Bool) -> b -> BuiltinFn b i m
binaryBoolFn op = mkBuiltinFn \case
  [VLiteral (LBool l), VLiteral (LBool r)] -> pure (VLiteral (LBool (op l r)))
  _ -> failInvariant "binary bool function"
{-# INLINE binaryBoolFn #-}

compareIntFn :: (BuiltinArity b, MonadCEK b i m) => (Integer -> Integer -> Bool) -> b -> BuiltinFn b i m
compareIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "int cmp function"
{-# INLINE compareIntFn #-}

compareDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal -> Bool) -> b -> BuiltinFn b i m
compareDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "dec cmp function"
{-# INLINE compareDecFn #-}

compareStrFn :: (BuiltinArity b, MonadCEK b i m) => (Text -> Text -> Bool) -> b -> BuiltinFn b i m
compareStrFn op = mkBuiltinFn \case
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "str cmp function"
{-# INLINE compareStrFn #-}

roundingFn :: (BuiltinArity b, MonadCEK b i m) => (Rational -> Integer) -> b -> BuiltinFn b i m
roundingFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i)] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> failInvariant "rounding function"
{-# INLINE roundingFn #-}

---------------------------------
-- integer ops
------------------------------
addInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
addInt = binaryIntFn (+)

subInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
subInt = binaryIntFn (-)

mulInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
mulInt = binaryIntFn (*)

divInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
divInt = mkBuiltinFn $ \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    else pure (VLiteral (LInteger (div i i')))
  _ -> failInvariant "binary int function"

negateInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
negateInt = unaryIntFn negate

modInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
modInt = binaryIntFn mod

eqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqInt = compareIntFn (==)

neqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqInt = compareIntFn (/=)

gtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
gtInt = compareIntFn (>)

ltInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
ltInt = compareIntFn (<)

geqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
geqInt = compareIntFn (>=)

leqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
leqInt = compareIntFn (<=)

bitAndInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
bitComplementInt = unaryIntFn complement

bitXorInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
bitXorInt = binaryIntFn xor

bitShiftInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
bitShiftInt = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger s)] ->
    pure (VLiteral (LInteger (shift i (fromIntegral s))))
  _ -> failInvariant "bit-shift-int"

absInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
absInt = unaryIntFn abs

expInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
expInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
  _ -> failInvariant "expInt"

lnInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
lnInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
  _ -> failInvariant "lnInt"

sqrtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
sqrtInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
  _ -> failInvariant "sqrtInt"

showInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
showInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showInt"

-- -------------------------
-- double ops
-- -------------------------

addDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
addDec = binaryDecFn (+)

subDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
subDec = binaryDecFn (-)

mulDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
mulDec = binaryDecFn (*)

divDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
divDec =  mkBuiltinFn \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero, decimal")
    else pure (VLiteral (LDecimal (i / i')))
  _ -> failInvariant "binary decimal function"

negateDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
negateDec = unaryDecFn negate

absDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
absDec = unaryDecFn abs

eqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqDec = compareDecFn (==)

neqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqDec = compareDecFn (/=)

gtDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
gtDec = compareDecFn (>)

geqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
geqDec = compareDecFn (>=)

ltDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
ltDec = compareDecFn (<)

leqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
leqDec = compareDecFn (<=)

showDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
showDec = mkBuiltinFn \case
  [VLiteral (LDecimal i)] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showDec"

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
roundDec = roundingFn round
floorDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
floorDec = roundingFn floor
ceilingDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
ceilingDec = roundingFn ceiling

expDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
expDec = unaryDecFn (f2Dec . exp . dec2F)

lnDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
lnDec = unaryDecFn (f2Dec . log . dec2F)

sqrtDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
sqrtDec = unaryDecFn (f2Dec . sqrt . dec2F)

---------------------------
-- bool ops
---------------------------
andBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
andBool = binaryBoolFn (&&)

orBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
orBool = binaryBoolFn (||)

notBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
notBool = mkBuiltinFn \case
  [VLiteral (LBool i)] -> pure (VLiteral (LBool (not i)))
  _ -> failInvariant "notBool"

eqBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqBool = binaryBoolFn (==)

neqBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqBool = binaryBoolFn (/=)

showBool :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
showBool = mkBuiltinFn \case
  [VLiteral (LBool i)] -> do
    let out = if i then "true" else "false"
    pure (VLiteral (LString out))
  _ -> failInvariant "showBool"

---------------------------
-- string ops
---------------------------
eqStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqStr = compareStrFn (==)

neqStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqStr = compareStrFn (/=)

gtStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
gtStr = compareStrFn (>)

geqStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
geqStr = compareStrFn (>=)

ltStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
ltStr = compareStrFn (<)

leqStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
leqStr = compareStrFn (<=)

addStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
addStr =  mkBuiltinFn \case
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
  _ -> failInvariant "addStr"

takeStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
takeStr = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.take (fromIntegral i) t)))
  _ -> failInvariant "takeStr"

dropStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
dropStr = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.drop (fromIntegral i) t)))
  _ -> failInvariant "dropStr"

lengthStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
lengthStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    pure (VLiteral (LInteger (fromIntegral (T.length t))))
  _ -> failInvariant "lengthStr"

reverseStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
reverseStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.reverse t)))
  _ -> failInvariant "reverseStr"

showStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
showStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    let out = "\"" <> t <> "\""
    pure (VLiteral (LString out))
  _ -> failInvariant "showStr"

concatStr :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
concatStr = mkBuiltinFn \case
  [VList li] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> failInvariant "concatStr"


---------------------------
-- Unit ops
---------------------------

eqUnit :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqUnit = mkBuiltinFn \case
  [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool True))
  _ -> failInvariant "eqUnit"

neqUnit :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqUnit = mkBuiltinFn \case
  [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool False))
  _ -> failInvariant "neqUnit"

showUnit :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
showUnit = mkBuiltinFn \case
  [VLiteral LUnit] -> pure (VLiteral (LString "()"))
  _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

-- eqObj :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- eqObj = mkBuiltinFn \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
--   _ -> failInvariant "eqObj"

-- neqObj :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- neqObj = mkBuiltinFn \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
--   _ -> failInvariant "neqObj"


------------------------------
--- conversions + unsafe ops
------------------------------
asBool :: MonadCEK b i m => CEKValue b i m -> m Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = failInvariant "asBool"

asString :: MonadCEK b i m => CEKValue b i m -> m Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

asList :: MonadCEK b i m => CEKValue b i m -> m (Vector (CEKValue b i m))
asList (VList l) = pure l
asList _ = failInvariant "asList"

-- unsafeEqLiteral :: Literal -> Literal -> Bool
-- unsafeEqLiteral (LString i) (LString i') = i == i'
-- unsafeEqLiteral (LInteger i) (LInteger i') = i == i'
-- unsafeEqLiteral (LDecimal i) (LDecimal i') = i == i'
-- unsafeEqLiteral LUnit LUnit = True
-- unsafeEqLiteral (LBool i) (LBool i') = i == i'
-- unsafeEqLiteral (LTime i) (LTime i') = i == i'
-- unsafeEqLiteral _ _ =
--   throw (FatalExecutionError "invariant failed in literal EQ")

-- unsafeNeqLiteral :: Literal -> Literal -> Bool
-- unsafeNeqLiteral a b = not (unsafeEqLiteral a b)

-- unsafeEqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
-- unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
-- unsafeEqCEKValue (VObject o) (VObject o') = and (Map.intersectionWith unsafeEqCEKValue o o')
-- unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
-- unsafeEqCEKValue _ _ = throw (FatalExecutionError "invariant failed in value Eq")

-- unsafeNeqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
-- unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
---------------------------
eqList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqList = mkBuiltinFn \case
  [eqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool False))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo eqClo a b) l r
      pure (VLiteral (LBool (and v')))
  _ -> failInvariant "eqList"

neqList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqList = mkBuiltinFn \case
  [neqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool True))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo neqClo a b) l r
      pure (VLiteral (LBool (or v')))
  _ -> failInvariant "neqList"

zipList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
zipList = mkBuiltinFn \case
  [clo, VList l, VList r] -> do
    v' <- V.zipWithM (unsafeApplyTwo clo) l r
    pure (VList v')
  _ -> failInvariant "zipList"

addList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
addList = mkBuiltinFn \case
  [VList l, VList r] -> pure (VList (l <> r))
  _ -> failInvariant "addList"

pcShowList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
pcShowList = mkBuiltinFn \case
  [showFn, VList l1] -> do
    strli <- traverse ((=<<) asString  . unsafeApplyOne showFn) (V.toList l1)
    let out = "[" <> T.intercalate ", " strli <> "]"
    pure (VLiteral (LString out))
  _ -> failInvariant "showList"

coreMap :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreMap = mkBuiltinFn \case
  [fn, VList li] -> do
    li' <- traverse (unsafeApplyOne fn) li
    pure (VList li')
  _ -> failInvariant "map"

coreFilter :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreFilter = mkBuiltinFn \case
  [fn, VList li] -> do
    let applyOne' arg = unsafeApplyOne fn arg >>= asBool
    li' <- V.filterM applyOne' li
    pure (VList li')
  _ -> failInvariant "filter"

coreFold :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreFold = mkBuiltinFn \case
  [fn, initElem, VList li] -> V.foldM' (unsafeApplyTwo fn) initElem li
  _ -> failInvariant "fold"

lengthList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
lengthList = mkBuiltinFn \case
  [VList li] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "lengthList"

takeList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
takeList = mkBuiltinFn \case
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.take (fromIntegral i) li))
  _ -> failInvariant "takeList"

dropList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
dropList = mkBuiltinFn \case
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.drop (fromIntegral i) li))
  _ -> failInvariant "dropList"

reverseList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
reverseList = mkBuiltinFn \case
  [VList li] ->
    pure (VList (V.reverse li))
  _ -> failInvariant "takeList"

coreEnumerate :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreEnumerate = mkBuiltinFn \case
  [VLiteral (LInteger from'), VLiteral (LInteger to')] -> enum' from' to'
  _ -> failInvariant "enumerate"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from' to'
    | to' >= from' = pure $ toVecList $ V.enumFromN from' (fromIntegral (to' - from' + 1))
    | otherwise = pure $ toVecList $ V.enumFromStepN from' (-1) (fromIntegral (from' - to' + 1))

coreEnumerateStepN :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreEnumerateStepN = mkBuiltinFn \case
  [VLiteral (LInteger from'), VLiteral (LInteger to'), VLiteral (LInteger step)] -> enum' from' to' step
  _ -> failInvariant "enumerate-step"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from' to' step
    | to' > from' && step > 0 = pure $ toVecList $ V.enumFromStepN from' step (fromIntegral ((to' - from' + 1) `quot` step))
    | from' > to' && step < 0 = pure $ toVecList $ V.enumFromStepN from' step (fromIntegral ((from' - to' + 1) `quot` step))
    | from' == to' && step == 0 = pure $ toVecList $ V.singleton from'
    | otherwise = throwExecutionError' (EnumeratationError "enumerate outside interval bounds")

concatList :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
concatList = mkBuiltinFn \case
  [VList li] -> do
    li' <- traverse asList li
    pure (VList (V.concat (V.toList li')))
  _ -> failInvariant "takeList"


coreEnforce :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreEnforce = mkBuiltinFn \case
  [VLiteral (LBool b), VLiteral (LString s)] ->
    if b then pure (VLiteral LUnit)
    else pure (VError s)
  _ -> failInvariant "enforce"

-- coreEnforceOne :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreEnforceOne = mkBuiltinFn \case
--   [VList v, VLiteral (LString msg)] ->
--     enforceFail msg (V.toList v)
--   _ -> failInvariant "coreEnforceOne"
--   where
--   handler msg rest = \case
--     EnforceException _ -> enforceFail msg rest
--     e -> throwM e
--   enforceClo _ [] = pure (VLiteral LUnit)
--   enforceClo msg (x:xs) = catch (unsafeApplyOne x (VLiteral LUnit)) (handler msg xs)
--   enforceFail msg [] = throwM (EnforceException msg)
--   enforceFail msg as = enforceClo msg as
-----------------------------------
-- Guards and reads
-----------------------------------

-- readError :: Text -> Text -> Text
-- readError field expected =
--   "invalid value at field " <> field <> " expected: " <> expected

-- coreReadInteger :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreReadInteger = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LInteger{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "integer"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-integer"

-- coreReadString :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreReadString = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv-> case pv of
--         PLiteral l@LString{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "string"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-string"

-- coreReadDecimal :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreReadDecimal = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LDecimal{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "decimal"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-decimal"

-- coreReadObject :: CEKRuntime b i => Row Void -> CEKValue b i m  -> EvalT b i (CEKValue b i m)
-- coreReadObject ty = \case
--   VLiteral (LString s) ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         t@PObject{} | checkPactValueType (TyRow ty) t -> pure (fromPactValue t)
--         _ -> throwM (ReadException (readError s "object"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "readObject"

-- coreReadKeyset :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreReadKeyset = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PObject m -> case lookupKs m of
--           Just ks -> pure (VGuard (GKeyset ks))
--           _ -> throwM (ReadException "Invalid keyset format")
--         _ -> throwM (ReadException (readError s "decimal"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-keyset"
--   where
--   -- Todo: public key parsing.
--   -- This is most certainly wrong, it needs more checks.
--   lookupKs m = do
--     ks <- Map.lookup (Field "keys") m >>= \case
--       PList v -> do
--         o <- traverse (preview (_PLiteral . _LString)) v
--         guard (all (T.all isHexDigit) o)
--         pure $ Set.fromList $ V.toList (PublicKey . T.encodeUtf8 <$> o)
--       _ -> Nothing
--     kspred <- case Map.lookup (Field "pred") m of
--       (Just (PLiteral LString{})) -> pure KeysAll
--       Just _ -> Nothing
--       Nothing -> pure KeysAll
--     pure (KeySet ks kspred)


-- coreKeysetRefGuard :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreKeysetRefGuard = mkBuiltinFn \case
--   [VLiteral (LString s)] -> pure (VGuard (GKeySetRef (KeySetName s)))
--   _ -> failInvariant "keyset-ref-guard"

-- coreEnforceGuard :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- coreEnforceGuard = mkBuiltinFn \case
--   [VGuard v] -> case v of
--     GKeyset ks -> enforceKeySet ks
--     GKeySetRef ksr -> enforceKeySetRef ksr
--     GUserGuard ug -> enforceUserGuard ug
--   _ -> failInvariant "enforceGuard"

-- enforceKeySet :: CEKRuntime b i => KeySet name -> EvalT b i (CEKValue b i m)
-- enforceKeySet (KeySet keys p) = do
--   let sigs = _ckeSigs ?cekRuntimeEnv
--       matched = Set.size $ Set.filter (`Set.member` keys) sigs
--       count = Set.size keys
--   case p of
--     KeysAll | matched == count -> pure (VLiteral LUnit)
--     Keys2 | matched >= 2 -> pure (VLiteral LUnit)
--     KeysAny | matched > 0 -> pure (VLiteral LUnit)
--     _ -> throwM (EnforceException "cannot match keyset predicate")

-- enforceKeySetRef :: CEKRuntime b i => KeySetName -> EvalT b i (CEKValue b i m)
-- enforceKeySetRef ksr = do
--   let pactDb = _ckePactDb ?cekRuntimeEnv
--   liftIO (_readKeyset pactDb ksr) >>= \case
--     Just ks -> enforceKeySet ks
--     Nothing -> throwM (EnforceException "no such keyset")

-- enforceUserGuard :: CEKRuntime b i => CEKValue b i m -> EvalT b i (CEKValue b i m)
-- enforceUserGuard = \case
--   v@VClosure{} -> unsafeApplyOne v (VLiteral LUnit) >>= \case
--     VLiteral LUnit -> pure (VLiteral LUnit)
--     _ -> failInvariant "expected a function returning unit"
--   _ -> failInvariant "invalid type for user closure"

-- createUserGuard :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- createUserGuard = mkBuiltinFn \case
--   [v@VClosure{}] -> pure (VGuard (GUserGuard v))
--   _ -> failInvariant "create-user-guard"

listAccess :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
listAccess = mkBuiltinFn \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> pure v
      _ -> throwExecutionError' (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  _ -> failInvariant "list-access"

-----------------------------------
-- Other Core forms
-----------------------------------

coreIf :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreIf = mkBuiltinFn \case
  [VLiteral (LBool b), VClosure tbody tenv, VClosure fbody fenv] ->
    if b then eval tenv tbody else  eval fenv fbody
  _ -> failInvariant "if"

coreB64Encode :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreB64Encode = mkBuiltinFn \case
  [VLiteral (LString l)] ->
    pure $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  _ -> failInvariant "base64-encode"


coreB64Decode :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreB64Decode = mkBuiltinFn \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError' (DecodeError "invalid b64 encoding")
    Right txt -> pure (VLiteral (LString txt))
  _ -> failInvariant "base64-encode"



-----------------------------------
-- Core definitions
-----------------------------------

unimplemented :: BuiltinFn b i m
unimplemented = error "unimplemented"

coreBuiltinRuntime :: MonadCEK CoreBuiltin i m => CoreBuiltin -> BuiltinFn CoreBuiltin i m
coreBuiltinRuntime = \case
  -- Int Add + num ops
  AddInt -> addInt AddInt
  SubInt -> subInt SubInt
  DivInt -> divInt DivInt
  MulInt -> mulInt MulInt
  NegateInt -> negateInt NegateInt
  AbsInt -> absInt AbsInt
  -- Int fractional
  ExpInt -> expInt ExpInt
  LnInt -> lnInt LnInt
  SqrtInt -> sqrtInt SqrtInt
  LogBaseInt -> unimplemented
  -- Geenral int ops
  ModInt -> modInt ModInt
  BitAndInt -> bitAndInt BitAndInt
  BitOrInt -> bitOrInt BitOrInt
  BitXorInt ->  bitXorInt BitXorInt
  BitShiftInt -> bitShiftInt BitShiftInt
  BitComplementInt -> bitComplementInt BitComplementInt
  -- Int Equality + Ord
  EqInt -> eqInt EqInt
  NeqInt -> neqInt NeqInt
  GTInt -> gtInt GTInt
  GEQInt -> geqInt GEQInt
  LTInt -> ltInt LTInt
  LEQInt -> leqInt LEQInt
  -- IntShow inst
  ShowInt -> showInt ShowInt
  -- If
  IfElse -> coreIf IfElse
  -- Decimal ops
  -- Add + Num
  AddDec -> addDec AddDec
  SubDec -> subDec SubDec
  DivDec -> divDec DivDec
  MulDec -> mulDec MulDec
  NegateDec -> negateDec NegateDec
  AbsDec -> absDec AbsDec
  -- Decimal rounding ops
  RoundDec -> roundDec RoundDec
  CeilingDec -> ceilingDec CeilingDec
  FloorDec -> floorDec FloorDec
  -- Decimal fractional
  ExpDec -> expDec ExpDec
  LnDec -> lnDec LnDec
  LogBaseDec -> unimplemented
  SqrtDec -> sqrtDec SqrtDec
  -- Decimal show
  ShowDec -> showDec ShowDec
  -- Decimal Equality + Ord
  EqDec -> eqDec EqDec
  NeqDec -> neqDec NeqDec
  GTDec -> gtDec GTDec
  GEQDec -> geqDec GEQDec
  LTDec -> ltDec LTDec
  LEQDec -> leqDec LEQDec
  -- Bool Ops
  AndBool -> andBool AndBool
  OrBool -> orBool OrBool
  NotBool -> notBool NotBool
  -- Bool Equality
  EqBool -> eqBool EqBool
  NeqBool -> neqBool NeqBool
  ShowBool -> showBool ShowBool
  -- String Equality + Ord
  EqStr -> eqStr EqStr
  NeqStr -> neqStr NeqStr
  GTStr -> gtStr GTStr
  GEQStr -> geqStr GEQStr
  LTStr -> ltStr LTStr
  LEQStr -> leqStr LEQStr
  -- String Ops
  AddStr -> addStr AddStr
  -- String listlike
  ConcatStr -> concatStr ConcatStr
  DropStr -> dropStr DropStr
  TakeStr -> takeStr TakeStr
  LengthStr -> lengthStr LengthStr
  ReverseStr -> reverseStr ReverseStr
  -- String show
  ShowStr -> showStr ShowStr
  -- Object equality
  -- EqObj -> eqObj EqObj
  -- NeqObj -> neqObj NeqObj
  -- List Equality + Ord
  EqList -> eqList EqList
  NeqList -> neqList NeqList
  GTList -> unimplemented
  GEQList -> unimplemented
  LTList -> unimplemented
  LEQList -> unimplemented
  -- List Show
  ShowList -> pcShowList ShowList
  -- ListAdd
  AddList -> addList AddList
  -- List ListlLike
  TakeList -> takeList TakeList
  DropList -> dropList DropList
  LengthList -> lengthList LengthList
  ConcatList -> concatList ConcatList
  ReverseList -> reverseList ReverseList
  -- misc list ops
  FilterList -> coreFilter FilterList
  DistinctList -> unimplemented
  ZipList -> zipList ZipList
  MapList -> coreMap MapList
  FoldList -> coreFold FoldList
  -- Unit ops
  EqUnit -> eqUnit EqUnit
  NeqUnit -> neqUnit NeqUnit
  ShowUnit -> showUnit ShowUnit
  Enforce -> coreEnforce Enforce
  EnforceOne -> unimplemented
    -- coreEnforceOne EnforceOne
  Enumerate -> coreEnumerate Enumerate
  EnumerateStepN -> coreEnumerateStepN EnumerateStepN
  ReadInteger -> unimplemented
  ReadDecimal -> unimplemented
  ReadString -> unimplemented
  -- ReadInteger -> coreReadInteger ReadInteger
  -- ReadDecimal -> coreReadDecimal ReadDecimal
  -- ReadString -> coreReadString ReadString
  -- ReadKeyset -> coreReadKeyset ReadKeyset
  -- EnforceGuard -> coreEnforceGuard EnforceGuard
  -- KeysetRefGuard -> coreKeysetRefGuard KeysetRefGuard
  -- CreateUserGuard -> createUserGuard CreateUserGuard
  ListAccess -> listAccess ListAccess
  B64Encode -> coreB64Encode B64Encode
  B64Decode -> coreB64Decode B64Decode

coreBuiltinLiftedRuntime
  :: (MonadCEK b i m, BuiltinArity b)
  => (CoreBuiltin -> b)
  -> CoreBuiltin
  -> BuiltinFn b i m
coreBuiltinLiftedRuntime f = \case
  -- Int Add + num ops
  AddInt -> addInt (f AddInt)
  SubInt -> subInt (f SubInt)
  DivInt -> divInt (f DivInt)
  MulInt -> mulInt (f MulInt)
  NegateInt -> negateInt (f NegateInt)
  AbsInt -> absInt (f AbsInt)
  -- Int fractional
  ExpInt -> expInt (f ExpInt)
  LnInt -> lnInt (f LnInt)
  SqrtInt -> sqrtInt (f SqrtInt)
  LogBaseInt -> unimplemented
  -- Geenral int ops
  ModInt -> modInt (f ModInt)
  BitAndInt -> bitAndInt (f BitAndInt)
  BitOrInt -> bitOrInt (f BitOrInt)
  BitXorInt ->  bitXorInt (f BitXorInt)
  BitShiftInt -> bitShiftInt (f BitShiftInt)
  BitComplementInt -> bitComplementInt (f BitComplementInt)
  -- Int Equality + Ord
  EqInt -> eqInt (f EqInt)
  NeqInt -> neqInt (f NeqInt)
  GTInt -> gtInt (f GTInt)
  GEQInt -> geqInt (f GEQInt)
  LTInt -> ltInt (f LTInt)
  LEQInt -> leqInt (f LEQInt)
  -- IntShow inst
  ShowInt -> showInt (f ShowInt)
  -- If
  IfElse -> coreIf (f IfElse)
  -- Decimal ops
  -- Add + Num
  AddDec -> addDec (f AddDec)
  SubDec -> subDec (f SubDec)
  DivDec -> divDec (f DivDec)
  MulDec -> mulDec (f MulDec)
  NegateDec -> negateDec (f NegateDec)
  AbsDec -> absDec (f AbsDec)
  -- Decimal rounding ops
  RoundDec -> roundDec (f RoundDec)
  CeilingDec -> ceilingDec (f CeilingDec)
  FloorDec -> floorDec (f FloorDec)
  -- Decimal fractional
  ExpDec -> expDec (f ExpDec)
  LnDec -> lnDec (f LnDec)
  LogBaseDec -> unimplemented
  SqrtDec -> sqrtDec (f SqrtDec)
  -- Decimal show
  ShowDec -> showDec (f ShowDec)
  -- Decimal Equality + Ord
  EqDec -> eqDec (f EqDec)
  NeqDec -> neqDec (f NeqDec)
  GTDec -> gtDec (f GTDec)
  GEQDec -> geqDec (f GEQDec)
  LTDec -> ltDec (f LTDec)
  LEQDec -> leqDec (f LEQDec)
  -- Bool Ops
  AndBool -> andBool (f AndBool)
  OrBool -> orBool (f OrBool)
  NotBool -> notBool (f NotBool)
  -- Bool Equality
  EqBool -> eqBool (f EqBool)
  NeqBool -> neqBool (f NeqBool)
  ShowBool -> showBool (f ShowBool)
  -- String Equality + Ord
  EqStr -> eqStr (f EqStr)
  NeqStr -> neqStr (f NeqStr)
  GTStr -> gtStr (f GTStr)
  GEQStr -> geqStr (f GEQStr)
  LTStr -> ltStr (f LTStr)
  LEQStr -> leqStr (f LEQStr)
  -- String Ops
  AddStr -> addStr (f AddStr)
  -- String listlike
  ConcatStr -> concatStr (f ConcatStr)
  DropStr -> dropStr (f DropStr)
  TakeStr -> takeStr (f TakeStr)
  LengthStr -> lengthStr (f LengthStr)
  ReverseStr -> reverseStr (f ReverseStr)
  -- String show
  ShowStr -> showStr (f ShowStr)
  -- Object equality
  -- EqObj -> eqObj EqObj
  -- NeqObj -> neqObj NeqObj
  -- List Equality + Ord
  EqList -> eqList (f EqList)
  NeqList -> neqList (f NeqList)
  GTList -> unimplemented
  GEQList -> unimplemented
  LTList -> unimplemented
  LEQList -> unimplemented
  -- List Show
  ShowList -> pcShowList (f ShowList)
  -- ListAdd
  AddList -> addList (f AddList)
  -- List ListlLike
  TakeList -> takeList (f TakeList)
  DropList -> dropList (f DropList)
  LengthList -> lengthList (f LengthList)
  ConcatList -> concatList (f ConcatList)
  ReverseList -> reverseList (f ReverseList)
  -- misc list ops
  FilterList -> coreFilter (f FilterList)
  DistinctList -> unimplemented
  ZipList -> zipList (f ZipList)
  MapList -> coreMap (f MapList)
  FoldList -> coreFold (f FoldList)
  -- Unit ops
  EqUnit -> eqUnit (f EqUnit)
  NeqUnit -> neqUnit (f NeqUnit)
  ShowUnit -> showUnit (f ShowUnit)
  Enforce -> coreEnforce (f Enforce)
  EnforceOne -> unimplemented
    -- coreEnforceOne EnforceOne
  Enumerate -> coreEnumerate (f Enumerate)
  EnumerateStepN -> coreEnumerateStepN (f EnumerateStepN)
  ReadInteger -> unimplemented
  ReadDecimal -> unimplemented
  ReadString -> unimplemented
  -- ReadInteger -> coreReadInteger ReadInteger
  -- ReadDecimal -> coreReadDecimal ReadDecimal
  -- ReadString -> coreReadString ReadString
  -- ReadKeyset -> coreReadKeyset ReadKeyset
  -- EnforceGuard -> coreEnforceGuard EnforceGuard
  -- KeysetRefGuard -> coreKeysetRefGuard KeysetRefGuard
  -- CreateUserGuard -> createUserGuard CreateUserGuard
  ListAccess -> listAccess (f ListAccess)
  B64Encode -> coreB64Encode (f B64Encode)
  B64Decode -> coreB64Decode (f B64Decode)
