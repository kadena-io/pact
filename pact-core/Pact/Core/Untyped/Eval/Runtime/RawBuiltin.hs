{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK Evaluator for untyped core using our RawBuiltins (aka untyped, no typechecking)
--

module Pact.Core.Untyped.Eval.Runtime.RawBuiltin
 ( rawBuiltinRuntime
 ) where

import Control.Exception(throw)
import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
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
-- runRawCEK
--   :: CEKRuntimeEnv RawBuiltin i
--   -- ^ Runtime environment
--   -> EvalTerm RawBuiltin i
--   -- ^ Term to evaluate
--   -> IO (CEKValue RawBuiltin i)
-- runRawCEK = runCEK

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (BuiltinArity b, MonadCEK b i m) => (Integer -> Integer) -> b -> BuiltinFn b i m
unaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i)] -> pure (VLiteral (LInteger (op i)))
  _ -> failInvariant "unary int function"
{-# INLINABLE unaryIntFn #-}

-- unaryDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal) -> b -> BuiltinFn b i m
-- unaryDecFn op = mkBuiltinFn \case
--   [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal (op i)))
--   _ -> failInvariant "unary decimal function"
-- {-# INLINE unaryDecFn #-}

binaryIntFn
  :: (BuiltinArity b, MonadCEK b i m)
  => (Integer -> Integer -> Integer)
  -> b
  -> BuiltinFn b i m
binaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (op i i')))
  _ -> failInvariant "binary int function"
{-# INLINE binaryIntFn #-}

-- binaryDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal -> Decimal) -> b -> BuiltinFn b i m
-- binaryDecFn op = mkBuiltinFn \case
--   [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (op i i')))
--   _ -> failInvariant "binary decimal function"
-- {-# INLINE binaryDecFn #-}

binaryBoolFn :: (BuiltinArity b, MonadCEK b i m) => (Bool -> Bool -> Bool) -> b -> BuiltinFn b i m
binaryBoolFn op = mkBuiltinFn \case
  [VLiteral (LBool l), VLiteral (LBool r)] -> pure (VLiteral (LBool (op l r)))
  _ -> failInvariant "binary bool function"
{-# INLINE binaryBoolFn #-}

-- compareIntFn :: (BuiltinArity b, MonadCEK b i m) => (Integer -> Integer -> Bool) -> b -> BuiltinFn b i m
-- compareIntFn op = mkBuiltinFn \case
--   [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (op i i')))
--   _ -> failInvariant "int cmp function"
-- {-# INLINE compareIntFn #-}

-- compareDecFn :: (BuiltinArity b, MonadCEK b i m) => (Decimal -> Decimal -> Bool) -> b -> BuiltinFn b i m
-- compareDecFn op = mkBuiltinFn \case
--   [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (op i i')))
--   _ -> failInvariant "dec cmp function"
-- {-# INLINE compareDecFn #-}

-- compareStrFn :: (BuiltinArity b, MonadCEK b i m) => (Text -> Text -> Bool) -> b -> BuiltinFn b i m
-- compareStrFn op = mkBuiltinFn \case
--   [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (op i i')))
--   _ -> failInvariant "str cmp function"
-- {-# INLINE compareStrFn #-}

roundingFn :: (BuiltinArity b, MonadCEK b i m) => (Rational -> Integer) -> b -> BuiltinFn b i m
roundingFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i)] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> failInvariant "rounding function"
{-# INLINE roundingFn #-}

---------------------------------
-- integer ops
------------------------------
-- addInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- addInt = binaryIntFn (+)

-- subInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- subInt = binaryIntFn (-)

-- mulInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- mulInt = binaryIntFn (*)

-- divInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- divInt = binaryIntFn quot

-- negateInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- negateInt = unaryIntFn negate

-- modInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- modInt = binaryIntFn mod

-- eqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- eqInt = compareIntFn (==)

-- neqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- neqInt = compareIntFn (/=)

-- gtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- gtInt = compareIntFn (>)

-- ltInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- ltInt = compareIntFn (<)

-- geqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- geqInt = compareIntFn (>=)

-- leqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- leqInt = compareIntFn (<=)

-- bitAndInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- bitAndInt = binaryIntFn (.&.)

-- bitOrInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- bitOrInt = binaryIntFn (.|.)

-- bitComplementInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- bitComplementInt = unaryIntFn complement

-- bitXorInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- bitXorInt = binaryIntFn xor

-- bitShiftInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- bitShiftInt = mkBuiltinFn \case
--   [VLiteral (LInteger i), VLiteral (LInteger s)] ->
--     pure (VLiteral (LInteger (shift i (fromIntegral s))))
--   _ -> failInvariant "bit-shift-int"

-- absInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- absInt = unaryIntFn abs

-- expInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- expInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
--   _ -> failInvariant "expInt"

-- lnInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- lnInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
--   _ -> failInvariant "lnInt"

-- sqrtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- sqrtInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
--   _ -> failInvariant "sqrtInt"

-- showInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- showInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LString (T.pack (show i))))
--   _ -> failInvariant "showInt"

-- -------------------------
-- double ops
-- -------------------------

-- addDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- addDec = binaryDecFn (+)

-- subDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- subDec = binaryDecFn (-)

-- mulDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- mulDec = binaryDecFn (*)

-- divDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- divDec = binaryDecFn (/)

-- negateDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- negateDec = unaryDecFn negate

-- absDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- absDec = unaryDecFn abs

-- eqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- eqDec = compareDecFn (==)

-- neqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- neqDec = compareDecFn (/=)

-- gtDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- gtDec = compareDecFn (>)

-- geqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- geqDec = compareDecFn (>=)

-- ltDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- ltDec = compareDecFn (<)

-- leqDec :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- leqDec = compareDecFn (<=)

-- showDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- showDec = mkBuiltinFn \case
--   [VLiteral (LDecimal i)] ->
--     pure (VLiteral (LString (T.pack (show i))))
--   _ -> failInvariant "showDec"

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

-- expDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- expDec = unaryDecFn (f2Dec . exp . dec2F)

-- lnDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- lnDec = unaryDecFn (f2Dec . log . dec2F)

-- sqrtDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- sqrtDec = unaryDecFn (f2Dec . sqrt . dec2F)

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

-- eqBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- eqBool = binaryBoolFn (==)

-- neqBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- neqBool = binaryBoolFn (/=)

-- showBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- showBool = mkBuiltinFn \case
--   [VLiteral (LBool i)] -> do
--     let out = if i then "true" else "false"addInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- addInt = binaryIntFn (+)

-- subInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- subInt = binaryIntFn (-)

-- mulInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- mulInt = binaryIntFn (*)

-- divInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- divInt = binaryIntFn quot

-- negateInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- negateInt = unaryIntFn negate

-- modInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- modInt = binaryIntFn mod

-- eqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- eqInt = compareIntFn (==)

-- neqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- neqInt = compareIntFn (/=)

-- gtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- gtInt = compareIntFn (>)

-- ltInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- ltInt = compareIntFn (<)

-- geqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- geqInt = compareIntFn (>=)

-- leqInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- leqInt = compareIntFn (<=)

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

-- absInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- absInt = unaryIntFn abs

-- expInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- expInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
--   _ -> failInvariant "expInt"

-- lnInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- lnInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
--   _ -> failInvariant "lnInt"

-- sqrtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- sqrtInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
--   _ -> failInvariant "sqrtInt"

-- showInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- showInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LString (T.pack (show i))))
--   _ -> failInvariant "showInt"

-- geqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- geqStr = compareStrFn (>=)

-- ltStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- ltStr = compareStrFn (<)

-- leqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- leqStr = compareStrFn (<=)

-- addStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- addStr =  mkBuiltinFn \case
--   [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
--   _ -> failInvariant "addStr"

-- takeStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- takeStr = mkBuiltinFn \case
--   [VLiteral (LInteger i), VLiteral (LString t)] -> do
--     pure (VLiteral (LString (T.take (fromIntegral i) t)))
--   _ -> failInvariant "takeStr"

-- dropStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- dropStr = mkBuiltinFn \case
--   [VLiteral (LInteger i), VLiteral (LString t)] -> do
--     pure (VLiteral (LString (T.drop (fromIntegral i) t)))
--   _ -> failInvariant "dropStr"

-- lengthStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- lengthStr = mkBuiltinFn \case
--   [VLiteral (LString t)] -> do
--     pure (VLiteral (LInteger (fromIntegral (T.length t))))
--   _ -> failInvariant "lengthStr"

-- reverseStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- reverseStr = mkBuiltinFn \case
--   [VLiteral (LString t)] -> do
--     pure (VLiteral (LString (T.reverse t)))
--   _ -> failInvariant "reverseStr"

-- showStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- showStr = mkBuiltinFn \case
--   [VLiteral (LString t)] -> do
--     let out = "\"" <> t <> "\""
--     pure (VLiteral (LString out))
--   _ -> failInvariant "showStr"

-- concatStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- concatStr = mkBuiltinFn \case
--   [VList li] -> do
--     li' <- traverse asString li
--     pure (VLiteral (LString (T.concat (V.toList li'))))
--   _ -> failInvariant "concatStr"


---------------------------
-- Unit ops
---------------------------

-- eqUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- eqUnit = mkBuiltinFn \case
--   [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool True))
--   _ -> failInvariant "eqUnit"

-- neqUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- neqUnit = mkBuiltinFn \case
--   [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool False))
--   _ -> failInvariant "neqUnit"

-- showUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- showUnit = mkBuiltinFn \case
--   [VLiteral LUnit] -> pure (VLiteral (LString "()"))
--   _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

-- eqObj :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- eqObj = mkBuiltinFn \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
--   _ -> failInvariant "eqObj"

-- neqObj :: CoreBuiltin -> BuiltinFn CoreBuiltin i
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

-- asList :: CEKValue b i m -> EvalT b i (Vector (CEKValue b i m))
-- asList (VList l) = pure l
-- asList _ = failInvariant "asList"

-- Todo: Likely this using `throw` is not a good idea.
unsafeEqLiteral :: Literal -> Literal -> Bool
unsafeEqLiteral (LString i) (LString i') = i == i'
unsafeEqLiteral (LInteger i) (LInteger i') = i == i'
unsafeEqLiteral (LDecimal i) (LDecimal i') = i == i'
unsafeEqLiteral LUnit LUnit = True
unsafeEqLiteral (LBool i) (LBool i') = i == i'
unsafeEqLiteral (LTime i) (LTime i') = i == i'
unsafeEqLiteral _ _ =
  throw (FatalExecutionError "invariant failed in literal EQ")

-- unsafeNeqLiteral :: Literal -> Literal -> Bool
-- unsafeNeqLiteral a b = not (unsafeEqLiteral a b)

unsafeEqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
unsafeEqCEKValue _ _ = throw (FatalExecutionError "invariant failed in value Eq")

unsafeNeqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
-- ---------------------------
-- eqList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- eqList = mkBuiltinFn \case
--   [eqClo, VList l, VList r] ->
--     if V.length l /= V.length r then
--       pure (VLiteral (LBool False))
--     else do
--       v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo eqClo a b) l r
--       pure (VLiteral (LBool (and v')))
--   _ -> failInvariant "eqList"

-- neqList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- neqList = mkBuiltinFn \case
--   [neqClo, VList l, VList r] ->
--     if V.length l /= V.length r then
--       pure (VLiteral (LBool True))
--     else do
--       v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo neqClo a b) l r
--       pure (VLiteral (LBool (or v')))
--   _ -> failInvariant "neqList"

coreZip :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreZip = mkBuiltinFn \case
  [clo, VList l, VList r] -> do
    v' <- V.zipWithM (unsafeApplyTwo clo) l r
    pure (VList v')
  _ -> failInvariant "zipList"

-- addList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- addList = mkBuiltinFn \case
--   [VList l, VList r] -> pure (VList (l <> r))
--   _ -> failInvariant "addList"

-- pcShowList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- pcShowList = mkBuiltinFn \case
--   [showFn, VList l1] -> do
--     strli <- traverse ((=<<) asString  . unsafeApplyOne showFn) (V.toList l1)
--     let out = "[" <> T.intercalate ", " strli <> "]"
--     pure (VLiteral (LString out))
--   _ -> failInvariant "showList"

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


coreLength :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreLength = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    pure (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "length"


-- lengthList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- lengthList = mkBuiltinFn \case
--   [VList li] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
--   _ -> failInvariant "lengthList"

-- takeList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- takeList = mkBuiltinFn \case
--   [VLiteral (LInteger i), VList li] ->
--     pure (VList (V.take (fromIntegral i) li))
--   _ -> failInvariant "takeList"

-- dropList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- dropList = mkBuiltinFn \case
--   [VLiteral (LInteger i), VList li] ->
--     pure (VList (V.drop (fromIntegral i) li))
--   _ -> failInvariant "dropList"

-- reverseList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
-- reverseList = mkBuiltinFn \case
--   [VList li] ->
--     pure (VList (V.reverse li))
  -- _ -> failInvariant "takeList"

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

coreTake :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreTake = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.take (fromIntegral i) t)))
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.take (fromIntegral i) li))
  _ -> failInvariant "takeStr"

coreDrop :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreDrop = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.drop (fromIntegral i) t)))
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.drop (fromIntegral i) li))
  _ -> failInvariant "dropStr"

coreReverse :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreReverse = mkBuiltinFn \case
  [VList li] ->
    pure (VList (V.reverse li))
  [VLiteral (LString s)] ->
    pure (VLiteral (LString (T.reverse s)))
  _ -> failInvariant "takeList"

coreConcat :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreConcat = mkBuiltinFn \case
  [VList li] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> failInvariant "concatStr"

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
      Nothing ->
        throwExecutionError' (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
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

rawBuiltinRuntime :: MonadCEK RawBuiltin i m => RawBuiltin -> BuiltinFn RawBuiltin i m
rawBuiltinRuntime = \case
  RawAdd -> addBuiltin RawAdd
  -- Num
  RawSub -> subBuiltin RawSub
  RawMultiply -> mulBuiltin RawMultiply
  RawDivide -> divBuiltin RawDivide
  RawNegate -> negateBuiltin RawNegate
  RawAbs -> absBuiltin RawAbs
  -- Boolean Ops
  RawAnd -> andBool RawAnd
  RawOr -> orBool RawOr
  RawNot -> notBool RawNot
  -- Equality and Comparisons
  RawEq -> eqBuiltin RawEq
  RawNeq -> neqBuiltin RawNeq
  -- Ord
  RawGT -> gtBuiltin RawGT
  RawGEQ -> geqBuiltin RawGEQ
  RawLT -> ltBuiltin RawLT
  RawLEQ -> leqBuiltin RawLEQ
  -- Bitwise Ops
  RawBitwiseAnd -> bitAndInt RawBitwiseAnd
  RawBitwiseOr -> bitOrInt RawBitwiseOr
  RawBitwiseXor -> bitXorInt RawBitwiseXor
  RawBitwiseFlip -> bitComplementInt RawBitwiseFlip
  RawBitShift -> bitShiftInt RawBitShift
  --  Rounding
  RawRound -> roundDec RawRound
  RawCeiling -> ceilingDec RawCeiling
  RawFloor -> floorDec RawFloor
  -- Fractional
  RawExp -> coreExp RawExp
  RawLn -> coreLn RawLn
  RawSqrt -> coreSqrt RawSqrt
  RawLogBase -> unimplemented
  -- List like
  RawLength -> coreLength RawLength
  RawTake -> coreTake RawTake
  RawDrop -> coreDrop RawDrop
  RawConcat -> coreConcat RawConcat
  RawReverse -> coreReverse RawReverse
  -- General
  RawMod -> modBuiltin RawMod
  RawMap -> coreMap RawMap
  RawFilter -> coreFilter RawFilter
  RawZip -> coreZip RawZip
  RawIf -> coreIf RawIf
  RawIntToStr -> unimplemented
  RawStrToInt -> unimplemented
  RawFold -> coreFold RawFold
  RawDistinct -> coreFold RawDistinct
  RawEnforce -> coreEnforce RawEnforce
  RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate RawEnumerate
  RawEnumerateStepN -> coreEnumerateStepN RawEnumerateStepN
  RawShow -> unimplemented
  RawReadInteger -> unimplemented
  RawReadDecimal -> unimplemented
  RawReadString -> unimplemented
  -- RawReadInteger -> coreReadInteger RawReadInteger
  -- RawReadDecimal -> coreReadDecimal RawReadDecimal
  -- RawReadString -> coreReadString RawReadString
  -- RawReadKeyset -> undefined
  -- RawEnforceGuard -> coreEnforceGuard RawEnforceGuard
  -- RawKeysetRefGuard -> coreKeysetRefGuard RawKeysetRefGuard
  -- RawCreateUserGuard -> createUserGuard RawCreateUserGuard
  RawListAccess -> listAccess RawListAccess
  RawB64Encode -> coreB64Encode RawB64Encode
  RawB64Decode -> coreB64Decode RawB64Decode

-- rawBuiltinRuntime :: Array.Array (BuiltinFn RawBuiltin i)
-- rawBuiltinRuntime = Array.arrayFromList (rawBuiltinFn <$> [minBound .. maxBound])

-- TODO: Coercions between int/double
addBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
addBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (i + i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (i + i')))
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
  [VList l, VList r] -> pure (VList (l <> r))
  _ -> failInvariant "add"

-- TODO: Coercions between int/double
-- Todo: factor defn
subBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
subBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (i - i')))
  _ -> failInvariant "sub"

-- TODO: Coercions between int/double
mulBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
mulBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (i * i')))
  _ -> failInvariant "mul"

divBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
divBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (quot i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (i / i')))
  _ -> failInvariant "div"

modBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
modBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (mod i i')))
  _ -> failInvariant "div"

eqBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
eqBuiltin = mkBuiltinFn \case
  [l, r] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
  _ -> failInvariant "eq"

--
neqBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
neqBuiltin = mkBuiltinFn \case
  [l, r] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
  _ -> failInvariant "neq"

gtBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
gtBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (i > i')))
  _ -> failInvariant "gt"

ltBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
ltBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (i < i')))
  _ -> failInvariant "lt"


geqBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
geqBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (i >= i')))
  _ -> failInvariant "geq"


leqBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
leqBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (i <= i')))
  _ -> failInvariant "lt"

absBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
absBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i)] -> pure (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal (abs i)))
  _ -> failInvariant "cmp function"

negateBuiltin :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
negateBuiltin = mkBuiltinFn \case
  [VLiteral (LInteger i)] -> pure (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal (negate i)))
  _ -> failInvariant "cmp function"

--------------------------------------------------------------------
--- Transcendentals!!!
-- Note: use musl
----------------------------------------------------------------
coreExp :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreExp = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal ((f2Dec . exp . dec2F) i)))
  _ -> failInvariant "expInt"

coreLn :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreLn = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal ((f2Dec . log . dec2F) i)))
  _ -> failInvariant "lnInt"

coreSqrt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
coreSqrt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal ((f2Dec . sqrt . dec2F) i)))
  _ -> failInvariant "sqrtInt"


-- fractionalNumericFn op = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (op (fromIntegral i)))))
--   _ -> failInvariant "expInt"

-- expInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- expInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
--   _ -> failInvariant "expInt"

-- lnInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- lnInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
--   _ -> failInvariant "lnInt"

-- sqrtInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- sqrtInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
--   _ -> failInvariant "sqrtInt"

-- showInt :: (BuiltinArity b, MonadCEK b i m) => b -> BuiltinFn b i m
-- showInt = mkBuiltinFn \case
--   [VLiteral (LInteger i)] ->
--     pure (VLiteral (LString (T.pack (show i))))
--   _ -> failInvariant "showInt"
