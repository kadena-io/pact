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
-- CEK Evaluator for untyped core.
--

module Pact.Core.Untyped.Eval.CEK
 ( CEKTLEnv
 , CEKEnv
 , CEKValue(..)
 , BuiltinFn(..)
 , CEKState(..)
 , CEKRuntime
 , runCEK
 , runCoreCEK
 , Cont(..)
 , coreBuiltinRuntime
 ) where

import Control.Lens hiding (op)
import Control.Monad(guard)
import Control.Monad.Catch
import Control.Exception(throw)
import Control.Monad.IO.Class(liftIO)
import Data.Bits
import Data.Char(isHexDigit)
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Primitive(Array, indexArray)
import Data.Void
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Vector as V
import qualified Data.Primitive.Array as Array
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as Set

import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.Type
import Pact.Core.Persistence
import Pact.Core.Hash

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.Runtime

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: CEKRuntime b i
  => CEKEnv b i
  -> EvalTerm b i
  -> EvalT b (CEKValue b i)
eval = evalCEK Mt
  where
  evalCEK
    :: CEKRuntime b i
    => Cont b i
    -> CEKEnv b i
    -> EvalTerm b i
    -> EvalT b (CEKValue b i)
  evalCEK cont env (Var n _)  =
    case _nKind n of
      NBound i -> returnCEK cont (env RAList.!! i)
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> let
        !fqn = FullyQualifiedName mname (_nName n) mh
        in case Map.lookup fqn ?cekLoaded of
          Just t -> evalCEK cont RAList.Nil t
          Nothing -> failInvariant "top level name not in scope"
  evalCEK cont _env (Constant l _)=
    returnCEK cont (VLiteral l)
  evalCEK cont env (App fn arg _) =
    evalCEK (Arg env arg cont) env fn
  evalCEK cont env (Lam body _) =
    returnCEK cont (VClosure body env)
  evalCEK cont _env (Builtin b _) =
    returnCEK cont (VNative (indexArray ?cekBuiltins (fromEnum b)))
  evalCEK cont env (ObjectLit obj _) = do
    vs <- traverse (evalCEK Mt env) obj
    returnCEK cont (VObject vs)
  evalCEK cont env (Block (t :| ts) _) =
    evalCEK (BlockC env ts cont) env t
  evalCEK cont env (ListLit ts _) = do
    ts' <- traverse (evalCEK Mt env) ts
    returnCEK cont (VList ts')
  evalCEK cont env (ObjectOp op _) = case op of
    ObjectAccess f o -> do
      o' <- evalCEK Mt env o
      v' <- objAccess f o'
      returnCEK cont v'
    ObjectRemove f o -> do
      o' <- evalCEK Mt env o
      v' <- objRemove f o'
      returnCEK cont v'
    ObjectExtend f v o -> do
      o' <- evalCEK Mt env o
      v' <- evalCEK Mt env v
      out <- objUpdate f o' v'
      returnCEK cont out
    ReadEnvObject ty o -> do
      o' <- evalCEK Mt env o
      v' <- coreReadObject ty o'
      returnCEK cont v'
  returnCEK
    :: CEKRuntime b i
    => Cont b i
    -> CEKValue b i
    -> EvalT b (CEKValue b i)
  returnCEK (Arg env arg cont) fn =
    evalCEK (Fn fn cont) env arg
  returnCEK (Fn fn ctx) arg =
    applyLam fn arg ctx
  returnCEK (BlockC env (t:ts) cont) _discarded =
    evalCEK (BlockC env ts cont) env t
  returnCEK (BlockC _ [] cont) v =
    returnCEK cont v
  returnCEK Mt v = return v
  applyLam (VClosure body env) arg cont =
    evalCEK cont (RAList.cons arg env) body
  applyLam (VNative (BuiltinFn b fn arity args)) arg cont
    | arity - 1 == 0 = fn (reverse (arg:args)) >>= returnCEK cont
    | otherwise = returnCEK cont (VNative (BuiltinFn b fn (arity - 1) (arg:args)))
  applyLam _ _ _ = failInvariant "Applying value to non-function"
  objAccess f (VObject o) = case Map.lookup f o of
    Just v -> pure v
    Nothing -> failInvariant "object access"
  objAccess _ _ = failInvariant "object access"
  objRemove f (VObject o) = pure (VObject (Map.delete f o))
  objRemove _ _ = failInvariant "object remove"
  objUpdate f v (VObject o) = pure (VObject (Map.insert f v o))
  objUpdate _ _ _ = failInvariant "object update"

runCEK
  :: Enum b
  => CEKTLEnv b i
  -- ^ Top levels
  -> Array (BuiltinFn b i)
  -- ^ Builtins
  -> RuntimeEnv b i
  -- ^ runtime environment
  -> EvalTerm b i
  -- ^ Term to evaluate
  -> IO (CEKValue b i, CEKState b)
runCEK env builtins renv term = let
  ?cekLoaded = env
  ?cekBuiltins = builtins
  ?cekRuntimeEnv = renv
  in let
    cekState = CEKState 0 (Just [])
  in runEvalT cekState (eval RAList.Nil term)

-- | Run our CEK interpreter
--   for only our core builtins
runCoreCEK
  :: CEKTLEnv CoreBuiltin i
  -- ^ Top levels
  -> RuntimeEnv CoreBuiltin i
  -- ^ Runtime environment
  -> EvalTerm CoreBuiltin i
  -- ^ Term to evaluate
  -> IO (CEKValue CoreBuiltin i, CEKState CoreBuiltin)
runCoreCEK env =
  runCEK env coreBuiltinRuntime

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------
applyTwo :: CEKRuntime b i => EvalTerm b i -> CEKEnv b  i -> CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
applyTwo body env arg1 arg2 = eval (RAList.cons arg2 (RAList.cons arg1 env)) body

unsafeApplyOne :: CEKRuntime b i => CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
unsafeApplyOne (VClosure body env) arg = eval (RAList.cons arg env) body
unsafeApplyOne (VNative (BuiltinFn b fn arity args)) arg =
  if arity - 1 <= 0 then fn (reverse (arg:args))
  else pure (VNative (BuiltinFn b fn (arity -1) (arg:args)))
unsafeApplyOne _ _ = error "impossible"

unsafeApplyTwo :: CEKRuntime b i => CEKValue b i -> CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
unsafeApplyTwo (VClosure (Lam body _) env) arg1 arg2 = applyTwo body env arg1 arg2
unsafeApplyTwo (VNative (BuiltinFn b fn arity args)) arg1 arg2 =
  if arity - 2 <= 0 then fn (reverse (arg1:arg2:args))
  else pure $ VNative $ BuiltinFn b fn (arity - 2) (arg1:arg2:args)
unsafeApplyTwo _ _ _ = error "impossible"

mkBuiltinFn
  :: BuiltinArity b
  => (CEKRuntime b i => [CEKValue b i] -> EvalT b (CEKValue b i))
  -> b
  -> BuiltinFn b i
mkBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkBuiltinFn #-}

-- -- Todo: runtime error
unaryIntFn :: BuiltinArity b => (Integer -> Integer) -> b -> BuiltinFn b i
unaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i)] -> pure (VLiteral (LInteger (op i)))
  _ -> failInvariant "unary int function"
{-# INLINE unaryIntFn #-}

unaryDecFn :: BuiltinArity b => (Decimal -> Decimal) -> b -> BuiltinFn b i
unaryDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i)] -> pure (VLiteral (LDecimal (op i)))
  _ -> failInvariant "unary decimal function"
{-# INLINE unaryDecFn #-}

binaryIntFn
  :: BuiltinArity b
  => (Integer -> Integer -> Integer)
  -> b
  -> BuiltinFn b i
binaryIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LInteger (op i i')))
  _ -> failInvariant "binary int function"
{-# INLINE binaryIntFn #-}

binaryDecFn :: BuiltinArity b => (Decimal -> Decimal -> Decimal) -> b -> BuiltinFn b i
binaryDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (op i i')))
  _ -> failInvariant "binary decimal function"
{-# INLINE binaryDecFn #-}

binaryBoolFn :: BuiltinArity b => (Bool -> Bool -> Bool) -> b -> BuiltinFn b i
binaryBoolFn op = mkBuiltinFn \case
  [VLiteral (LBool l), VLiteral (LBool r)] -> pure (VLiteral (LBool (op l r)))
  _ -> failInvariant "binary bool function"
{-# INLINE binaryBoolFn #-}

compareIntFn :: BuiltinArity b => (Integer -> Integer -> Bool) -> b -> BuiltinFn b i
compareIntFn op = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "int cmp function"
{-# INLINE compareIntFn #-}

compareDecFn :: BuiltinArity b => (Decimal -> Decimal -> Bool) -> b -> BuiltinFn b i
compareDecFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "dec cmp function"
{-# INLINE compareDecFn #-}

compareStrFn :: BuiltinArity b => (Text -> Text -> Bool) -> b -> BuiltinFn b i
compareStrFn op = mkBuiltinFn \case
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LBool (op i i')))
  _ -> failInvariant "str cmp function"
{-# INLINE compareStrFn #-}

roundingFn :: BuiltinArity b => (Rational -> Integer) -> b -> BuiltinFn b i
roundingFn op = mkBuiltinFn \case
  [VLiteral (LDecimal i)] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> failInvariant "rounding function"
{-# INLINE roundingFn #-}

---------------------------------
-- integer ops
------------------------------
addInt :: BuiltinArity b => b -> BuiltinFn b i
addInt = binaryIntFn (+)

subInt :: BuiltinArity b => b -> BuiltinFn b i
subInt = binaryIntFn (-)

mulInt :: BuiltinArity b => b -> BuiltinFn b i
mulInt = binaryIntFn (*)

divInt :: BuiltinArity b => b -> BuiltinFn b i
divInt = binaryIntFn quot

negateInt :: BuiltinArity b => b -> BuiltinFn b i
negateInt = unaryIntFn negate

modInt :: BuiltinArity b => b -> BuiltinFn b i
modInt = binaryIntFn mod

eqInt :: BuiltinArity b => b -> BuiltinFn b i
eqInt = compareIntFn (==)

neqInt :: BuiltinArity b => b -> BuiltinFn b i
neqInt = compareIntFn (/=)

gtInt :: BuiltinArity b => b -> BuiltinFn b i
gtInt = compareIntFn (>)

ltInt :: BuiltinArity b => b -> BuiltinFn b i
ltInt = compareIntFn (<)

geqInt :: BuiltinArity b => b -> BuiltinFn b i
geqInt = compareIntFn (>=)

leqInt :: BuiltinArity b => b -> BuiltinFn b i
leqInt = compareIntFn (<=)

bitAndInt :: BuiltinArity b => b -> BuiltinFn b i
bitAndInt = binaryIntFn (.&.)

bitOrInt :: BuiltinArity b => b -> BuiltinFn b i
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: BuiltinArity b => b -> BuiltinFn b i
bitComplementInt = unaryIntFn complement

bitXorInt :: BuiltinArity b => b -> BuiltinFn b i
bitXorInt = binaryIntFn xor

bitShiftInt :: BuiltinArity b => b -> BuiltinFn b i
bitShiftInt = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LInteger s)] ->
    pure (VLiteral (LInteger (shift i (fromIntegral s))))
  _ -> failInvariant "bit-shift-int"

absInt :: BuiltinArity b => b -> BuiltinFn b i
absInt = unaryIntFn abs

expInt :: BuiltinArity b => b -> BuiltinFn b i
expInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
  _ -> failInvariant "expInt"

lnInt :: BuiltinArity b => b -> BuiltinFn b i
lnInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
  _ -> failInvariant "lnInt"

sqrtInt :: BuiltinArity b => b -> BuiltinFn b i
sqrtInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
  _ -> failInvariant "sqrtInt"

showInt :: BuiltinArity b => b -> BuiltinFn b i
showInt = mkBuiltinFn \case
  [VLiteral (LInteger i)] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showInt"

-- -------------------------
-- double ops
-- -------------------------

addDec :: BuiltinArity b => b -> BuiltinFn b i
addDec = binaryDecFn (+)

subDec :: BuiltinArity b => b -> BuiltinFn b i
subDec = binaryDecFn (-)

mulDec :: BuiltinArity b => b -> BuiltinFn b i
mulDec = binaryDecFn (*)

divDec :: BuiltinArity b => b -> BuiltinFn b i
divDec = binaryDecFn (/)

negateDec :: BuiltinArity b => b -> BuiltinFn b i
negateDec = unaryDecFn negate

absDec :: BuiltinArity b => b -> BuiltinFn b i
absDec = unaryDecFn abs

eqDec :: BuiltinArity b => b -> BuiltinFn b i
eqDec = compareDecFn (==)

neqDec :: BuiltinArity b => b -> BuiltinFn b i
neqDec = compareDecFn (/=)

gtDec :: BuiltinArity b => b -> BuiltinFn b i
gtDec = compareDecFn (>)

geqDec :: BuiltinArity b => b -> BuiltinFn b i
geqDec = compareDecFn (>=)

ltDec :: BuiltinArity b => b -> BuiltinFn b i
ltDec = compareDecFn (<)

leqDec :: BuiltinArity b => b -> BuiltinFn b i
leqDec = compareDecFn (<=)

showDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
showDec = mkBuiltinFn \case
  [VLiteral (LDecimal i)] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showDec"

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
roundDec = roundingFn round
floorDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
floorDec = roundingFn floor
ceilingDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
ceilingDec = roundingFn ceiling

expDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
expDec = unaryDecFn (f2Dec . exp . dec2F)

lnDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
lnDec = unaryDecFn (f2Dec . log . dec2F)

sqrtDec :: CoreBuiltin -> BuiltinFn CoreBuiltin i
sqrtDec = unaryDecFn (f2Dec . sqrt . dec2F)

---------------------------
-- bool ops
---------------------------
andBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
andBool = binaryBoolFn (&&)

orBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
orBool = binaryBoolFn (||)

notBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
notBool = mkBuiltinFn \case
  [VLiteral (LBool i)] -> pure (VLiteral (LBool (not i)))
  _ -> failInvariant "notBool"

eqBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
eqBool = binaryBoolFn (==)

neqBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
neqBool = binaryBoolFn (/=)

showBool :: CoreBuiltin -> BuiltinFn CoreBuiltin i
showBool = mkBuiltinFn \case
  [VLiteral (LBool i)] -> do
    let out = if i then "true" else "false"
    pure (VLiteral (LString out))
  _ -> failInvariant "showBool"

---------------------------
-- string ops
---------------------------
eqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
eqStr = compareStrFn (==)

neqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
neqStr = compareStrFn (/=)

gtStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
gtStr = compareStrFn (>)

geqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
geqStr = compareStrFn (>=)

ltStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
ltStr = compareStrFn (<)

leqStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
leqStr = compareStrFn (<=)

addStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
addStr =  mkBuiltinFn \case
  [VLiteral (LString i), VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
  _ -> failInvariant "addStr"

takeStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
takeStr = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.take (fromIntegral i) t)))
  _ -> failInvariant "takeStr"

dropStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
dropStr = mkBuiltinFn \case
  [VLiteral (LInteger i), VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.drop (fromIntegral i) t)))
  _ -> failInvariant "dropStr"

lengthStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
lengthStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    pure (VLiteral (LInteger (fromIntegral (T.length t))))
  _ -> failInvariant "lengthStr"

reverseStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
reverseStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.reverse t)))
  _ -> failInvariant "reverseStr"

showStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
showStr = mkBuiltinFn \case
  [VLiteral (LString t)] -> do
    let out = "\"" <> t <> "\""
    pure (VLiteral (LString out))
  _ -> failInvariant "showStr"

concatStr :: CoreBuiltin -> BuiltinFn CoreBuiltin i
concatStr = mkBuiltinFn \case
  [VList li] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> failInvariant "concatStr"


---------------------------
-- Unit ops
---------------------------

eqUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
eqUnit = mkBuiltinFn \case
  [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool True))
  _ -> failInvariant "eqUnit"

neqUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
neqUnit = mkBuiltinFn \case
  [VLiteral LUnit, VLiteral LUnit] -> pure (VLiteral (LBool False))
  _ -> failInvariant "neqUnit"

showUnit :: CoreBuiltin -> BuiltinFn CoreBuiltin i
showUnit = mkBuiltinFn \case
  [VLiteral LUnit] -> pure (VLiteral (LString "()"))
  _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

eqObj :: CoreBuiltin -> BuiltinFn CoreBuiltin i
eqObj = mkBuiltinFn \case
  [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
  _ -> failInvariant "eqObj"

neqObj :: CoreBuiltin -> BuiltinFn CoreBuiltin i
neqObj = mkBuiltinFn \case
  [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
  _ -> failInvariant "neqObj"


------------------------------
--- conversions + unsafe ops
------------------------------
asBool :: CEKValue b i -> EvalT b Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = failInvariant "asBool"

asString :: CEKValue b i -> EvalT b Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

asList :: CEKValue b i -> EvalT b (Vector (CEKValue b i))
asList (VList l) = pure l
asList _ = failInvariant "asList"

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

unsafeEqCEKValue :: CEKValue b i -> CEKValue b i -> Bool
unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
unsafeEqCEKValue (VObject o) (VObject o') = and (Map.intersectionWith unsafeEqCEKValue o o')
unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
unsafeEqCEKValue _ _ = throw (FatalExecutionError "invariant failed in value Eq")

unsafeNeqCEKValue :: CEKValue b i -> CEKValue b i -> Bool
unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
---------------------------
eqList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
eqList = mkBuiltinFn \case
  [eqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool False))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo eqClo a b) l r
      pure (VLiteral (LBool (and v')))
  _ -> failInvariant "eqList"

neqList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
neqList = mkBuiltinFn \case
  [neqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool True))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo neqClo a b) l r
      pure (VLiteral (LBool (or v')))
  _ -> failInvariant "neqList"

zipList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
zipList = mkBuiltinFn \case
  [clo, VList l, VList r] -> do
    v' <- V.zipWithM (unsafeApplyTwo clo) l r
    pure (VList v')
  _ -> failInvariant "zipList"

addList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
addList = mkBuiltinFn \case
  [VList l, VList r] -> pure (VList (l <> r))
  _ -> failInvariant "addList"

pcShowList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
pcShowList = mkBuiltinFn \case
  [showFn, VList l1] -> do
    strli <- traverse ((=<<) asString  . unsafeApplyOne showFn) (V.toList l1)
    let out = "[" <> T.intercalate ", " strli <> "]"
    pure (VLiteral (LString out))
  _ -> failInvariant "showList"

coreMap :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreMap = mkBuiltinFn \case
  [fn, VList li] -> do
    li' <- traverse (unsafeApplyOne fn) li
    pure (VList li')
  _ -> failInvariant "map"

coreFilter :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreFilter = mkBuiltinFn \case
  [fn, VList li] -> do
    let applyOne' arg = unsafeApplyOne fn arg >>= asBool
    li' <- V.filterM applyOne' li
    pure (VList li')
  _ -> failInvariant "filter"

coreFold :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreFold = mkBuiltinFn \case
  [fn, initElem, VList li] -> V.foldM' (unsafeApplyTwo fn) initElem li
  _ -> failInvariant "fold"

lengthList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
lengthList = mkBuiltinFn \case
  [VList li] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "lengthList"

takeList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
takeList = mkBuiltinFn \case
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.take (fromIntegral i) li))
  _ -> failInvariant "takeList"

dropList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
dropList = mkBuiltinFn \case
  [VLiteral (LInteger i), VList li] ->
    pure (VList (V.drop (fromIntegral i) li))
  _ -> failInvariant "dropList"

reverseList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
reverseList = mkBuiltinFn \case
  [VList li] ->
    pure (VList (V.reverse li))
  _ -> failInvariant "takeList"

coreEnumerate :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreEnumerate = mkBuiltinFn \case
  [VLiteral (LInteger from'), VLiteral (LInteger to')] -> enum' from' to'
  _ -> failInvariant "enumerate"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from' to'
    | to' >= from' = pure $ toVecList $ V.enumFromN from' (fromIntegral (to' - from' + 1))
    | otherwise = pure $ toVecList $ V.enumFromStepN from' (-1) (fromIntegral (from' - to' + 1))

coreEnumerateStepN :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreEnumerateStepN = mkBuiltinFn \case
  [VLiteral (LInteger from'), VLiteral (LInteger to'), VLiteral (LInteger step)] -> enum' from' to' step
  _ -> failInvariant "enumerate-step"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from' to' step
    | to' > from' && step > 0 = pure $ toVecList $ V.enumFromStepN from' step (fromIntegral ((to' - from' + 1) `quot` step))
    | from' > to' && step < 0 = pure $ toVecList $ V.enumFromStepN from' step (fromIntegral ((from' - to' + 1) `quot` step))
    | from' == to' && step == 0 = pure $ toVecList $ V.singleton from'
    | otherwise = throwM (EnumeratationError "enumerate outside interval bounds")

concatList :: CoreBuiltin -> BuiltinFn CoreBuiltin i
concatList = mkBuiltinFn \case
  [VList li] -> do
    li' <- traverse asList li
    pure (VList (V.concat (V.toList li')))
  _ -> failInvariant "takeList"


coreEnforce :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreEnforce = mkBuiltinFn \case
  [VLiteral (LBool b), VLiteral (LString s)] ->
    if b then pure (VLiteral LUnit)
    else throwM (EnforceException s)
  _ -> failInvariant "enforce"

coreEnforceOne :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreEnforceOne = mkBuiltinFn \case
  [VList v, VLiteral (LString msg)] ->
    enforceFail msg (V.toList v)
  _ -> failInvariant "coreEnforceOne"
  where
  handler msg rest = \case
    EnforceException _ -> enforceFail msg rest
    e -> throwM e
  enforceClo _ [] = pure (VLiteral LUnit)
  enforceClo msg (x:xs) = catch (unsafeApplyOne x (VLiteral LUnit)) (handler msg xs)
  enforceFail msg [] = throwM (EnforceException msg)
  enforceFail msg as = enforceClo msg as
-----------------------------------
-- Guards and reads
-----------------------------------

readError :: Text -> Text -> Text
readError field expected =
  "invalid value at field " <> field <> " expected: " <> expected

coreReadInteger :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreReadInteger = mkBuiltinFn \case
  [VLiteral (LString s)] ->
    case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
      Just pv -> case pv of
        PLiteral l@LInteger{} -> pure (VLiteral l)
        _ -> throwM (ReadException (readError s "integer"))
      _ -> throwM (ReadException ("no field at key " <> s))
  _ -> failInvariant "read-integer"

coreReadString :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreReadString = mkBuiltinFn \case
  [VLiteral (LString s)] ->
    case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
      Just pv-> case pv of
        PLiteral l@LString{} -> pure (VLiteral l)
        _ -> throwM (ReadException (readError s "string"))
      _ -> throwM (ReadException ("no field at key " <> s))
  _ -> failInvariant "read-string"

coreReadDecimal :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreReadDecimal = mkBuiltinFn \case
  [VLiteral (LString s)] ->
    case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
      Just pv -> case pv of
        PLiteral l@LDecimal{} -> pure (VLiteral l)
        _ -> throwM (ReadException (readError s "decimal"))
      _ -> throwM (ReadException ("no field at key " <> s))
  _ -> failInvariant "read-decimal"

coreReadObject :: CEKRuntime b i => Row Void -> CEKValue b i  -> EvalT b (CEKValue b i)
coreReadObject ty = \case
  VLiteral (LString s) ->
    case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
      Just pv -> case pv of
        t@PObject{} | checkPactValueType (TyRow ty) t -> pure (fromPactValue t)
        _ -> throwM (ReadException (readError s "object"))
      _ -> throwM (ReadException ("no field at key " <> s))
  _ -> failInvariant "readObject"

coreReadKeyset :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreReadKeyset = mkBuiltinFn \case
  [VLiteral (LString s)] ->
    case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
      Just pv -> case pv of
        PObject m -> case lookupKs m of
          Just ks -> pure (VGuard (GKeyset ks))
          _ -> throwM (ReadException "Invalid keyset format")
        _ -> throwM (ReadException (readError s "decimal"))
      _ -> throwM (ReadException ("no field at key " <> s))
  _ -> failInvariant "read-keyset"
  where
  -- Todo: public key parsing.
  -- This is most certainly wrong, it needs more checks.
  lookupKs m = do
    ks <- Map.lookup (Field "keys") m >>= \case
      PList v -> do
        o <- traverse (preview (_PLiteral . _LString)) v
        guard (all (T.all isHexDigit) o)
        pure $ Set.fromList $ V.toList (PublicKey . T.encodeUtf8 <$> o)
      _ -> Nothing
    kspred <- case Map.lookup (Field "pred") m of
      (Just (PLiteral LString{})) -> pure KeysAll
      Just _ -> Nothing
      Nothing -> pure KeysAll
    pure (KeySet ks kspred)


coreKeysetRefGuard :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreKeysetRefGuard = mkBuiltinFn \case
  [VLiteral (LString s)] -> pure (VGuard (GKeySetRef (KeySetName s)))
  _ -> failInvariant "keyset-ref-guard"

coreEnforceGuard :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreEnforceGuard = mkBuiltinFn \case
  [VGuard v] -> case v of
    GKeyset ks -> enforceKeySet ks
    GKeySetRef ksr -> enforceKeySetRef ksr
    GUserGuard ug -> enforceUserGuard ug
  _ -> failInvariant "enforceGuard"

enforceKeySet :: CEKRuntime b i => KeySet name -> EvalT b (CEKValue b i)
enforceKeySet (KeySet keys p) = do
  let sigs = _ckeSigs ?cekRuntimeEnv
      matched = Set.size $ Set.filter (`Set.member` keys) sigs
      count = Set.size keys
  case p of
    KeysAll | matched == count -> pure (VLiteral LUnit)
    Keys2 | matched >= 2 -> pure (VLiteral LUnit)
    KeysAny | matched > 0 -> pure (VLiteral LUnit)
    _ -> throwM (EnforceException "cannot match keyset predicate")

enforceKeySetRef :: CEKRuntime b i => KeySetName -> EvalT b (CEKValue b i)
enforceKeySetRef ksr = do
  let pactDb = _ckePactDb ?cekRuntimeEnv
  liftIO (_readKeyset pactDb ksr) >>= \case
    Just ks -> enforceKeySet ks
    Nothing -> throwM (EnforceException "no such keyset")

enforceUserGuard :: CEKRuntime b i => CEKValue b i -> EvalT b (CEKValue b i)
enforceUserGuard = \case
  v@VClosure{} -> unsafeApplyOne v (VLiteral LUnit) >>= \case
    VLiteral LUnit -> pure (VLiteral LUnit)
    _ -> failInvariant "expected a function returning unit"
  _ -> failInvariant "invalid type for user closure"

createUserGuard :: CoreBuiltin -> BuiltinFn CoreBuiltin i
createUserGuard = mkBuiltinFn \case
  [v@VClosure{}] -> pure (VGuard (GUserGuard v))
  _ -> failInvariant "create-user-guard"

listAccess :: CoreBuiltin -> BuiltinFn CoreBuiltin i
listAccess = mkBuiltinFn \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> pure v
      _ -> throwM ArrayOutOfBoundsException
  _ -> failInvariant "list-access"

-----------------------------------
-- Other Core forms
-----------------------------------

coreIf :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreIf = mkBuiltinFn \case
  [VLiteral (LBool b), VClosure tbody tenv, VClosure fbody fenv] ->
    if b then eval tenv tbody else  eval fenv fbody
  _ -> failInvariant "if"

coreB64Encode :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreB64Encode = mkBuiltinFn \case
  [VLiteral (LString l)] ->
    pure $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  _ -> failInvariant "base64-encode"


coreB64Decode :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreB64Decode = mkBuiltinFn \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwM (DecodeError "invalid b64 encoding")
    Right txt -> pure (VLiteral (LString txt))
  _ -> failInvariant "base64-encode"



-----------------------------------
-- Core definitions
-----------------------------------

unimplemented :: BuiltinFn CoreBuiltin i
unimplemented = error "unimplemented"

failInvariant :: Text -> EvalT b a
failInvariant b =
  throwM (FatalExecutionError ("invariant failure, native arg failure: " <> b))

coreBuiltinFn :: CoreBuiltin -> BuiltinFn CoreBuiltin i
coreBuiltinFn = \case
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
  EqObj -> eqObj EqObj
  NeqObj -> neqObj NeqObj
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
  EnforceOne -> coreEnforceOne EnforceOne
  Enumerate -> coreEnumerate Enumerate
  EnumerateStepN -> coreEnumerateStepN EnumerateStepN
  ReadInteger -> coreReadInteger ReadInteger
  ReadDecimal -> coreReadDecimal ReadDecimal
  ReadString -> coreReadString ReadString
  ReadKeyset -> coreReadKeyset ReadKeyset
  EnforceGuard -> coreEnforceGuard EnforceGuard
  KeysetRefGuard -> coreKeysetRefGuard KeysetRefGuard
  CreateUserGuard -> createUserGuard CreateUserGuard
  ListAccess -> listAccess ListAccess
  B64Encode -> coreB64Encode B64Encode
  B64Decode -> coreB64Decode B64Decode

coreBuiltinRuntime :: Array.Array (BuiltinFn CoreBuiltin i)
coreBuiltinRuntime = Array.arrayFromList (coreBuiltinFn <$> [minBound .. maxBound])
