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
-- CEK Evaluator for typed core.
-- Todo: exporting `eval` is sort of a problem
--

module Pact.Core.Typed.Eval.CEK
 ( CEKTLEnv
 , CEKEnv
 , CEKValue(..)
 , BuiltinFn(..)
 , CEKState(..)
 , CEKRuntime
 , runCEK
 , runCEKCorebuiltin
 , Cont(..)
 , coreBuiltinRuntime
 ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Primitive(Array, indexArray)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Vector as V
import qualified Data.Primitive.Array as Array
import qualified Data.Text as T

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Typed.Term
import Pact.Core.Builtin
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import qualified Pact.Core.Pretty as P

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalTerm b i)
-- | Locally bound variables
type CEKEnv b i = RAList (CEKValue b i)

-- | Top level constraint
type HasTLEnv b i = (?cekLoaded :: CEKTLEnv b i)
-- | List of builtins
type HasBuiltinEnv b i = (?cekBuiltins :: Array (BuiltinFn b i))
-- | runtime env
type CEKRuntime b i = (HasTLEnv b i, HasBuiltinEnv b i, Enum b)


newtype BuiltinFn b i = BuiltinFn (CEKRuntime b i => NonEmpty (CEKValue b i) -> EvalT b (CEKValue b i))


data CEKState b
  = CEKState
  { _cekGas :: Gas
  , _cekEvalLog :: Maybe [Text]
  } deriving Show

newtype EvalT b a =
  EvalT (StateT (CEKState b) IO a)
  deriving
    ( Functor, Applicative, Monad
    , MonadState (CEKState b)
    , MonadIO
    , MonadFail)
  via (StateT (CEKState b) IO)

runEvalT :: CEKState b -> EvalT b a -> IO (a, CEKState b)
runEvalT s (EvalT action) = runStateT action s

data ModuleGuard name
  = ModuleGuard
  { _mgModuleName :: name
  , _mgName :: !Text
  } deriving (Eq, Show)

data Closure b i
  = Closure ![Name] !(EvalTerm b i) !(CEKEnv b i)
  deriving (Show)

data Guard name i
  = GKeyset (KeySet name)
  | GKeySetRef KeySetName
  | GUserGuard (Closure name i)
  | GModuleGuard (ModuleGuard name)
  deriving (Show)

data CEKValue b i
  = VLiteral !Literal
  | VObject !(Map Field (CEKValue b i))
  | VList !(Vector (CEKValue b i))
  | VClosure ![Name] !(EvalTerm b i) !(CEKEnv b i)
  | VNative !b
  | VGuard !(Guard Name (CEKValue b i))
  | VError Text
  deriving (Show)

data Cont b i
  = Fn (CEKValue b i) (Cont b i)
  | Arg (CEKEnv b i) (NonEmpty (EvalTerm b i)) (Cont b i)
  | BlockC (CEKEnv b i) [EvalTerm b i] (Cont b i)
  | Mt
  deriving Show

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
      NTopLevel mname mh ->
        let !t = ?cekLoaded Map.! FullyQualifiedName mname (_nName n) mh
        in evalCEK cont RAList.Nil t
  evalCEK cont _env (Constant l _)=
    returnCEK cont (VLiteral l)
  evalCEK cont env (App fn arg _) =
    evalCEK (Arg env arg cont) env fn
  evalCEK cont env (Lam ns body _) =
    returnCEK cont (VClosure (NE.toList (fst <$> ns)) body env)
  evalCEK cont env (Let n e1 e2 _) =
    returnCEK (Arg env (e1 :| []) cont) (VClosure [n] e2 env)
  evalCEK cont _env (Builtin b _) = do
    returnCEK cont (VNative b)
  evalCEK cont env (ObjectLit obj _) = do
    vs <- traverse (evalCEK Mt env) obj
    returnCEK cont (VObject vs)
  evalCEK cont env (Block (t :| ts) _) = do
    evalCEK (BlockC env ts cont) env t
  evalCEK cont env (ListLit _ ts _) = do
    ts' <- traverse (evalCEK Mt env) ts
    returnCEK cont (VList ts')
  evalCEK cont env (TyApp t _ _) =
    evalCEK cont env t
  evalCEK cont env (TyAbs _ t _) =
    evalCEK cont env t
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
  returnCEK
    :: CEKRuntime b i
    => Cont b i
    -> CEKValue b i
    -> EvalT b (CEKValue b i)
  returnCEK (Arg env args cont) fn = do
    args' <- traverse (evalCEK Mt env) args
    applyLam fn args' cont
  returnCEK (Fn fn ctx) arg =
    applyLam fn (arg :| []) ctx
  returnCEK (BlockC env (t:ts) cont) _discarded =
    evalCEK (BlockC env ts cont) env t
  returnCEK (BlockC _ [] cont) v =
    returnCEK cont v
  returnCEK Mt v = return v
  applyLam (VClosure ns body env) args cont =
    applyArgs ns (NE.toList args) env body cont
  applyLam (VNative b) args cont =
    let (BuiltinFn f) = indexArray ?cekBuiltins (fromEnum b)
    in f args >>= returnCEK cont
  applyLam _ _ _ = error "applying to non-fun"
  applyArgs (_ : ns') (arg : args') env body cont =
    applyArgs ns' args' (RAList.cons arg env) body cont
  -- Todo: create stack frame here.
  -- function application is saturated.
  applyArgs [] [] env body cont = evalCEK cont env body
    -- local (addFrame (StackFrame lamn DTDefun)) $ evalCEK cont env body
  -- Args unsaturated, create a closure and return as an argument
  applyArgs (n:ns') [] env body cont =
    returnCEK cont (VClosure (n:ns') body env)
  applyArgs [] (_args:_args') _env _body _cont =
    error "too many arguments in fn application"
  objAccess f (VObject o) = pure (o Map.! f)
  objAccess _ _ = error "fail"
  objRemove f (VObject o) = pure (VObject (Map.delete f o))
  objRemove _ _ = error "fail"
  objUpdate f v (VObject o) = pure (VObject (Map.insert f v o))
  objUpdate _ _ _ = error "fail"

runCEK
  :: Enum b
  => CEKTLEnv b i
  -- ^ Top levels
  -> Array (BuiltinFn b i)
  -- ^ Builtins
  -> EvalTerm b i
  -- ^ Term to evaluate
  -> IO (CEKValue b i, CEKState b)
runCEK env builtins term = let
  ?cekLoaded = env
  ?cekBuiltins = builtins
  in let
    cekState = CEKState 0 (Just [])
  in runEvalT cekState (eval RAList.Nil term)
{-# SPECIALISE runCEK
  :: CEKTLEnv CoreBuiltin i
  -> Array (BuiltinFn CoreBuiltin i)
  -> EvalTerm CoreBuiltin i
  -> IO (CEKValue CoreBuiltin i, CEKState CoreBuiltin) #-}

-- | Run our CEK interpreter
--   for only our core builtins
runCEKCorebuiltin
  :: CEKTLEnv CoreBuiltin i
  -- ^ Top levels
  -> EvalTerm CoreBuiltin i
  -- ^ Term to evaluate
  -> IO (CEKValue CoreBuiltin i, CEKState CoreBuiltin)
runCEKCorebuiltin env =
  runCEK env coreBuiltinRuntime

instance Pretty b => Pretty (CEKValue b i) where
  pretty = \case
    VLiteral i -> pretty i
    VObject o ->
      let toBind (k, e) = pretty k <> ":" <> pretty e
      in P.braces $ P.hsep (P.punctuate P.comma (toBind <$> Map.toList o))
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> error "undefined"
    VError e -> "Error:" <+> pretty e

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------
applyOne :: CEKRuntime b i => EvalTerm b i -> CEKEnv b i -> CEKValue b i -> EvalT b (CEKValue b i)
applyOne body env arg = eval (RAList.cons arg env) body

applyTwo :: CEKRuntime b i => EvalTerm b i -> CEKEnv b  i -> CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
applyTwo body env arg1 arg2 = eval (RAList.cons arg2 (RAList.cons arg1 env)) body

unsafeApplyOne :: CEKRuntime b i => CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
unsafeApplyOne (VClosure (_:ns) body env) arg = case ns of
  [] -> applyOne body env arg
  _ -> pure (VClosure ns body (RAList.cons arg env))
unsafeApplyOne (VNative b) arg = do
  let (BuiltinFn f) = Array.indexArray ?cekBuiltins (fromEnum b)
  f (arg :| [])
unsafeApplyOne _ _ = error "impossible"

unsafeApplyTwo :: CEKRuntime b i => CEKValue b i -> CEKValue b i -> CEKValue b i -> EvalT b (CEKValue b i)
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
unaryIntFn :: (Integer -> Integer) -> BuiltinFn b i
unaryIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [] -> pure (VLiteral (LInteger (op i)))
  _ -> fail "impossible"
{-# INLINE unaryIntFn #-}

unaryDecFn :: (Decimal -> Decimal) -> BuiltinFn b i
unaryDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [] -> pure (VLiteral (LDecimal (op i)))
  _ -> fail "impossible"
{-# INLINE unaryDecFn #-}

binaryIntFn :: (Integer -> Integer -> Integer) -> BuiltinFn b i
binaryIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger i')] -> pure (VLiteral (LInteger (op i i')))
  _ -> fail "impossible"
{-# INLINE binaryIntFn #-}

binaryDecFn :: (Decimal -> Decimal -> Decimal) -> BuiltinFn b i
binaryDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [VLiteral (LDecimal i')] -> pure (VLiteral (LDecimal (op i i')))
  _ -> fail "impossible"
{-# INLINE binaryDecFn #-}

binaryBoolFn :: (Bool -> Bool -> Bool) -> BuiltinFn b i
binaryBoolFn op = BuiltinFn \case
  VLiteral (LBool l) :| [VLiteral (LBool r)] -> pure (VLiteral (LBool (op l r)))
  _ -> fail "impossible"
{-# INLINE binaryBoolFn #-}

compareIntFn :: (Integer -> Integer -> Bool) -> BuiltinFn b i
compareIntFn op = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareIntFn #-}

compareDecFn :: (Decimal -> Decimal -> Bool) -> BuiltinFn b i
compareDecFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [VLiteral (LDecimal i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareDecFn #-}

compareStrFn :: (Text -> Text -> Bool) -> BuiltinFn b i
compareStrFn op = BuiltinFn \case
  VLiteral (LString i) :| [VLiteral (LString i')] -> pure (VLiteral (LBool (op i i')))
  _ -> fail "impossible"
{-# INLINE compareStrFn #-}

roundingFn :: (Rational -> Integer) -> BuiltinFn b i
roundingFn op = BuiltinFn \case
  VLiteral (LDecimal i) :| [] -> pure (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> fail "impossible"
{-# INLINE roundingFn #-}

---------------------------------
-- integer ops
------------------------------
addInt :: BuiltinFn b i
addInt = binaryIntFn (+)

subInt :: BuiltinFn b i
subInt = binaryIntFn (-)

mulInt :: BuiltinFn b i
mulInt = binaryIntFn (*)

divInt :: BuiltinFn b i
divInt = binaryIntFn quot

negateInt :: BuiltinFn b i
negateInt = unaryIntFn negate

modInt :: BuiltinFn b i
modInt = binaryIntFn mod

eqInt :: BuiltinFn b i
eqInt = compareIntFn (==)

neqInt :: BuiltinFn b i
neqInt = compareIntFn (/=)

gtInt :: BuiltinFn b i
gtInt = compareIntFn (>)

ltInt :: BuiltinFn b i
ltInt = compareIntFn (<)

geqInt :: BuiltinFn b i
geqInt = compareIntFn (>=)

leqInt :: BuiltinFn b i
leqInt = compareIntFn (<=)

bitAndInt :: BuiltinFn b i
bitAndInt = binaryIntFn (.&.)

bitOrInt :: BuiltinFn b i
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: BuiltinFn b i
bitComplementInt = unaryIntFn complement

bitXorInt :: BuiltinFn b i
bitXorInt = binaryIntFn xor

bitShiftInt :: BuiltinFn b i
bitShiftInt = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LInteger s)] ->
    pure (VLiteral (LInteger (shift i (fromIntegral s))))
  _ -> fail "impossible"

absInt :: BuiltinFn b i
absInt = unaryIntFn abs

expInt :: BuiltinFn b i
expInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (exp (fromIntegral i)))))
  _ -> fail "impossible"

lnInt :: BuiltinFn b i
lnInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (log (fromIntegral i)))))
  _ -> fail "impossible"

sqrtInt :: BuiltinFn b i
sqrtInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LDecimal (f2Dec (sqrt (fromIntegral i)))))
  _ -> fail "impossible"

showInt :: BuiltinFn b i
showInt = BuiltinFn \case
  VLiteral (LInteger i) :| [] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> fail "impossible"

---------------------------
-- double ops
---------------------------

addDec :: BuiltinFn b i
addDec = binaryDecFn (+)

subDec :: BuiltinFn b i
subDec = binaryDecFn (-)

mulDec :: BuiltinFn b i
mulDec = binaryDecFn (*)

divDec :: BuiltinFn b i
divDec = binaryDecFn (/)

negateDec :: BuiltinFn b i
negateDec = unaryDecFn negate

absDec :: BuiltinFn b i
absDec = unaryDecFn abs

eqDec :: BuiltinFn b i
eqDec = compareDecFn (==)

neqDec :: BuiltinFn b i
neqDec = compareDecFn (/=)

gtDec :: BuiltinFn b i
gtDec = compareDecFn (>)

geqDec :: BuiltinFn b i
geqDec = compareDecFn (>=)

ltDec :: BuiltinFn b i
ltDec = compareDecFn (<)

leqDec :: BuiltinFn b i
leqDec = compareDecFn (<=)

showDec :: BuiltinFn b i
showDec = BuiltinFn \case
  VLiteral (LDecimal i) :| [] ->
    pure (VLiteral (LString (T.pack (show i))))
  _ -> fail "impossible"

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec, floorDec, ceilingDec :: BuiltinFn b i
roundDec = roundingFn round
floorDec = roundingFn floor
ceilingDec = roundingFn ceiling

expDec :: BuiltinFn b i
expDec = unaryDecFn (f2Dec . exp . dec2F)

lnDec :: BuiltinFn b i
lnDec = unaryDecFn (f2Dec . log . dec2F)

sqrtDec :: BuiltinFn b i
sqrtDec = unaryDecFn (f2Dec . sqrt . dec2F)

---------------------------
-- bool ops
---------------------------
andBool :: BuiltinFn b i
andBool = binaryBoolFn (&&)

orBool :: BuiltinFn b i
orBool = binaryBoolFn (||)

notBool :: BuiltinFn b i
notBool = BuiltinFn \case
  VLiteral (LBool i) :| [] -> pure (VLiteral (LBool (not i)))
  _ -> fail "impossible"

eqBool :: BuiltinFn b i
eqBool = binaryBoolFn (==)

neqBool :: BuiltinFn b i
neqBool = binaryBoolFn (/=)

showBool :: BuiltinFn b i
showBool = BuiltinFn \case
  VLiteral (LBool i) :| [] -> do
    let out = if i then "true" else "false"
    pure (VLiteral (LString out))
  _ -> fail "impossible"

---------------------------
-- string ops
---------------------------
eqStr :: BuiltinFn b i
eqStr = compareStrFn (==)

neqStr :: BuiltinFn b i
neqStr = compareStrFn (/=)

gtStr :: BuiltinFn b i
gtStr = compareStrFn (>)

geqStr :: BuiltinFn b i
geqStr = compareStrFn (>=)

ltStr :: BuiltinFn b i
ltStr = compareStrFn (<)

leqStr :: BuiltinFn b i
leqStr = compareStrFn (<=)

addStr :: BuiltinFn b i
addStr =  BuiltinFn \case
  VLiteral (LString i) :| [VLiteral (LString i')] -> pure (VLiteral (LString (i <> i')))
  _ -> fail "impossible"

takeStr :: BuiltinFn b i
takeStr = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.take (fromIntegral i) t)))
  _ -> fail "impossible"

dropStr :: BuiltinFn b i
dropStr = BuiltinFn \case
  VLiteral (LInteger i) :| [VLiteral (LString t)] -> do
    pure (VLiteral (LString (T.drop (fromIntegral i) t)))
  _ -> fail "impossible"

lengthStr :: BuiltinFn b i
lengthStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    pure (VLiteral (LInteger (fromIntegral (T.length t))))
  _ -> fail "impossible"

reverseStr :: BuiltinFn b i
reverseStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    pure (VLiteral (LString (T.reverse t)))
  _ -> fail "impossible"

showStr :: BuiltinFn b i
showStr = BuiltinFn \case
  VLiteral (LString t) :| [] -> do
    let out = "\"" <> t <> "\""
    pure (VLiteral (LString out))
  _ -> fail "impossible"

concatStr :: BuiltinFn b i
concatStr = BuiltinFn \case
  VList li :| [] -> do
    li' <- traverse asString li
    pure (VLiteral (LString (T.concat (V.toList li'))))
  _ -> fail "impossible"


---------------------------
-- Unit ops
---------------------------

eqUnit :: BuiltinFn b i
eqUnit = BuiltinFn \case
  VLiteral LUnit :| [VLiteral LUnit] -> pure (VLiteral (LBool True))
  _ -> fail "impossible"

neqUnit :: BuiltinFn b i
neqUnit = BuiltinFn \case
  VLiteral LUnit :| [VLiteral LUnit] -> pure (VLiteral (LBool False))
  _ -> fail "impossible"

showUnit :: BuiltinFn b i
showUnit = BuiltinFn \case
  VLiteral LUnit :| [] -> pure (VLiteral (LString "()"))
  _ -> fail "impossible"

---------------------------
-- Object ops
---------------------------

eqObj :: BuiltinFn b i
eqObj = BuiltinFn \case
  l@VObject{} :| [r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
  _ -> fail "impossible"

neqObj :: BuiltinFn b i
neqObj = BuiltinFn \case
  l@VObject{} :| [r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
  _ -> fail "impossible"


------------------------------
--- conversions + unsafe ops
------------------------------
asBool :: CEKValue b i -> EvalT b Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = fail "impossible"

asString :: CEKValue b i -> EvalT b Text
asString (VLiteral (LString b)) = pure b
asString _ = fail "impossible"

asList :: CEKValue b i -> EvalT b (Vector (CEKValue b i))
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

unsafeEqCEKValue :: CEKValue b i -> CEKValue b i -> Bool
unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
unsafeEqCEKValue (VObject o) (VObject o') = and (Map.intersectionWith unsafeEqCEKValue o o')
unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
unsafeEqCEKValue _ _ = error "todo: throw invariant failure exception"

unsafeNeqCEKValue :: CEKValue b i -> CEKValue b i -> Bool
unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
---------------------------
eqList :: BuiltinFn b i
eqList = BuiltinFn \case
  eqClo :| [VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool False))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo eqClo a b) l r
      pure (VLiteral (LBool (and v')))
  _ -> fail "impossible"

neqList :: BuiltinFn b i
neqList = BuiltinFn \case
  neqClo :| [VList l, VList r] ->
    if V.length l /= V.length r then
      pure (VLiteral (LBool True))
    else do
      v' <- V.zipWithM (\a b -> asBool =<< unsafeApplyTwo neqClo a b) l r
      pure (VLiteral (LBool (or v')))
  _ -> fail "impossible"

zipList :: BuiltinFn b i
zipList = BuiltinFn \case
  clo :| [VList l, VList r] -> do
    v' <- V.zipWithM (unsafeApplyTwo clo) l r
    pure (VList v')
  _ -> fail "impossible"

addList :: BuiltinFn b i
addList = BuiltinFn \case
  VList l :| [VList r] -> pure (VList (l <> r))
  _ -> fail "impossible"

pcShowList :: BuiltinFn b i
pcShowList = BuiltinFn \case
  showFn :| [VList l1] -> do
    strli <- traverse ((=<<) asString  . unsafeApplyOne showFn) (V.toList l1)
    let out = "[" <> T.intercalate ", " strli <> "]"
    pure (VLiteral (LString out))
  _ -> fail "impossible"

coreMap :: BuiltinFn b i
coreMap = BuiltinFn \case
  fn :| [VList li] -> do
    li' <- traverse (unsafeApplyOne fn) li
    pure (VList li')
  _ -> fail "impossible"

coreFilter :: BuiltinFn b i
coreFilter = BuiltinFn \case
  fn :| [VList li] -> do
    let applyOne' arg = unsafeApplyOne fn arg >>= asBool
    li' <- V.filterM applyOne' li
    pure (VList li')
  _ -> fail "impossible"

coreFold :: BuiltinFn b i
coreFold = BuiltinFn \case
  fn :| [initElem, VList li] -> V.foldM' (unsafeApplyTwo fn) initElem li
  _ -> fail "impossible"

lengthList :: BuiltinFn b i
lengthList = BuiltinFn \case
  VList li :| [] -> pure (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> fail "impossible"

takeList :: BuiltinFn b i
takeList = BuiltinFn \case
  VLiteral (LInteger i) :| [VList li] ->
    pure (VList (V.take (fromIntegral i) li))
  _ -> fail "impossible"

dropList :: BuiltinFn b i
dropList = BuiltinFn \case
  VLiteral (LInteger i) :| [VList li] ->
    pure (VList (V.drop (fromIntegral i) li))
  _ -> fail "impossible"

reverseList :: BuiltinFn b i
reverseList = BuiltinFn \case
  VList li :| [] ->
    pure (VList (V.reverse li))
  _ -> fail "impossible"

coreEnumerate :: BuiltinFn b i
coreEnumerate = BuiltinFn \case
  VLiteral (LInteger from) :| [VLiteral (LInteger to)] -> enum' from to
  _ -> fail "impossible"
  where
  toVecList = VList . fmap (VLiteral . LInteger)
  enum' from to
    | to >= from = pure $ toVecList $ V.enumFromN from (fromIntegral (to - from + 1))
    | otherwise = pure $ toVecList $ V.enumFromStepN from (-1) (fromIntegral (from - to + 1))

coreEnumerateStepN :: BuiltinFn b i
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

concatList :: BuiltinFn b i
concatList = BuiltinFn \case
  VList li :| [] -> do
    li' <- traverse asList li
    pure (VList (V.concat (V.toList li')))
  _ -> fail "impossible"

-----------------------------------
-- Other Core forms
-----------------------------------

coreIf :: BuiltinFn b i
coreIf = BuiltinFn \case
  VLiteral (LBool b) :| [VClosure _ ibody ienv, VClosure _ ebody eenv] ->
    if b then applyOne ibody ienv (VLiteral LUnit) else  applyOne ebody eenv (VLiteral LUnit)
  _ -> fail "impossible"

unimplemented :: BuiltinFn b i
unimplemented = BuiltinFn \case
  _ -> fail "unimplemented"

coreBuiltinFn :: CoreBuiltin -> BuiltinFn CoreBuiltin i
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

coreBuiltinRuntime :: Array.Array (BuiltinFn CoreBuiltin i)
coreBuiltinRuntime = Array.arrayFromList (coreBuiltinFn <$> [minBound .. maxBound])
