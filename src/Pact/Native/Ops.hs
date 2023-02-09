{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Pact.Native.Ops
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Operators and math built-ins.
--


module Pact.Native.Ops
    ( opDefs
    , orDef, andDef, notDef
    , modDef, addDef, subDef, mulDef
    , divDef, powDef, logDef
    , sqrtDef, lnDef, expDef, absDef
    , roundDef, ceilDef, floorDef
    , gtDef, ltDef, gteDef, lteDef, eqDef, neqDef
    , bitAndDef, bitOrDef, xorDef, complementDef, shiftDef
    ) where


import Control.Monad (when)
import Data.Bits
import Data.Decimal
import Data.Default
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Pretty
import Pact.Types.Runtime
#if !defined(ghcjs_HOST_OS)
import Pact.Native.Trans.TOps
#endif


modDef :: NativeDef
modDef = defRNative "mod" mod' (binTy tTyInteger tTyInteger tTyInteger)
  ["(mod 13 8)"] "X modulo Y."
  where
    mod' :: RNativeFun e
    mod' _ [TLitInteger a, TLitInteger b] = return $ toTerm $ a `mod` b
    mod' i as = argsError i as

addDef :: NativeDef
addDef = defGasRNative "+" plus plusTy
  [ "(+ 1 2)"
  , "(+ 5.0 0.5)"
  , "(+ \"every\" \"body\")"
  , "(+ [1 2] [3 4])"
  , "(+ { \"foo\": 100 } { \"foo\": 1, \"bar\": 2 })"
  ]
  "Add numbers, concatenate strings/lists, or merge objects."
  where
    plus :: GasRNativeFun e
    plus g fi [TLitString a,TLitString b] = gasConcat g fi (T.length a) (T.length b) $
      (return (tStr $ a <> b))
    plus g fi [TList a aty _,TList b bty _] = gasConcat g fi (length a) (length b) $
      return (TList
      (a <> b)
      (if aty == bty then aty else TyAny)
      def)
    plus g fi [TObject (Object (ObjectMap as) aty _ _) _,TObject (Object (ObjectMap bs) bty _ _) _] =
      gasConcat g fi (M.size as) (M.size bs) $
      return ((`TObject` def) $ Object
           (ObjectMap $ M.union as bs)
           (if aty == bty then aty else TyAny)
           def
           def)
    plus g i as =
      (g,) <$> binop' "+" (+) (+) i as
    {-# INLINE plus #-}

    gasConcat g fi aLength bLength = computeGas' g fi (GConcatenation aLength bLength)

    plusTy :: FunTypes n
    plusTy = coerceBinNum <> binTy plusA plusA plusA

subDef :: NativeDef
subDef = defRNative "-" minus (coerceBinNum <> unaryNumTys)
  ["(- 1.0)", "(- 3 2)"]
  "Negate X, or subtract Y from X."
  where

    minus :: RNativeFun e
    minus _ [TLiteral (LInteger n) _] = return (toTerm (negate n))
    minus _ [TLiteral (LDecimal n) _] = return (toTerm (negate n))
    minus i as = binop' "-" (-) (-) i as
    {-# INLINE minus #-}

    unaryNumTys :: FunTypes n
    unaryNumTys = unaryTy numA numA

mulDef :: NativeDef
mulDef = defRNative "*" mul' coerceBinNum
  ["(* 0.5 10.0)", "(* 3 5)"] "Multiply X by Y."
  where
  mul' :: RNativeFun e
  mul' = binop' "*" (*) (*)


divDef :: NativeDef
divDef = defRNative "/" divide' coerceBinNum
  ["(/ 10.0 2.0)", "(/ 8 3)"] "Divide X by Y."
  where
    divide' :: RNativeFun e
    divide' fi as@[TLiteral a _,TLiteral b _] =
      binop "/" divDec divInt fi as
      where
      nonZeroDiv b' = when (b' == 0) $ evalError' fi $ "Division by 0" <> ": " <> pretty (a,b)
      divDec a' b' = nonZeroDiv b' *> liftDecimalOp (/) a' b'
      divInt a' b' = nonZeroDiv b' *> liftIntegerOp div a' b'
    divide' fi as = argsError fi as

powDef :: NativeDef
powDef = defRNative "^" pow coerceBinNum ["(^ 2 3)"] "Raise X to Y power."
  where
  pow :: RNativeFun e
  pow i as@[TLiteral a _,TLiteral b _] = do
#if defined(ghcjs_HOST_OS)
    binop "^" (\a' b' -> liftDecF i (**) a' b') intPow i as
#else
    decimalPow <- ifExecutionFlagSet' FlagDisableNewTrans (liftDecPowF i (**)) (liftDecPowF i trans_pow)
    binop "^" decimalPow intPow i as
#endif
    where
    liftDecPowF fi f lop rop = do
      _ <- computeGasCommit def "" (GDecimalOpCost lop rop)
      liftDecF fi f lop rop
    oldIntPow  b' e = do
      when (b' < 0) $ evalError' i $ "Integral power must be >= 0" <> ": " <> pretty (a,b)
      liftIntegerOp (^) b' e
    -- See: https://hackage.haskell.org/package/base-4.16.1.0/docs/src/GHC-Real.html
    intPow :: Integer -> Integer -> Eval e Integer
    intPow b' e =
      ifExecutionFlagSet FlagDisablePact43 (oldIntPow b' e) (intPow' b' e)
    intPow' x0 y0
      | y0 < 0 = evalError' i $ "Integral power must be >= 0" <> ": " <> pretty (a,b)
      | y0 == 0 = pure 1
      | otherwise = evens x0 y0
    evens x y
      | even y = twoArgIntOpGas x x *> evens (x * x) (y `quot` 2)
      | y == 1 = pure x
      | otherwise = twoArgIntOpGas x x *> odds (x * x) (y `quot` 2) x
    odds x y z
      | even y = twoArgIntOpGas x x *> odds (x * x) (y `quot` 2) z
      | y == 1 = twoArgIntOpGas x z *> pure (x * z)
      | otherwise = twoArgIntOpGas x x *> odds (x * x) (y `quot` 2) (x * z)
  pow i as = argsError i as

twoArgIntOpGas :: Integer -> Integer -> Eval e Gas
twoArgIntOpGas loperand roperand =
  computeGasCommit def "" (GIntegerOpCost (loperand, Nothing) (roperand, Nothing))

twoArgDecOpGas :: Decimal -> Decimal -> Eval e Gas
twoArgDecOpGas loperand roperand =
  computeGasCommit def ""
  (GIntegerOpCost
    (decimalMantissa loperand, Just (fromIntegral (decimalPlaces loperand)))
    (decimalMantissa roperand, Just (fromIntegral (decimalPlaces roperand))))

legalLogArg :: Literal -> Bool
legalLogArg = \case
  LInteger i -> i > 0
  LDecimal i -> i > 0
  _ -> False

litGt0 :: Literal -> Bool
litGt0 = \case
  LInteger i -> i >= 0
  LDecimal i -> i >= 0
  _ -> False

logDef :: NativeDef
logDef = defRNative "log" log' coerceBinNum ["(log 2 256)"] "Log of Y base X."
  where
  liftLogDec fi f a b = do
    _ <- computeGasCommit def "" (GDecimalOpCost a b)
    liftDecF fi f a b
  log' :: RNativeFun e
  log' fi as@[TLiteral base _,TLiteral v _] = do
    unlessExecutionFlagSet FlagDisablePact43 $
      when (not (litGt0 base) || not (legalLogArg v)) $ evalError' fi "Illegal base or argument in log"
#if defined(ghcjs_HOST_OS)
    binop "log"
          (\a b -> liftDecF fi logBase a b)
          (\a b -> liftIntF fi logBase a b)
          fi
          as
#else
    decimalLogBase <-
      ifExecutionFlagSet' FlagDisableNewTrans (liftLogDec fi logBase) (liftLogDec fi trans_log)
    integerLogBase <-
      ifExecutionFlagSet' FlagDisableNewTrans (liftIntF fi logBase) (liftIntF fi trans_log)
    binop "log" decimalLogBase integerLogBase fi as
#endif
  log' fi as = argsError fi as

sqrtDef :: NativeDef
sqrtDef = defRNative "sqrt" sqrt' unopTy ["(sqrt 25)"] "Square root of X."
  where
  sqrt' fi as@[TLiteral a _] = do
    unlessExecutionFlagSet FlagDisablePact43 $
      when (not (litGt0 a)) $ evalError' fi "Sqrt must be non-negative"
#if defined(ghcjs_HOST_OS)
    unopd sqrt fi as
#else
    decimalSqrt <- ifExecutionFlagSet' FlagDisableNewTrans (unopd sqrt) (unopd trans_sqrt)
    decimalSqrt fi as
#endif
  sqrt' fi as = argsError fi as

lnDef :: NativeDef
lnDef = defRNative "ln" ln' unopTy ["(round (ln 60) 6)"] "Natural log of X."
  where
  ln' fi as@[TLiteral a _] = do
    unlessExecutionFlagSet FlagDisablePact43 $
      when (not (legalLogArg a)) $ evalError' fi "Illegal argument for ln: must be greater than zero"
#if defined(ghcjs_HOST_OS)
    unopd log fi as
#else
    decimalLog <- ifExecutionFlagSet' FlagDisableNewTrans (unopd log) (unopd trans_ln)
    decimalLog fi as
#endif
  ln' fi as = argsError fi as

expDef :: NativeDef
expDef = defRNative "exp" go
  unopTy ["(round (exp 3) 6)"] "Exp of X."
  where
#if defined(ghcjs_HOST_OS)
  go = unopd exp
#else
  go fi as = do
    decimalExp <- ifExecutionFlagSet' FlagDisableNewTrans (unopd exp) (unopd trans_exp)
    decimalExp fi as
#endif

absDef :: NativeDef
absDef = defRNative "abs" abs' (unaryTy tTyDecimal tTyDecimal <> unaryTy tTyInteger tTyInteger)
  ["(abs (- 10 23))"] "Absolute value of X."
  where
  abs' :: RNativeFun e
  abs' _ [TLitInteger a] = return $ toTerm $ abs a
  abs' _ [TLiteral (LDecimal n) _] = return $ toTerm $ abs n
  abs' i as = argsError i as

roundDef :: NativeDef
roundDef = defTrunc "round" "Performs Banker's rounding" round

ceilDef :: NativeDef
ceilDef = defTrunc "ceiling" "Rounds up" ceiling

floorDef :: NativeDef
floorDef = defTrunc "floor" "Rounds down" floor

gtDef :: NativeDef
gtDef = defCmp ">" (cmp (== GT))

ltDef :: NativeDef
ltDef = defCmp "<" (cmp (== LT))

gteDef :: NativeDef
gteDef = defCmp ">=" (cmp (`elem` [GT,EQ]))

lteDef :: NativeDef
lteDef = defCmp "<=" (cmp (`elem` [LT,EQ]))

eqDef :: NativeDef
eqDef = defRNative "=" (eq id) eqTy
  ["(= [1 2 3] [1 2 3])", "(= 'foo \"foo\")", "(= { 'a: 2 } { 'a: 2})"]
  ( "Compare alike terms for equality, returning TRUE if X is equal to Y. "
  <> "Equality comparisons will fail immediately on type mismatch, or if types "
  <> "are not value types."
  )

neqDef :: NativeDef
neqDef = defRNative "!=" (eq not) eqTy
  ["(!= \"hello\" \"goodbye\")"] "True if X does not equal Y."

eqTy :: FunTypes n
eqTy = binTy tTyBool eqA eqA

eqA :: Type n
eqA = mkTyVar "a"
    [tTyInteger
    ,tTyString
    ,tTyTime
    ,tTyDecimal
    ,tTyBool
    ,TyList (mkTyVar "l" [])
    ,TySchema TyObject (mkSchemaVar "o") def
    ,tTyKeySet
    ,tTyGuard Nothing
    ,tTyModRef
    ]

tTyModRef :: Type n
tTyModRef = TyModule (Just [])

orDef :: NativeDef
orDef = defLogic "or" (||) True

andDef :: NativeDef
andDef = defLogic "and" (&&) False

notDef :: NativeDef
notDef = defRNative "not" not' (unaryTy tTyBool tTyBool) ["(not (> 1 2))"] "Boolean not."
  where
    not' :: RNativeFun e
    not' _ [TLiteral (LBool a) _] = return $ toTerm $ not a
    not' i as = argsError i as

opDefs :: NativeModule
opDefs = ("Operators",
    [liftLogic "or?" (||) "or" True
    ,liftLogic "and?" (&&) "and" False
    ,defNative "not?" liftNot
     (funType tTyBool [("app",logicLam r),("value",r)])
     ["(not? (> 20) 15)"]
     "Apply logical 'not' to the results of applying VALUE to APP."

    ,orDef, andDef, notDef
    ,gtDef, ltDef, gteDef, lteDef, eqDef, neqDef
    ,addDef, subDef, mulDef, divDef, powDef, logDef
    ,modDef, sqrtDef, lnDef, expDef, absDef, roundDef, ceilDef, floorDef
    ,bitAndDef, bitOrDef, xorDef, complementDef, shiftDef
    ])
    where r = mkTyVar "r" []

unopTy :: FunTypes n
unopTy = unaryTy numA numA

numA :: Type n
numA = numV "a"

numV :: TypeVarName -> Type n
numV a = mkTyVar a [tTyInteger,tTyDecimal]

coerceBinNum :: FunTypes n
coerceBinNum = binTy numA numA numA <> binTy tTyDecimal numA (numV "b")

plusA :: Type n
plusA = mkTyVar "a" [tTyString,TyList (mkTyVar "l" []),TySchema TyObject (mkSchemaVar "o") def]

defTrunc :: NativeDefName -> Text -> (Rational -> Integer) -> NativeDef
defTrunc n desc op = defRNative n fun (funType tTyDecimal [("x",tTyDecimal),("prec",tTyInteger)] <>
                                      unaryTy tTyInteger tTyDecimal)
                     [ ExecExample $ "(" <> asString n <> " 3.5)"
                     , ExecExample $ "(" <> asString n <> " 100.15234 2)"
                     ]
                     (desc <> " value of decimal X as integer, or to PREC precision as decimal.")
    where fun :: RNativeFun e
          fun _ [TLiteral (LDecimal d) _] = return $ tLit $ LInteger $ truncate $ roundTo' op 0 d
          fun i [TLiteral (LDecimal d) _,TLitInteger p]
              | p >= 0 = return $ toTerm $ roundTo' op (fromIntegral p) d
              | otherwise = evalError' i "Negative precision not allowed"
          fun i as = argsError i as

defLogic :: NativeDefName -> (Bool -> Bool -> Bool) -> Bool -> NativeDef
defLogic n bop shortC = defNative n fun (binTy tTyBool tTyBool tTyBool)
    [ExecExample $ "(" <> asString n <> " true false)"]
    "Boolean logic with short-circuit."
    where
      fun :: NativeFun e
      fun i as@[a,b] = gasUnreduced i as $ reduce a >>= \a' -> case a' of
        TLitBool x | x == shortC -> return $ toTerm x
                   | otherwise -> reduce b >>= \b' -> case b' of
                       TLitBool y -> return $ toTerm $ x `bop` y
                       _ -> argsError' i as
        _ -> argsError' i as
      fun i as = argsError' i as

logicLam :: Type v -> Type v
logicLam argTy = TyFun $ funType' tTyBool [("x",argTy)]

delegateError :: Doc -> Term Ref -> Term Name -> Eval m a
delegateError desc app r = evalError (_tInfo app) $ desc <> ": Non-boolean result from delegate: " <> pretty r

liftLogic :: NativeDefName -> (Bool -> Bool -> Bool) -> Text -> Bool -> NativeDef
liftLogic n bop desc shortCircuit =
  defNative n fun (funType tTyBool [("a",logicLam r),("b",logicLam r),("value",r)])
    [ExecExample $ "(" <> asString n <> " (> 20) (> 10) 15)"]
    ("Apply logical '" <> desc <> "' to the results of applying VALUE to A and B, with short-circuit.")
  where
    r = mkTyVar "r" []
    fun i as@[tLamToApp -> a@TApp{},tLamToApp -> b@TApp{},v'] = gasUnreduced i as $ reduce v' >>= \v -> do
      ar <- apply (_tApp a) [v]
      case ar of
        TLitBool ab
          | ab == shortCircuit -> return $ toTerm shortCircuit
          | otherwise -> do
              br <- apply (_tApp b) [v]
              case br of
                TLitBool bb -> return $ toTerm $ bop ab bb
                _ -> delegateError (pretty n) b br
        _ -> delegateError (pretty n) a ar
    fun i as = argsError' i as

liftNot :: NativeFun e
liftNot i as@[tLamToApp -> app@TApp{},v'] = gasUnreduced i as $ reduce v' >>= \v -> apply (_tApp app) [v] >>= \r -> case r of
  TLitBool b -> return $ toTerm $ not b
  _ -> delegateError "not?" app r
liftNot i as = argsError' i as


eq :: (Bool -> Bool) -> RNativeFun e
eq f i as = case as of
  [a,b] -> if a `canEq` b
    then return $ toTerm $ f (a `termEq` b)
    else evalError' i
         $ "cannot compare incompatible types: "
         <> pretty (typeof' a) <> ", " <> pretty (typeof' b)
  _ -> argsError i as
{-# INLINE eq #-}

-- | Convenience for singleton unary 'FunTys'. Argument is named X.
unaryTy :: Type n -> Type n -> FunTypes n
unaryTy rt ta = funType rt [("x",ta)]

-- | Convenience for singleton binary 'FunTys'. Arguments are named X and Y.
binTy :: Type n -> Type n -> Type n -> FunTypes n
binTy rt ta tb = funType rt [("x",ta),("y",tb)]

defCmp :: NativeDefName -> RNativeFun e -> NativeDef
defCmp o f =
  defRNative o f (binTy tTyBool a a)
    [ ex "1" "3"
    , ex "5.24" "2.52"
    , ex "\"abc\"" "\"def\""
    ]
    $ "True if X " <> o' <> " Y."
  where
    o' = asString o
    ex a' b = ExecExample $ "(" <> o' <> " " <> a' <> " " <> b <> ")"
    a = mkTyVar "a" [tTyInteger,tTyDecimal,tTyString,tTyTime]


-- | Monomorphic compare.
cmp :: (Ordering -> Bool) -> RNativeFun e
cmp cmpFun fi as = do
  c <- case as of
    [TLiteral a _,TLiteral b _] -> case (a,b) of
      (LInteger i,LInteger j) -> return $ i `compare` j
      (LDecimal i,LDecimal j) -> return $ i `compare` j
      (LString i,LString j) -> return $ i `compare` j
      (LTime i,LTime j) -> return $ i `compare` j
      _ -> argsError fi as
    _ -> argsError fi as
  return $ toTerm (cmpFun c)
{-# INLINE cmp #-}

liftIntegerOp :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Eval e Integer
liftIntegerOp f a b = do
  unlessExecutionFlagSet FlagDisablePact43 $ twoArgIntOpGas a b
  pure (f a b)

liftDecimalOp :: (Decimal -> Decimal -> Decimal) -> Decimal -> Decimal -> Eval e Decimal
liftDecimalOp f a b = do
  unlessExecutionFlagSet FlagDisablePact43 $ twoArgDecOpGas a b
  pure (f a b)


binop'
  :: NativeDefName
  -> (Decimal -> Decimal -> Decimal)
  -> (Integer -> Integer -> Integer)
  -> RNativeFun e
binop' ndef dop iop i as = binop ndef (liftDecimalOp dop) (liftIntegerOp iop) i as

-- | Perform binary math operator with coercion to Decimal as necessary.
binop
  :: NativeDefName
  -> (Decimal -> Decimal -> Eval e Decimal)
  -> (Integer -> Integer -> Eval e Integer)
  -> RNativeFun e
binop ndef dop iop fi as@[TLiteral a _,TLiteral b _] = do
  case (a,b) of
    (LInteger i,LInteger j) -> toTerm <$> (i `iop` j)
    (LDecimal i,LDecimal j) ->
      toTerm <$> (i `dop` j)
    (LInteger i,LDecimal j) -> do
      emitPactWarning $ DeprecatedOverload ndef "decimal/integer operator overload is deprecated"
      toTerm <$> (fromIntegral i `dop` j)
    (LDecimal i,LInteger j) -> do
      emitPactWarning $ DeprecatedOverload ndef "decimal/integer operator overload is deprecated"
      toTerm <$> (i `dop` fromIntegral j)
    _ -> argsError fi as
binop _ _ _ fi as = argsError fi as
{-# INLINE binop #-}

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational
int2F :: Integer -> Double
int2F = fromIntegral
f2Int :: Double -> Integer
f2Int = round

liftDecF :: HasInfo i => i -> (Double -> Double -> Double) -> Decimal -> Decimal -> Eval e Decimal
liftDecF i f a b = do
  let !out = (dec2F a `f` dec2F b)
  unlessExecutionFlagSet FlagDisablePact43 $
    when (isNaN out || isInfinite out) $ evalError' i "Operation resulted in +- infinity or NaN"
  pure $ f2Dec out

  -- Right $ f2Dec (dec2F a `f` dec2F b)
liftIntF :: HasInfo i => i -> (Double -> Double -> Double) -> Integer -> Integer -> Eval e Integer
liftIntF i f a b = do
  let !out = (int2F a `f` int2F b)
  unlessExecutionFlagSet FlagDisablePact43 $
    when (isNaN out || isInfinite out) $ evalError' i "Operation resulted in +- infinity or NaN"
  pure $ f2Int out

unopd :: (Double -> Double) -> RNativeFun e
unopd op _ [TLitInteger i] = return $ toTerm $ f2Dec $ op $ int2F i
unopd op _ [TLiteral (LDecimal n) _] = return $ toTerm $ f2Dec $ op $ dec2F n
unopd _ i as = argsError i as


doBits :: NativeDefName -> [Example] -> Text -> (Integer -> Integer -> Integer) -> NativeDef
doBits n exs verb f = defRNative n go
  (binTy tTyInteger tTyInteger tTyInteger)
  exs
  ("Compute bitwise X " <> verb <> " Y.")
  where
    go _i [TLitInteger x,TLitInteger y] = return $ toTerm $ x `f` y
    go i as = argsError i as

xorDef :: NativeDef
xorDef = doBits "xor" ["(xor 127 64)","(xor 5 -7)"] "xor" xor

bitAndDef :: NativeDef
bitAndDef = doBits "&" ["(& 2 3)","(& 5 -7)"] "and" (.&.)

bitOrDef :: NativeDef
bitOrDef = doBits "|" ["(| 2 3)","(| 5 -7)"] "or" (.|.)

complementDef :: NativeDef
complementDef = defRNative "~" go
  (unaryTy tTyInteger tTyInteger)
  ["(~ 15)"]
  "Reverse all bits in X."
  where
    go _i [TLitInteger x] = return $ toTerm $ complement x
    go i as = argsError i as

shiftDef :: NativeDef
shiftDef =  defRNative "shift" go
  (binTy tTyInteger tTyInteger tTyInteger)
  ["(shift 255 8)","(shift 255 -1)","(shift -255 8)","(shift -255 -1)"]
  ( "Shift X Y bits left if Y is positive, or right by -Y bits otherwise. "
  <> "Right shifts perform sign extension on signed number types; "
  <> "i.e. they fill the top bits with 1 if the x is negative and with 0 otherwise."
  )
  where
    go _ [TLitInteger x,TLitInteger y] = return $ toTerm $ shift x (fromIntegral y)
    go i as = argsError i as
