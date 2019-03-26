{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
    ) where


import Data.Decimal
import Data.Default
import qualified Data.Map.Strict as M

import Pact.Native.Internal
import Pact.Types.Runtime
import Pact.Types.Pretty

import Pact.Eval


modDef :: NativeDef
modDef = defRNative "mod" mod' (binTy tTyInteger tTyInteger tTyInteger)
  ["(mod 13 8)"] "X modulo Y."
  where
    mod' :: RNativeFun e
    mod' _ [TLitInteger a, TLitInteger b] = return $ toTerm $ a `mod` b
    mod' i as = argsError i as

addDef :: NativeDef
addDef = defRNative "+" plus plusTy
  [ "(+ 1 2)"
  , "(+ 5.0 0.5)"
  , "(+ \"every\" \"body\")"
  , "(+ [1 2] [3 4])"
  , "(+ { \"foo\": 100 } { \"foo\": 1, \"bar\": 2 })"
  ]
  "Add numbers, concatenate strings/lists, or merge objects."
  where

    plus :: RNativeFun e
    plus _ [TLitString a,TLitString b] = return (tStr $ a <> b)
    plus _ [TList a aty _,TList b bty _] = return $ TList
      (a <> b)
      (if aty == bty then aty else TyAny)
      def
    plus _ [TObject (Object as aty _) _,TObject (Object bs bty _) _] =
      let reps (a,b) = (renderCompactText a,(a,b))
          mapit = M.fromList . map reps
      in return $ (`TObject` def) $ Object
           (M.elems $ M.union (mapit as) (mapit bs))
           (if aty == bty then aty else TyAny)
           def
    plus i as = binop' (+) (+) i as
    {-# INLINE plus #-}

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
    minus i as = binop' (-) (-) i as
    {-# INLINE minus #-}

    unaryNumTys :: FunTypes n
    unaryNumTys = unaryTy numA numA

mulDef :: NativeDef
mulDef = defRNative "*" (binop' (*) (*)) coerceBinNum
  ["(* 0.5 10.0)", "(* 3 5)"] "Multiply X by Y."

divDef :: NativeDef
divDef = defRNative "/" divide' coerceBinNum
  ["(/ 10.0 2.0)", "(/ 8 3)"] "Divide X by Y."
  where
    divide' :: RNativeFun e
    divide' = binop (\a b -> assert (b /= 0) "Division by 0" $ liftOp (/) a b)
                    (\a b -> assert (b /= 0) "Division by 0" $ liftOp div a b)

powDef :: NativeDef
powDef = defRNative "^" pow coerceBinNum ["(^ 2 3)"] "Raise X to Y power."
  where
    pow :: RNativeFun e
    pow = binop (\a b -> liftDecF (**) a b)
                (\a b -> assert (b >= 0) "Integral power must be >= 0" $ liftOp (^) a b)

logDef :: NativeDef
logDef = defRNative "log" log' coerceBinNum ["(log 2 256)"] "Log of Y base X."
  where
    log' :: RNativeFun e
    log' = binop (\a b -> liftDecF logBase a b)
                 (\a b -> liftIntF logBase a b)

sqrtDef :: NativeDef
sqrtDef = defRNative "sqrt" (unopd sqrt) unopTy ["(sqrt 25)"] "Square root of X."

lnDef :: NativeDef
lnDef = defRNative "ln" (unopd log) unopTy ["(round (ln 60) 6)"] "Natural log of X."

expDef :: NativeDef
expDef = defRNative "exp" (unopd exp) unopTy ["(round (exp 3) 6)"] "Exp of X."

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
  "True if X equals Y."

neqDef :: NativeDef
neqDef = defRNative "!=" (eq not) eqTy
  ["(!= \"hello\" \"goodbye\")"] "True if X does not equal Y."

eqTy :: FunTypes n
eqTy = binTy tTyBool eqA eqA

eqA :: Type n
eqA = mkTyVar "a" [tTyInteger,tTyString,tTyTime,tTyDecimal,tTyBool,
  TyList (mkTyVar "l" []),TySchema TyObject (mkSchemaVar "o") def,tTyKeySet]

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

defTrunc :: NativeDefName -> Text -> (Decimal -> Integer) -> NativeDef
defTrunc n desc op = defRNative n fun (funType tTyDecimal [("x",tTyDecimal),("prec",tTyInteger)] <>
                                      unaryTy tTyInteger tTyDecimal)
                     [ ExecExample $ "(" <> asString n <> " 3.5)"
                     , ExecExample $ "(" <> asString n <> " 100.15234 2)"
                     ]
                     (desc <> " value of decimal X as integer, or to PREC precision as decimal.")
    where fun :: RNativeFun e
          fun _ [TLiteral (LDecimal d) _] = return $ toTerm $ op d
          fun i [TLiteral (LDecimal d) _,TLitInteger p]
              | p >= 0 = let p10 = (10 ^ p :: Decimal) in
                         return $ toTerm (fromIntegral (op (d * p10)) / p10)
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
    fun i as@[a@TApp{},b@TApp{},v'] = gasUnreduced i as $ reduce v' >>= \v -> do
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
liftNot i as@[app@TApp{},v'] = gasUnreduced i as $ reduce v' >>= \v -> apply (_tApp app) [v] >>= \r -> case r of
  TLitBool b -> return $ toTerm $ not b
  _ -> delegateError "not?" app r
liftNot i as = argsError' i as


eq :: (Bool -> Bool) -> RNativeFun e
eq f _ [a,b] = return $ toTerm $ f (a `termEq` b)
eq _ i as = argsError i as
{-# INLINE eq #-}

unaryTy :: Type n -> Type n -> FunTypes n
unaryTy rt ta = funType rt [("x",ta)]
binTy :: Type n -> Type n -> Type n -> FunTypes n
binTy rt ta tb = funType rt [("x",ta),("y",tb)]

defCmp :: NativeDefName -> RNativeFun e -> NativeDef
defCmp o f = let o' = asString o
                 ex a' b
                   = ExecExample $ "(" <> o' <> " " <> a' <> " " <> b <> ")"
                 a = mkTyVar "a" [tTyInteger,tTyDecimal,tTyString,tTyTime]
             in
             defRNative o f (binTy tTyBool a a)
             [ ex "1" "3"
             , ex "5.24" "2.52"
             , ex "\"abc\"" "\"def\""
             ]
             $ "True if X " <> o' <> " Y."

-- | Monomorphic compare.
cmp :: (Ordering -> Bool) -> RNativeFun e
cmp cmpFun fi as@[TLiteral a _,TLiteral b _] = do
    c <- case (a,b) of
           (LInteger i,LInteger j) -> return $ i `compare` j
           (LDecimal i,LDecimal j) -> return $ i `compare` j
           (LString i,LString j) -> return $ i `compare` j
           (LTime i,LTime j) -> return $ i `compare` j
           _ -> argsError fi as
    return $ toTerm (cmpFun c)
cmp _ fi as = argsError fi as
{-# INLINE cmp #-}

liftOp :: (a -> a -> a) -> a -> a -> Either b a
liftOp f a b = Right (f a b)

binop' :: (Decimal -> Decimal -> Decimal) -> (Integer -> Integer -> Integer) -> RNativeFun e
binop' dop iop i as = binop (liftOp dop) (liftOp iop) i as

-- | Perform binary math operator with coercion to Decimal as necessary.
binop :: (Decimal -> Decimal -> Either String Decimal) ->
       (Integer -> Integer -> Either String Integer) -> RNativeFun e
binop dop iop fi as@[TLiteral a _,TLiteral b _] = do
  let hdl (Right v) = return $ toTerm v
      hdl (Left err) = evalError' fi $ pretty err <> ": " <> pretty (a,b)
  case (a,b) of
    (LInteger i,LInteger j) -> hdl (i `iop` j)
    (LDecimal i,LDecimal j) -> hdl (i `dop` j)
    (LInteger i,LDecimal j) -> hdl (fromIntegral i `dop` j)
    (LDecimal i,LInteger j) -> hdl (i `dop` fromIntegral j)
    _ -> argsError fi as
binop _ _ fi as = argsError fi as
{-# INLINE binop #-}

assert :: Bool -> String -> Either String a -> Either String a
assert test msg act | test = act
                    | otherwise = Left msg


dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational
int2F :: Integer -> Double
int2F = fromIntegral
f2Int :: Double -> Integer
f2Int = round
liftDecF :: (Double -> Double -> Double) -> Decimal -> Decimal -> Either String Decimal
liftDecF f a b = Right $ f2Dec (dec2F a `f` dec2F b)
liftIntF :: (Double -> Double -> Double) -> Integer -> Integer -> Either String Integer
liftIntF f a b = Right $ f2Int (int2F a `f` int2F b)

unopd :: (Double -> Double) -> RNativeFun e
unopd op _ [TLitInteger i] = return $ toTerm $ f2Dec $ op $ int2F i
unopd op _ [TLiteral (LDecimal n) _] = return $ toTerm $ f2Dec $ op $ dec2F n
unopd _ i as = argsError i as
