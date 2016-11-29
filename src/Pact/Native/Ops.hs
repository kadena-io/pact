{-# LANGUAGE TupleSections #-}
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
    (opDefs)
    where


import Data.Decimal
import Pact.Native.Internal
import Pact.Types
import Data.Default
import qualified Data.Map.Strict as M
import Data.Semigroup


opDefs :: Eval e NativeDef
opDefs = foldDefs
    [defCmp ">" (cmp (== GT))
    ,defCmp "<" (cmp (== LT))
    ,defCmp ">=" (cmp (`elem` [GT,EQ]))
    ,defCmp "<=" (cmp (`elem` [LT,EQ]))
    ,defRNative "=" (eq id) eqTy
     "True if X equals Y. `(= [1 2 3] [1 2 3])` `(= 'foo \"foo\")` `(= { 1: 2 } { 1: 2})`"
    ,defRNative "!=" (eq not) eqTy
     "True if X does not equal Y. `(!= \"hello\" \"goodbye\")`"
    ,defLogic "or" (||)
    ,defLogic "and" (&&)
    ,defRNative "not" not' (unaryTy TyBool TyBool) "Boolean logic. `(not (> 1 2))`"
    ,defRNative "-" minus (coerceBinNum <> unaryNumTys)
     "Negate X, or subtract Y from X. `(- 1.0)` `(- 3 2)`"
    ,defRNative "+" plus plusTy
     "Add numbers, concatenate strings/lists, or merge objects. \
     \`(+ 1 2)` `(+ 5.0 0.5)` `(+ \"every\" \"body\")` `(+ [1 2] [3 4])` \
     \`(+ { \"foo\": 100 } { \"foo\": 1, \"bar\": 2 })`"
    ,defRNative "*" (binop' (*) (*)) coerceBinNum
     "Multiply X by Y. `(* 0.5 10.0)` `(* 3 5)`"
    ,defRNative "/" divide' coerceBinNum
     "Divide X by Y. `(/ 10.0 2.0)` `(/ 8 3)`"
    ,defRNative "^" pow coerceBinNum "Raise X to Y power. `(^ 2 3)`"
    ,defRNative "sqrt" (unopd sqrt) unopTy "Square root of X. `(sqrt 25)`"
    ,defRNative "mod" mod' (binTy TyInteger TyInteger TyInteger) "X modulo Y. `(mod 13 8)`"
    ,defRNative "log" log' coerceBinNum "Log of Y base X. `(log 2 256)`"
    ,defRNative "ln" (unopd log) unopTy "Natural log of X. `(round (ln 60) 6)`"
    ,defRNative "exp" (unopd exp) unopTy "Exp of X `(round (exp 3) 6)`"
    ,defRNative "abs" abs' (unaryTy TyDecimal TyDecimal <> unaryTy TyInteger TyInteger)
     "Absolute value of X. `(abs (- 10 23))`"
    ,defTrunc "round" "Performs Banker's rounding" round
    ,defTrunc "ceiling" "Rounds up" ceiling
    ,defTrunc "floor" "Rounds down" floor
    ]
    where eqTy = binTy TyBool eqA eqA
          eqA = TyVar "a" [TyInteger,TyString,TyTime,TyDecimal,TyBool,TyList Nothing,TyObject Nothing,TyKeySet]
          numA = numV "a"
          numV a = TyVar a [TyInteger,TyDecimal]
          coerceBinNum = binTy numA numA numA <> binTy TyDecimal numA (numV "b")
          unaryNumTys = unaryTy numA numA
          plusA = TyVar "a" [TyString,TyList Nothing,TyObject Nothing]
          plusTy = coerceBinNum <> binTy plusA plusA plusA
          unopTy = unaryTy numA numA

defTrunc :: NativeDefName -> String -> (Decimal -> Integer) -> Eval e (String,Term Name)
defTrunc n desc op = defRNative n fun (funType TyDecimal [("x",TyDecimal),("prec",TyInteger)] <>
                                      unaryTy TyInteger TyDecimal)
                     (desc ++ " value of decimal X as integer, or to PREC precision as decimal. " ++
                     "`(" ++ asString n ++ " 3.5)` `(" ++ asString n ++ " 100.15234 2)`")
    where fun :: RNativeFun e
          fun _ [TLiteral (LDecimal d) _] = return $ toTerm $ op d
          fun i [TLiteral (LDecimal d) _,TLitInteger p]
              | p >= 0 = let p10 = (10 ^ p :: Decimal) in
                         return $ toTerm (fromIntegral (op (d * p10)) / p10)
              | otherwise = evalError' i "Negative precision not allowed"
          fun i as = argsError i as

defLogic :: NativeDefName -> (Bool -> Bool -> Bool) -> Eval e (String,Term Name)
defLogic n bop = defRNative n fun (binTy TyBool TyBool TyBool) $
                 "Boolean logic. `(" ++ asString n ++ " true false)`"
    where fun _ [TLiteral (LBool a) _,TLiteral (LBool b) _] = return $ toTerm $ a `bop` b
          fun i as = argsError i as

not' :: RNativeFun e
not' _ [TLiteral (LBool a) _] = return $ toTerm $ not a
not' i as = argsError i as

eq :: (Bool -> Bool) -> RNativeFun e
eq f _ [a,b] = return $ toTerm $ f (a `termEq` b)
eq _ i as = argsError i as
{-# INLINE eq #-}

unaryTy :: Type -> Type -> FunTypes
unaryTy rt ta = funType rt [("x",ta)]
binTy :: Type -> Type -> Type -> FunTypes
binTy rt ta tb = funType rt [("x",ta),("y",tb)]

defCmp :: NativeDefName -> RNativeFun e -> Eval e (String,Term Name)
defCmp o f = let o' = asString o
                 ex a' b = " `(" ++ o' ++ " " ++ a' ++ " " ++ b ++ ")`"
                 a = TyVar "a" [TyInteger,TyDecimal,TyString,TyTime]
             in
             defRNative o f (binTy TyBool a a) $
             "True if X " ++ o' ++ " Y." ++
             ex "1" "3" ++
             ex "5.24" "2.52" ++
             ex "\"abc\"" "\"def\""

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
      hdl (Left err) = evalError' fi $ err ++ ": " ++ show (a,b)
  case (a,b) of
    (LInteger i,LInteger j) -> hdl (i `iop` j)
    (LDecimal i,LDecimal j) -> hdl (i `dop` j)
    (LInteger i,LDecimal j) -> hdl (fromIntegral i `dop` j)
    (LDecimal i,LInteger j) -> hdl (i `dop` fromIntegral j)
    _ -> argsError fi as
binop _ _ fi as = argsError fi as
{-# INLINE binop #-}

plus :: RNativeFun e
plus _ [TLitString a,TLitString b] = return (tStr $ a ++ b)
plus _ [TList a _ _,TList b _ _] = return (TList (a ++ b) def def)
plus _ [TObject as _ _,TObject bs _ _] =
  let reps (a,b) = (abbrev a,(a,b))
      mapit = M.fromList . map reps
  in return $ TObject (M.elems $ M.union (mapit as) (mapit bs)) def def
plus i as = binop' (+) (+) i as
{-# INLINE plus #-}

minus :: RNativeFun e
minus _ [TLiteral (LInteger n) _] = return (toTerm (negate n))
minus _ [TLiteral (LDecimal n) _] = return (toTerm (negate n))
minus i as = binop' (-) (-) i as
{-# INLINE minus #-}

assert :: Bool -> String -> Either String a -> Either String a
assert test msg act | test = act
                    | otherwise = Left msg

divide' :: RNativeFun e
divide' = binop (\a b -> assert (b /= 0) "Division by 0" $ liftOp (/) a b)
                (\a b -> assert (b /= 0) "Division by 0" $ liftOp div a b)


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

pow :: RNativeFun e
pow = binop (\a b -> liftDecF (**) a b)
            (\a b -> assert (b >= 0) "Integral power must be >= 0" $ liftOp (^) a b)

log' :: RNativeFun e
log' = binop (\a b -> liftDecF logBase a b)
             (\a b -> liftIntF logBase a b)

unopd :: (Double -> Double) -> RNativeFun e
unopd op _ [TLitInteger i] = return $ toTerm $ f2Dec $ op $ int2F i
unopd op _ [TLiteral (LDecimal n) _] = return $ toTerm $ f2Dec $ op $ dec2F n
unopd _ i as = argsError i as


mod' :: RNativeFun e
mod' _ [TLitInteger a, TLitInteger b] = return $ toTerm $ a `mod` b
mod' i as = argsError i as

abs' :: RNativeFun e
abs' _ [TLitInteger a] = return $ toTerm $ abs a
abs' _ [TLiteral (LDecimal n) _] = return $ toTerm $ abs n
abs' i as = argsError i as
