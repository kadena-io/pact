{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Pact.Native.Pairing where

import Prelude
import qualified Prelude as P

import Data.Bits((.&.))
import Data.Foldable(foldl')
import Data.Group(Group(..))
import Data.Euclidean (Euclidean, GcdDomain)
import Data.Semiring (Semiring, Ring)
import Data.Field (Field)
import qualified Data.Semiring as SR
import qualified Data.Euclidean as E
import Data.Mod
import Data.Poly
import GHC.Real(Ratio(..))
import GHC.Exts(IsList(..))

import Control.DeepSeq (NFData)

type Q = 21888242871839275222246405745257275088696311157297823662689037894645226208583
type Fq = Mod Q

fieldModulus :: Integer
fieldModulus = 21888242871839275222246405745257275088696311157297823662689037894645226208583

newtype Extension p k
  = Extension { _extension :: VPoly k }
  deriving (Show, Eq, Ord, NFData)

class (Field k, Fractional k, Ord k, Show k) => ExtensionField p k | p -> k where
  fieldDegree :: Extension p k -> Word
  fieldPoly :: Extension p k -> VPoly k

-----------------------------------------------------------------
-- Num instances
-----------------------------------------------------------------
instance ExtensionField p k => Num (Extension p k) where
  (Extension x) + (Extension y) = Extension (x + y)
  {-# INLINE (+) #-}
  (Extension x) * (Extension y) = Extension (E.rem (x * y) (fieldPoly (undefined :: Extension p k)))
  {-# INLINABLE (*) #-}
  (Extension x) - (Extension y) = Extension (x - y)
  {-# INLINE (-) #-}
  negate (Extension x) = Extension (P.negate x)
  {-# INLINE negate #-}
  fromInteger  = Extension . fromInteger
  {-# INLINABLE fromInteger #-}
  abs          = error "abs not implemented for Field Extensions"
  signum       = error "signum not implemented for Field Extensions"

instance ExtensionField p k => Fractional (Extension p k) where
  recip (Extension vp) = case E.gcdExt vp (fieldPoly (undefined :: Extension p k)) of
    (1, vp') -> Extension vp'
    _ -> error "Division by zero: Extension"
  {-# INLINABLE recip #-}
  fromRational (x :% y) = fromInteger x / fromInteger y
  {-# INLINABLE fromRational #-}

-----------------------------------------------------------------
-- Group instances
-----------------------------------------------------------------
instance ExtensionField p k => Semigroup (Extension p k) where
  (<>)   = (*)
  {-# INLINE (<>) #-}

instance ExtensionField p k => Monoid (Extension p k) where
  mempty = Extension 1
  {-# INLINE mempty #-}

instance ExtensionField p k => Group (Extension p k) where
  invert  = recip
  {-# INLINE invert #-}
  pow x n
    | n >= 0    = x ^ n
    | otherwise = recip x ^ negate n
  {-# INLINE pow #-}

instance ExtensionField p k => Euclidean (Extension p k) where
  degree  = undefined
  quotRem l r = (l / r, 0)
  {-# INLINE quotRem #-}

instance ExtensionField p k => Field (Extension p k)

instance ExtensionField p k => GcdDomain (Extension p k)

instance ExtensionField p k => Ring (Extension p k) where
  negate = P.negate
  {-# INLINE negate #-}

instance ExtensionField p k => Semiring (Extension p k) where
  fromNatural = fromIntegral
  {-# INLINABLE fromNatural #-}
  one         = Extension 1
  {-# INLINE one #-}
  plus        = (+)
  {-# INLINE plus #-}
  times       = (*)
  {-# INLINE times #-}
  zero        = Extension 0
  {-# INLINE zero #-}

data F1

data F2

data F3

instance ExtensionField p k => IsList (Extension p k) where
  type instance Item (Extension p k) = k
  fromList     = Extension . fromList
  {-# INLINABLE fromList #-}
  toList (Extension x) = toList $ unPoly x
  {-# INLINABLE toList #-}

type Fq2 = Extension F1 Fq

instance ExtensionField F1 Fq where
  fieldDegree _ = 2
  fieldPoly _ = fromList [1, 0, 1]

xi :: Fq2
xi = fromList [9, 1]
{-# INLINABLE xi #-}

type Fq6 = Extension F2 Fq2

instance ExtensionField F2 Fq2 where
  fieldDegree _ = 6
  fieldPoly _ = fromList [-xi, 0, 0, 1]
  {-# INLINABLE fieldPoly #-}

type Fq12 = Extension F3 Fq6

instance ExtensionField F3 Fq6 where
  fieldDegree _ = 12
  fieldPoly _ = fromList [fromList [0, -1], 0, 1]
  {-# INLINABLE fieldPoly #-}

-- Curve is y**2 = x**3 + 3
curveB :: Fq
curveB = 3

-- Twisted curve over FQ**2
b2 :: Fq2
b2 = let
  v1 = fromList [3, 0]
  v2 = fromList [9, 1]
  in v1 / v2

b12 :: Fq12
b12 = fromList [fromList [3, 0], 0]

g1 :: CurvePoint Fq
g1 = Point 1 2

g2 :: CurvePoint Fq2
g2 = Point
  (fromList [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634])
  (fromList [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531])

data CurvePoint a
  = Point !a !a
  | CurveInf
  deriving (Eq, Show)

-- todo: sort of scuffed, num instance?
double :: (Field a, Num a) => CurvePoint a -> CurvePoint a
double CurveInf = CurveInf
double (Point x y) = let
  y1 = y + y
  x1 = x * x
  l = (x1 + x1 + x1) `E.quot` y1
  newx = (l * l) - (x + x)
  newy = SR.times (negate l) newx + (SR.times l x) - y
  in Point newx newy

add :: (Field a, Eq a, Num a) => CurvePoint a -> CurvePoint a -> CurvePoint a
add CurveInf r = r
add l CurveInf = l
add p1@(Point x1 y1) (Point x2 y2)
  | x2 == x1 && y2 == y1 = double p1
  | x2 == x1 = CurveInf
  | otherwise = let
    l = (y2 - y1) `E.quot` (x2 - x1)
    newx = (SR.times l l) - x1 - x2
    newy = SR.times (SR.negate l) newx + SR.times l x1 - y1
    in Point newx newy

multiply :: (Field a, Eq a, Num a) => CurvePoint a -> Integer -> CurvePoint a
multiply pt n
  | n == 0 = CurveInf
  | n == 1 = pt
  | even n = multiply (double pt) (n `div` 2)
  | otherwise =
    add (multiply (double pt) (n `div` 2)) pt

negatePt :: SR.Ring a => CurvePoint a -> CurvePoint a
negatePt (Point x y) =
  Point x (SR.negate y)
negatePt CurveInf = CurveInf

w :: Fq12
w = fromList [fromList [0, 1], 0]

twist :: CurvePoint Fq2 -> CurvePoint Fq12
twist CurveInf = CurveInf
twist (Point x y) = let
  -- Elements of Fq
  x' = toList x
  y' = toList y
  -- Single Fq element
  x1 = (x' !! 0 :: Fq)
  x2 = x' !! 1
  y1 = (y' !! 0 :: Fq)
  y2 = y' !! 1
  nx = fromList [fromList [fromList [x1 - x2 * 9]], fromList [fromList [x2]]]
    -- fromList ([xcoeffs !! 0] ++ replicate 5 0 ++ [xcoeffs !! 1] ++ replicate 5 0)
  ny = fromList [fromList [fromList [y1 - y2 * 9]], fromList [fromList [y2]]]
  -- ([fromList ([ycoeffs !! 0] ++ replicate 5 0) (fromList ([ycoeffs !! 1] ++ replicate 5 0))])
  in Point (nx * (w * w)) (ny  * (w * w * w))

g12 :: CurvePoint Fq12
g12 = twist g2

curveOrder :: Integer
curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

ate_loop_count :: Integer
ate_loop_count = 29793968203157093288

log_ate_loop_count :: Integer
log_ate_loop_count = 63

lineFunc :: (Eq a, Euclidean a, Num a) => CurvePoint a -> CurvePoint a -> CurvePoint a -> a
lineFunc (Point x1 y1) (Point x2 y2) (Point xt yt)
  | x1 /= x2 = let
    -- if x1 != x2:
    -- m = (y2 - y1) / (x2 - x1)
    -- return m * (xt - x1) - (yt - y1)
    m = (y2 - y1) `E.quot` (x2 - x1)
    in m * (xt - x1) - (yt - y1)
    --  elif y1 == y2:
    --     m = 3 * x1**2 / (2 * y1)
    --     return m * (xt - x1) - (yt - y1)
  | y1 == y2 = let
    x = (x1 * x2)
    m = (x + x + x) `E.quot` (y1 + y1)
    in m * (xt - x1) - (yt - y1)
  | otherwise = xt - x1
lineFunc _ _ _ = error "boom"

one' :: CurvePoint Fq
one' = g1

two :: CurvePoint Fq
two = double g1

three :: CurvePoint Fq
three = multiply g1 3

negone :: CurvePoint Fq
negone = multiply g1 (curveOrder - 1)

negtwo :: CurvePoint Fq
negtwo = multiply g1 (curveOrder - 2)

negthree :: CurvePoint Fq
negthree = multiply g1 (curveOrder - 3)

millerLoop :: CurvePoint Fq12 -> CurvePoint Fq12 -> Fq12
millerLoop _ CurveInf = 1
millerLoop CurveInf _ = 1
millerLoop q@(Point x1 y1) p = let
  f = 1
  loop (f', r') i = let
    f'' = f' * f' * lineFunc r' r' p
    r'' = double r'
    in if ate_loop_count .&. (2 ^ i) /= 0 then (f'' * lineFunc r'' q p, add r'' q)
       else (f'', r'')
  (f1, r1) = foldl' loop (f, q) (reverse [0 .. ate_loop_count])
  q1x = (x1 ^ fieldModulus)
  q1y = (y1 ^ fieldModulus)
  q1 = Point q1x q1y
  nQ2 = Point (q1x ^ fieldModulus) (negate q1y ^ fieldModulus)
  f2 = f1 * lineFunc r1 q1 p
  r2 = add r1 q1
  f3 = f2 * lineFunc r2 nQ2 p
  in f3 ^ ((fieldModulus ^ (12 :: Int) - 1) `div` curveOrder)

castToFq12 :: CurvePoint Fq -> CurvePoint Fq12
castToFq12 CurveInf = CurveInf
castToFq12 (Point x y) =
  Point
    (fromList [fromList [fromList [x]], 0])
    (fromList [fromList [fromList [y]], 0])

pairing :: CurvePoint Fq2 -> CurvePoint Fq -> Fq12
pairing q p =
  millerLoop (twist q) (castToFq12 p)
