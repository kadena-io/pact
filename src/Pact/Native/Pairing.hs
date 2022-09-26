{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Native.Pairing where

import Prelude hiding ((+), (-), (*), fromIntegral)

import Control.Lens
import Control.Monad
import Control.Monad.ST

-- import Data.Field(Field)
import Data.Vector(Vector)
import Data.Semiring
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import qualified Data.Semiring as SR
-- import qualified Data.Euclidean as E

newtype FQ = FQ { _unFQ :: Integer }
  deriving (Eq, Ord, Show)

class Euclidean a where
  equot :: a -> a -> a

class (Ring a, Euclidean a) => Field a

fieldModulus :: Integer
fieldModulus =
  21888242871839275222246405745257275088696311157297823662689037894645226208583

(-.) :: FQ -> FQ -> FQ
(FQ l) -. (FQ r) = FQ ((l - r) `mod` fieldModulus)

fqDiv :: FQ -> FQ -> FQ
(FQ l) `fqDiv` (FQ r) = FQ ((l * inv r fieldModulus) `mod` fieldModulus)

fqNegate :: FQ -> FQ
fqNegate (FQ a) = FQ (inv a fieldModulus)

fqPow :: FQ -> Integer -> FQ
fqPow a p
  | p == 0 = FQ 1
  | p == 1 = a
  | otherwise = evens a p
  where
  evens x y
    | even y = evens (x * x) (y `div` 2)
    | y == 1 = x
    | otherwise = odds (x * x) (y `div` 2) x
  odds x y z
    | even y = odds (x * x) (y `div` 2) x
    | y == 1 = x * z
    | otherwise = odds (x * x) (y `div` 2) (x * z)

inv :: Integer -> Integer -> Integer
inv a n
  | a == 0 = 0
  | otherwise = go 1 0 (a `mod` n) n
  where
  go !lm !hm !low !high | low > 1 = let
    r = high `div` low
    nm = hm - lm * r
    new = high - low * r
    in go nm lm new low
  go !lm _ _ _ = lm `mod` n

type FQVector = Vector FQ
type CoeffVector = Vector FQ

instance Semiring FQ where
  plus (FQ l) (FQ r) = FQ ((l + r) `mod` fieldModulus)
  zero = FQ 0
  times (FQ l) (FQ r) = FQ ((l * r) `mod` fieldModulus)
  one = FQ 1

instance Ring FQ where
  negate (FQ r) = (FQ (-r))



deg :: Vector FQ -> Int
deg p =
  let d = length p - 1
  in go d
  where
  go d | d >= 0 && (p V.! d == FQ 0) = go (d - 1)
       | otherwise = d

class ExtensionField p where
  fieldCoeffs :: Lens' p FQVector
  fieldDegree :: p -> Int
  fieldModulusCoeffs :: p -> CoeffVector


newtype FQ2 =
  FQ2 { _fq2Coeffs :: FQVector }
  deriving (Eq, Show)

makeLenses ''FQ2

instance ExtensionField FQ2 where
  fieldCoeffs = fq2Coeffs
  fieldDegree _ = 2
  fieldModulusCoeffs _ = V.fromList (fmap FQ [1, 0])

mkFQ2 :: [Integer] -> FQ2
mkFQ2 li = FQ2 (V.fromList $ fmap FQ li)

newtype FQ12 =
  FQ12 { _fq12Coeffs :: FQVector }
  deriving (Eq, Show)

makeLenses ''FQ12

instance ExtensionField FQ12 where
  fieldCoeffs = fq12Coeffs
  fieldDegree _ = 12
  fieldModulusCoeffs _ = V.fromList (fmap FQ [82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0])

instance Semiring FQ12 where
  plus = fqpAdd
  zero = mkFQ12 (replicate 12 0)
  times = fqpMul
  one = mkFQ12 ([1] ++ replicate 11 0)


mkFQ12 :: [Integer] -> FQ12
mkFQ12 li = FQ12 (V.fromList $ fmap FQ li)

fqpAdd :: ExtensionField p => p -> p -> p
fqpAdd l r =
  set fieldCoeffs (V.zipWith (+) (view fieldCoeffs l) (view fieldCoeffs r)) l

fqpSub :: ExtensionField p => p -> p -> p
fqpSub l r =
  set fieldCoeffs (V.zipWith (-.) (view fieldCoeffs l) (view fieldCoeffs r)) l

fqpMul :: ExtensionField p => p -> p -> p
fqpMul l r = runST $ do
  let blen = deg' * 2 - 1
  -- Note -1 on blen because haskell ranges are inclusive
  b <- MV.replicate blen (FQ 0)
  forM_ [0 .. deg' - 1] (\i -> forM_ [0 .. deg' - 1] $ \j ->
    MV.modify b (\a' -> a' + (coeff i l * coeff j r)) (i + j))
  -- out :: STVector s FQ <- MV.new deg'
  forM_ (reverse [deg' + 1 .. blen - 1]) $ \i -> do
    let exp' = blen - deg' - 1
    top <- MV.read b i
    forM [0 .. deg' - 1] $ \j -> do
      bval <- MV.read b (exp' + j)
      MV.write b (exp' + j) (bval -. (top * (modCoeffs V.! fromIntegral j)))
  out' <- V.freeze (MV.take deg' b)
  pure (set fieldCoeffs out' l)
  where
  deg' = fieldDegree l
  modCoeffs = fieldModulusCoeffs l
  coeff i p = view fieldCoeffs p V.! (fromIntegral i)

fqpCoeffDiv :: ExtensionField p => p -> FQ -> p
fqpCoeffDiv l coeff =
  over (fieldCoeffs.mapped) (`fqDiv` coeff) l

--- Todo: list concat + vec maybe instead?
fqpInv :: ExtensionField p => p -> p
fqpInv fqp1 = runST $ do
  lm <- MV.replicate (degree + 1) (FQ 0)
  MV.write lm 0 (FQ 1)
  hm <- MV.replicate (degree + 1) (FQ 0)
  low <- MV.replicate (degree + 1) (FQ 0)
  forM_ [0 .. degree - 1] $ \i ->
    MV.write low i (view fieldCoeffs fqp1 V.! i)
  high <- MV.replicate (degree + 1) (FQ 1)
  forM_ [0 .. degree - 1] $ \i ->
    MV.write high i (coeffs V.! i)
  go lm hm low high
  where
  degree = fieldDegree fqp1
  coeffs = fieldModulusCoeffs fqp1
  go lm hm low high = do
    ldeg <- mdeg low
    if ldeg > 0 then do
      r <- polyRoundedDiv high low
      r' <- MV.replicate (degree + 1) (FQ 0)
      forM_ [0 .. MV.length r - 1] $ \i -> do
        v <- MV.read r i
        MV.write r' i v
      nm <- MV.clone hm
      new <- MV.clone high
      forM_ [0 .. degree] $ \i -> do
        forM_ [0 .. degree - i] $ \j -> do
          lmi <- MV.read lm i
          rj <- MV.read r' j
          lowi <- MV.read low i
          MV.modify nm (\nmij -> nmij -. (lmi * rj)) (i + j)
          MV.modify new (\nij -> nij * (lowi * rj)) (i + j)
      go nm lm new low
    else do
      v <- V.freeze (MV.take degree lm)
      let fqp' = set fieldCoeffs v fqp1
      c <- MV.read low 0
      pure (fqpCoeffDiv fqp' c)
  polyRoundedDiv a b = do
    temp <- MV.clone a
    o <- MV.replicate (MV.length a) (FQ 0)
    dega <- mdeg a
    degb <- mdeg b
    let v = dega - degb
    forM_ (reverse [-1 .. v]) $ \i -> do
      v1 <- MV.read temp (degb + i)
      v2 <- MV.read b degb
      let v' = v1 `fqDiv` v2
      MV.modify o (\a' -> a' + v') i
      forM_ [0..degb] $ \c -> do
        c' <- MV.read o c
        MV.modify temp (\a' -> a' -. c') (c + i)
    degO <- mdeg o
    pure (MV.take (degO + 1) o)
  -- mdeg :: MV.MVector s FQ -> ST s Int
  mdeg p = do
    let d = MV.length p - 1
    go' d
    where
    go' d
      | d >= 0 = MV.read p d >>= \d' ->
        if d' == FQ 0 then go' (d - 1)
        else pure d
      | otherwise = pure d


fqpDiv :: ExtensionField p => p -> p -> p
fqpDiv l r =
  fqpMul l (fqpInv r)

-- Curve is y**2 = x**3 + 3
curveB :: FQ
curveB = FQ 3

-- Twisted curve over FQ**2
b2 :: FQ2
b2 = let
  v1 = FQ2 (V.fromList [FQ 3, FQ 0])
  v2 = FQ2 (V.fromList [FQ 9, FQ 1])
  in fqpDiv v1 v2

b12 :: FQ12
b12 = FQ12 (V.fromList $ [FQ 3] ++ replicate 11 (FQ 0))

g1 :: CurvePoint FQ
g1 = Point (FQ 1) (FQ 2)

g2 :: CurvePoint FQ2
g2 = Point
  (mkFQ2 [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634])
  (mkFQ2 [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531])

data CurvePoint a
  = Point !a !a
  | CurveInf
  deriving (Eq, Show)

-- todo: sort of scuffed, num instance?
double :: Field a => CurvePoint a -> CurvePoint a
double CurveInf = CurveInf
double (Point x y) = let
  y1 = y SR.+ y
  x1 = x SR.* x
  l = (x1 SR.+ x1 SR.+ x1) `equot` y1
  newx = (l SR.* l) SR.- (x SR.+ x)
  newy = SR.times  (SR.negate l) newx SR.+ (SR.times l x) SR.- y
  in Point newx newy

add :: (Field a, Eq a) => CurvePoint a -> CurvePoint a -> CurvePoint a
add CurveInf r = r
add l CurveInf = l
add p1@(Point x1 y1) (Point x2 y2)
  | x2 == x1 && y2 == y1 = double p1
  | x2 == x1 = CurveInf
  | otherwise = let
    l = (y2 SR.- y1) `equot` (x2 SR.- x1)
    newx = (SR.times l l) SR.- x1 SR.- x2
    newy = SR.times (SR.negate l) newx SR.+ SR.times l x1 SR.- y1
    in Point newx newy

multiply :: (Field a, Eq a) => CurvePoint a -> Int -> CurvePoint a
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

w :: FQ12
w = mkFQ12 ([0, 1] ++ replicate 10 0)

twist :: CurvePoint FQ2 -> CurvePoint FQ12
twist CurveInf = CurveInf
twist (Point x y) = let
  x1 = view fieldCoeffs x V.! 0
  x2 = view fieldCoeffs x V.! 1
  y1 = view fieldCoeffs y V.! 0
  y2 = view fieldCoeffs y V.! 1
  xcoeffs = [x1 - x2 * (fromIntegral (9 :: Int)), x2]
  ycoeffs = [y1 - y2 * (fromIntegral (9 :: Int)), y2]
  nx = FQ12 $ V.fromList ([xcoeffs !! 0] ++ replicate 5 (FQ 0) ++ [xcoeffs !! 1] ++ replicate 5 (FQ 0))
  ny = FQ12 $ V.fromList ([ycoeffs !! 0] ++ replicate 5 (FQ 0) ++ [ycoeffs !! 1] ++ replicate 5 (FQ 0))
  in Point (nx * (w * w)) (ny  * (w * w * w))

g12 :: CurvePoint FQ12
g12 = twist g2
