{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pact.Native.Pairing
  ( pairing
  , CurvePoint(..)
  , Fq(..)
  , Fq2
  , Fq6
  , Fq12
  , g1
  , g2
  , multiply
  , add
  , double
  , negatePt
  , b1
  , b2
  , b12
  , G1
  , G2
  )
  where

import Prelude
import qualified Prelude as P

import Control.Monad(join)
import Data.Bits(shiftR)
import Data.Group(Group(..))
import Data.Euclidean (Euclidean, GcdDomain)
import Data.Semiring (Semiring, Ring)
import Data.Field (Field)
import Data.Foldable (forM_, foldl')
import qualified Data.Vector as G
import qualified Data.Vector.Mutable as MG
import qualified Data.Semiring as SR
import qualified Data.Euclidean as E
import Control.Monad.ST
import Data.Mod
import Data.Poly
import Data.Vector(Vector)
import Data.Int(Int8)
import GHC.Real(Ratio(..))
import GHC.Exts(IsList(..))

import Control.DeepSeq (NFData)
import Numeric.Natural(Natural)

-----------------------------------------------------
-- Galois fields and field extensions
------------------------------------------------------
class (Field k, Fractional k, Ord k, Show k) => GaloisField k where
  -- | The characteristic of the field
  characteristic :: k -> Natural

  -- | The degree of the finite field
  degree :: k -> Word

  frobenius :: k -> k

  -- | order of a field p^k
  order :: k -> Natural
  order k = characteristic k ^ degree k
  {-# INLINE order #-}

class GaloisField k => ExtensionField p k | p -> k, k -> p where
  -- | The degree of the
  -- fieldDegree :: Extension p k -> Word
  fieldPoly :: VPoly k


type Q = 21888242871839275222246405745257275088696311157297823662689037894645226208583

newtype Fq = Fq (Mod Q)
  deriving (Eq, Show, Ord, Num, Fractional, Euclidean, Field, GcdDomain, Ring, Semiring, Bounded, Enum, NFData)

newtype Extension p k
  = Extension (VPoly k)
  deriving (Show, Eq, Ord, NFData)


instance GaloisField Fq where
  characteristic _ = 21888242871839275222246405745257275088696311157297823662689037894645226208583

  degree _ = 1

  frobenius = id

-- | Frobenius endomorphism precomputation.
frobenius' :: GaloisField k => Vector k -> Vector k -> Maybe (Vector k)
frobenius' [ ] _ = Just []
frobenius' [a] _ = Just [frobenius a]
frobenius' [a, b] [x, 0, 1]
  | degree x == 2  = Just [a, negate b]
  | characteristic x == 2 = Just [frobenius a - frobenius b * x]
  | otherwise   = Just [frobenius a, frobenius b * nxq]
  where
    nxq = negate x ^ shiftR (characteristic x) 1
frobenius' [a, b] [x, 0, 0, 1]
  | characteristic x == 3 = Just [frobenius a - frobenius b * x]
  | r == 1      = Just [frobenius a, frobenius b * nxq]
  | otherwise   = Just [frobenius a, 0, frobenius b * nxq]
  where
    (q, r) = quotRem (characteristic x) 3
    nxq    = negate x ^ q
frobenius' [a, b, c] [x, 0, 0, 1]
  | characteristic x == 3 = Just [frobenius a - (frobenius b - frobenius c * x) * x]
  | r == 1      = Just [frobenius a, frobenius b * nxq, frobenius c * nxq * nxq]
  | otherwise   = Just [frobenius a, frobenius c * nx * nxq * nxq, frobenius b * nxq]
  where
    (q, r) = quotRem (characteristic x) 3
    nx     = negate x
    nxq    = nx ^ q
frobenius' _ _   = Nothing
{-# INLINE frobenius' #-}
{-# SPECIALIZE frobenius' :: Vector Fq -> Vector Fq -> Maybe (Vector Fq) #-}
{-# SPECIALIZE frobenius' :: Vector Fq2 -> Vector Fq2 -> Maybe (Vector Fq2) #-}
{-# SPECIALIZE frobenius' :: Vector Fq6 -> Vector Fq6 -> Maybe (Vector Fq6) #-}
{-# SPECIALIZE frobenius' :: Vector Fq12 -> Vector Fq12 -> Maybe (Vector Fq12) #-}

-----------------------------------------------------------------
-- Num instances
-----------------------------------------------------------------
instance ExtensionField p k => Num (Extension p k) where
  (Extension x) + (Extension y) = Extension (x + y)
  {-# INLINE (+) #-}
  (Extension x) * (Extension y) =
    Extension (E.rem (toPoly (karatsuba (unPoly x) (unPoly y))) fieldPoly)
  {-# INLINE (*) #-}
  (Extension x) - (Extension y) = Extension (x - y)
  {-# INLINE (-) #-}
  negate (Extension x) = Extension (P.negate x)
  {-# INLINE negate #-}
  fromInteger  = Extension . fromInteger
  {-# INLINE fromInteger #-}
  abs          = error "abs not implemented for Field Extensions"
  signum       = error "signum not implemented for Field Extensions"

karatsubaThreshold :: Int
karatsubaThreshold = 32

plusPoly
  :: Num a
  => Vector a
  -> Vector a
  -> Vector a
plusPoly xs ys = runST $ do
  zs <- MG.unsafeNew lenMx
  forM_ ([0 .. lenMn - 1] :: [Int]) $ \i ->
    MG.unsafeWrite zs i (G.unsafeIndex xs i + G.unsafeIndex ys i)
  G.unsafeCopy
    (MG.unsafeSlice lenMn (lenMx - lenMn) zs)
    (G.unsafeSlice  lenMn (lenMx - lenMn) (if lenXs <= lenYs then ys else xs))
  G.unsafeFreeze zs
  where
    lenXs = G.length xs
    lenYs = G.length ys
    lenMn = lenXs `min` lenYs
    lenMx = lenXs `max` lenYs
{-# INLINE plusPoly #-}
{-# SPECIALIZE plusPoly :: Vector Fq -> Vector Fq -> Vector Fq #-}
{-# SPECIALIZE plusPoly :: Vector Fq2 -> Vector Fq2 -> Vector Fq2 #-}
{-# SPECIALIZE plusPoly :: Vector Fq6 -> Vector Fq6 -> Vector Fq6 #-}
{-# SPECIALIZE plusPoly :: Vector Fq12 -> Vector Fq12 -> Vector Fq12 #-}

karatsuba
  :: (Eq a, Num a)
  => Vector a
  -> Vector a
  -> Vector a
karatsuba xs ys
  | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold
  = convolution xs ys
  | otherwise = runST $ do
    zs <- MG.unsafeNew lenZs
    let lzs0 = G.length zs0
        lzs11 = G.length zs11
        lzs2 = G.length zs2
    forM_ ([0 .. lenZs - 1] :: [Int]) $ \k -> do
      let z0 = if k < lzs0
               then G.unsafeIndex zs0 k
               else 0
          z11 = if k - m >= 0 && k - m < lzs11
               then G.unsafeIndex zs11 (k - m)
               else 0
          z10 = if k - m >= 0 && k - m < lzs0
               then G.unsafeIndex zs0 (k - m)
               else 0
          z12 = if k - m >= 0 && k - m < lzs2
               then G.unsafeIndex zs2 (k - m)
               else 0
          z2 = if k - 2 * m >= 0 && k - 2 * m < lzs2
               then G.unsafeIndex zs2 (k - 2 * m)
               else 0
      MG.unsafeWrite zs k (z0 + (z11 - z10 - z12) + z2)
    G.unsafeFreeze zs
  where
    lenXs = G.length xs
    lenYs = G.length ys
    lenZs = lenXs + lenYs - 1

    m    = ((lenXs `min` lenYs) + 1) `shiftR` 1

    xs0  = G.unsafeSlice 0 m xs
    xs1  = G.unsafeSlice m (lenXs - m) xs
    ys0  = G.unsafeSlice 0 m ys
    ys1  = G.unsafeSlice m (lenYs - m) ys

    xs01 = plusPoly xs0 xs1
    ys01 = plusPoly ys0 ys1
    zs0  = karatsuba xs0 ys0
    zs2  = karatsuba xs1 ys1
    zs11 = karatsuba xs01 ys01
{-# INLINE karatsuba #-}
{-# SPECIALIZE karatsuba :: Vector Fq -> Vector Fq -> Vector Fq #-}
{-# SPECIALIZE karatsuba :: Vector Fq2 -> Vector Fq2 -> Vector Fq2 #-}
{-# SPECIALIZE karatsuba :: Vector Fq6 -> Vector Fq6 -> Vector Fq6 #-}
{-# SPECIALIZE karatsuba :: Vector Fq12 -> Vector Fq12 -> Vector Fq12 #-}

convolution
  :: Num a
  => Vector a
  -> Vector a
  -> Vector a
convolution xs ys
  | lenXs == 0 || lenYs == 0 = G.empty
  | otherwise = G.generate lenZs $ \k -> foldl'
    (\acc i -> acc + (G.unsafeIndex xs i * G.unsafeIndex ys (k - i)))
    0
    ([max (k - lenYs + 1) 0 .. min k (lenXs - 1)] :: [Int])
{-
  | otherwise =
    G.generate lenZs $ \k ->
      let !b = max (k - lenYs + 1) 0
          !e = min k (lenXs - 1)
          !l = (e - b) + 1 in
      sum (zipWith (*) (G.toList (G.unsafeSlice b l xs))
                       (G.toList (G.reverse (G.unsafeSlice (k - e) l ys))))
-}
  where
    !lenXs = G.length xs
    !lenYs = G.length ys
    lenZs = lenXs + lenYs - 1
{-# INLINE convolution #-}
{-# SPECIALIZE convolution :: Vector Fq -> Vector Fq -> Vector Fq #-}
{-# SPECIALIZE convolution :: Vector Fq2 -> Vector Fq2 -> Vector Fq2 #-}
{-# SPECIALIZE convolution :: Vector Fq6 -> Vector Fq6 -> Vector Fq6 #-}
{-# SPECIALIZE convolution :: Vector Fq12 -> Vector Fq12 -> Vector Fq12 #-}

instance ExtensionField p k => Fractional (Extension p k) where
  recip (Extension vp) = case leading g of
    Just (0, vp') -> Extension $ scale 0 (recip vp') y
    _ -> error "Division by zero: Extension"
    where
      (g, y) = E.gcdExt vp fieldPoly
  {-# INLINE recip #-}
  fromRational (x :% y) = fromInteger x / fromInteger y
  {-# INLINE fromRational #-}


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
    | otherwise = recip x ^ P.negate n
  {-# INLINE pow #-}

instance ExtensionField p k => Euclidean (Extension p k) where
  degree  = error "Not Defined: Euclidean degree for Extension"
  quotRem l r = (l / r, 0)
  {-# INLINE quotRem #-}

instance ExtensionField p k => Field (Extension p k)

instance ExtensionField p k => GaloisField (Extension p k) where
  characteristic _ = characteristic (undefined :: k)
  degree _ = degree (undefined :: k) * deg'
    where
    deg' = fromIntegral (E.degree (fieldPoly :: VPoly k))
  frobenius y@(Extension x) = case frobenius' (unPoly x) (unPoly (fieldPoly :: VPoly k)) of
    Just f -> Extension (toPoly f)
    Nothing -> pow y (characteristic y)

{-# RULES "Extension.pow"
  forall (k :: ExtensionField p k => Extension p k) n . (^) k n = pow k n
  #-}


instance ExtensionField p k => GcdDomain (Extension p k)

instance ExtensionField p k => Ring (Extension p k) where
  negate = P.negate
  {-# INLINE negate #-}

instance ExtensionField p k => Semiring (Extension p k) where
  fromNatural = fromIntegral
  {-# INLINE fromNatural #-}
  one         = Extension 1
  {-# INLINE one #-}
  plus        = (+)
  {-# INLINE plus #-}
  times       = (*)
  {-# INLINE times #-}
  zero        = Extension 0
  {-# INLINE zero #-}

instance ExtensionField p k => IsList (Extension p k) where
  type instance Item (Extension p k) = k
  fromList     = Extension . fromList
  {-# INLINE fromList #-}
  toList (Extension x) = toList $ unPoly x
  {-# INLINE toList #-}

--------------------------------------------------------------------------------------
-- Curve implementation
--------------------------------------------------------------------------------------

data F1

data F2

data F3

type Fq2 = Extension F1 Fq

instance ExtensionField F1 Fq where
  fieldPoly = fromList [1, 0, 1]

xiFq2 :: Fq2
xiFq2 = fromList [9, 1]
{-# INLINE xiFq2 #-}

type Fq6 = Extension F2 Fq2

instance ExtensionField F2 Fq2 where
  fieldPoly = fromList [-xiFq2, 0, 0, 1]
  {-# INLINE fieldPoly #-}

type Fq12 = Extension F3 Fq6

instance ExtensionField F3 Fq6 where
  fieldPoly = fromList [[0, -1], 0, 1]
  {-# INLINE fieldPoly #-}

type G1 = CurvePoint Fq
type G2 = CurvePoint Fq2

-----------------------------------------------------------------------------------
-- Curve implementation
-----------------------------------------------------------------------------------

-- Curve is y**2 = x**3 + 3
b1 :: Fq
b1 = 3

-- Twisted curve over FQ**2
b2 :: Fq2
b2 = let
  v1 = fromList [3, 0]
  v2 = fromList [9, 1]
  in v1 / v2

b12 :: Fq12
b12 = [3]

g1 :: CurvePoint Fq
g1 = Point 1 2
{-# INLINE g1 #-}

g2 :: CurvePoint Fq2
g2 = Point
  [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634]
  [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531]

data CurvePoint a
  = Point !a !a
  | CurveInf
  deriving (Eq, Show)

-- todo: sort of scuffed, num instance?
double :: (Field a, Num a, Eq a) => CurvePoint a -> CurvePoint a
double CurveInf = CurveInf
double (Point x y)
  | y == 0 = CurveInf
  | otherwise = let
    y1 = y + y
    x1 = x * x
    l = (x1 + x1 + x1) `E.quot` y1
    newx = (l * l) - (x + x)
    newy = l * (x - newx) - y
    in Point newx newy
{-# SPECIALIZE double :: CurvePoint Fq -> CurvePoint Fq #-}
{-# SPECIALIZE double :: CurvePoint Fq2 -> CurvePoint Fq2 #-}
{-# SPECIALIZE double :: CurvePoint Fq6 -> CurvePoint Fq6 #-}
{-# SPECIALIZE double :: CurvePoint Fq12 -> CurvePoint Fq12 #-}

add :: (Field a, Eq a, Num a) => CurvePoint a -> CurvePoint a -> CurvePoint a
add CurveInf r = r
add l CurveInf = l
add p1@(Point x1 y1) (Point x2 y2)
  | x2 == x1 && y2 == y1 = double p1
  | x2 == x1 = CurveInf
  | otherwise = let
    l = (y2 - y1) `E.quot` (x2 - x1)
    newx = (l * l) - x1 - x2
    newy =  l * (x1 - newx) - y1
    in Point newx newy
{-# SPECIALIZE add :: CurvePoint Fq -> CurvePoint Fq -> CurvePoint Fq #-}
{-# SPECIALIZE add :: CurvePoint Fq2 -> CurvePoint Fq2 -> CurvePoint Fq2 #-}
{-# SPECIALIZE add :: CurvePoint Fq6 -> CurvePoint Fq6 -> CurvePoint Fq6 #-}
{-# SPECIALIZE add :: CurvePoint Fq12 -> CurvePoint Fq12 -> CurvePoint Fq12 #-}

multiply :: (Field a, Eq a, Num a) => CurvePoint a -> Integer -> CurvePoint a
multiply pt n
  | n < 0 = multiply (negatePt pt) (-n)
  | n == 0 = CurveInf
  | n == 1 = pt
  | even n = multiply (double pt) (n `div` 2)
  | otherwise =
      add (multiply (double pt) (n `div` 2)) pt
{-# SPECIALIZE multiply :: CurvePoint Fq -> Integer -> CurvePoint Fq #-}
{-# SPECIALIZE multiply :: CurvePoint Fq2 -> Integer -> CurvePoint Fq2 #-}
{-# SPECIALIZE multiply :: CurvePoint Fq6 -> Integer -> CurvePoint Fq6 #-}
{-# SPECIALIZE multiply :: CurvePoint Fq12 -> Integer -> CurvePoint Fq12 #-}

negatePt :: Num a => CurvePoint a -> CurvePoint a
negatePt (Point x y) =
  Point x (negate y)
negatePt CurveInf = CurveInf
{-# SPECIALIZE negatePt :: CurvePoint Fq -> CurvePoint Fq #-}
{-# SPECIALIZE negatePt :: CurvePoint Fq2 -> CurvePoint Fq2 #-}
{-# SPECIALIZE negatePt :: CurvePoint Fq6 -> CurvePoint Fq6 #-}
{-# SPECIALIZE negatePt :: CurvePoint Fq12 -> CurvePoint Fq12 #-}

frobTwisted
  :: Fq2
  -> CurvePoint Fq2
  -> CurvePoint Fq2
frobTwisted xi (Point x y) = Point (frobenius x * pow xi tx) (frobenius y * pow xi ty)
  where
    tx = quot (characteristic (undefined :: Fq) - 1) 3
    ty = shiftR (characteristic (undefined :: Fq)) 1
frobTwisted _ _        = CurveInf

lineFunction
  :: CurvePoint Fq  -- ^ Point @P@.
  -> CurvePoint Fq2 -- ^ Point @T@.
  -> CurvePoint Fq2 -- ^ Point @Q@.
  -> (CurvePoint Fq2, Fq12)   -- ^ Points @T + Q@ and @Line(T, Q, P)@.
lineFunction (Point x y) (Point x1 y1) (Point x2 y2)
  | x1 /= x2       = (Point x3 y3 , [embedFqToFq6 (-y), [scalarEmbed x l , y1 - l  * x1]])
  | y1 + y2 == 0   = (CurveInf , [embedFqToFq6 x, embedFq2ToFq6 (-x1)])
  | otherwise      = (Point x3' y3', [embedFqToFq6 (-y), [scalarEmbed x l', y1 - l' * x1]])
  where
    scalarEmbed e1 e2 = embedFqToFq2 e1 * e2
    embedFqToFq2 q = Extension (monomial 0 q)
    embedFqToFq6 q = embedFq2ToFq6 (Extension (monomial 0 q))
    embedFq2ToFq6 q = Extension (monomial 0 q) :: Fq6
    l   = (y2 - y1) / (x2 - x1)
    x3  = l * l - x1 - x2
    y3  = l * (x1 - x3) - y1
    x12 = x1 * x1
    l'  = (x12 + x12 + x12) / (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
lineFunction _ _ _ = (CurveInf, mempty)
{-# INLINE lineFunction #-}

powUnitary :: ExtensionField p k
  => Extension p k -- ^ Element @x@ in cyclotomic subgroup.
  -> Integer       -- ^ Integer @n@.
  -> Extension p k -- ^ Element @x ^ n@.
powUnitary x n
  | n < 0     = pow (conj x) (negate n)
  | otherwise = pow x n
{-# INLINE powUnitary #-}
{-# SPECIALIZE powUnitary :: Extension F1 Fq -> Integer -> Extension F1 Fq #-}
{-# SPECIALIZE powUnitary :: Extension F2 Fq2 -> Integer -> Extension F2 Fq2 #-}
{-# SPECIALIZE powUnitary :: Extension F3 Fq6 -> Integer -> Extension F3 Fq6 #-}

-- | Complex conjugation @a+bi -> a-bi@ of quadratic extension field.
conj :: forall p k. ExtensionField p k => Extension p k -> Extension p k
conj (Extension x) = case unPoly (fieldPoly @p @k) of
  [_, 0, 1] -> case x of
    [a, b] -> [a, negate b]
    [a]    -> [a]
    _      -> []
  _         -> error "conj: extension degree is not two."
{-# INLINE conj #-}
{-# SPECIALIZE conj :: Extension F1 Fq -> Extension F1 Fq #-}
{-# SPECIALIZE conj :: Extension F2 Fq2 -> Extension F2 Fq2 #-}
{-# SPECIALIZE conj :: Extension F3 Fq6 -> Extension F3 Fq6 #-}

additionStep
  :: CurvePoint Fq
  -> CurvePoint Fq2
  -> (CurvePoint Fq2, Fq12)
  -> (CurvePoint Fq2, Fq12)
additionStep p q (t, f) = (<>) f <$> lineFunction p q t
{-# INLINE additionStep #-}

-- Doubling step, line 4.
doublingStep
  :: CurvePoint Fq
  -> (CurvePoint Fq2, Fq12)
  -> (CurvePoint Fq2, Fq12)
doublingStep p (t, f) = (<>) f . (<>) f <$> lineFunction p t t
{-# INLINE doublingStep #-}

parameterHex :: Integer
parameterHex = 0x44e992b44a6909f1

-- | [Final exponentiation for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
finalExponentiate :: Integer -> Fq12 -> Fq12
finalExponentiate u = hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip              -- f^(p^6 - 1)
        p2 = (*) <$> id <*> frobenius . frobenius -- f^(p^2 + 1)
    hardPart !f = p4
      where
        !fu  = powUnitary f u                      -- f^u
        !fu2 = powUnitary fu u                     -- f^(u^2)
        !fu3 = powUnitary fu2 u                    -- f^(u^3)
        !fpu = frobenius fu2                          -- f^(pu^2)
        !y0  = frobenius (f * frobenius (f * frobenius f))  -- f^(p + p^2 + p^3)
        !y1  = conj f                              -- f^(-1)
        !y2  = frobenius fpu                          -- f^(p^2u^2)
        !y3  = conj $ frobenius fu                    -- f^(-pu)
        !y4  = conj $ fu * fpu                     -- f^(-u - pu^2)
        !y5  = conj fu2                            -- f^(-u^2)
        !y6  = conj $ fu3 * frobenius fu3             -- f^(-u^3 - pu^3)
        !p4  = p4' * y0 * join (*) (p4' * y1)      -- f^((p^4 - p^2 + 1) / r)
          where
            p4'  = join (*) $ p4'' * y2 * join (*) (p4'' * y3 * y5)
            p4'' = y4 * y5 * join (*) y6
{-# INLINE finalExponentiate #-}


-- | [Miller algorithm for Barreto-Naehrig curves]
-- (https://eprint.iacr.org/2010/354.pdf).
pairing
  :: G1
  -> G2
  -> Fq12
pairing p q =
  finalExponentiate parameterHex $
  finalStepBN $
  millerLoop parameterBin (q, mempty)
  where
  millerLoop []     tf = tf
  millerLoop (x:xs) tf = case doublingStep p tf of
    tf2
      | x == 0    -> millerLoop xs tf2
      | x == 1    -> millerLoop xs $ additionStep p q tf2
      | otherwise -> millerLoop xs $ additionStep p (negatePt q) tf2
  finalStepBN (t, f) = case lineFunction p t q1 of
                  (t', f') -> case lineFunction p t' q2 of
                    (_, f'') -> f <> f' <> f''
    where
      q1 = frobTwisted xiFq2 q
      q2 = negatePt $ frobTwisted xiFq2 q1

  parameterBin :: [Int8]
  parameterBin = [ 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
                 , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
                 , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
                 , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0 ]
{-# INLINABLE pairing #-}
