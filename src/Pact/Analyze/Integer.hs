{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pact.Analyze.Integer where

import Algebra.Lattice
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import Pact.Analyze.Types

instance MembershipInfo Bounds Integer where
  isNonEmptySet (Bounds (Just (SymLT ub)) (Just (SymGT lb))) = ub > succ lb
  isNonEmptySet (Bounds (Just (SymLTE ub)) (Just (SymGT lb)))
    | ub > lb = True
    | otherwise = False
  isNonEmptySet (Bounds (Just (SymLT ub)) (Just (SymGTE lb)))
    | ub > lb = True
    | otherwise = False
  isNonEmptySet (Bounds (Just (SymLTE ub)) (Just (SymGTE lb)))
    | ub > lb = True
    | ub == lb = True
    | otherwise = False
  isNonEmptySet _ = True

  getConcreteValue (Bounds (Just (SymLT ub)) (Just (SymGT lb)))
    | ub == succ (succ lb) = Just $ succ lb
    | otherwise = Nothing
  getConcreteValue (Bounds (Just (SymLTE ub)) (Just (SymGT lb)))
    | ub == succ lb = Just $ succ lb
    | otherwise = Nothing
  getConcreteValue (Bounds (Just (SymLT ub)) (Just (SymGTE lb)))
    | ub == succ lb = Just lb
    | otherwise = Nothing
  getConcreteValue (Bounds (Just (SymLTE ub)) (Just (SymGTE lb)))
    | ub == lb = Just ub
    | otherwise = Nothing
  getConcreteValue _ = Nothing

  valueIsMember (Bounds (Just (SymLT ub)) (Just (SymGT lb))) v = ub > v && v > lb
  valueIsMember (Bounds (Just (SymLTE ub)) (Just (SymGT lb))) v = ub >= v && v > lb
  valueIsMember (Bounds (Just (SymLT ub)) (Just (SymGTE lb))) v = ub > v && v >= lb
  valueIsMember (Bounds (Just (SymLTE ub)) (Just (SymGTE lb))) v = ub >= v && v >= lb

  valueIsMember (Bounds (Just (SymLT ub)) Nothing) v = ub > v
  valueIsMember (Bounds (Just (SymLTE ub)) Nothing) v = ub >= v
  valueIsMember (Bounds Nothing (Just (SymGT lb))) v = v > lb
  valueIsMember (Bounds Nothing (Just (SymGTE lb))) v = v >= lb

  valueIsMember (Bounds Nothing Nothing) _ = True

bnds0 :: Sym Bounds Integer
bnds0 = Constrained $ Bounds (Just $ SymLT 5) (Just $ SymGT 1)

bnds1 :: Sym Bounds Integer
bnds1 = Constrained $ Bounds (Just $ SymLT 10) (Just $ SymGTE 4)

bnds2 :: Sym Bounds Integer
bnds2 = Constrained $ Bounds (Just $ SymLT 3) (Just $ SymGTE (-10))

instance MembershipInfo Var Integer where
  isNonEmptySet Var{..}
    | not $ isNonEmptySet varBounds = False
    | otherwise =
      let prunedSet = Set.filter (valueIsMember varBounds) varIsNever
          lazyList = case (boundLower varBounds, boundUpper varBounds) of
            (Nothing,_) -> Nothing
            (_,Nothing) -> Nothing
            (Just (SymGT x), Just (SymLT y))
              | x <= y -> Just [(x + 1)..(y - 1)]
              | otherwise -> Nothing
            (Just (SymGTE x), Just (SymLT y))
              | x <= y -> Just [x..(y - 1)]
              | otherwise -> Nothing
            (Just (SymGT x), Just (SymLTE y))
              | x <= y -> Just [(x + 1)..y]
              | otherwise -> Nothing
            (Just (SymGTE x), Just (SymLTE y))
              | x <= y -> Just [x..y]
              | otherwise -> Nothing
      in case lazyList of
        Nothing -> False
        Just ls -> not $ any (`Set.notMember` prunedSet) ls

  getConcreteValue Var{..} =
    let boundSays = getConcreteValue varBounds
    in case boundSays of
      Nothing -> Nothing
      Just x -> if Set.notMember x varIsNever then Just x else Nothing

  valueIsMember Var{..} v
    | Set.notMember v varIsNever && valueIsMember varBounds v = True
    | otherwise = False
