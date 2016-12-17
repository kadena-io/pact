{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SymExec where

import Algebra.Lattice
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

data Sym c a =
  -- This is an error case, say `Concrete 2 \/ Concrete 3`
  Top { symWhy :: [String] }|
  -- When something has become so constrained that it can take only a single value
  Concrete {fixedVal :: a} |
  -- Something between top and bottom
  Constrained { constrained :: c a } |
  -- The Symbolic a has been fixed to some value
  Bottom
  deriving (Show, Eq)

data LBound a = SymGTE a | SymGT a
  deriving (Show, Eq)

instance (Ord a) => JoinSemiLattice (LBound a) where
  (\/) (SymGT x) (SymGT y)
    | x > y = SymGT x
    | otherwise = SymGT y
  (\/) (SymGTE x) (SymGTE y)
    | x > y = SymGTE x
    | otherwise = SymGTE y
  (\/) (SymGTE x) (SymGT y)
    | x > y = SymGTE x
    | otherwise = SymGT y
  (\/) x@(SymGT _) y@(SymGTE _) = (\/) y x

data UBound a = SymLT a | SymLTE a
  deriving (Show, Eq)

instance (Ord a) => JoinSemiLattice (UBound a) where
  (\/) (SymLT x) (SymLT y)
    | x < y = SymLT x
    | otherwise = SymLT y
  (\/) (SymLTE x) (SymLTE y)
    | x < y = SymLTE x
    | otherwise = SymLTE y
  (\/) (SymLTE x) (SymLT y)
    | x < y = SymLTE x
    | otherwise = SymLT y
  (\/) x@(SymLT _) y@(SymLTE _) = (\/) y x

data Bounds a = Bounds
  { boundUpper :: Maybe (UBound a)
  , boundLower :: Maybe (LBound a)
  } deriving (Show, Eq)

instance (Ord a) => JoinSemiLattice (Bounds a) where
  (\/) (Bounds u0 l0) (Bounds u1 l1) = Bounds (joinBounds u0 u1) (joinBounds l0 l1)
    where
      joinBounds Nothing x = x
      joinBounds x Nothing = x
      joinBounds (Just x) (Just y) = Just (x \/ y)

class MembershipInfo c a where
  -- is the set defined by the bounds non-zero
  isNonEmptySet :: c a -> Bool
  -- if the set defined by the bounds is a singleton set, get the value
  getConcreteValue :: c a -> Maybe a
  -- check if a value could be a member of Bounds' set
  valueIsMember :: c a -> a -> Bool

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

instance (Show a, Ord a, MembershipInfo Bounds a) => JoinSemiLattice (Sym Bounds a) where
  (\/) (Top x) (Top y) = Top (x++y)
  (\/) (Top x) _       = Top x
  (\/) _       (Top y) = Top y

  (\/) (Constrained x) (Concrete y)
    | valueIsMember x y = Concrete y
    | otherwise = Top ["Concrete Value '" ++ show y ++ "' does not fall within the bounds: " ++ show x]
  (\/) x@(Concrete _) y@(Constrained _) = y \/ x
  (\/) (Concrete x) (Concrete y)
    | x == y = Concrete x
    | otherwise = Top ["Conflicted Concrete Values: " ++ show x ++ " != " ++ show y]

  (\/) (Constrained x) (Constrained y) = let
    newBs = x \/ y
    in if isNonEmptySet newBs
       then case getConcreteValue newBs of
              Nothing -> Constrained newBs
              Just fixed' -> Concrete fixed'
       else Top ["Bounds form an uninhabited set: " ++ show newBs]

  (\/) Bottom y = y
  (\/) x Bottom = x

bnds0 :: Sym Bounds Integer
bnds0 = Constrained $ Bounds (Just $ SymLT 5) (Just $ SymGT 1)

bnds1 :: Sym Bounds Integer
bnds1 = Constrained $ Bounds (Just $ SymLT 10) (Just $ SymGTE 4)

bnds2 :: Sym Bounds Integer
bnds2 = Constrained $ Bounds (Just $ SymLT 3) (Just $ SymGTE (-10))

data Relationship = Relationship deriving (Show, Eq)

data Var a = Var
  { varBounds :: Bounds a
  , varIsNever :: Set a
  } deriving (Show, Eq)

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

instance (Ord a, Show a, MembershipInfo Var a, MembershipInfo Bounds a) => JoinSemiLattice (Sym Var a) where
  (\/) (Top x) (Top y) = Top (x++y)
  (\/) (Top x) _       = Top x
  (\/) _       (Top y) = Top y

  (\/) (Constrained Var{..}) (Concrete y)
    | Set.member y varIsNever = Top ["Concrete Value '" ++ show y ++ "' is a member of a set that it can never be from " ++ show varIsNever]
    | not $ valueIsMember varBounds y = Top ["Concrete Value '" ++ show y ++ "' does not fall within the bounds: " ++ show varBounds]
    | otherwise = Concrete y
  (\/) x@(Concrete _) y@(Constrained _) = y \/ x
  (\/) (Concrete x) (Concrete y)
    | x == y = Concrete x
    | otherwise = Top ["Conflicted Concrete Values: " ++ show x ++ " != " ++ show y]

  (\/) (Constrained x) (Constrained y) = let
    newBs = varBounds x \/ varBounds y
    newIsNever = Set.filter (valueIsMember newBs) $ Set.union (varIsNever x) (varIsNever y)
    newVar = Var newBs newIsNever
    in if isNonEmptySet newVar
       then case getConcreteValue newVar of
              Nothing -> Constrained newVar
              Just fixed' -> Concrete fixed'
       else Top ["Bounds form an uninhabited set: " ++ show newBs]



  (\/) Bottom y = y
  (\/) x Bottom = x
