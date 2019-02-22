{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type definitions and utilities for for building 'HList's and object schema
-- singletons.
module Pact.Analyze.Types.ObjUtil
  (
  -- * Normalization
    Normalize
  , IsNormalized
  , normalize

  -- * Union
  , Union
  , union

  -- * Insertion
  , type Insert
  , insert

  -- * Utilities
  , mkSObject
  , normalizeSchema
  ) where

import           Data.Type.Bool           (If)
import           Data.Type.Equality       (type (==))
import           Data.Typeable            (Typeable)
import           GHC.TypeLits             (CmpSymbol, KnownSymbol, Symbol,
                                           symbolVal)
import           Unsafe.Coerce            (unsafeCoerce)

import           Pact.Analyze.Types.Types

-- | Only to be used in this module, for when it's too arduous to convince GHC
-- that two types are equivalent
sansProof :: a -> b
sansProof = unsafeCoerce

-- inspiration:
-- https://github.com/dorchard/type-level-sets/blob/master/src/Data/Type/Set.hs

compareKeys :: SingSymbol a -> SingSymbol b -> Ordering
compareKeys a b = case a of
  SSymbol -> case b of
    SSymbol -> symbolVal a `compare` symbolVal b

-- section: Append

-- | Type-level list append
type family (:++) (x :: [ k ]) (y :: [ k ]) :: [ k ] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

infixr 5 :++

-- | Singleton list append
(+++) :: HList f s -> HList f t -> HList f (s :++ t)
SNil         +++ x  = x
SCons k v xs +++ ys = SCons k v (xs +++ ys)

infixr 5 +++

-- section: Filter

-- | Remove keys >= vs < the pivot
--
--   [@SRemoveGE@] Remove keys >= the pivot
--   [@SRemoveLT@] Remove keys <  the pivot
data Flag = RemoveGE | RemoveLT

data instance Sing (f :: Flag) where
  SRemoveGE :: Sing 'RemoveGE
  SRemoveLT :: Sing 'RemoveLT

-- | Type-level filter elements less-than or greater-than-or-equal to the pivot
type family Filter (f :: Flag) (p :: Symbol) (xs :: [ (Symbol, Ty) ])
  :: [ (Symbol, Ty) ] where
  Filter f p '[] = '[]
  Filter 'RemoveGE p ('(k, x) ': xs)
    = If (CmpSymbol k p == 'LT)
         ('(k, x) ': Filter 'RemoveGE p xs)
         (Filter 'RemoveGE p xs)
  Filter 'RemoveLT p ('(k, x) ': xs)
    = If (CmpSymbol k p == 'LT)
         (Filter 'RemoveLT p xs)
         ('(k, x) ': Filter 'RemoveLT p xs)

-- | Filter out keys above or below the pivot
filterV
  :: forall
     (flag :: Flag)
     (p :: Symbol)
     (xs :: [ (Symbol, Ty) ])
     (f :: Ty -> *).
     Sing flag
  -> Sing p
  -> HList f xs
  -> HList f (Filter flag p xs)
filterV _ _ SNil = SNil
filterV flag p (SCons k x xs) = case compareKeys k p of
  LT -> case flag of
    SRemoveGE -> sansProof $ SCons k x (filterV flag p xs)
    SRemoveLT -> sansProof $ filterV flag p xs
  _  -> case flag of
    SRemoveGE -> sansProof $ filterV flag p xs
    SRemoveLT -> sansProof $ SCons k x (filterV flag p xs)

-- section: Nub

-- | Remove duplicates from a sorted list
type family Nub (t :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
  Nub '[]                           = '[]
  Nub '[e]                          = '[e]
  Nub ('(k1, v1) ': '(k2, v2) ': s) = If (k1 == k2)
                 (Nub ('(k1, v2) ': s))
    ('(k1, v1) ': Nub ('(k2, v2) ': s))

-- | Value-level counterpart to the type-level 'Nub'
nub :: HList f t -> HList f (Nub t)
nub SNil = SNil
nub (SCons k v SNil) = SCons k v SNil
nub (SCons k1 v1 (SCons k2 v2 s)) = case compareKeys k1 k2 of
  EQ -> sansProof $               nub $ SCons k2 v2 s
  _  -> sansProof $ SCons k1 v1 $ nub $ SCons k2 v2 s

-- section: Sort

-- | Type-level quick sort for normalising the representation of sets
type family Sort (xs :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
  Sort '[] = '[]
  Sort ('(k, x) ': xs)
    =   Sort (Filter 'RemoveGE k xs)
    :++ '[ '(k, x) ]
    :++ Sort (Filter 'RemoveLT k xs)

-- | Value-level quick sort that respects the type-level ordering
quicksort :: HList f t -> HList f (Sort t)
quicksort SNil = SNil
quicksort (SCons k p xs)
  =   quicksort (filterV SRemoveGE k xs)
  +++ SCons k p SNil
  +++ quicksort (filterV SRemoveLT k xs)

-- section: Normalization

type Normalize t = Nub (Sort t)

type IsNormalized t = t ~ Nub (Sort t)

normalize :: HList f t -> HList f (Normalize t)
normalize = nub . quicksort

-- section: Union

type Union s t = Nub (Sort (s :++ t))

union :: HList f s -> HList f t -> HList f (Union s t)
union s t = nub $ quicksort $ s +++ t

-- section: Insert

type family Insert (v :: (Symbol, Ty)) (vs :: [ (Symbol, Ty) ])
  :: [ (Symbol, Ty) ] where
  Insert y '[] = '[y]
  Insert '(k, v) ('(k', v') ': vs) = If
    (CmpSymbol k k' == 'LT)
    ('(k , v ) ':        '(k', v') ': vs)
    ('(k', v') ': Insert '(k , v )    vs)

insert
  :: (SingI v, Typeable v, KnownSymbol k)
  => SingSymbol k -> f v -> HList f vs -> HList f (Insert '(k, v) vs)
insert k v SNil = SCons k v SNil
insert k v (SCons k' v' kvs) = case compareKeys k k' of
  LT -> sansProof $ SCons k  v  $ SCons  k' v' kvs
  _  -> sansProof $ SCons k' v' $ insert k  v  kvs

-- section: SObject wrangling

-- | This should /always/ be used to construct an @SObject@.
mkSObject :: Sing schema -> Sing ('TyObject (Normalize schema))
mkSObject = SObjectUnsafe . eraseList . normalize . UnSingList

normalizeSchema :: SingList schema -> SingList (Normalize schema)
normalizeSchema = eraseList . normalize . UnSingList
