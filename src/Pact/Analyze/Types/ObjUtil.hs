{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Pact.Analyze.Types.ObjUtil where

import Data.Typeable      (Typeable)
import Data.Type.Bool     (If, type (||))
import Data.Type.Equality (type (==))
import GHC.TypeLits       (Symbol, CmpSymbol, KnownSymbol)
import Prelude            hiding ((++))

import Pact.Analyze.Types.Types

-- inspiration:
-- https://github.com/dorchard/type-level-sets/blob/master/src/Data/Type/Set.hs

-- section: Append

-- | Type-level list append
type family (:++) (x :: [ k ]) (y :: [ k ]) :: [ k ] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

infixr 5 :++

-- | Singleton list append
(++) :: HList f s -> HList f t -> HList f (s :++ t)
SNil           ++ x  = x
(SCons k v xs) ++ ys = SCons k v (xs ++ ys)

infixr 5 ++

-- section: Filter

data Flag = FMin | FMax

data instance Sing (f :: Flag) where
  SFMin :: Sing 'FMin
  SFMax :: Sing 'FMax

type SingFlag (f :: Flag) = Sing f

instance SingI 'FMin where
  sing = SFMin

instance SingI 'FMax where
  sing = SFMax

-- | Type-level filter elements less-than or greater-than-or-equal to the pivot
type family Filter (f :: Flag) (p :: Symbol) (xs :: [ (Symbol, Ty) ])
  :: [ (Symbol, Ty) ] where
  Filter f p '[] = '[]
  Filter 'FMin p ('(k, x) ': xs)
    = If (CmpSymbol k p == 'LT)
         ('(k, x) ': Filter 'FMin p xs)
         (Filter 'FMin p xs)
  Filter 'FMax p ('(k, x) ': xs)
    = If (CmpSymbol k p == 'GT || CmpSymbol k p == 'EQ)
         ('(k, x) ': Filter 'FMax p xs)
         (Filter 'FMax p xs)



class Conder (g :: Bool) where
  cond :: SingBool g -> HList f s -> HList f t -> HList f (If g s t)

instance Conder 'True where
  cond _ s _ = s

instance Conder 'False where
  cond _ _ t = t

-- | Filter out the elements less-than or greater-than-or-equal to the pivot
class FilterV (flag :: Flag) (p :: Symbol) (xs :: [ (Symbol, Ty) ]) where
  filterV :: Sing flag -> Sing p -> HList f xs -> HList f (Filter flag p xs)

instance FilterV flag p '[] where
  filterV _ _ SNil = SNil

instance
  ( Conder (CmpSymbol k p == 'LT)
  , SingI (CmpSymbol k p == 'LT)
  , FilterV 'FMin p xs
  , SingI x
  , Typeable x
  , KnownSymbol k
  ) => FilterV 'FMin p ('(k, x) ': xs) where
    filterV flag p (SCons k x xs)
      = cond (sing @(CmpSymbol k p == 'LT))
             (SCons k x (filterV flag p xs))
             (filterV flag p xs)

instance
  ( Conder (CmpSymbol k p == 'GT || CmpSymbol k p == 'EQ)
  , SingI ((CmpSymbol k p == 'GT) || (CmpSymbol k p == 'EQ))
  , FilterV 'FMax p xs
  , SingI x
  , Typeable x
  , KnownSymbol k
  ) => FilterV 'FMax p ('(k, x) ': xs) where
    filterV flag p (SCons k x xs)
      = cond (sing @(CmpSymbol k p == 'GT || CmpSymbol k p == 'EQ))
             (SCons k x (filterV flag p xs))
             (filterV flag p xs)

-- section: Nub

-- | Remove duplicates from a sorted list
type family Nub (t :: [ k ]) :: [ k ] where
  Nub '[]           = '[]
  Nub '[e]          = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub (e ': f ': s) = e ': Nub (f ': s)

-- | Value-level counterpart to the type-level 'Nub'
class Nubable t where
  nub :: HList f t -> HList f (Nub t)

instance Nubable '[] where
  nub SNil = SNil

instance Nubable '[e] where
  nub (SCons k v SNil) = SCons k v SNil

instance Nubable (e ': s) => Nubable (e ': e ': s) where
  nub (SCons _ _ (SCons k v s)) = nub (SCons k v s)

instance {-# OVERLAPS #-}
  ( Nub (e ': f ': s) ~ (e ': Nub (f ': s))
  , Nubable (f ': s)
  ) => Nubable (e ': f ': s) where
  nub (SCons k1 v1 (SCons k2 v2 s))
    = SCons k1 v1 (nub (SCons k2 v2 s))

-- section: Sort

-- | Type-level quick sort for normalising the representation of sets
type family Sort (xs :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
  Sort '[] = '[]
  Sort ('(k, x) ': xs)
    = Sort (Filter 'FMin k xs) :++ '[ '(k, x) ] :++ Sort (Filter 'FMax k xs)

-- | Value-level quick sort that respects the type-level ordering
class Sortable t where
  quicksort :: HList f t -> HList f (Sort t)

instance Sortable '[] where
  quicksort SNil = SNil

instance
  ( Sortable (Filter 'FMin p xs)
  , Sortable (Filter 'FMax p xs)
  , FilterV 'FMin p xs
  , FilterV 'FMax p xs
  , SingI x
  , Typeable x
  , KnownSymbol p
  ) => Sortable ('(p, x) ': xs) where
  quicksort (SCons k p xs) =
    quicksort (less k xs) ++ SCons k p SNil ++ quicksort (more k xs)
    where less = filterV (sing @'FMin)
          more = filterV (sing @'FMax)

-- section: Normalization

type Normalize t = Nub (Sort t)

type IsNormalized t = t ~ Nub (Sort t)

normalize
  :: (Sortable t, Nubable (Sort t))
  => HList f t -> HList f (Normalize t)
normalize = nub . quicksort

-- section: Union

type Unionable s t = (Sortable (s :++ t), Nubable (Sort (s :++ t)))

type Union s t = Nub (Sort (s :++ t))

union :: Unionable s t => HList f s -> HList f t -> HList f (Union s t)
union s t = nub (quicksort (s ++ t))

-- section: Insert

-- type family Insert (x :: (Symbol, Ty)) (xs :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
--   Insert y '[] = '[y]
--   Insert '(ky, vy) ('(kx, vx) ': xs) = If
--     (CmpSymbol ky kx == 'LT)
--     ('(ky, vy) ': '(kx, vx) ': xs)
--     ('(kx, vx) ': Insert '(ky, vy) xs)

-- class InsertV (k :: Symbol) (v :: Ty) (xs :: [ (Symbol, Ty) ]) where
--   insert :: Sing k -> Sing v -> SingList xs -> SingList ('(k, v) ': xs)

-- instance (SingI v, Typeable v, KnownSymbol k) => InsertV k v '[] where
--   insert k v _ = SingList (SCons k v SNil)

-- instance
--   ( SingI v
--   , Typeable v
--   , KnownSymbol k
--   , Conder (CmpSymbol k k' == 'LT)
--   , InsertV k v kvs
--   ) => InsertV k v ('(k', v') ': kvs) where
--   insert k v (SingList (SCons k' v' kvs))
--     = cond (Proxy @(CmpSymbol k k' == 'LT))
--            (SingList (SCons k v (SCons k' v' kvs)))
--            (SingList (SCons k' v' (UnSingList (insert k v (SingList kvs)))))
