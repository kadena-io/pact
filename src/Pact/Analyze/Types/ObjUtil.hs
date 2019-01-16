{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Pact.Analyze.Types.ObjUtil where

import Data.Type.Bool     (If, type (||))
import Data.Type.Equality (type (==))
import GHC.TypeLits       (Symbol, CmpSymbol, symbolVal)
import Prelude            hiding ((++))

import Pact.Analyze.Types.Types


import Unsafe.Coerce (unsafeCoerce)

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

data instance Sing (o :: Ordering) where
  SLT :: Sing 'LT
  SEQ :: Sing 'EQ
  SGT :: Sing 'GT

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

data BoolExpr
  = ETrue
  | EFalse
  | EOr BoolExpr BoolExpr
  | EOrderingEq Ordering Ordering

type family NormalizeB (b :: BoolExpr) :: Bool where
  NormalizeB 'ETrue             = 'True
  NormalizeB 'EFalse            = 'False
  NormalizeB ('EOr a b)         = NormalizeB a || NormalizeB b
  NormalizeB ('EOrderingEq a b) = a == b

type SingBoolExpr (e :: BoolExpr) = Sing e

data instance Sing (a :: BoolExpr) where
  SETrue       ::                     Sing 'ETrue
  SEFalse      ::                     Sing 'EFalse
  SEOr         :: Sing a -> Sing b -> Sing ('EOr a b)
  SEOrderingEq :: Sing a -> Sing b -> Sing ('EOrderingEq a b)
  -- SESymEq      :: SingSymbol a -> SingSymbol b -> Sing (a == b)

evalBoolExpr :: SingBoolExpr e -> Sing (NormalizeB e)
evalBoolExpr = \case
  SETrue   -> STrue
  SEFalse  -> SFalse
  SEOr a b -> case (evalBoolExpr a, evalBoolExpr b) of
    (SFalse, SFalse) -> SFalse
    (SFalse, STrue ) -> STrue
    (STrue,  SFalse) -> STrue
    (STrue,  STrue ) -> STrue
  SEOrderingEq SLT SLT -> STrue
  SEOrderingEq SLT SEQ -> SFalse
  SEOrderingEq SLT SGT -> SFalse
  SEOrderingEq SEQ SLT -> SFalse
  SEOrderingEq SEQ SEQ -> STrue
  SEOrderingEq SEQ SGT -> SFalse
  SEOrderingEq SGT SLT -> SFalse
  SEOrderingEq SGT SEQ -> SFalse
  SEOrderingEq SGT SGT -> STrue
  -- SESymEq      a   b   -> case sameSymbol a b of
  --   Just Refl -> STrue
  --   Nothing   -> SFalse

data OrderingExpr
  = ECmpSymbol Symbol Symbol

data instance Sing (o :: OrderingExpr) where
  SCmpSymbol :: SingSymbol a -> SingSymbol b -> Sing ('ECmpSymbol a b)

type family NormalizeO (a :: OrderingExpr) :: Ordering where
  NormalizeO ('ECmpSymbol a b) = CmpSymbol a b

evalOrderingExpr :: Sing o -> Sing (NormalizeO o)
evalOrderingExpr (SCmpSymbol a b) = case a of
  SSymbol -> case b of
    SSymbol -> case symbolVal a `compare` symbolVal b of
      LT -> unsafeCoerce SLT
      EQ -> unsafeCoerce SEQ
      GT -> unsafeCoerce SGT

data ListExpr
  = EIf BoolExpr ListExpr ListExpr
  | EList [ (Symbol, Ty) ]

type family NormalizeL (l :: ListExpr) :: [ (Symbol, Ty) ] where
  NormalizeL ('EIf b l r) = If (NormalizeB b) (NormalizeL l) (NormalizeL r)
  NormalizeL ('EList l)   = l

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
filterV SFMin p (SCons k x xs) = unsafeCoerce $ ite'
  (evalBoolExpr (SEOrderingEq (evalOrderingExpr (SCmpSymbol k p)) SLT))
  (SCons k x (filterV SFMin p xs))
  (filterV SFMin p xs)
filterV SFMax p (SCons k x xs) = unsafeCoerce $ ite'
  (evalBoolExpr
    -- k > p || k = p
    (SEOr
      (SEOrderingEq (evalOrderingExpr (SCmpSymbol k p)) SGT)
      (SEOrderingEq (evalOrderingExpr (SCmpSymbol k p)) SEQ)))
  (SCons k x (filterV SFMax p xs))
  (filterV SFMax p xs)

-- section: Nub

-- | Remove duplicates from a sorted list
type family Nub (t :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
  Nub '[]                           = '[]
  Nub '[e]                          = '[e]
  Nub ('(k1, v1) ': '(k2, v2) ': s) = If (k1 == k2)
    (Nub ('(k1, v2) ': s))
    ('(k1, v1) ': Nub ('(k2, v2) ': s))

ite' :: SingBool b -> l -> r -> If b l r
ite' STrue  l _ = l
ite' SFalse _ r = r

-- | Value-level counterpart to the type-level 'Nub'
nub :: HList f t -> HList f (Nub t)
nub SNil = SNil
nub (SCons k v SNil) = SCons k v SNil
nub (SCons k1 v1 (SCons k2 v2 s)) = unsafeCoerce $ ite'
  (evalBoolExpr (SEOrderingEq (evalOrderingExpr (SCmpSymbol k1 k2)) SEQ))
  (nub (SCons k2 v2 s))
  (SCons k1 v1 (nub (SCons k2 v2 s)))

-- section: Sort

-- | Type-level quick sort for normalising the representation of sets
type family Sort (xs :: [ (Symbol, Ty) ]) :: [ (Symbol, Ty) ] where
  Sort '[] = '[]
  Sort ('(k, x) ': xs)
    = Sort (Filter 'FMin k xs) :++ '[ '(k, x) ] :++ Sort (Filter 'FMax k xs)

-- | Value-level quick sort that respects the type-level ordering
quicksort :: HList f t -> HList f (Sort t)
quicksort SNil = SNil
quicksort (SCons k p xs) =
  quicksort (less k xs) ++ SCons k p SNil ++ quicksort (more k xs)
  where less = filterV (sing @'FMin)
        more = filterV (sing @'FMax)

-- section: Normalization

type Normalize t = Nub (Sort t)

type IsNormalized t = t ~ Nub (Sort t)

normalize :: HList f t -> HList f (Normalize t)
normalize = nub . quicksort

-- section: Union

-- type Unionable s t = (Sortable (s :++ t), Nubable (Sort (s :++ t)))

type Union s t = Nub (Sort (s :++ t))

union -- :: Unionable s t =>
  :: HList f s -> HList f t -> HList f (Union s t)
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
