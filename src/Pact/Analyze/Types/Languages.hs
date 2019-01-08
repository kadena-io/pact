{-# LANGUAGE UndecidableInstances  #-} -- UserShow (Core tm a)
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Pact.Analyze.Types.Languages
  ( (:<:)(inject, project)
  , (:*<:)(inject', project')
  , EInvariant
  , EProp
  , ETerm
  , Core(..)
  , Invariant(..)
  , Prop(..)
  , PropSpecific(..)
  , Term(..)
  , BeforeOrAfter(..)
  , Open(..)

  , toPact
  , fromPact
  , valueToProp

  , pattern IntegerComparison
  , pattern DecimalComparison
  , pattern TimeComparison
  , pattern StrComparison
  , pattern BoolComparison
  , pattern KeySetComparison
  , pattern ILiteral
  , pattern ILogicalOp
  , pattern Inj
  , pattern PAnd
  , pattern PObjAt
  , pattern PDecAddTime
  , pattern PIntAddTime
  , pattern Lit'
  , pattern StrLit
  , pattern TextLit
  , pattern PLogical
  , pattern PNot
  , pattern PNumerical
  , pattern POr
  , pattern PStrConcat
  , pattern PStrLength
  , pattern PVar

  , mkLiteralList

  , singEqTm
  , singEqListTm
  , singShowsTm
  , singUserShowTm
  , singUserShowListTm
  ) where

import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable                ((:~:) (Refl))
import           Prelude                      hiding (Float)
import           Text.Show                    (showListWith)

import           Pact.Types.Persistence       (WriteType)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Sym, Var, col, str, obj, dec, ks)
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types
import           Pact.Analyze.Types.UserShow
import           Pact.Analyze.Util

-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class (sub :: Ty -> *) :<: (sup :: Ty -> *) where
  inject  :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

instance f :<: f where
  inject  = id
  project = Just

pattern Inj :: sub :<: sup => sub a -> sup a
pattern Inj a <- (project -> Just a) where
  Inj a = inject a

-- | Modified subtyping relation
--
-- This can be read as "subtype-with-an-asterisk". This is for relations where
-- the type on the left is indexed by a *concrete* type. For example, @S
-- Integer@ is injectable into @Term TyInteger@ because @Integer ~ Concrete
-- TyInteger@.
class (sub :: * -> *) :*<: (sup :: Ty -> *) where
  inject'  :: sub (Concrete a) -> sup a
  project' :: sup a            -> Maybe (sub (Concrete a))

-- | An open term (: 'b') with a free variable (: 'a').
data Open (a :: Ty) (tm :: Ty -> *) (b :: Ty) = Open !VarId !Text !(tm b)
  deriving (Eq, Show)

-- | Core terms.
--
-- These are the expressions shared by all three languages ('Prop',
-- 'Invariant', and 'Term'). Another way of thinking about this type is the
-- pure subset of any of then languages. This happens to coincide with all of
-- the invariant language, but properties and terms have more constructions.
--
-- This consists of:
--
-- * comparisons
--   - { <, >, <=, >= } apply to { integer, decimal, string, time }
--   - { =, != } apply to { integer, decimal, string, time, bool, keyset }
-- * literals
-- * variables
-- * logical operations
-- * string length and concatenation
-- * 'add-time'
-- * 'at'
-- * lit operations
data Core (t :: Ty -> *) (a :: Ty) where
  Lit :: Concrete a -> Core t a
  -- | Injects a symbolic value into the language
  Sym :: S (Concrete a) -> Core t a

  -- | Refers to a function argument, universally/existentially-quantified
  -- variable, or column
  Var :: VarId -> Text -> Core t a

  Identity     :: SingTy a   -> t a        -> Core t a
  Constantly   :: SingTy b   -> t a -> t b -> Core t a

  -- compose
  -- - f :: a -> b (free a)
  -- - g :: b -> c (free b)
  -- :: c
  Compose
    :: SingTy a -> SingTy   b -> SingTy   c
    ->      t a -> Open a t b -> Open b t c -> Core t c

  -- string ops
  -- | The concatenation of two 'String' expressions
  StrConcat    :: t 'TyStr     -> t 'TyStr -> Core t 'TyStr
  -- | The length of a 'String' expression
  StrLength    :: t 'TyStr     ->             Core t 'TyInteger
  -- | Conversion of a base-10 string to an integer
  StrToInt     :: t 'TyStr     ->             Core t 'TyInteger
  -- | Conversion of a base-2-16 string to an integer
  StrToIntBase :: t 'TyInteger -> t 'TyStr -> Core t 'TyInteger
  StrContains  :: t 'TyStr     -> t 'TyStr -> Core t 'TyBool

  -- numeric ops
  Numerical    :: Numerical t a -> Core t a

  -- Time
  -- | Adds an 'Integer' expression to a 'Time' expression
  IntAddTime :: t 'TyTime -> t 'TyInteger -> Core t 'TyTime
  -- | Adds a 'Decimal' expression to a 'Time' expression
  DecAddTime :: t 'TyTime -> t 'TyDecimal -> Core t 'TyTime

  -- comparison. Note that while it's cumbersome to define five different
  -- monomorphized comparisons, the alternative is implementing Eq by hand
  -- here.

  -- | A 'ComparisonOp' expression over two expressions
  --
  -- Note: for bool and keyset this is a wider comparison than pact supports
  Comparison :: SingTy a -> ComparisonOp -> t a -> t a -> Core t 'TyBool

  -- object ops
  ObjectEqNeq
    :: SingTy ('TyObject m1) -> SingTy ('TyObject m2)
    -> EqNeq -> t ('TyObject m1) -> t ('TyObject m2) -> Core t 'TyBool
  ObjAt       :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t a
  ObjContains :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t 'TyBool
  ObjDrop     :: t ('TyList 'TyStr) -> t ('TyObject m) -> Core t ('TyObject m)
  ObjTake     :: t ('TyList 'TyStr) -> t ('TyObject m) -> Core t ('TyObject m)
  ObjMerge    :: SingTy o1 -> SingTy o2 -> t o1 -> t o2 -> Core t o

  -- TODO(joel): combine with `Lit`?
  LiteralObject :: SingTy ('TyObject m) -> Object t m -> Core t ('TyObject m)

  -- boolean ops
  -- | A 'Logical' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.
  Logical :: LogicalOp -> [t 'TyBool] -> Core t 'TyBool

  -- list ops. Each of these operations contains a singleton of the type of
  -- list elements (needed so we can implement `Eq`, `Show`, etc).

  ListEqNeq    :: SingTy a -> EqNeq -> t ('TyList a) -> t ('TyList a) -> Core t 'TyBool
  ListAt       :: SingTy a -> t 'TyInteger -> t ('TyList a) -> Core t a
  ListContains :: SingTy a -> t a      -> t ('TyList a) -> Core t 'TyBool

  ListLength   :: SingTy a -> t ('TyList a) -> Core t 'TyInteger

  ListReverse  :: SingTy a -> t ('TyList a) -> Core t ('TyList a)
  ListSort     :: SingTy a -> t ('TyList a) -> Core t ('TyList a)

  ListConcat   :: SingTy a -> t ('TyList a) -> t ('TyList a) -> Core t ('TyList a)

  ListDrop     :: SingTy a -> t 'TyInteger -> t ('TyList a) -> Core t ('TyList a)
  ListTake     :: SingTy a -> t 'TyInteger -> t ('TyList a) -> Core t ('TyList a)

  MakeList     :: SingTy a -> t 'TyInteger -> t a -> Core t ('TyList a)

  LiteralList  :: SingTy a -> [t a] -> Core t ('TyList a)

  ListMap
    :: SingTy a -> SingTy b
    -> Open a t b
    -> t ('TyList a)
    -> Core t ('TyList b)

  ListFilter
    :: SingTy a
    -> Open a t 'TyBool -> t ('TyList a) -> Core t ('TyList a)

  ListFold
    :: SingTy a -> SingTy b
    -> Open a (Open b t) a -> t a -> t ('TyList b) -> Core t a

  AndQ
    :: SingTy a
    -> Open a t 'TyBool -> Open a t 'TyBool -> t a -> Core t 'TyBool
  OrQ
    :: SingTy a
    -> Open a t 'TyBool -> Open a t 'TyBool -> t a -> Core t 'TyBool

  Where
    :: SingTy ('TyObject m) -> SingTy a
    -> t 'TyStr -> Open a t 'TyBool -> t ('TyObject m) -> Core t 'TyBool

  Typeof :: SingTy a -> t a -> Core t 'TyStr

pattern IntegerComparison
  :: ComparisonOp -> t 'TyInteger -> t 'TyInteger -> Core t 'TyBool
pattern IntegerComparison op a b = Comparison SInteger op a b

pattern DecimalComparison
  :: ComparisonOp -> t 'TyDecimal -> t 'TyDecimal -> Core t 'TyBool
pattern DecimalComparison op a b = Comparison SDecimal op a b

pattern TimeComparison
  :: ComparisonOp -> t 'TyTime -> t 'TyTime -> Core t 'TyBool
pattern TimeComparison op a b = Comparison STime op a b

pattern StrComparison
  :: ComparisonOp -> t 'TyStr -> t 'TyStr -> Core t 'TyBool
pattern StrComparison op a b = Comparison SStr op a b

pattern BoolComparison
  :: ComparisonOp -> t 'TyBool -> t 'TyBool -> Core t 'TyBool
pattern BoolComparison op a b = Comparison SBool op a b

pattern KeySetComparison
  :: ComparisonOp -> t 'TyKeySet -> t 'TyKeySet -> Core t 'TyBool
pattern KeySetComparison op a b = Comparison SKeySet op a b

-- Note [Sing Functions]:
--
-- The `sing*` family of 9 (+3) functions differs in two dimensions:
-- * The class required is `Eq`, `UserShow`, or `Show`
-- * It is applied at either `tm ('TyList a)`, `[tm a]`, or `tm a`
--
-- It looks like this should be generalizable using something like `withEq`,
-- etc. I've attempted something like this a few times but have failed every
-- time. For now I'm content to write this boilerplate by hand.

singEqTmList
  :: IsTerm tm => SingTy a -> tm ('TyList a) -> tm ('TyList a) -> Bool
singEqTmList ty t1 t2 = singEqTm' (SList ty) t1 t2

singEqListTm :: IsTerm tm => SingTy a -> [tm a] -> [tm a] -> Bool
singEqListTm ty t1 t2 = and $ zipWith (singEqTm' ty) t1 t2

singEqTm :: IsTerm tm => SingTy a -> tm a -> tm a -> Bool
singEqTm = singEqTm'

singEqOpen :: IsTerm tm => SingTy a -> Open x tm a -> Open x tm a -> Bool
singEqOpen ty (Open v1 nm1 a1) (Open v2 nm2 a2)
  = singEqTm' ty a1 a2 && v1 == v2 && nm1 == nm2

singUserShowTmList :: IsTerm tm => SingTy a -> tm ('TyList a) -> Text
singUserShowTmList ty tm = singUserShowTm' (SList ty) tm

singUserShowListTm :: IsTerm tm => SingTy a -> [tm a] -> Text
singUserShowListTm ty tms =
  "[" <> Text.intercalate ", " (singUserShowTm ty <$> tms) <> "]"

singUserShowTm :: IsTerm tm => SingTy a -> tm a -> Text
singUserShowTm = singUserShowTm'

singUserShowOpen :: IsTerm tm => SingTy a -> Open x tm a -> Text
singUserShowOpen ty (Open _ nm a)
  = parenList [ "lambda", nm, singUserShowTm' ty a ]

singShowsTmList :: IsTerm tm => SingTy a -> Int -> tm ('TyList a) -> ShowS
singShowsTmList ty = singShowsTm' (SList ty)

singShowsListTm :: IsTerm tm => SingTy a -> Int -> [tm a] -> ShowS
singShowsListTm ty _ = showListWith (singShowsTm ty 0)

singShowsTm :: IsTerm tm => SingTy a -> Int -> tm a -> ShowS
singShowsTm = singShowsTm'

singShowsOpen :: IsTerm tm => SingTy a -> Open x tm a -> ShowS
singShowsOpen ty (Open v nm a) = showParen True $
    showsPrec 11 v
  . showString " "
  . showsPrec 11 nm
  . showString " "
  . singShowsTm' ty 11 a

eqNumerical :: IsTerm tm => SingTy a -> Numerical tm a -> Numerical tm a -> Bool
eqNumerical _ty (DecArithOp op1 a1 b1) (DecArithOp op2 a2 b2)
  = op1 == op2 && eqTm a1 a2 && eqTm b1 b2
eqNumerical _ty (IntArithOp op1 a1 b1) (IntArithOp op2 a2 b2)
  = op1 == op2 && eqTm a1 a2 && eqTm b1 b2
eqNumerical _ty (DecUnaryArithOp op1 a1) (DecUnaryArithOp op2 a2)
  = op1 == op2 && eqTm a1 a2
eqNumerical _ty (IntUnaryArithOp op1 a1) (IntUnaryArithOp op2 a2)
  = op1 == op2 && eqTm a1 a2
eqNumerical _ty (DecIntArithOp op1 a1 b1) (DecIntArithOp op2 a2 b2)
  = op1 == op2 && eqTm a1 a2 && eqTm b1 b2
eqNumerical _ty (IntDecArithOp op1 a1 b1) (IntDecArithOp op2 a2 b2)
  = op1 == op2 && eqTm a1 a2 && eqTm b1 b2
eqNumerical _ty (ModOp a1 b1) (ModOp a2 b2)
  = eqTm a1 a2 && eqTm b1 b2
eqNumerical _ty (RoundingLikeOp1 op1 a1) (RoundingLikeOp1 op2 a2)
  = op1 == op2 && eqTm a1 a2
eqNumerical _ty (RoundingLikeOp2 op1 a1 b1) (RoundingLikeOp2 op2 a2 b2)
  = op1 == op2 && eqTm a1 a2 && eqTm b1 b2
eqNumerical _ _ _ = False

showsNumerical :: IsTerm tm => SingTy a -> Int -> Numerical tm a -> ShowS
showsNumerical _ty p tm = showParen (p > 10) $ case tm of
  DecArithOp op a b ->
      showString "DecArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b
  IntArithOp op a b ->
      showString "IntArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b
  DecUnaryArithOp op a ->
      showString "DecUnaryArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
  IntUnaryArithOp op a ->
      showString "IntUnaryArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
  DecIntArithOp op a b ->
      showString "DecIntArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b
  IntDecArithOp op a b ->
      showString "IntDecArithOp "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b
  ModOp a b ->
      showString "ModOp "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b
  RoundingLikeOp1 op a ->
      showString "RoundingLikeOp1 "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
  RoundingLikeOp2 op a b ->
      showString "RoundingLikeOp2 "
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b

userShowNumerical :: IsTerm tm => SingTy a -> Numerical tm a -> Text
userShowNumerical _ty = \case
  DecArithOp op a b      -> parenList [userShow op, userShowTm a, userShowTm b]
  IntArithOp op a b      -> parenList [userShow op, userShowTm a, userShowTm b]
  DecUnaryArithOp op a   -> parenList [userShow op, userShowTm a]
  IntUnaryArithOp op a   -> parenList [userShow op, userShowTm a]
  DecIntArithOp op a b   -> parenList [userShow op, userShowTm a, userShowTm b]
  IntDecArithOp op a b   -> parenList [userShow op, userShowTm a, userShowTm b]
  ModOp a b              -> parenList [userShowTm a, userShowTm b]
  RoundingLikeOp1 op a   -> parenList [userShow op, userShowTm a]
  RoundingLikeOp2 op a b -> parenList [userShow op, userShowTm a, userShowTm b]

eqCoreTm :: IsTerm tm => SingTy ty -> Core tm ty -> Core tm ty -> Bool
eqCoreTm ty (Lit a)                      (Lit b)
  = withEq ty $ a == b
eqCoreTm _ (Sym a)                       (Sym b)
  = a == b
eqCoreTm _ (Var a1 b1)                   (Var a2 b2)
  = a1 == a2 && b1 == b2
eqCoreTm _ (Identity ty1 a1)             (Identity _ty2 a2)
  = singEqTm ty1 a1 a2
eqCoreTm ty (Constantly tyb1 a1 b1)      (Constantly tyb2 a2 b2)
  = case singEq tyb1 tyb2 of
    Nothing   -> False
    Just Refl -> singEqTm ty a1 a2 && singEqTm tyb1 b1 b2
eqCoreTm _ (Compose tya1 tyb1 tyc1 a1 b1 c1) (Compose tya2 tyb2 tyc2 a2 b2 c2)
  = fromMaybe False $ do
    Refl <- singEq tya1 tya2
    Refl <- singEq tyb1 tyb2
    Refl <- singEq tyc1 tyc2
    pure $ singEqTm tya1 a1 a2 && singEqOpen tyb1 b1 b2 && singEqOpen tyc1 c1 c2

eqCoreTm _ (StrConcat a1 b1)             (StrConcat a2 b2)
  = eqTm a1 a2 && eqTm b1 b2
eqCoreTm _ (StrLength a)                 (StrLength b)
  = eqTm a b
eqCoreTm _ (StrToInt s1)                 (StrToInt s2)
  = eqTm s1 s2
eqCoreTm _ (StrToIntBase b1 s1)          (StrToIntBase b2 s2)
  = eqTm b1 b2 && eqTm s1 s2
eqCoreTm _ (StrContains a1 b1)           (StrContains a2 b2)
  = eqTm a1 a2 && eqTm b1 b2
eqCoreTm ty (Numerical a)                (Numerical b)
  = eqNumerical ty a b
eqCoreTm _ (IntAddTime a1 b1)            (IntAddTime a2 b2)
  = eqTm a1 a2 && eqTm b1 b2
eqCoreTm _ (DecAddTime a1 b1)            (DecAddTime a2 b2)
  = eqTm a1 a2 && eqTm b1 b2
eqCoreTm _ (Comparison ty1 op1 a1 b1) (Comparison ty2 op2 a2 b2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> op1 == op2 && singEqTm ty1 a1 a2 && singEqTm ty1 b1 b2
eqCoreTm _ (ObjectEqNeq ty11 ty21 op1 a1 b1) (ObjectEqNeq ty12 ty22 op2 a2 b2)
  = fromMaybe False $ do
    Refl <- singEq ty11 ty12
    Refl <- singEq ty21 ty22
    pure $ op1 == op2 && singEqTm ty11 a1 a2 && singEqTm ty21 b1 b2
eqCoreTm _ (ObjAt ty1 a1 b1)             (ObjAt ty2 a2 b2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> eqTm a1 a2 && singEqTm ty1 b1 b2
eqCoreTm _ (ObjContains ty1 a1 b1)       (ObjContains ty2 a2 b2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> eqTm a1 a2 && singEqTm ty1 b1 b2
eqCoreTm ty (ObjDrop b1 c1)            (ObjDrop b2 c2)
  = eqTm b1 b2 && singEqTm ty c1 c2
eqCoreTm ty (ObjTake b1 c1)            (ObjTake b2 c2)
  = eqTm b1 b2 && singEqTm ty c1 c2
eqCoreTm _ (ObjMerge ty11 ty21 a1 b1)          (ObjMerge ty12 ty22 a2 b2)
  = fromMaybe False $ do
    Refl <- singEq ty11 ty12
    Refl <- singEq ty21 ty22
    pure $ singEqTm ty11 a1 a2 && singEqTm ty21 b1 b2
eqCoreTm _ (LiteralObject _ _m1)            (LiteralObject _ _m2)
  = error "TODO" -- m1 == m2
eqCoreTm _ (Logical op1 args1)           (Logical op2 args2)
  = op1 == op2 && and (zipWith eqTm args1 args2)

eqCoreTm _ (ListEqNeq ty1 op1 a1 b1)     (ListEqNeq ty2 op2 a2 b2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> op1 == op2 && singEqTmList ty1 a1 a2 && singEqTmList ty1 b1 b2
eqCoreTm _ (ListAt ty1 a1 b1)            (ListAt _ty2 a2 b2)
  = eqTm a1 a2 && singEqTmList ty1 b1 b2
eqCoreTm _ (ListContains ty1 a1 b1)      (ListContains ty2 a2 b2)
  = case singEq ty1 ty2 of
    Just Refl -> singEqTm ty1 a1 a2 && singEqTmList ty1 b1 b2
    Nothing   -> False
eqCoreTm _ (ListLength ty1 a1)           (ListLength ty2 a2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> singEqTmList ty1 a1 a2
eqCoreTm _ (ListReverse ty1 a1)          (ListReverse _ty2 a2)
  = singEqTmList ty1 a1 a2
eqCoreTm _ (ListSort ty1 a1)             (ListSort _ty2 a2)
  = singEqTmList ty1 a1 a2
eqCoreTm _ (ListConcat ty1 a1 b1)        (ListConcat _ty2 a2 b2)
  = singEqTmList ty1 a1 a2 && singEqTmList ty1 b1 b2
eqCoreTm _ (ListDrop ty1 i1 l1)          (ListDrop _ty2 i2 l2)
  = eqTm i1 i2 && singEqTmList ty1 l1 l2
eqCoreTm _ (ListTake ty1 i1 l1)          (ListTake _ty2 i2 l2)
  = eqTm i1 i2 && singEqTmList ty1 l1 l2
eqCoreTm _ (MakeList ty1 a1 b1)          (MakeList _ty2 a2 b2)
  = eqTm a1 a2 && singEqTm ty1 b1 b2
eqCoreTm _ (LiteralList ty1 l1)          (LiteralList _ty2 l2)
  = singEqListTm ty1 l1 l2
eqCoreTm _ (ListMap tya1 tyb1 f1 as1)    (ListMap tya2 _ f2 as2)
  = case singEq tya1 tya2 of
      Nothing   -> False
      Just Refl -> singEqOpen tyb1 f1 f2 && singEqTmList tya1 as1 as2
eqCoreTm _ (ListFilter ty1 f1 b1) (ListFilter _ty2 f2 b2)
  = singEqOpen SBool f1 f2 && singEqTmList ty1 b1 b2
eqCoreTm _ (ListFold tya1 tyb1 (Open v1 nm1 f1) b1 c1)
    (ListFold _tya2 tyb2 (Open v2 nm2 f2) b2 c2)
  = case singEq tyb1 tyb2 of
        Nothing   -> False
        Just Refl -> v1 == v2 && nm1 == nm2 && singEqOpen tya1 f1 f2
          && singEqTm tya1 b1 b2 && singEqTmList tyb1 c1 c2
eqCoreTm _ (AndQ tya1 f1 g1 a1) (AndQ tya2 f2 g2 a2)
  = case singEq tya1 tya2 of
      Nothing   -> False
      Just Refl -> singEqOpen SBool f1 f2 && singEqOpen SBool g1 g2
        && singEqTm tya1 a1 a2
eqCoreTm _ (OrQ tya1 f1 g1 a1) (OrQ tya2 f2 g2 a2)
  = case singEq tya1 tya2 of
      Nothing   -> False
      Just Refl -> singEqOpen SBool f1 f2 && singEqOpen SBool g1 g2
        && singEqTm tya1 a1 a2
eqCoreTm _ (Where tyobj1 tya1 k1 f1 obj1) (Where tyobj2 tya2 k2 f2 obj2)
  = fromMaybe False $ do
    Refl <- singEq tyobj1 tyobj2
    Refl <- singEq tya1   tya2
    pure $ eqTm k1 k2 && singEqOpen SBool f1 f2 && singEqTm tyobj1 obj1 obj2
eqCoreTm _ (Typeof ty1 a1) (Typeof ty2 a2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> singEqTm ty1 a1 a2

eqCoreTm _ _ _                          = False

showsPrecCore :: IsTerm tm => SingTy a -> Int -> Core tm a -> ShowS
showsPrecCore ty p core = showParen (p > 10) $ case core of
  Lit a            -> showString "Lit "        . withShow ty (showsPrec 11 a)
  Sym a            -> showString "Sym "        . showsPrec 11 a
  Var a b          -> showString "Var "        . showsPrec 11 a . showString " " . showsPrec 11 b
  Identity a b     -> showString "Identity "   . showsPrec 11 a . showString " " . singShowsTm a 11 b
  Constantly tyb a b ->
      showString "Constantly "
    . showsPrec 11 ty
    . showString " "
    . singShowsTm ty 11 a
    . showString " "
    . singShowsTm tyb 11 b
  Compose tya tyb tyc a b c ->
      showString "Compose "
    . showsPrec 11 tya
    . showString " "
    . showsPrec 11 tyb
    . showString " "
    . showsPrec 11 tyc
    . showString " "
    . singShowsTm tya 11 a
    . singShowsOpen tyb b
    . showString " "
    . singShowsOpen tyc c
  StrConcat a b    -> showString "StrConcat "    . showsTm 11 a . showString " " . showsTm 11 b
  StrLength a      -> showString "StrLength "    . showsTm 11 a
  StrToInt a       -> showString "StrToInt "     . showsTm 11 a
  StrToIntBase a b -> showString "StrToIntBase " . showsTm 11 a . showString " " . showsTm 11 b
  StrContains  a b -> showString "StrContains "  . showsTm 11 a . showString " " . showsTm 11 b
  Numerical a      -> showString "Numerical "    . showsNumerical ty 11 a
  IntAddTime a b   -> showString "IntAddTime "   . showsTm 11 a . showString " " . showsTm 11 b
  DecAddTime a b   -> showString "DecAddTime "   . showsTm 11 a . showString " " . showsTm 11 b
  Comparison ty' op a b ->
      showString "Comparison "
    . showsPrec 11 ty'
    . showString " "
    . showsPrec 11 op
    . showString " "
    . singShowsTm ty' 11 a
    . showString " "
    . singShowsTm ty' 11 b
  ObjectEqNeq ty1 ty2 op a b ->
      showString "ObjectEqNeq "
    . showsPrec 11 ty1
    . showString " "
    . showsPrec 11 ty2
    . showString " "
    . showsPrec 11 op
    . showString " "
    . singShowsTm ty1 11 a
    . showString " "
    . singShowsTm ty2 11 b

  ObjAt ty' a b ->
      showString "ObjAt "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 a
    . showString " "
    . singShowsTm ty' 11 b
  ObjContains ty' a b ->
      showString "ObjContains "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 a
    . showString " "
    . singShowsTm ty' 11 b
  ObjDrop b c ->
      showString "ObjDrop "
    . showsTm 11 b
    . showString " "
    . singShowsTm ty 11 c
  ObjTake b c ->
      showString "ObjTake "
    . showsTm 11 b
    . showString " "
    . singShowsTm ty 11 c
  ObjMerge ty1 ty2 a b ->
      showString "ObjMerge "
    . showsPrec 11 ty1
    . showString " "
    . showsPrec 11 ty2
    . showString " "
    . singShowsTm ty1 11 a
    . showString " "
    . singShowsTm ty2 11 b
  LiteralObject _ _ -> showString "LiteralObject " -- TODO . showsPrec 11 m

  Logical op args ->
      showString "Logical "
    . showsPrec 11 op
    . showString " "
    . showString "TODO"
    . showListWith (singShowsTm SBool 0) args

  ListEqNeq ty' op a b ->
      showString "ListEqNeq "
    . showsPrec 11 ty'
    . showString " "
    . showsPrec 11 op
    . showString " "
    . singShowsTmList ty' 11 a
    . showString " "
    . singShowsTmList ty' 11 b
  ListAt ty' a b ->
      showString "ListAt "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 a
    . showString " "
    . singShowsTmList ty' 11 b
  ListContains ty' a b ->
      showString "ListContains "
    . showsPrec 11 ty'
    . showString " "
    . singShowsTm ty' 11 a
    . showString " "
    . singShowsTmList ty' 11 b
  ListLength ty' a ->
      showString "ListLength "
    . showsPrec 11 ty'
    . showString " "
    . singShowsTmList ty' 11 a
  ListReverse ty' a ->
      showString "ListReverse "
    . showsPrec 11 ty'
    . showString " "
    . singShowsTmList ty' 11 a
  ListSort ty' a ->
      showString "ListSort "
    . showsPrec 11 ty'
    . showString " "
    . singShowsTmList ty' 11 a
  ListDrop ty' i l ->
      showString "ListDrop "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 i
    . showString " "
    . singShowsTmList ty' 11 l
  ListTake ty' a b ->
      showString "ListTake "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 a
    . showString " "
    . singShowsTmList ty' 11 b
  ListConcat ty' a b ->
      showString "ListConcat "
    . showsPrec 11 ty'
    . showString " "
    . singShowsTmList ty' 11 a
    . showString " "
    . singShowsTmList ty' 11 b
  MakeList ty' a b ->
      showString "MakeList "
    . showsPrec 11 ty'
    . showString " "
    . showsTm 11 a
    . showString " "
    . singShowsTm ty' 11 b
  LiteralList ty' l ->
      showString "LiteralList "
    . showsPrec 11 ty'
    . showString " "
    . singShowsListTm ty' 11 l
  ListMap tya tyb b as ->
      showString "ListMap "
    . showsPrec 11 tya
    . showString " "
    . showsPrec 11 tyb
    . showString " "
    . singShowsOpen tyb b
    . showString " "
    . singShowsTmList tya 11 as
  ListFilter tya f as ->
      showString "ListFilter "
    . showsPrec 11 tya
    . showString " "
    . singShowsOpen sing f
    . showString " "
    . singShowsTmList tya 11 as
  ListFold tya tyb (Open vid nm f) a bs ->
      showString "ListFold "
    . showsPrec 11 tya
    . showString " "
    . showsPrec 11 tyb
    . showString " (Open "
      . showsPrec 11 vid
      . showString " "
      . showsPrec 11 nm
      . singShowsOpen tya f
    . showString ") "
    . singShowsTm tya 11 a
    . showString " "
    . singShowsTmList tyb 11 bs
  AndQ tya f g a ->
      showString "AndQ "
    . showsPrec 11 tya
    . showString " "
    . singShowsOpen sing f
    . showString " "
    . singShowsOpen sing g
    . showString " "
    . singShowsTm tya 11 a
  OrQ tya f g a ->
      showString "OrQ "
    . showsPrec 11 tya
    . showString " "
    . singShowsOpen sing f
    . showString " "
    . singShowsOpen sing g
    . showString " "
    . singShowsTm tya 11 a
  Where tyo tya str f obj ->
      showString "Where "
    . showsPrec 11 tyo
    . showString " "
    . showsPrec 11 tya
    . showString " "
    . showsTm 11 str
    . showString " "
    . singShowsOpen sing f
    . showString " "
    . singShowsTm tyo 11 obj
  Typeof tya a ->
      showString "Typeof "
    . showsPrec 11 tya
    . showString " "
    . singShowsTm tya 11 a

userShowCore :: IsTerm tm => SingTy ty -> Int -> Core tm ty -> Text
userShowCore ty _p = \case
  Lit a                    -> withUserShow ty $ userShow a
  Sym s                    -> tShow s
  Var _vid name            -> name
  Identity ty' x           -> parenList [SIdentity, singUserShowTm ty' x]
  Constantly tyb a b       -> parenList [SConstantly, singUserShowTm ty a, singUserShowTm tyb b]
  Compose _ tyb tyc _ b c  -> parenList [SCompose, singUserShowOpen tyb b, singUserShowOpen tyc c]
  StrConcat x y            -> parenList [SConcatenation, userShowTm x, userShowTm y]
  StrLength str            -> parenList [SStringLength, userShowTm str]
  StrToInt s               -> parenList [SStringToInteger, userShowTm s]
  StrToIntBase b s         -> parenList [SStringToInteger, userShowTm b, userShowTm s]
  StrContains needle haystack
    -> parenList [SContains, userShowTm needle, userShowTm haystack]
  Numerical tm             -> userShowNumerical ty tm
  IntAddTime x y           -> parenList [STemporalAddition, userShowTm x, userShowTm y]
  DecAddTime x y           -> parenList [STemporalAddition, userShowTm x, userShowTm y]
  Comparison ty' op x y    -> parenList [userShow op, singUserShowTm ty' x, singUserShowTm ty' y]
  ObjectEqNeq ty1 ty2 op x y
    -> parenList [userShow op, singUserShowTm ty1 x, singUserShowTm ty2 y]
  ObjAt ty' k obj          -> parenList [userShowTm k, singUserShowTm ty' obj]
  ObjContains ty' k obj    -> parenList [SContains, userShowTm k, singUserShowTm ty' obj]
  ObjDrop ks obj           -> parenList [SObjectDrop, userShowTm ks, singUserShowTm ty obj]
  ObjTake ks obj           -> parenList [SObjectTake, userShowTm ks, singUserShowTm ty obj]
  ObjMerge ty1 ty2 x y     -> parenList [SObjectMerge, singUserShowTm ty1 x, singUserShowTm ty2 y]
  LiteralObject _ _obj       -> "LiteralObject TODO" -- userShow obj
  Logical op args          -> parenList $ userShow op : fmap userShowTm args

  ListEqNeq ty' op x y     -> parenList [userShow op, singUserShowTmList ty' x, singUserShowTmList ty' y]
  ListAt ty' k lst         -> parenList [userShowTm k, singUserShowTmList ty' lst]
  ListContains ty' needle haystack
    -> parenList [SContains, singUserShowTm ty' needle, singUserShowTmList ty' haystack]
  ListLength ty' x         -> parenList [SListLength, singUserShowTmList ty' x]
  ListReverse ty' lst      -> parenList [SReverse, singUserShowTmList ty' lst]
  ListSort ty' lst         -> parenList [SSort, singUserShowTmList ty' lst]
  ListDrop ty' n lst       -> parenList [SListDrop, userShowTm n, singUserShowTmList ty' lst]
  ListTake ty' n lst       -> parenList [SListTake, userShowTm n, singUserShowTmList ty' lst]
  ListConcat ty' x y       -> parenList [SConcatenation, singUserShowTmList ty' x, singUserShowTmList ty' y]
  MakeList ty' x y         -> parenList [SMakeList, userShowTm x, singUserShowTm ty' y]
  LiteralList ty' lst      -> singUserShowListTm ty' lst
  ListMap tya tyb b as -> parenList
    [ SMap
    , singUserShowOpen tyb b
    , singUserShowTmList tya as
    ]
  ListFilter ty' a b -> parenList
    [ SFilter
    , singUserShowOpen SBool a
    , singUserShowTmList ty' b
    ]
  ListFold tya tyb (Open _ nm a) b c -> parenList
    [ SFold
    , parenList [ "lambda", nm, singUserShowOpen tya a ]
    , singUserShowTm tya b
    , singUserShowTmList tyb c
    ]
  AndQ ty' a b c -> parenList
    [ SAndQ
    , singUserShowOpen SBool a
    , singUserShowOpen SBool b
    , singUserShowTm ty' c
    ]
  OrQ ty' a b c -> parenList
    [ SOrQ
    , singUserShowOpen SBool a
    , singUserShowOpen SBool b
    , singUserShowTm ty' c
    ]
  Where tyobj _tya k f obj -> parenList
    [ SWhere
    , userShowTm k
    , singUserShowOpen SBool f
    , singUserShowTm tyobj obj
    ]
  Typeof ty' a -> parenList [STypeof, singUserShowTm ty' a]


data BeforeOrAfter = Before | After
  deriving (Eq, Show)

instance UserShow BeforeOrAfter where
  userShowPrec _p = \case
    Before -> "'before"
    After  -> "'after"

-- | Property-specific constructions.
--
-- This encompasses every construction that can appear in a 'Prop' that's not
-- in 'Core'.
data PropSpecific (a :: Ty) where

  -- TX success/failure

  --
  -- TODO: remove either Success Or Abort.
  --

  -- | Whether a transaction aborts (does not succeed)
  Abort   :: PropSpecific 'TyBool
  -- | Whether a transaction succeeds (does not abort)
  Success :: PropSpecific 'TyBool
  -- | The return value of the function under examination
  Result  :: PropSpecific a

  -- Abstraction

  -- | Introduces a universally-quantified variable over another property
  Forall :: VarId -> Text -> QType -> Prop 'TyBool -> PropSpecific 'TyBool
  -- | Introduces an existentially-quantified variable over another property
  Exists :: VarId -> Text -> QType -> Prop 'TyBool -> PropSpecific 'TyBool

  -- DB properties

  -- | True when anything in the table is written
  TableWrite :: Prop TyTableName  ->                PropSpecific 'TyBool
  -- | True when anything in the table is read
  TableRead  :: Prop TyTableName  ->                PropSpecific 'TyBool

  --
  -- NOTE: it's possible that in a standard library we could implement these in
  --       terms of "CellRead"/"CellWrite" and existential quantification.
  --
  -- | Whether a column is written
  ColumnWritten :: Prop TyTableName  -> Prop TyColumnName  -> PropSpecific 'TyBool
  -- | Whether a column is read
  ColumnRead    :: Prop TyTableName  -> Prop TyColumnName  -> PropSpecific 'TyBool

  --
  -- TODO: rewrite these in terms of CellBefore, CellAfter, ColumnSumBefore,
  --       ColumnSumAfter:
  --
  -- | The difference (@after-before@) in a cell's integer value across a transaction
  IntCellDelta   :: Prop TyTableName  -> Prop TyColumnName  -> Prop TyRowKey -> PropSpecific 'TyInteger
  -- | The difference (@after-before@) in a cell's decimal value across a transaction
  DecCellDelta   :: Prop TyTableName  -> Prop TyColumnName  -> Prop TyRowKey -> PropSpecific 'TyDecimal
  -- | The difference (@after-before@) in a column's integer sum across a transaction
  IntColumnDelta :: Prop TyTableName  -> Prop TyColumnName                 -> PropSpecific 'TyInteger
  -- | The difference (@after-before@) in a column's decimal sum across a transaction
  DecColumnDelta :: Prop TyTableName  -> Prop TyColumnName                 -> PropSpecific 'TyDecimal

  -- | Whether a row is read
  RowRead       :: Prop TyTableName  -> Prop TyRowKey -> PropSpecific 'TyBool
  -- | Number of times a row is read
  RowReadCount  :: Prop TyTableName  -> Prop TyRowKey -> PropSpecific 'TyInteger
  -- | Whether a row is written
  RowWrite      :: Prop TyTableName  -> Prop TyRowKey -> PropSpecific 'TyBool
  -- | Number of times a row is written
  RowWriteCount :: Prop TyTableName  -> Prop TyRowKey -> PropSpecific 'TyInteger
  -- | Whether a row exists prior to the transaction
  RowExists     :: Prop TyTableName  -> Prop TyRowKey -> BeforeOrAfter -> PropSpecific 'TyBool

  --
  -- TODO: StaleRead?
  --

  -- Authorization

  -- | Whether a transaction contains a signature that satisfied the named key set
  KsNameAuthorized :: KeySetName      ->                                   PropSpecific 'TyBool
  -- | Whether a row has its keyset @enforce@d in a transaction
  RowEnforced      :: Prop TyTableName  -> Prop TyColumnName -> Prop TyRowKey -> PropSpecific 'TyBool

  PropRead :: SingTy ('TyObject m) -> BeforeOrAfter -> Prop TyTableName -> Prop TyRowKey -> PropSpecific ('TyObject m)

deriving instance SingI a => Show (PropSpecific a)
deriving instance SingI a => Eq   (PropSpecific a)

data Prop (a :: Ty)
  = PropSpecific (PropSpecific a)
  | CoreProp     (Core Prop a)

instance UserShow (PropSpecific a) where
  userShowPrec _d = \case
    Abort                   -> STransactionAborts
    Success                 -> STransactionSucceeds
    Result                  -> SFunctionResult
    Forall _ var ty x       -> parenList
      [SUniversalQuantification, parens (var <> ":" <> userShow ty), userShowTm x]
    Exists _ var ty x       -> parenList
      [SExistentialQuantification, parens (var <> ":" <> userShow ty), userShowTm x]
    TableWrite tab          -> parenList [STableWritten, userShowTm tab]
    TableRead  tab          -> parenList [STableRead, userShowTm tab]
    ColumnWritten tab col   -> parenList ["column-written", userShowTm tab, userShowTm col]
    ColumnRead tab col      -> parenList ["column-read", userShowTm tab, userShowTm col]
    IntCellDelta tab col rk -> parenList [SCellDelta, userShowTm tab, userShowTm col, userShowTm rk]
    DecCellDelta tab col rk -> parenList [SCellDelta, userShowTm tab, userShowTm col, userShowTm rk]
    IntColumnDelta tab col  -> parenList [SColumnDelta, userShowTm tab, userShowTm col]
    DecColumnDelta tab col  -> parenList [SColumnDelta, userShowTm tab, userShowTm col]
    RowRead tab rk          -> parenList [SRowRead, userShowTm tab, userShowTm rk]
    RowReadCount tab rk     -> parenList [SRowReadCount, userShowTm tab, userShowTm rk]
    RowWrite tab rk         -> parenList [SRowWritten, userShowTm tab, userShowTm rk]
    RowWriteCount tab rk    -> parenList [SRowWriteCount, userShowTm tab, userShowTm rk]
    KsNameAuthorized name   -> parenList [SAuthorizedBy, userShow name]
    RowEnforced tn cn rk    -> parenList [SRowEnforced, userShowTm tn, userShowTm cn, userShowTm rk]
    RowExists tn rk ba      -> parenList [SRowExists, userShowTm tn, userShowTm rk, userShow ba]
    PropRead _ty ba tn rk   -> parenList [SPropRead, userShowTm tn, userShowTm rk, userShow ba]

instance S :*<: Prop where
  inject' = CoreProp . Sym
  project' = \case
    CoreProp (Sym a) -> Just a
    _                -> Nothing

instance PropSpecific :<: Prop where
  inject = PropSpecific
  project = \case
    PropSpecific a -> Just a
    _              -> Nothing

instance Core Prop :<: Prop where
  inject = CoreProp
  project = \case
    CoreProp a -> Just a
    _          -> Nothing

instance Numerical Prop :<: Prop where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance IsString (Prop 'TyStr) where
  fromString = Lit' . fromString

instance Boolean (Prop 'TyBool) where
  sTrue     = Lit' True
  sFalse    = Lit' False
  sNot p    = CoreProp $ Logical NotOp [p]
  p1 .&& p2 = PAnd p1 p2
  p1 .|| p2 = POr  p1 p2

instance Num (Prop 'TyInteger) where
  fromInteger = Lit' . fromInteger
  (+)         = inject ... IntArithOp Add
  (*)         = inject ... IntArithOp Mul
  abs         = inject .   IntUnaryArithOp Abs
  signum      = inject .   IntUnaryArithOp Signum
  negate      = inject .   IntUnaryArithOp Negate

instance Num (Prop 'TyDecimal) where
  fromInteger = Lit' . fromPact decimalIso . fromInteger
  (+)         = inject ... DecArithOp Add
  (*)         = inject ... DecArithOp Mul
  abs         = inject .   DecUnaryArithOp Abs
  signum      = inject .   DecUnaryArithOp Signum
  negate      = inject .   DecUnaryArithOp Negate

type EProp = Existential Prop

mkLiteralList :: [Existential tm] -> Maybe (Existential (Core tm))
mkLiteralList [] = Just $ Existential (SList SAny) (LiteralList SAny [])
mkLiteralList xs@(Existential ty0 _ : _) = foldr
  (\case
    Existential ty y -> \case
      Nothing -> Nothing
      Just (Existential (SList ty') (LiteralList _ty ys)) -> case singEq ty ty' of
        Nothing   -> Nothing
        Just Refl -> Just (Existential (SList ty') (LiteralList ty' (y:ys)))
      _ -> error "impossible")
  (Just (Existential (SList ty0) (LiteralList ty0 [])))
  xs

pattern Lit' :: forall tm ty. Core tm :<: tm => Concrete ty -> tm ty
pattern Lit' a <- (project @(Core tm) @tm -> Just (Lit a)) where
  Lit' a = inject @(Core tm) @tm (Lit a)
-- maybe this will work in the future:
-- pattern Lit' a = Inj @(Core tm) @tm (Lit a)

pattern StrLit :: forall tm. Core tm :<: tm => String -> tm 'TyStr
pattern StrLit str = Lit' (Str str)

pattern TextLit :: forall tm. Core tm :<: tm => Text -> tm 'TyStr
pattern TextLit text <- Lit' (Str (Text.pack -> text)) where
  TextLit text = Lit' (Str (Text.unpack text))

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = CoreProp (Var vid name)

pattern PNumerical :: Numerical Prop t -> Prop t
pattern PNumerical x = CoreProp (Numerical x)

pattern PStrConcat :: Prop 'TyStr -> Prop 'TyStr -> Prop 'TyStr
pattern PStrConcat x y = CoreProp (StrConcat x y)

pattern PIntAddTime :: Prop 'TyTime -> Prop 'TyInteger -> Prop 'TyTime
pattern PIntAddTime x y = CoreProp (IntAddTime x y)

pattern PDecAddTime :: Prop 'TyTime -> Prop 'TyDecimal -> Prop 'TyTime
pattern PDecAddTime x y = CoreProp (DecAddTime x y)

pattern PObjAt
  :: SingTy ('TyObject m) -> Prop 'TyStr -> Prop ('TyObject m) -> Prop t
pattern PObjAt a b c = CoreProp (ObjAt a b c)

pattern PLogical :: LogicalOp -> [Prop 'TyBool] -> Prop 'TyBool
pattern PLogical op args = CoreProp (Logical op args)

pattern PStrLength :: Prop 'TyStr -> Prop 'TyInteger
pattern PStrLength str = CoreProp (StrLength str)

pattern PAnd :: Prop 'TyBool -> Prop 'TyBool -> Prop 'TyBool
pattern PAnd a b = CoreProp (Logical AndOp [a, b])

pattern POr :: Prop 'TyBool -> Prop 'TyBool -> Prop 'TyBool
pattern POr a b = CoreProp (Logical OrOp [a, b])

pattern PNot :: Prop 'TyBool -> Prop 'TyBool
pattern PNot a = CoreProp (Logical NotOp [a])


-- | The schema invariant language.
--
-- This language is pure / stateless. It includes exactly the same
-- constructions as 'Core'.
newtype Invariant a = CoreInvariant (Core Invariant a)

instance Core Invariant :<: Invariant where
  inject                    = CoreInvariant
  project (CoreInvariant a) = Just a

instance Numerical Invariant :<: Invariant where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance S :*<: Invariant where
  inject' = CoreInvariant . Sym
  project' = \case
    CoreInvariant (Sym a) -> Just a
    _                     -> Nothing

type EInvariant = Existential Invariant

pattern ILiteral :: Concrete a -> Invariant a
pattern ILiteral a = CoreInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant 'TyBool] -> Invariant 'TyBool
pattern ILogicalOp op args = CoreInvariant (Logical op args)

type ETerm = Existential Term

data Term (a :: Ty) where
  CoreTerm        :: Core Term a -> Term a

  -- In principle, this should be a pure term, however, the analyze monad needs
  -- to be `Mergeable`. `Analyze` is, but `Query` isn't, due to having
  -- `Symbolic` in its stack.
  --
  -- TODO(joel): In principle this could be pure and applied to all the
  -- languages. Unfortunately, we can't add this to props because `Query` has
  -- `Symbolic` in its stack, so it can't do an `ite`.
  -- TODO(joel): the above no longer applies!
  IfThenElse      :: SingTy a -> Term 'TyBool -> (Path, Term a) -> (Path, Term a) -> Term a

  -- Variable binding
  Let             :: Text -> VarId -> TagId -> ETerm -> Term a -> Term a

  -- Control flow
  Sequence        :: ETerm     -> Term a ->           Term a

  -- Conditional transaction abort
  Enforce         :: Maybe TagId -> Term 'TyBool   -> Term 'TyBool -- Only a TagId for an assertion; i.e. not keyset enforcement
  -- Left to be tagged if the list of cases is empty. We do this because we
  -- need a way to signal a failure due to this particular scenario in model
  -- reporting. Right _1 to be tagged if the case fails, Right _2 to be tagged
  -- if the case succeeds:
  EnforceOne      :: Either TagId [((Path, Path), Term 'TyBool)] -> Term 'TyBool

  -- Reading from environment
  ReadKeySet      :: Term 'TyStr -> Term 'TyKeySet
  ReadDecimal     :: Term 'TyStr -> Term 'TyDecimal
  ReadInteger     :: Term 'TyStr -> Term 'TyInteger

  -- TODO: ReadInteger, ReadMsg

  -- Keyset access
  KsAuthorized    :: TagId -> Term 'TyKeySet -> Term 'TyBool
  NameAuthorized  :: TagId -> Term 'TyStr -> Term 'TyBool

  -- Table access
  Read            :: SingTy ('TyObject m) -> TagId -> TableName
    -> Term 'TyStr -> Term ('TyObject m)
  Write           :: SingTy ('TyObject m)
    -> WriteType -> TagId -> TableName
    -> Term 'TyStr -> Term ('TyObject m) -> Term 'TyStr

  PactVersion     :: Term 'TyStr

  Format          :: Term 'TyStr         -> [ETerm]     -> Term 'TyStr
  FormatTime      :: Term 'TyStr         -> Term 'TyTime   -> Term 'TyStr
  ParseTime       :: Maybe (Term 'TyStr) -> Term 'TyStr -> Term 'TyTime
  Hash            :: ETerm                              -> Term 'TyStr

showsTerm :: SingTy ty -> Int -> Term ty -> ShowS
showsTerm ty p tm = withSing ty $ showParen (p > 10) $ case tm of
  CoreTerm tm' -> showString "CoreTerm " . showsTm 11 tm'
  IfThenElse ty' x y z ->
      showString "IfThenElse "
    . showsPrec 11 ty'
    . showChar ' '
    . showsPrec 11 x
    . showChar ' '
    . showsPrec 11 y
    . showChar ' '
    . showsPrec 11 z
  Let a b c d e ->
      showString "Let "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
    . showChar ' '
    . showsPrec 11 d
    . showChar ' '
    . showsPrec 11 e
  Sequence x y ->
      showString "Sequence "
    . showsPrec 11 x
    . showChar ' '
    . showsPrec 11 y
  EnforceOne a -> showString "EnforceOne " . showsPrec 11 a
  Enforce a b ->
      showString "Enforce "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b

  KsAuthorized a b ->
      showString "KsAuthorized "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
  NameAuthorized a b ->
      showString "NameAuthorized "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b

  Read a b c d ->
      showString "Read "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
    . showChar ' '
    . showsPrec 11 d

  Write a b c d e f -> withSing a $
      showString "Write "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
    . showChar ' '
    . showsPrec 11 d
    . showChar ' '
    . showsPrec 11 e
    . showChar ' '
    . showsPrec 11 f

  PactVersion -> showString "PactVersion"
  Format a b ->
      showString "Format "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b

  ParseTime a b ->
      showString "ParseTime "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
  FormatTime a b ->
      showString "FormatTime "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
  Hash a -> showString "Hash " . showsPrec 11 a
  ReadKeySet  name -> showString "ReadKeySet " . showsPrec 11 name
  ReadDecimal name -> showString "ReadDecimal " . showsPrec 11 name
  ReadInteger name -> showString "ReadInteger " . showsPrec 11 name

showsProp :: SingTy ty -> Int -> Prop ty -> ShowS
showsProp ty p = withSing ty $ \case
  PropSpecific tm -> showsPrec p tm
  CoreProp     tm -> showsPrecCore ty p tm

eqProp :: SingTy ty -> Prop ty -> Prop ty -> Bool
eqProp ty (CoreProp     a) (CoreProp     b) = eqCoreTm ty a b
eqProp ty (PropSpecific a) (PropSpecific b) = withSing ty $ a == b
eqProp _  _                _                = False

userShowTerm :: SingTy ty -> Int -> Term ty -> Text
userShowTerm ty p = \case
  CoreTerm tm -> userShowCore ty p tm
  IfThenElse ty' x (_, y) (_, z) -> parenList
    [ "if"
    , userShowTm x
    , singUserShowTm ty' y
    , singUserShowTm ty' z
    ]
  Let var _ _ x y -> parenList
    [ "let"
    , userShow var
    , userShow x
    , userShowTerm ty 0 y
    ]
  Sequence x y -> Text.unlines [userShow x, userShowTerm ty 0 y]

  EnforceOne (Left _)        -> parenList
    [ "enforce-one"
    , "\"(generated enforce-one)\""
    , userShow ([] :: [Integer])
    ]
  EnforceOne (Right x)       -> parenList
    [ "enforce-one"
    , "\"(generated enforce-one)\""
    , userShow $ fmap snd x
    ]

  Enforce _ (KsAuthorized _ x)   -> parenList ["enforce-keyset", userShow x]
  Enforce _ (NameAuthorized _ x) -> parenList ["enforce-keyset", userShow x]
  Enforce _ x                    -> parenList ["enforce", userShow x]
  KsAuthorized   _ _
    -> error "KsAuthorized should only appear inside of an Enforce"
  NameAuthorized _ _
    -> error "NameAuthorized should only appear inside of an Enforce"

  Read _ _ tab x       -> parenList ["read", userShow tab, userShow x]
  Write ty' _ _ tab x y -> parenList ["write", userShow tab, userShow x, singUserShowTm ty' y]
  PactVersion          -> parenList ["pact-version"]
  Format x y           -> parenList ["format", userShow x, userShow y]
  FormatTime x y       -> parenList ["format", userShow x, userShow y]
  ParseTime Nothing y  -> parenList ["parse-time", userShow y]
  ParseTime (Just x) y -> parenList ["parse-time", userShow x, userShow y]
  Hash x               -> parenList ["hash", userShow x]
  ReadKeySet name      -> parenList ["read-keyset", userShow name]
  ReadDecimal name     -> parenList ["read-decimal", userShow name]
  ReadInteger name     -> parenList ["read-integer", userShow name]

eqTerm :: SingTy ty -> Term ty -> Term ty -> Bool
eqTerm ty (CoreTerm a1) (CoreTerm a2) = singEqTm' ty a1 a2
eqTerm ty (IfThenElse _ty1 a1 (b1, c1) (d1, e1))
          (IfThenElse _ty2 a2 (b2, c2) (d2, e2))
  = eqTm a1 a2 && b1 == b2 && singEqTm ty c1 c2 && d1 == d2 && singEqTm ty e1 e2
eqTerm ty (Let a1 b1 c1 d1 e1) (Let a2 b2 c2 d2 e2)
  = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && singEqTm ty e1 e2
eqTerm ty (Sequence a1 b1) (Sequence a2 b2)
  = a1 == a2 && singEqTm ty b1 b2
eqTerm _ty (Enforce a1 b1) (Enforce a2 b2)
  = a1 == a2 && b1 == b2
eqTerm _ty (EnforceOne a) (EnforceOne b)
  = a == b
eqTerm _ty (ReadKeySet a) (ReadKeySet b)
  = a == b
eqTerm _ty (ReadDecimal a) (ReadDecimal b)
  = a == b
eqTerm _ty (ReadInteger a) (ReadInteger b)
  = a == b
eqTerm _ty (KsAuthorized a1 b1) (KsAuthorized a2 b2)
  = a1 == a2 && b1 == b2
eqTerm _ty (NameAuthorized a1 b1) (NameAuthorized a2 b2)
  = a1 == a2 && b1 == b2
eqTerm _ty (Read _ a1 b1 c1) (Read _ a2 b2 c2)
  = a1 == a2 && b1 == b2 && c1 == c2
eqTerm _ty (Write ty1 a1 b1 c1 d1 e1) (Write ty2 a2 b2 c2 d2 e2)
  = case singEq ty1 ty2 of
      Nothing   -> False
      Just Refl -> a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && singEqTm ty1 e1 e2
eqTerm _ty PactVersion PactVersion = True
eqTerm _ty (Format a1 b1) (Format a2 b2) = a1 == a2 && b1 == b2
eqTerm _ty (FormatTime a1 b1) (FormatTime a2 b2) = a1 == a2 && b1 == b2
eqTerm _ty (ParseTime a1 b1) (ParseTime a2 b2) = a1 == a2 && b1 == b2
eqTerm _ty (Hash a1) (Hash a2) = a1 == a2
eqTerm _ _ _ = False

instance S :*<: Term where
  inject' = CoreTerm . Sym
  project' = \case
    CoreTerm (Sym a) -> Just a
    _                -> Nothing

instance Core Term :<: Term where
  inject = CoreTerm
  project = \case
    CoreTerm a -> Just a
    _          -> Nothing

instance Numerical Term :<: Term where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance Num (Term 'TyInteger) where
  fromInteger = Lit' . fromInteger
  (+)    = inject ... IntArithOp Add
  (*)    = inject ... IntArithOp Mul
  abs    = inject .   IntUnaryArithOp Abs
  signum = inject .   IntUnaryArithOp Signum
  negate = inject .   IntUnaryArithOp Negate

instance Num (Term 'TyDecimal) where
  fromInteger = Lit' . fromPact decimalIso . fromInteger
  (+)    = inject ... DecArithOp Add
  (*)    = inject ... DecArithOp Mul
  abs    = inject .   DecUnaryArithOp Abs
  signum = inject .   DecUnaryArithOp Signum
  negate = inject .   DecUnaryArithOp Negate

valueToProp :: ETerm -> Either String EProp
valueToProp = \case
  Existential ty (CoreTerm (Lit l))
    -> Right $ Existential ty (CoreProp (Lit l))
  Existential _ _ -> Left "can only convert (simple) values terms to props"

-- Note [instances]:
-- The following nine instances seem like they should be
--
--     instance (IsTerm tm, SingI ty) => UserShow (tm ty) where
--
-- Unfortunately, that's a nightmare in terms of overlap. So we implement an
-- instance for each term type.

instance SingI ty => UserShow (Term ty) where
  userShowPrec _ = singUserShowTm sing

instance SingI ty => UserShow (Prop ty) where
  userShowPrec _ = singUserShowTm sing

instance SingI ty => UserShow (Invariant ty) where
  userShowPrec _ = singUserShowTm sing

instance SingI ty => Show (Term ty) where
  showsPrec = singShowsTm sing

instance SingI ty => Show (Prop ty) where
  showsPrec = singShowsTm sing

instance SingI ty => Show (Invariant ty) where
  showsPrec = singShowsTm sing

instance SingI ty => Eq (Term ty) where
  (==) = singEqTm sing

instance SingI ty => Eq (Prop ty) where
  (==) = singEqTm sing

instance SingI ty => Eq (Invariant ty) where
  (==) = singEqTm sing

instance IsTerm Term where
  singEqTm'          = eqTerm
  singShowsTm'       = showsTerm
  singUserShowTm' ty = userShowTerm ty 0

instance IsTerm Prop where
  singEqTm'          = eqProp
  singShowsTm'       = showsProp
  singUserShowTm' ty = \case
    PropSpecific tm -> userShowPrec 0 tm
    CoreProp     tm -> singUserShowTm' ty tm

instance IsTerm Invariant where
  singEqTm' ty (CoreInvariant tm1) (CoreInvariant tm2) = singEqTm' ty tm1 tm2
  singShowsTm' ty p (CoreInvariant tm)                 = singShowsTm' ty p tm
  singUserShowTm' ty (CoreInvariant tm)                = singUserShowTm' ty tm

instance IsTerm tm => IsTerm (Core tm) where
  singEqTm'          = eqCoreTm
  singShowsTm'       = showsPrecCore
  singUserShowTm' ty = userShowCore ty 0
