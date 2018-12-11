{-# LANGUAGE CPP                   #-}
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

  , pattern ILiteral
  , pattern ILogicalOp
  , pattern Inj
  , pattern PAnd
  , pattern PObjAt
  , pattern PDecAddTime
  , pattern PIntAddTime
  , pattern PKeySetEqNeq
  , pattern Lit'
  , pattern StrLit
  , pattern TextLit
  , pattern PLogical
  , pattern PNot
  , pattern PNumerical
  , pattern PObjectEqNeq
  , pattern POr
  , pattern PStrConcat
  , pattern PStrLength
  , pattern PVar

  , mkLiteralList
  ) where

import           Data.SBV                     (Boolean (bnot, false, true, (&&&), (|||)))
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable                ((:~:) (Refl))
import           Prelude                      hiding (Float)

import           Pact.Types.Persistence       (WriteType)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Sym, Var, col, str, obj, dec)
import           Pact.Analyze.Types.Map (Map)
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types
import           Pact.Analyze.Types.UserShow
import           Pact.Analyze.Util

#define EQ_EXISTENTIAL(tm)                                        \
instance Eq (Existential tm) where                                \
  Existential sa ia == Existential sb ib = case singEq sa sb of { \
    Just Refl -> withEq sa (ia == ib);                            \
    Nothing   -> False};                                          \

#define SHOW_EXISTENTIAL(tm)                                                   \
instance Show (Existential tm) where {                                         \
  showsPrec d e = showParen (d > 10) $ case e of                               \
    Existential ty inv -> showString "Existential " . showsPrec 11 ty . showString " " \
      . withShow ty (showsPrec 11 inv); };                                     \
instance UserShow (Existential tm) where                                       \
  userShowPrec d e = case e of                                                 \
    Existential ty a -> withUserShow ty (userShowPrec d a)                   \

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

  Identity     :: SingTy a   -> t a      -> Core t a
  Constantly   :: SingTy a   -> t a -> Existential t -> Core t a

  -- compose
  -- * f :: a -> b (free a)
  -- * g :: b -> c (free b)
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

  -- | A 'ComparisonOp' expression over two 'Integer' expressions
  IntegerComparison :: ComparisonOp -> t 'TyInteger -> t 'TyInteger -> Core t 'TyBool
  -- | A 'ComparisonOp' expression over two 'Decimal' expressions
  DecimalComparison :: ComparisonOp -> t 'TyDecimal -> t 'TyDecimal -> Core t 'TyBool
  -- | A 'ComparisonOp' expression over two 'Time' expressions
  TimeComparison    :: ComparisonOp -> t 'TyTime    -> t 'TyTime    -> Core t 'TyBool
  -- | A 'ComparisonOp' expression over two 'String' expressions
  StrComparison     :: ComparisonOp -> t 'TyStr     -> t 'TyStr     -> Core t 'TyBool
  -- | A 'ComparisonOp' expression over two 'Bool' expressions
  --
  -- note: this is more broad than the set ({=, !=}) of comparisons pact
  -- supports on bools.
  BoolComparison    :: ComparisonOp -> t 'TyBool    -> t 'TyBool    -> Core t 'TyBool

  -- object ops

  KeySetEqNeq :: EqNeq -> t 'TyKeySet     -> t 'TyKeySet   -> Core t 'TyBool
  ObjectEqNeq :: SingTy ('TyObject m) -> EqNeq -> t ('TyObject m) -> t ('TyObject m) -> Core t 'TyBool

  ObjAt       :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t a
  ObjContains :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t 'TyBool
  ObjDrop     :: SingTy ('TyObject m) -> t ('TyList 'TyStr) -> t ('TyObject m) -> Core t ('TyObject m)
  ObjTake     :: SingTy ('TyObject m) -> t ('TyList 'TyStr) -> t ('TyObject m) -> Core t ('TyObject m)
  ObjMerge    :: SingTy ('TyObject m) -> t ('TyObject m)    -> t ('TyObject m) -> Core t ('TyObject m)

  LiteralObject :: Map m -> Core t ('TyObject m)

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

-- Note [Sing Functions]:
--
-- The `sing*` family of 9 (+3) functions differs in two dimensions:
-- * The class required is `Eq`, `UserShow`, or `Show`
-- * It is applied at either `tm ('TyList a)`, `[tm a]`, or `tm a`
--
-- It looks like this should be generalizable using something like `withEq`,
-- etc. I've attempted something like this a few times but have failed every
-- time. For now I'm content to write this boilerplate by hand.

class
  ( c (tm 'TyStr)
  , c (tm 'TyInteger)
  , c (tm 'TyTime)
  , c (tm 'TyDecimal)
  , c (tm 'TyBool)
  , c (tm 'TyKeySet)
  -- , c (tm 'TyObject)
  , c (tm 'TyAny)
  , c (tm ('TyList 'TyStr))
  , c (tm ('TyList 'TyInteger))
  , c (tm ('TyList 'TyTime))
  , c (tm ('TyList 'TyDecimal))
  , c (tm ('TyList 'TyBool))
  , c (tm ('TyList 'TyKeySet))
  , c (tm ('TyList 'TyAny))
  ) => OfPactTypes c tm where
instance OfPactTypes Eq Prop      where
instance OfPactTypes Eq Invariant where
instance OfPactTypes Eq Term      where
instance OfPactTypes Show Prop      where
instance OfPactTypes Show Invariant where
instance OfPactTypes Show Term      where
instance OfPactTypes UserShow Prop      where
instance OfPactTypes UserShow Invariant where
instance OfPactTypes UserShow Term      where

singEqTmList
  :: OfPactTypes Eq tm
  => SingTy a -> tm ('TyList a) -> tm ('TyList a) -> Bool
singEqTmList ty t1 t2 = case ty of
  SStr     -> t1 == t2
  SInteger -> t1 == t2
  STime    -> t1 == t2
  SDecimal -> t1 == t2
  SBool    -> t1 == t2
  SKeySet  -> t1 == t2
  SAny     -> t1 == t2
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singEqListTm
  :: OfPactTypes Eq tm
  => SingTy a -> [tm a] -> [tm a] -> Bool
singEqListTm ty t1 t2 = case ty of
  SStr     -> t1 == t2
  SInteger -> t1 == t2
  STime    -> t1 == t2
  SDecimal -> t1 == t2
  SBool    -> t1 == t2
  SKeySet  -> t1 == t2
  SAny     -> t1 == t2
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singEqTm
  :: OfPactTypes Eq tm
  => SingTy a -> tm a -> tm a -> Bool
singEqTm ty t1 t2 = case ty of
  SStr     -> t1 == t2
  SInteger -> t1 == t2
  STime    -> t1 == t2
  SDecimal -> t1 == t2
  SBool    -> t1 == t2
  SKeySet  -> t1 == t2
  SAny     -> t1 == t2

  SList SStr     -> t1 == t2
  SList SInteger -> t1 == t2
  SList STime    -> t1 == t2
  SList SDecimal -> t1 == t2
  SList SBool    -> t1 == t2
  SList SKeySet  -> t1 == t2
  SList SAny     -> t1 == t2

  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singEqOpen
  :: OfPactTypes Eq tm
  => SingTy a -> Open x tm a -> Open x tm a -> Bool
singEqOpen ty (Open v1 nm1 a1) (Open v2 nm2 a2)
  = singEqTm ty a1 a2 && v1 == v2 && nm1 == nm2

singUserShowTmList
  :: OfPactTypes UserShow tm
  => SingTy a -> tm ('TyList a) -> Text
singUserShowTmList ty tm = case ty of
  SStr     -> userShow tm
  SInteger -> userShow tm
  STime    -> userShow tm
  SDecimal -> userShow tm
  SBool    -> userShow tm
  SKeySet  -> userShow tm
  SAny     -> userShow tm
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singUserShowListTm
  :: OfPactTypes UserShow tm
  => SingTy a -> [tm a] -> Text
singUserShowListTm ty tm = case ty of
  SStr     -> userShow tm
  SInteger -> userShow tm
  STime    -> userShow tm
  SDecimal -> userShow tm
  SBool    -> userShow tm
  SKeySet  -> userShow tm
  SAny     -> userShow tm
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singUserShowTm
  :: OfPactTypes UserShow tm
  => SingTy a -> tm a -> Text
singUserShowTm ty tm = case ty of
  SStr     -> userShow tm
  SInteger -> userShow tm
  STime    -> userShow tm
  SDecimal -> userShow tm
  SBool    -> userShow tm
  SKeySet  -> userShow tm
  SAny     -> userShow tm

  SList SStr     -> userShow tm
  SList SInteger -> userShow tm
  SList STime    -> userShow tm
  SList SDecimal -> userShow tm
  SList SBool    -> userShow tm
  SList SKeySet  -> userShow tm
  SList SAny     -> userShow tm

  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singUserShowOpen
  :: OfPactTypes UserShow tm
  => SingTy a -> Open x tm a -> Text
singUserShowOpen ty (Open _ nm a)
  = parenList [ "lambda", nm, singUserShowTm ty a ]

singShowsTmList
  :: OfPactTypes Show tm
  => SingTy a -> Int -> tm ('TyList a) -> ShowS
singShowsTmList ty p t = case ty of
  SStr     -> showsPrec p t
  SInteger -> showsPrec p t
  STime    -> showsPrec p t
  SDecimal -> showsPrec p t
  SBool    -> showsPrec p t
  SKeySet  -> showsPrec p t
  SAny     -> showsPrec p t
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singShowsListTm
  :: OfPactTypes Show tm
  => SingTy a -> Int -> [tm a] -> ShowS
singShowsListTm ty p t = case ty of
  SStr     -> showsPrec p t
  SInteger -> showsPrec p t
  STime    -> showsPrec p t
  SDecimal -> showsPrec p t
  SBool    -> showsPrec p t
  SKeySet  -> showsPrec p t
  SAny     -> showsPrec p t
  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singShowsTm
  :: OfPactTypes Show tm
  => SingTy a -> Int -> tm a -> ShowS
singShowsTm ty p t = case ty of
  SStr     -> showsPrec p t
  SInteger -> showsPrec p t
  STime    -> showsPrec p t
  SDecimal -> showsPrec p t
  SBool    -> showsPrec p t
  SKeySet  -> showsPrec p t
  SAny     -> showsPrec p t

  SList SStr     -> showsPrec p t
  SList SInteger -> showsPrec p t
  SList STime    -> showsPrec p t
  SList SDecimal -> showsPrec p t
  SList SBool    -> showsPrec p t
  SList SKeySet  -> showsPrec p t
  SList SAny     -> showsPrec p t

  SList   _ -> error "TODO"
  SObject _ -> error "TODO"

singShowsOpen
  :: OfPactTypes Show tm
  => SingTy a -> Open x tm a -> ShowS
singShowsOpen ty (Open v nm a) = showParen true $
    showsPrec 11 v
  . showString " "
  . showsPrec 11 nm
  . showString " "
  . singShowsTm ty 11 a

instance
  ( Eq (Concrete a)
  , Eq (Existential tm)
  , OfPactTypes Eq tm
  ) => Eq (Core tm a) where

  Lit a                       == Lit b                       = a == b
  Sym a                       == Sym b                       = a == b
  Var a1 b1                   == Var a2 b2                   = a1 == a2 && b1 == b2
  Identity ty1 a1             == Identity _ty2 a2            = singEqTm ty1 a1 a2
  Constantly ty1 a1 e1        == Constantly _ty2 a2 e2       = singEqTm ty1 a1 a2 && e1 == e2
  Compose tya1 tyb1 tyc1 a1 b1 c1
    == Compose tya2 tyb2 tyc2 a2 b2 c2=
    case singEq tya1 tya2 of
      Nothing -> False
      Just Refl -> case singEq tyb1 tyb2 of
        Nothing -> False
        Just Refl -> case singEq tyc1 tyc2 of
          Nothing -> False
          Just Refl -> singEqTm tya1 a1 a2 && singEqOpen tyb1 b1 b2 && singEqOpen tyc1 c1 c2
  StrConcat a1 b1             == StrConcat a2 b2             = a1 == a2 && b1 == b2
  StrLength a                 == StrLength b                 = a == b
  StrToInt s1                 == StrToInt s2                 = s1 == s2
  StrToIntBase b1 s1          == StrToIntBase b2 s2          = b1 == b2 && s1 == s2
  StrContains a1 b1           == StrContains a2 b2           = a1 == a2 && b1 == b2
  Numerical a                 == Numerical b                 = a == b
  IntAddTime a1 b1            == IntAddTime a2 b2            = a1 == a2 && b1 == b2
  DecAddTime a1 b1            == DecAddTime a2 b2            = a1 == a2 && b1 == b2
  IntegerComparison op1 a1 b1 == IntegerComparison op2 a2 b2 = op1 == op2 && a1 == a2 && b1 == b2
  DecimalComparison op1 a1 b1 == DecimalComparison op2 a2 b2 = op1 == op2 && a1 == a2 && b1 == b2
  TimeComparison op1 a1 b1    == TimeComparison op2 a2 b2    = op1 == op2 && a1 == a2 && b1 == b2
  StrComparison op1 a1 b1     == StrComparison op2 a2 b2  = op1 == op2 && a1 == a2 && b1 == b2
  BoolComparison op1 a1 b1    == BoolComparison op2 a2 b2    = op1 == op2 && a1 == a2 && b1 == b2
  KeySetEqNeq op1 a1 b1       == KeySetEqNeq op2 a2 b2       = op1 == op2 && a1 == a2 && b1 == b2
  ObjectEqNeq ty1 op1 a1 b1   == ObjectEqNeq ty2 op2 a2 b2   = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> op1 == op2 && singEqTm ty1 a1 a2 && singEqTm ty1 b1 b2
  ObjAt ty1 a1 b1             == ObjAt ty2 a2 b2             = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> a1 == a2 && singEqTm ty1 b1 b2
  ObjContains ty1 a1 b1       == ObjContains ty2 a2 b2       = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> a1 == a2 && singEqTm ty1 b1 b2
  -- ObjDrop a1 b1 c1            == ObjDrop a2 b2 c2            = a1 == a2 && b1 == b2 && c1 == c2
  -- ObjTake a1 b1 c1            == ObjTake a2 b2 c2            = a1 == a2 && b1 == b2 && c1 == c2
  -- ObjMerge a1 b1              == ObjMerge a2 b2              = a1 == a2 && b1 == b2
  -- LiteralObject m1            == LiteralObject m2            = m1 == m2
  Logical op1 args1           == Logical op2 args2           = op1 == op2 && args1 == args2

  ListEqNeq ty1 op1 a1 b1     == ListEqNeq ty2 op2 a2 b2     = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> op1 == op2 && singEqTmList ty1 a1 a2 && singEqTmList ty1 b1 b2
  ListAt ty1 a1 b1            == ListAt _ty2 a2 b2           = a1 == a2 && singEqTmList ty1 b1 b2
  ListContains ty1 a1 b1      == ListContains ty2 a2 b2      = case singEq ty1 ty2 of
    Just Refl -> singEqTm ty1 a1 a2 && singEqTmList ty1 b1 b2
    Nothing   -> False
  ListLength ty1 a1           == ListLength ty2 a2           = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> singEqTmList ty1 a1 a2
  ListDrop ty1 i1 l1          == ListDrop _ty2 i2 l2         = i1 == i2 && singEqTmList ty1 l1 l2
  ListConcat ty1 a1 b1        == ListConcat _ty2 a2 b2       = singEqTmList ty1 a1 a2 && singEqTmList ty1 b1 b2
  MakeList ty1 a1 b1          == MakeList _ty2 a2 b2         = a1 == a2 && singEqTm ty1 b1 b2
  LiteralList ty1 l1          == LiteralList _ty2 l2         = singEqListTm ty1 l1 l2
  ListMap tya1 tyb1 (Open v1 nm1 b1) as1 == ListMap tya2 _ (Open v2 nm2 b2) as2
    = case singEq tya1 tya2 of
      Nothing   -> False
      Just Refl -> singEqTm tyb1 b1 b2 && singEqTmList tya1 as1 as2
        && v1 == v2 && nm1 == nm2

  _                           == _                           = False

instance
  ( Show (Concrete a)
  , Show (Existential tm)
  , OfPactTypes Show tm
  ) => Show (Core tm a) where
  showsPrec p core = showParen (p > 10) $ case core of
    Lit a            -> showString "Lit "        . showsPrec 11 a
    Sym a            -> showString "Sym "        . showsPrec 11 a
    Var a b          -> showString "Var "        . showsPrec 11 a . showString " " . showsPrec 11 b
    Identity a b     -> showString "Identity "   . showsPrec 11 a . showString " " . singShowsTm a 11 b
    Constantly a b c ->
        showString "Constantly "
      . showsPrec 11 a
      . showString " "
      . singShowsTm a 11 b
      . showString " "
      . showsPrec 11 c
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
    StrConcat a b    -> showString "StrConcat "  . showsPrec 11 a . showString " " . showsPrec 11 b
    StrLength a      -> showString "StrLength "  . showsPrec 11 a
    StrToInt a       -> showString "StrToInt "   . showsPrec 11 a
    StrToIntBase a b -> showString "StrToIntBase " . showsPrec 11 a . showString " " . showsPrec 11 b
    StrContains  a b -> showString "StrContains " . showsPrec 11 a . showString " " . showsPrec 11 b
    Numerical a      -> showString "Numerical "  . showsPrec 11 a
    IntAddTime a b   -> showString "IntAddTime " . showsPrec 11 a . showString " " . showsPrec 11 b
    DecAddTime a b   -> showString "DecAddTime " . showsPrec 11 a . showString " " . showsPrec 11 b
    IntegerComparison op a b ->
        showString "IntegerComparison "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    DecimalComparison op a b ->
        showString "DecimalComparison "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    TimeComparison op a b ->
        showString "TimeComparison "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    StrComparison op a b ->
        showString "StrComparison "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    BoolComparison op a b ->
        showString "BoolComparison "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    KeySetEqNeq op a b ->
        showString "KeySetEqNeq "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b
    ObjectEqNeq ty op a b ->
        showString "ObjectEqNeq "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 op
      . showString " "
      . singShowsTm ty 11 a
      . showString " "
      . singShowsTm ty 11 b

    ObjAt ty a b ->
        showString "ObjAt "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 a
      . showString " "
      . singShowsTm ty 11 b
    ObjContains ty a b ->
        showString "ObjContains "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 a
      . showString " "
      . singShowsTm ty 11 b
    ObjDrop ty b c ->
        showString "ObjDrop "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 b
      . showString " "
      . singShowsTm ty 11 c
    ObjTake ty b c ->
        showString "ObjTake "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 b
      . showString " "
      . singShowsTm ty 11 c
    ObjMerge ty a b ->
        showString "ObjMerge "
      . showsPrec 11 ty
      . showString " "
      . singShowsTm ty 11 a
      . showString " "
      . singShowsTm ty 11 b
    LiteralObject _ -> showString "LiteralObject " -- TODO . showsPrec 11 m

    Logical op args ->
        showString "Logical "
      . showsPrec 11 op
      . showString " "
      . showsPrec 11 args

    ListEqNeq ty op a b ->
        showString "ListEqNeq "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 op
      . showString " "
      . singShowsTmList ty 11 a
      . showString " "
      . singShowsTmList ty 11 b
    ListAt ty a b ->
        showString "ListAt "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 a
      . showString " "
      . singShowsTmList ty 11 b
    ListContains ty a b ->
        showString "ListContains "
      . showsPrec 11 ty
      . showString " "
      . singShowsTm ty 11 a
      . showString " "
      . singShowsTmList ty 11 b
    ListLength ty a ->
        showString "ListLength "
      . showsPrec 11 ty
      . showString " "
      . singShowsTmList ty 11 a
    ListReverse ty a ->
        showString "ListReverse "
      . showsPrec 11 ty
      . showString " "
      . singShowsTmList ty 11 a
    ListSort ty a ->
        showString "ListSort "
      . showsPrec 11 ty
      . showString " "
      . singShowsTmList ty 11 a
    ListDrop ty i l ->
        showString "ListDrop "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 i
      . showString " "
      . singShowsTmList ty 11 l
    ListTake ty a b ->
        showString "ListTake "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 a
      . showString " "
      . singShowsTmList ty 11 b
    ListConcat ty a b ->
        showString "ListConcat "
      . showsPrec 11 ty
      . showString " "
      . singShowsTmList ty 11 a
      . showString " "
      . singShowsTmList ty 11 b
    MakeList ty a b ->
        showString "MakeList "
      . showsPrec 11 ty
      . showString " "
      . showsPrec 11 a
      . showString " "
      . singShowsTm ty 11 b
    LiteralList ty l ->
        showString "LiteralList "
      . showsPrec 11 ty
      . showString " "
      . singShowsListTm ty 11 l
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
      . showsPrec 11 f
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
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
      . showString " "
      . singShowsTm tya 11 a
    OrQ tya f g a ->
        showString "OrQ "
      . showsPrec 11 tya
      . showString " "
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
      . showString " "
      . singShowsTm tya 11 a
    Where tyo tya str f obj ->
        showString "Where "
      . showsPrec 11 tyo
      . showString " "
      . showsPrec 11 tya
      . showString " "
      . showsPrec 11 str
      . showString " "
      . showsPrec 11 f
      . showString " "
      . singShowsTm tyo 11 obj
    Typeof tya a ->
        showString "Typeof "
      . showsPrec 11 tya
      . showString " "
      . singShowsTm tya 11 a

instance
  ( OfPactTypes UserShow tm
  , UserShow (Concrete a)
  , UserShow (Existential tm)
  ) => UserShow (Core tm a) where
  userShowPrec d = \case
    Lit a                    -> userShowPrec d a
    Sym s                    -> tShow s
    Var _vid name            -> name
    Identity ty x            -> parenList ["identity", singUserShowTm ty x]
    Constantly ty x y        -> parenList ["constantly", singUserShowTm ty x, userShowPrec 11 y]
    Compose _ tyb tyc _ b c  -> parenList ["compose", singUserShowOpen tyb b, singUserShowOpen tyc c]
    StrConcat x y            -> parenList [SConcatenation, userShow x, userShow y]
    StrLength str            -> parenList [SStringLength, userShow str]
    StrToInt s               -> parenList [SStringToInteger, userShow s]
    StrToIntBase b s         -> parenList [SStringToInteger, userShow b, userShow s]
    StrContains needle haystack
      -> parenList [SContains, userShow needle, userShow haystack]
    Numerical tm             -> userShowPrec d tm
    IntAddTime x y           -> parenList [STemporalAddition, userShow x, userShow y]
    DecAddTime x y           -> parenList [STemporalAddition, userShow x, userShow y]
    IntegerComparison op x y -> parenList [userShow op, userShow x, userShow y]
    DecimalComparison op x y -> parenList [userShow op, userShow x, userShow y]
    TimeComparison op x y    -> parenList [userShow op, userShow x, userShow y]
    StrComparison op x y     -> parenList [userShow op, userShow x, userShow y]
    BoolComparison op x y    -> parenList [userShow op, userShow x, userShow y]
    KeySetEqNeq op x y       -> parenList [userShow op, userShow x, userShow y]
    -- ObjectEqNeq _ty op x y   -> parenList [userShow op, userShow x, userShow y]
    -- ObjAt _schema k obj _ty  -> parenList [userShow k, userShow obj]
    -- ObjContains _schema a b  -> parenList [SContains, userShow a, userShow b]
    -- ObjDrop _schema k obj    -> parenList [SObjectDrop, userShow k, userShow obj]
    -- ObjTake _schema k obj    -> parenList [SObjectTake, userShow k, userShow obj]
    ObjMerge ty x y          -> parenList [SObjectMerge, singUserShowTm ty x, singUserShowTm ty y]
    LiteralObject _obj       -> "LiteralObject TODO" -- userShow obj
    Logical op args          -> parenList $ userShow op : fmap userShow args

    ListEqNeq ty op x y      -> parenList [userShow op, singUserShowTmList ty x, singUserShowTmList ty y]
    ListAt ty k lst          -> parenList [userShow k, singUserShowTmList ty lst]
    ListContains ty needle haystack
      -> parenList [SContains, singUserShowTm ty needle, singUserShowTmList ty haystack]
    ListLength ty x          -> parenList [SListLength, singUserShowTmList ty x]
    ListReverse ty lst       -> parenList [SReverse, singUserShowTmList ty lst]
    ListSort ty lst          -> parenList [SSort, singUserShowTmList ty lst]
    ListDrop ty n lst        -> parenList [SListDrop, userShow n, singUserShowTmList ty lst]
    ListTake ty n lst        -> parenList [SListTake, userShow n, singUserShowTmList ty lst]
    ListConcat ty x y        -> parenList [SConcatenation, singUserShowTmList ty x, singUserShowTmList ty y]
    MakeList ty x y          -> parenList [SMakeList, userShow x, singUserShowTm ty y]
    LiteralList ty lst       -> singUserShowListTm ty lst
    ListMap tya tyb b as -> parenList
      [ "map"
      , singUserShowOpen tyb b
      , singUserShowTmList tya as
      ]
    ListFilter ty a b -> parenList
      [ "filter"
      , singUserShowOpen SBool a
      , singUserShowTmList ty b
      ]
    ListFold tya tyb (Open _ nm a) b c -> parenList
      [ "fold"
      , parenList [ "lambda", nm, singUserShowOpen tya a ]
      , singUserShowTm tya b
      , singUserShowTmList tyb c
      ]
    AndQ ty a b c -> parenList
      [ "and?"
      , singUserShowOpen SBool a
      , singUserShowOpen SBool b
      , singUserShowTm ty c
      ]
    OrQ ty a b c -> parenList
      [ "or?"
      , singUserShowOpen SBool a
      , singUserShowOpen SBool b
      , singUserShowTm ty c
      ]
    -- Where _ _ a b c -> parenList ["where", userShow a, singUserShowOpen SBool b, userShow c]
    Typeof ty a -> parenList ["typeof", singUserShowTm ty a]
    _ -> "TODO: UserShow (Core tm)"


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

  PropRead :: BeforeOrAfter -> Schema -> Prop TyTableName -> Prop TyRowKey -> PropSpecific ('TyObject m)

deriving instance Eq   (Concrete a) => Eq   (PropSpecific a)
deriving instance Show (Concrete a) => Show (PropSpecific a)


data Prop (a :: Ty)
  = PropSpecific (PropSpecific a)
  | CoreProp     (Core Prop a)

deriving instance Eq   (Concrete a) => Eq   (Prop a)
deriving instance Show (Concrete a) => Show (Prop a)

instance UserShow (Concrete a) => UserShow (PropSpecific a) where
  userShowPrec _d = \case
    Abort                   -> STransactionAborts
    Success                 -> STransactionSucceeds
    Result                  -> SFunctionResult
    Forall _ var ty x       -> parenList
      [SUniversalQuantification, parens (var <> ":" <> userShow ty), userShow x]
    Exists _ var ty x       -> parenList
      [SExistentialQuantification, parens (var <> ":" <> userShow ty), userShow x]
    TableWrite tab          -> parenList [STableWritten, userShow tab]
    TableRead  tab          -> parenList [STableRead, userShow tab]
    ColumnWritten tab col   -> parenList ["column-written", userShow tab, userShow col]
    ColumnRead tab col      -> parenList ["column-read", userShow tab, userShow col]
    IntCellDelta tab col rk -> parenList [SCellDelta, userShow tab, userShow col, userShow rk]
    DecCellDelta tab col rk -> parenList [SCellDelta, userShow tab, userShow col, userShow rk]
    IntColumnDelta tab col  -> parenList [SColumnDelta, userShow tab, userShow col]
    DecColumnDelta tab col  -> parenList [SColumnDelta, userShow tab, userShow col]
    RowRead tab rk          -> parenList [SRowRead, userShow tab, userShow rk]
    RowReadCount tab rk     -> parenList [SRowReadCount, userShow tab, userShow rk]
    RowWrite tab rk         -> parenList [SRowWritten, userShow tab, userShow rk]
    RowWriteCount tab rk    -> parenList [SRowWriteCount, userShow tab, userShow rk]
    KsNameAuthorized name   -> parenList [SAuthorizedBy, userShow name]
    RowEnforced tn cn rk    -> parenList [SRowEnforced, userShow tn, userShow cn, userShow rk]
    RowExists tn rk ba      -> parenList [SRowExists, userShow tn, userShow rk, userShow ba]
    PropRead ba _sch tn rk  -> parenList [SPropRead, userShow tn, userShow rk, userShow ba]

instance UserShow (Concrete a) => UserShow (Prop a) where
  userShowPrec d = \case
    PropSpecific p -> userShowPrec d p
    CoreProp     p -> userShowPrec d p

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
  true      = Lit' True
  false     = Lit' False
  bnot p    = CoreProp $ Logical NotOp [p]
  p1 &&& p2 = PAnd p1 p2
  p1 ||| p2 = POr  p1 p2

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
EQ_EXISTENTIAL(Prop)
SHOW_EXISTENTIAL(Prop)

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

pattern PKeySetEqNeq :: EqNeq -> Prop 'TyKeySet -> Prop 'TyKeySet -> Prop 'TyBool
pattern PKeySetEqNeq op x y = CoreProp (KeySetEqNeq op x y)

pattern PObjectEqNeq :: SingTy ('TyObject m) -> EqNeq -> Prop ('TyObject m) -> Prop ('TyObject m) -> Prop 'TyBool
pattern PObjectEqNeq ty op x y = CoreProp (ObjectEqNeq ty op x y)

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

deriving instance Eq   (Concrete a) => Eq   (Invariant a)
deriving instance Show (Concrete a) => Show (Invariant a)

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

instance UserShow (Concrete a) => UserShow (Invariant a) where
  userShowPrec d (CoreInvariant a) = userShowPrec d a

type EInvariant = Existential Invariant
EQ_EXISTENTIAL(Invariant)
SHOW_EXISTENTIAL(Invariant)

pattern ILiteral :: Concrete a -> Invariant a
pattern ILiteral a = CoreInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant 'TyBool] -> Invariant 'TyBool
pattern ILogicalOp op args = CoreInvariant (Logical op args)

type ETerm = Existential Term
EQ_EXISTENTIAL(Term)
SHOW_EXISTENTIAL(Term)

SHOW_EXISTENTIAL((Core Term))

data Term (a :: Ty) where
  CoreTerm        :: Core Term a -> Term a

  -- In principle, this should be a pure term, however, the analyze monad needs
  -- to be `Mergeable`. `Analyze` is, but `Query` isn't, due to having
  -- `Symbolic` in its stack.
  --
  -- TODO(joel): In principle this could be pure and applied to all the
  -- languages. Unfortunately, we can't add this to props because `Query` has
  -- `Symbolic` in its stack, so it can't do an `ite`.
  IfThenElse      :: Term 'TyBool -> (Path, Term a) -> (Path, Term a) -> Term a

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
  Read            ::              TagId -> TableName -> Schema -> Term 'TyStr ->                Term ('TyObject m)
  Write           :: SingTy ('TyObject m) -> WriteType -> TagId -> TableName -> Schema -> Term 'TyStr -> Term ('TyObject m) -> Term 'TyStr

  PactVersion     :: Term 'TyStr

  Format          :: Term 'TyStr         -> [ETerm]     -> Term 'TyStr
  FormatTime      :: Term 'TyStr         -> Term 'TyTime   -> Term 'TyStr
  ParseTime       :: Maybe (Term 'TyStr) -> Term 'TyStr -> Term 'TyTime
  Hash            :: ETerm                              -> Term 'TyStr

instance UserShow (Concrete a) => UserShow (Term a) where
  userShowPrec _ = \case
    CoreTerm tm                -> userShow tm
    IfThenElse x (_, y) (_, z) -> parenList ["if", userShow x, userShow y, userShow z]
    Let var _ _ x y            -> parenList ["let", userShow var, userShow x, userShow y]
    Sequence x y               -> Text.unlines [userShow x, userShow y]

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

    Read _ tab _ x       -> parenList ["read", userShow tab, userShow x]
    Write ty _ _ tab _ x y -> parenList ["write", userShow tab, userShow x, singUserShowTm ty y]
    PactVersion          -> parenList ["pact-version"]
    Format x y           -> parenList ["format", userShow x, userShow y]
    FormatTime x y       -> parenList ["format", userShow x, userShow y]
    ParseTime Nothing y  -> parenList ["parse-time", userShow y]
    ParseTime (Just x) y -> parenList ["parse-time", userShow x, userShow y]
    Hash x               -> parenList ["hash", userShow x]
    ReadKeySet name      -> parenList ["read-keyset", userShow name]
    ReadDecimal name     -> parenList ["read-decimal", userShow name]
    ReadInteger name     -> parenList ["read-integer", userShow name]

instance Eq   (Concrete a) => Eq   (Term a) where
  (==) = error "TODO"
instance Show (Concrete a) => Show (Term a) where
  show = error "TODO"
-- deriving instance Eq   (Concrete a) => Eq   (Term a)
-- deriving instance Show (Concrete a) => Show (Term a)

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
  Existential ty (CoreTerm (Lit l)) -> Right $ Existential ty (CoreProp (Lit l))
  Existential _ _ -> Left "can only convert (simple) values terms to props"
