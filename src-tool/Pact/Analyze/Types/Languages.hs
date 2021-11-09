{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-} -- Pretty (Core tm a)
{-# LANGUAGE ViewPatterns          #-}

-- | Type definitions for each of the languages we analyze, including the three
-- main languages of programs ('Term'), invariants ('Invariant'), and
-- properties ('Prop').
module Pact.Analyze.Types.Languages
  ( (:<:)(subP)
  , inject, project
  , (:*<:)(subP')
  , inject', project'
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
  , PactStep(..)

  , toPact
  , fromPact
  , constantToProp
  , sortLiteralObject
  , mkLiteralList
  , mkLiteralObject

  , pattern IntegerComparison
  , pattern DecimalComparison
  , pattern TimeComparison
  , pattern StrComparison
  , pattern BoolComparison
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
  , pattern PStrTake
  , pattern PStrDrop
  , pattern PStrLength
  , pattern PVar

  , singEqTm
  , singEqListTm
  , singShowsTm
  , singPrettyTm
  , singPrettyListTm

  , propToInvariant
  ) where

import           Control.Lens                  (Prism', review, preview, prism')
import           Control.Monad                 ((>=>))
import           Data.Maybe                    (fromMaybe)
import           Data.String                   (IsString (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Typeable                 ((:~:) (Refl), Proxy)
import           GHC.TypeLits                  (symbolVal, someSymbolVal,
                                                SomeSymbol(SomeSymbol))
import           Prelude                       hiding (Float)
import           Text.Show                     (showListWith)

import           Pact.Types.Pretty             (commaBraces, commaBrackets,
                                                parens, parensSep,
                                                Pretty(pretty), Doc, viaShow,
                                                vsep, prettyString, prettyList,
                                                renderPrettyString',
                                                RenderColor(RPlain))
import           Pact.Types.Persistence        (WriteType)

import           Pact.Analyze.Feature          hiding (Doc, Sym, Var, col, str,
                                                obj, dec, ks)
import           Pact.Analyze.Types.Capability
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.ObjUtil    (mkSObject, normalize)
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types
import           Pact.Analyze.Util

-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class (sub :: Ty -> *) :<: (sup :: Ty -> *) where
  subP :: Prism' (sup a) (sub a)

-- | Inject a subtype into a supertype via 'subP'
inject  :: sub :<: sup => sub a -> sup a
inject = review subP

-- | Try to project a subtype from a supertype via 'subP'
project :: sub :<: sup => sup a -> Maybe (sub a)
project = preview subP

instance f :<: f where
  subP = id

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
  subP' :: Prism' (sup a) (sub (Concrete a))

-- | Inject a subtype into a supertype via 'subP''
inject'  :: sub :*<: sup => sub (Concrete a) -> sup a
inject' = review subP'

-- | Try to project a subtype from a supertype via 'subP''
project' :: sub :*<: sup => sup a -> Maybe (sub (Concrete a))
project' = preview subP'

-- | An open term (@: b@) with a free variable (@: a@).
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
--     - @{ '<', '>', '<=', '>=' }@ apply to @{ integer, decimal, string, time }@
--     - @{ =, != }@ apply to @{ integer, decimal, string, time, bool, keyset }@
-- * literals
-- * variables
-- * logical operations
-- * string length and concatenation
-- * @add-time@
-- * @at@
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
  -- | Take on strings
  StrTake      :: t 'TyInteger -> t 'TyStr -> Core t 'TyStr
  -- | Drop on strings
  StrDrop      :: t 'TyInteger -> t 'TyStr -> Core t 'TyStr
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

  GuardEqNeq :: EqNeq -> t 'TyGuard -> t 'TyGuard -> Core t 'TyBool

  -- object ops
  ObjectEqNeq
    :: SingTy ('TyObject m1) -> SingTy ('TyObject m2)
    -> EqNeq -> t ('TyObject m1) -> t ('TyObject m2) -> Core t 'TyBool
  ObjAt       :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t a
  ObjContains :: SingTy ('TyObject m) -> t 'TyStr -> t ('TyObject m) -> Core t 'TyBool
  ObjLength   :: SingTy ('TyObject m) -> t ('TyObject m) -> Core t 'TyInteger

  -- TODO: ObjDrop and ObjTake could be combined into one
  ObjDrop
    :: SingTy ('TyObject schema)
    -> t ('TyList 'TyStr)
    -> t ('TyObject schema)
    -> Core t ('TyObject schema')
  ObjTake
    :: SingTy ('TyObject schema)
    -> t ('TyList 'TyStr)
    -> t ('TyObject schema)
    -> Core t ('TyObject schema')
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

mkLiteralObject
  :: (IsTerm tm, Monad m)
  => (forall a. String -> Existential (Core tm) -> m a)
  -> [(Text, Existential tm)]
  -> m (Existential (Core tm))
mkLiteralObject err = mkUnsortedLiteralObject err >=> sortLiteralObject err

mkUnsortedLiteralObject
  :: (IsTerm tm, Monad m)
  => (forall a. String -> Existential (Core tm) -> m a)
  -> [(Text, Existential tm)]
  -> m (Existential (Core tm))
mkUnsortedLiteralObject _ [] = pure $
  let ty = mkSObject (SingList SNil)
  in Some ty (LiteralObject ty (Object SNil))
mkUnsortedLiteralObject err ((name, Some ty tm) : tms) = do
  someObj <- mkUnsortedLiteralObject err tms
  case someObj of
    Some (SObject (SingList objTy)) (LiteralObject _ (Object obj)) ->
      case someSymbolVal (Text.unpack name) of
        SomeSymbol (_proxy :: Proxy k) -> withTypeable ty $ withSing ty $ pure $
          let sym    = SSymbol @k
              objTy' = SObjectUnsafe (SingList (SCons sym ty objTy))
          in Some objTy' $
               LiteralObject objTy' $
                 Object $ SCons sym (Column ty tm) obj
    notObj -> err "mkUnsortedLiteralObject: expected an object: " notObj

sortLiteralObject
  :: Applicative m
  => (forall a. String -> Existential (Core tm) -> m a)
  -> Existential (Core tm)
  -> m (Existential (Core tm))
sortLiteralObject err = \case
  Some _objTy (LiteralObject _ (Object obj)) -> do
    let obj'  = normalize obj
        retTy = SObjectUnsafe $ eraseList obj'
    pure $ Some retTy $ LiteralObject retTy $ Object obj'
  notObj -> err "sortLiteralObject: this must be an object: " notObj

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

-- Note [Sing Functions]:
--
-- The `sing*` family of 9 (+3) functions differs in two dimensions:
-- * The class required is `Eq`, `Pretty`, or `Show`
-- * It is applied at either `tm ('TyList a)`, `[tm a]`, or `tm a`
--
-- It looks like this should be generalizable using something like `withEq`,
-- etc. I've attempted something like this a few times but have failed every
-- time. For now I'm content to write this boilerplate by hand.

singEqTmList
  :: IsTerm tm => SingTy a -> tm ('TyList a) -> tm ('TyList a) -> Bool
singEqTmList ty t1 t2 = singEqTm (SList ty) t1 t2

singEqListTm :: IsTerm tm => SingTy a -> [tm a] -> [tm a] -> Bool
singEqListTm ty t1 t2 = and $ zipWith (singEqTm ty) t1 t2

singEqObject
  :: IsTerm tm
  => SingTy ('TyObject a) -> Object tm a -> Object tm a -> Bool
singEqObject SObjectNil (Object _) (Object _) = True
singEqObject
  (SObjectUnsafe (SingList (SCons _ colty objTy)))
  (Object (SCons _ (Column _ v1) obj1))
  (Object (SCons _ (Column _ v2) obj2))
  = singEqTm colty v1 v2 &&
    singEqObject (SObjectUnsafe (SingList objTy)) (Object obj1) (Object obj2)
singEqObject _ _ _ = False

singShowsObject
  :: IsTerm tm
  => SingTy ('TyObject a) -> Object tm a -> ShowS
singShowsObject SObjectNil (Object SNil)
  = showString "SNil'"
singShowsObject
  (SObjectUnsafe (SingList (SCons _ _ objty)))
  (Object (SCons k (Column vTy v) obj))
    = showString "SCons' (SSymbol @\""
    . showString (symbolVal k)
    . showString "\") "
    . singShowsTm vTy 11 v
    . showChar ' '
    . singShowsObject (SObjectUnsafe (SingList objty)) (Object obj)
singShowsObject _ _ = error "malformed object"

singPrettyObject
  :: IsTerm tm
  => SingTy ('TyObject a) -> Object tm a -> [Doc]
singPrettyObject SObjectNil (Object SNil) = [""]
singPrettyObject
  (SObjectUnsafe (SingList (SCons _ _ objty)))
  (Object (SCons k (Column vTy v) obj))
    = ("'" <> prettyString (symbolVal k) <> ": " <> singPrettyTm vTy v)
      : singPrettyObject (SObjectUnsafe (SingList objty)) (Object obj)
singPrettyObject _ _ = error "malformed object"

singEqOpen :: IsTerm tm => SingTy a -> Open x tm a -> Open x tm a -> Bool
singEqOpen ty (Open v1 nm1 a1) (Open v2 nm2 a2)
  = singEqTm ty a1 a2 && v1 == v2 && nm1 == nm2

singPrettyTmList :: IsTerm tm => SingTy a -> tm ('TyList a) -> Doc
singPrettyTmList ty tm = singPrettyTm (SList ty) tm

singPrettyListTm :: IsTerm tm => SingTy a -> [tm a] -> Doc
singPrettyListTm ty tms = commaBrackets $ singPrettyTm ty <$> tms

singPrettyOpen :: IsTerm tm => SingTy a -> Open x tm a -> Doc
singPrettyOpen ty (Open _ nm a)
  = parensSep [ "lambda", pretty nm, singPrettyTm ty a ]

singShowsTmList :: IsTerm tm => SingTy a -> Int -> tm ('TyList a) -> ShowS
singShowsTmList ty = singShowsTm (SList ty)

singShowsListTm :: IsTerm tm => SingTy a -> Int -> [tm a] -> ShowS
singShowsListTm ty _ = showListWith (singShowsTm ty 0)

singShowsOpen :: IsTerm tm => SingTy a -> Open x tm a -> ShowS
singShowsOpen ty (Open v nm a) = showParen True $
    showsPrec 11 v
  . showChar ' '
  . showsPrec 11 nm
  . showChar ' '
  . singShowsTm ty 11 a

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
eqNumerical _ty (BitwiseOp op1 args1) (BitwiseOp op2 args2)
  = op1 == op2 && and (zipWith eqTm args1 args2)
eqNumerical _ _ _ = False

showsNumerical :: IsTerm tm => SingTy a -> Int -> Numerical tm a -> ShowS
showsNumerical _ty p tm = showParen (p > 10) $ case tm of
  DecArithOp op a b ->
      showString "DecArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  IntArithOp op a b ->
      showString "IntArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  DecUnaryArithOp op a ->
      showString "DecUnaryArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
  IntUnaryArithOp op a ->
      showString "IntUnaryArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
  DecIntArithOp op a b ->
      showString "DecIntArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  IntDecArithOp op a b ->
      showString "IntDecArithOp "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  ModOp a b ->
      showString "ModOp "
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  RoundingLikeOp1 op a ->
      showString "RoundingLikeOp1 "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
  RoundingLikeOp2 op a b ->
      showString "RoundingLikeOp2 "
    . showsPrec 11 op
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . showsTm 11 b
  BitwiseOp op args ->
      showString "BitwiseOp "
    . showsPrec 11 op
    . showChar ' '
    . showListWith (singShowsTm SInteger 0) args

prettyNumerical :: IsTerm tm => SingTy a -> Numerical tm a -> Doc
prettyNumerical _ty = \case
  DecArithOp op a b      -> parensSep [pretty op,  prettyTm a, prettyTm b]
  IntArithOp op a b      -> parensSep [pretty op,  prettyTm a, prettyTm b]
  DecUnaryArithOp op a   -> parensSep [pretty op,  prettyTm a            ]
  IntUnaryArithOp op a   -> parensSep [pretty op,  prettyTm a            ]
  DecIntArithOp op a b   -> parensSep [pretty op,  prettyTm a, prettyTm b]
  IntDecArithOp op a b   -> parensSep [pretty op,  prettyTm a, prettyTm b]
  ModOp a b              -> parensSep [prettyTm a, prettyTm b            ]
  RoundingLikeOp1 op a   -> parensSep [pretty op,  prettyTm a            ]
  RoundingLikeOp2 op a b -> parensSep [pretty op,  prettyTm a, prettyTm b]
  BitwiseOp op args      -> parensSep $ pretty op : fmap prettyTm args

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
eqCoreTm _ (StrTake i1 l1)          (StrTake i2 l2)
  = eqTm i1 i2 && eqTm l1 l2
eqCoreTm _ (StrDrop i1 l1)          (StrDrop i2 l2)
  = eqTm i1 i2 && eqTm l1 l2
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
eqCoreTm _ (ObjLength ty1 a1)            (ObjLength ty2 a2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> singEqTm ty1 a1 a2
eqCoreTm _ty (ObjDrop ty1 b1 c1)        (ObjDrop ty2 b2 c2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> eqTm b1 b2 && singEqTm ty1 c1 c2
eqCoreTm _ty (ObjTake ty1 b1 c1)        (ObjTake ty2 b2 c2)
  = case singEq ty1 ty2 of
    Nothing   -> False
    Just Refl -> eqTm b1 b2 && singEqTm ty1 c1 c2
eqCoreTm _ (ObjMerge ty11 ty21 a1 b1)          (ObjMerge ty12 ty22 a2 b2)
  = fromMaybe False $ do
    Refl <- singEq ty11 ty12
    Refl <- singEq ty21 ty22
    pure $ singEqTm ty11 a1 a2 && singEqTm ty21 b1 b2
eqCoreTm ty (LiteralObject _ m1)            (LiteralObject _ m2)
  = singEqObject ty m1 m2
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
  Var a b          -> showString "Var "        . showsPrec 11 a . showChar ' ' . showsPrec 11 b
  Identity a b     -> showString "Identity "   . showsPrec 11 a . showChar ' ' . singShowsTm a 11 b
  Constantly tyb a b ->
      showString "Constantly "
    . showsPrec 11 ty
    . showChar ' '
    . singShowsTm ty 11 a
    . showChar ' '
    . singShowsTm tyb 11 b
  Compose tya tyb tyc a b c ->
      showString "Compose "
    . showsPrec 11 tya
    . showChar ' '
    . showsPrec 11 tyb
    . showChar ' '
    . showsPrec 11 tyc
    . showChar ' '
    . singShowsTm tya 11 a
    . singShowsOpen tyb b
    . showChar ' '
    . singShowsOpen tyc c
  StrConcat a b    -> showString "StrConcat "    . showsTm 11 a . showChar ' ' . showsTm 11 b
  StrTake a b      -> showString "StrTake "      . showsTm 11 a . showChar ' ' . showsTm 11 b
  StrDrop a b      -> showString "StrDrop "      . showsTm 11 a . showChar ' ' . showsTm 11 b
  StrLength a      -> showString "StrLength "    . showsTm 11 a
  StrToInt a       -> showString "StrToInt "     . showsTm 11 a
  StrToIntBase a b -> showString "StrToIntBase " . showsTm 11 a . showChar ' ' . showsTm 11 b
  StrContains  a b -> showString "StrContains "  . showsTm 11 a . showChar ' ' . showsTm 11 b
  Numerical a      -> showString "Numerical "    . showsNumerical ty 11 a
  IntAddTime a b   -> showString "IntAddTime "   . showsTm 11 a . showChar ' ' . showsTm 11 b
  DecAddTime a b   -> showString "DecAddTime "   . showsTm 11 a . showChar ' ' . showsTm 11 b
  Comparison ty' op a b ->
      showString "Comparison "
    . showsPrec 11 ty'
    . showChar ' '
    . showsPrec 11 op
    . showChar ' '
    . singShowsTm ty' 11 a
    . showChar ' '
    . singShowsTm ty' 11 b

  GuardEqNeq op a b ->
      showString "GuardEqNeq"
    . showsPrec 11 op
    . showString " "
    . showsTm 11 a
    . showString " "
    . showsTm 11 b

  ObjectEqNeq ty1 ty2 op a b ->
      showString "ObjectEqNeq "
    . showsPrec 11 ty1
    . showChar ' '
    . showsPrec 11 ty2
    . showChar ' '
    . showsPrec 11 op
    . showChar ' '
    . singShowsTm ty1 11 a
    . showChar ' '
    . singShowsTm ty2 11 b

  ObjAt ty' a b ->
      showString "ObjAt "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . singShowsTm ty' 11 b
  ObjContains ty' a b ->
      showString "ObjContains "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . singShowsTm ty' 11 b
  ObjLength ty' a ->
      showString "ObjLength "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTm ty' 11 a
  ObjDrop a b c ->
      showString "ObjDrop "
    . showsPrec 11 a
    . showChar ' '
    . showsTm 11 b
    . showChar ' '
    . singShowsTm a 11 c
  ObjTake a b c ->
      showString "ObjTake "
    . showsPrec 11 a
    . showChar ' '
    . showsTm 11 b
    . showChar ' '
    . singShowsTm a 11 c
  ObjMerge ty1 ty2 a b ->
      showString "ObjMerge "
    . showsPrec 11 ty1
    . showChar ' '
    . showsPrec 11 ty2
    . showChar ' '
    . singShowsTm ty1 11 a
    . showChar ' '
    . singShowsTm ty2 11 b
  LiteralObject ty' obj ->
      showString "LiteralObject ("
    . singShowsObject ty' obj
    . showChar ')'

  Logical op args ->
      showString "Logical "
    . showsPrec 11 op
    . showChar ' '
    . showListWith (singShowsTm SBool 0) args

  ListEqNeq ty' op a b ->
      showString "ListEqNeq "
    . showsPrec 11 ty'
    . showChar ' '
    . showsPrec 11 op
    . showChar ' '
    . singShowsTmList ty' 11 a
    . showChar ' '
    . singShowsTmList ty' 11 b
  ListAt ty' a b ->
      showString "ListAt "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . singShowsTmList ty' 11 b
  ListContains ty' a b ->
      showString "ListContains "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTm ty' 11 a
    . showChar ' '
    . singShowsTmList ty' 11 b
  ListLength ty' a ->
      showString "ListLength "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTmList ty' 11 a
  ListReverse ty' a ->
      showString "ListReverse "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTmList ty' 11 a
  ListSort ty' a ->
      showString "ListSort "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTmList ty' 11 a
  ListDrop ty' i l ->
      showString "ListDrop "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 i
    . showChar ' '
    . singShowsTmList ty' 11 l
  ListTake ty' a b ->
      showString "ListTake "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . singShowsTmList ty' 11 b
  ListConcat ty' a b ->
      showString "ListConcat "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsTmList ty' 11 a
    . showChar ' '
    . singShowsTmList ty' 11 b
  MakeList ty' a b ->
      showString "MakeList "
    . showsPrec 11 ty'
    . showChar ' '
    . showsTm 11 a
    . showChar ' '
    . singShowsTm ty' 11 b
  LiteralList ty' l ->
      showString "LiteralList "
    . showsPrec 11 ty'
    . showChar ' '
    . singShowsListTm ty' 11 l
  ListMap tya tyb b as ->
      showString "ListMap "
    . showsPrec 11 tya
    . showChar ' '
    . showsPrec 11 tyb
    . showChar ' '
    . singShowsOpen tyb b
    . showChar ' '
    . singShowsTmList tya 11 as
  ListFilter tya f as ->
      showString "ListFilter "
    . showsPrec 11 tya
    . showChar ' '
    . singShowsOpen sing f
    . showChar ' '
    . singShowsTmList tya 11 as
  ListFold tya tyb (Open vid nm f) a bs ->
      showString "ListFold "
    . showsPrec 11 tya
    . showChar ' '
    . showsPrec 11 tyb
    . showString " (Open "
      . showsPrec 11 vid
      . showChar ' '
      . showsPrec 11 nm
      . singShowsOpen tya f
    . showString ") "
    . singShowsTm tya 11 a
    . showChar ' '
    . singShowsTmList tyb 11 bs
  AndQ tya f g a ->
      showString "AndQ "
    . showsPrec 11 tya
    . showChar ' '
    . singShowsOpen sing f
    . showChar ' '
    . singShowsOpen sing g
    . showChar ' '
    . singShowsTm tya 11 a
  OrQ tya f g a ->
      showString "OrQ "
    . showsPrec 11 tya
    . showChar ' '
    . singShowsOpen sing f
    . showChar ' '
    . singShowsOpen sing g
    . showChar ' '
    . singShowsTm tya 11 a
  Where tyo tya str f obj ->
      showString "Where "
    . showsPrec 11 tyo
    . showChar ' '
    . showsPrec 11 tya
    . showChar ' '
    . showsTm 11 str
    . showChar ' '
    . singShowsOpen sing f
    . showChar ' '
    . singShowsTm tyo 11 obj
  Typeof tya a ->
      showString "Typeof "
    . showsPrec 11 tya
    . showChar ' '
    . singShowsTm tya 11 a

prettyCore :: IsTerm tm => SingTy ty -> Core tm ty -> Doc
prettyCore ty = \case
  Lit a                    -> withPretty ty $ pretty a
  Sym s                    -> viaShow s
  Var _vid name            -> pretty name
  Identity ty' x           -> parensSep [pretty SIdentity, singPrettyTm ty' x]
  Constantly tyb a b       -> parensSep [pretty SConstantly, singPrettyTm ty a, singPrettyTm tyb b]
  Compose _ tyb tyc _ b c  -> parensSep [pretty SCompose, singPrettyOpen tyb b, singPrettyOpen tyc c]
  StrConcat x y            -> parensSep [pretty SConcatenation, prettyTm x, prettyTm y]
  StrTake l r              -> parensSep [pretty SStringTake, prettyTm l, prettyTm r]
  StrDrop l r              -> parensSep [pretty SStringDrop, prettyTm l, prettyTm r]
  StrLength str            -> parensSep [pretty SStringLength, prettyTm str]
  StrToInt s               -> parensSep [pretty SStringToInteger, prettyTm s]
  StrToIntBase b s         -> parensSep [pretty SStringToInteger, prettyTm b, prettyTm s]
  StrContains needle haystack
    -> parensSep [pretty SContains, prettyTm needle, prettyTm haystack]
  Numerical tm             -> prettyNumerical ty tm
  IntAddTime x y           -> parensSep [pretty STemporalAddition, prettyTm x, prettyTm y]
  DecAddTime x y           -> parensSep [pretty STemporalAddition, prettyTm x, prettyTm y]
  Comparison ty' op x y    -> parensSep [pretty op, singPrettyTm ty' x, singPrettyTm ty' y]
  GuardEqNeq op x y        -> parensSep [pretty op, prettyTm x, prettyTm y]
  ObjectEqNeq ty1 ty2 op x y
    -> parensSep [pretty op, singPrettyTm ty1 x, singPrettyTm ty2 y]
  ObjAt ty' k obj          -> parensSep [pretty SObjectProjection, prettyTm k, singPrettyTm ty' obj]
  ObjContains ty' k obj    -> parensSep [pretty SContains, prettyTm k, singPrettyTm ty' obj]
  ObjLength ty' obj        -> parensSep [pretty SObjectLength, singPrettyTm ty' obj]
  ObjDrop ty' ks obj       -> parensSep [pretty SObjectDrop, prettyTm ks, singPrettyTm ty' obj]
  ObjTake ty' ks obj       -> parensSep [pretty SObjectTake, prettyTm ks, singPrettyTm ty' obj]
  ObjMerge ty1 ty2 x y     -> parensSep [pretty SObjectMerge, singPrettyTm ty1 x, singPrettyTm ty2 y]
  LiteralObject ty' obj    -> commaBraces (singPrettyObject ty' obj)
  Logical op args          -> parensSep $ pretty op : fmap prettyTm args

  ListEqNeq ty' op x y     -> parensSep [pretty op, singPrettyTmList ty' x, singPrettyTmList ty' y]
  ListAt ty' k lst         -> parensSep [prettyTm k, singPrettyTmList ty' lst]
  ListContains ty' needle haystack
    -> parensSep [pretty SContains, singPrettyTm ty' needle, singPrettyTmList ty' haystack]
  ListLength ty' x         -> parensSep [pretty SListLength, singPrettyTmList ty' x]
  ListReverse ty' lst      -> parensSep [pretty SReverse, singPrettyTmList ty' lst]
  ListSort ty' lst         -> parensSep [pretty SSort, singPrettyTmList ty' lst]
  ListDrop ty' n lst       -> parensSep [pretty SListDrop, prettyTm n, singPrettyTmList ty' lst]
  ListTake ty' n lst       -> parensSep [pretty SListTake, prettyTm n, singPrettyTmList ty' lst]
  ListConcat ty' x y       -> parensSep [pretty SConcatenation, singPrettyTmList ty' x, singPrettyTmList ty' y]
  MakeList ty' x y         -> parensSep [pretty SMakeList, prettyTm x, singPrettyTm ty' y]
  LiteralList ty' lst      -> singPrettyListTm ty' lst
  ListMap tya tyb b as -> parensSep
    [ pretty SMap
    , singPrettyOpen tyb b
    , singPrettyTmList tya as
    ]
  ListFilter ty' a b -> parensSep
    [ pretty SFilter
    , singPrettyOpen SBool a
    , singPrettyTmList ty' b
    ]
  ListFold tya tyb (Open _ nm a) b c -> parensSep
    [ pretty SFold
    , parensSep [ "lambda", pretty nm, singPrettyOpen tya a ]
    , singPrettyTm tya b
    , singPrettyTmList tyb c
    ]
  AndQ ty' a b c -> parensSep
    [ pretty SAndQ
    , singPrettyOpen SBool a
    , singPrettyOpen SBool b
    , singPrettyTm ty' c
    ]
  OrQ ty' a b c -> parensSep
    [ pretty SOrQ
    , singPrettyOpen SBool a
    , singPrettyOpen SBool b
    , singPrettyTm ty' c
    ]
  Where tyobj _tya k f obj -> parensSep
    [ pretty SWhere
    , prettyTm k
    , singPrettyOpen SBool f
    , singPrettyTm tyobj obj
    ]
  Typeof ty' a -> parensSep [pretty STypeof, singPrettyTm ty' a]


data BeforeOrAfter = Before | After
  deriving (Eq, Show)

instance Pretty BeforeOrAfter where
  pretty = \case
    Before -> "'before"
    After  -> "'after"

-- | Property-specific constructions.
--
-- This encompasses every construction that can appear in a 'Prop' that's not
-- in 'Core'.
data PropSpecific (a :: Ty) where

  -- TX success/failure

  --
  -- TODO: remove either Success or Abort.
  --

  -- | Whether a transaction aborts (does not succeed)
  Abort     :: PropSpecific 'TyBool
  -- | Whether a transaction succeeds (does not abort)
  Success   :: PropSpecific 'TyBool
  -- | Whether the governance predicate passes
  GovPasses :: PropSpecific 'TyBool
  -- | The return value of the function under examination
  Result    :: PropSpecific a

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

  -- | Whether a transaction satisfies the named guard in the registry
  GuardPassed :: RegistryName                                            -> PropSpecific 'TyBool
  -- | Whether a row has its keyset @enforce@d in a transaction
  RowEnforced :: Prop TyTableName  -> Prop TyColumnName -> Prop TyRowKey -> PropSpecific 'TyBool

  PropRead :: SingTy ('TyObject m) -> BeforeOrAfter -> Prop TyTableName -> Prop TyRowKey -> PropSpecific ('TyObject m)

deriving instance SingI a => Show (PropSpecific a)
deriving instance SingI a => Eq   (PropSpecific a)

data Prop (a :: Ty)
  = PropSpecific (PropSpecific a)
  | CoreProp     (Core Prop a)

instance Pretty (PropSpecific a) where
  pretty = \case
    Abort                   -> pretty STransactionAborts
    Success                 -> pretty STransactionSucceeds
    GovPasses               -> pretty SGovernancePasses
    Result                  -> pretty SFunctionResult
    Forall _ var ty x       -> parensSep
      [pretty SUniversalQuantification, parens (pretty var <> ":" <> pretty ty), prettyTm x]
    Exists _ var ty x       -> parensSep
      [pretty SExistentialQuantification, parens (pretty var <> ":" <> pretty ty), prettyTm x]
    TableWrite tab          -> parensSep [pretty STableWritten, prettyTm tab]
    TableRead  tab          -> parensSep [pretty STableRead, prettyTm tab]
    ColumnWritten tab col   -> parensSep ["column-written", prettyTm tab, prettyTm col]
    ColumnRead tab col      -> parensSep ["column-read", prettyTm tab, prettyTm col]
    IntCellDelta tab col rk -> parensSep [pretty SCellDelta, prettyTm tab, prettyTm col, prettyTm rk]
    DecCellDelta tab col rk -> parensSep [pretty SCellDelta, prettyTm tab, prettyTm col, prettyTm rk]
    IntColumnDelta tab col  -> parensSep [pretty SColumnDelta, prettyTm tab, prettyTm col]
    DecColumnDelta tab col  -> parensSep [pretty SColumnDelta, prettyTm tab, prettyTm col]
    RowRead tab rk          -> parensSep [pretty SRowRead, prettyTm tab, prettyTm rk]
    RowReadCount tab rk     -> parensSep [pretty SRowReadCount, prettyTm tab, prettyTm rk]
    RowWrite tab rk         -> parensSep [pretty SRowWritten, prettyTm tab, prettyTm rk]
    RowWriteCount tab rk    -> parensSep [pretty SRowWriteCount, prettyTm tab, prettyTm rk]
    GuardPassed name        -> parensSep [pretty SAuthorizedBy, pretty name]
    RowEnforced tn cn rk    -> parensSep [pretty SRowEnforced, prettyTm tn, prettyTm cn, prettyTm rk]
    RowExists tn rk ba      -> parensSep [pretty SRowExists, prettyTm tn, prettyTm rk, pretty ba]
    PropRead _ty ba tn rk   -> parensSep [pretty SPropRead, prettyTm tn, prettyTm rk, pretty ba]

instance S :*<: Prop where
  subP' = prism' (CoreProp . Sym) $ \case
    CoreProp (Sym a) -> Just a
    _                -> Nothing

instance PropSpecific :<: Prop where
  subP = prism' PropSpecific $ \case
    PropSpecific a -> Just a
    _              -> Nothing

instance Core Prop :<: Prop where
  subP = prism' CoreProp $ \case
    CoreProp a -> Just a
    _          -> Nothing

instance Numerical Prop :<: Prop where
  subP = prism' (Inj . Numerical) $ \case
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

-- Try to build a literal list of @a@s and if that fails builds a
-- @LiteralList@.
data ListBuilder tm where
  ListBuilder
    :: Core tm :<: tm
    => SingTy a
    -> Maybe [Concrete a]
    -> Maybe (Core tm ('TyList a))
    -> ListBuilder tm

-- | This makes a literal list in one of two forms.
--
-- If possible we'd like to build a @Lit@ which is entirely concrete (@Lit ::
-- Concrete a -> Core t a@). However, we can't always concretize the entire
-- thing, so we build a @LiteralList@ (@LiteralList  :: SingTy a -> [t a] ->
-- Core t ('TyList a)@).
mkLiteralList
  :: Core tm :<: tm
  => [Existential tm]
  -> Maybe (Existential (Core tm))
mkLiteralList tms = case mkLiteralList' tms of
  ListBuilder ty (Just lits) _          -> Just (Some (SList ty) (Lit lits))
  ListBuilder ty Nothing (Just litList) -> Just (Some (SList ty) litList)
  ListBuilder _ Nothing Nothing         -> Nothing

mkLiteralList'
  :: forall tm. Core tm :<: tm
  => [Existential tm]
  -> ListBuilder tm
mkLiteralList' []
  = ListBuilder SAny
    (Just [])
    (Just $ LiteralList SAny [])

mkLiteralList' xs@(Some ty0 _ : _) = foldr
  (\case
    Some ty y -> \case
      ListBuilder ty' mLits mLitList ->

        -- If there's a wrong type we give up on both lists
        fromMaybe (ListBuilder ty Nothing Nothing) $ do
          Refl <- singEq ty ty'

          pure $ ListBuilder ty
            (do ys     <- mLits
                Lit y' <- project @(Core tm) @tm y
                Just $ y' : ys)
            (do LiteralList _ty tms <- mLitList
                Just $ LiteralList ty' $ Inj y : tms))
  (ListBuilder ty0 (Just []) (Just $ LiteralList ty0 []))
  xs

pattern Lit' :: forall tm ty. Core tm :<: tm => Concrete ty -> tm ty
pattern Lit' a <- (project @(Core tm) @tm -> Just (Lit a)) where
  Lit' a = inject @(Core tm) @tm (Lit a)
-- maybe this will work in the future:
-- pattern Lit' a = Inj @(Core tm) @tm (Lit a)

pattern StrLit :: forall tm. Core tm :<: tm => String -> tm 'TyStr
pattern StrLit str = Lit' (Str str)

pattern TextLit :: forall tm. Core tm :<: tm => Text -> tm 'TyStr
pattern TextLit str <- Lit' (Str (Text.pack -> str)) where
  TextLit str = Lit' (Str (Text.unpack str))

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = CoreProp (Var vid name)

pattern PNumerical :: Numerical Prop t -> Prop t
pattern PNumerical x = CoreProp (Numerical x)

pattern PStrConcat :: Prop 'TyStr -> Prop 'TyStr -> Prop 'TyStr
pattern PStrConcat x y = CoreProp (StrConcat x y)

pattern PStrTake :: Prop 'TyInteger -> Prop 'TyStr -> Prop 'TyStr
pattern PStrTake x y = CoreProp (StrTake x y)

pattern PStrDrop :: Prop 'TyInteger -> Prop 'TyStr -> Prop 'TyStr
pattern PStrDrop x y = CoreProp (StrDrop x y)

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
  subP = prism' CoreInvariant $ \(CoreInvariant a) -> Just a

instance Numerical Invariant :<: Invariant where
  subP = prism' (Inj . Numerical) $ \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance S :*<: Invariant where
  subP' = prism' (CoreInvariant . Sym) $ \case
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

  -- Control flow
  Sequence        :: ETerm -> Term a -> Term a

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
  Let             :: Text  -> VarId -> ETerm -> Term a -> Term a
  -- A return value from a scope; this exists only so we can "tag" the return value
  Return          :: TagId -> Term a                   -> Term a

  -- Conditional transaction abort
  Enforce         :: Maybe TagId -> Term 'TyBool   -> Term 'TyBool -- Only a TagId for an assertion; i.e. not keyset enforcement
  -- Left to be tagged if the list of cases is empty. We do this because we
  -- need a way to signal a failure due to this particular scenario in model
  -- reporting. Right _1 to be tagged if the case fails, Right _2 to be tagged
  -- if the case succeeds:
  EnforceOne      :: Either TagId [((Path, Path), Term 'TyBool)] -> Term 'TyBool

  -- Capabilities
  WithCapability  :: ETerm      -> Term a            -> Term a
  Granting        :: Capability -> [VarId] -> Term a -> Term a
  HasGrant        :: TagId -> Capability -> [(Text, VarId)] -> Term 'TyBool

  -- Reading from environment
  ReadKeySet      :: Term 'TyStr -> Term 'TyGuard
  ReadDecimal     :: Term 'TyStr -> Term 'TyDecimal
  ReadInteger     :: Term 'TyStr -> Term 'TyInteger
  ReadString      :: Term 'TyStr -> Term 'TyStr

  -- TODO: ReadMsg

  PactId    ::                         Term 'TyStr
  ChainData :: SingTy ('TyObject m) -> Term ('TyObject m)

  -- Guards
  MkKsRefGuard  :: Term 'TyStr                  -> Term 'TyGuard
  MkPactGuard   :: Term 'TyStr                  -> Term 'TyGuard
  MkUserGuard   :: Guard       -> ETerm         -> Term 'TyGuard
  MkModuleGuard :: Term 'TyStr                  -> Term 'TyGuard
  GuardPasses   :: TagId       -> Term 'TyGuard -> Term 'TyBool

  -- Table access
  Read
    :: SingTy ('TyObject row)        -- schema of the full row
    -> SingTy ('TyObject proj)       -- schema of the subset we're projecting
    -> Maybe (Term ('TyObject proj)) -- optional default values for the subset
    -> TagId -> TableName -> Term 'TyStr
    -> Term ('TyObject proj)
  Write
    :: SingTy ('TyObject m)
    -> WriteType -> TagId -> TableName
    -> Term 'TyStr -> Term ('TyObject m) -> Term 'TyStr

  PactVersion     :: Term 'TyStr

  Format          :: Term 'TyStr         -> [ETerm]      -> Term 'TyStr
  FormatTime      :: Term 'TyStr         -> Term 'TyTime -> Term 'TyStr
  ParseTime       :: Maybe (Term 'TyStr) -> Term 'TyStr  -> Term 'TyTime
  Hash            :: ETerm                               -> Term 'TyStr

  -- Pacts
  Pact   :: [PactStep]      -> Term 'TyStr
  Yield  :: TagId -> Term a -> Term a
  Resume :: TagId ->           Term a

data PactStep where
  Step
    :: (Term a, SingTy a)  -- exec
    -> Path                -- corresponds to the graph edge for this step
    -> Maybe (Term 'TyStr) -- entity
    -- first:  Nothing
    -- others: Just
    -> Maybe (Path, VarId) -- does this step cancel. invariant: the first
                           -- step does not have this vid. all other steps do.
                           -- "C"
    -- last:   Nothing
    -- others: Just or Nothing
    -> Maybe (Path, ETerm) -- rollback (R1)
    -> PactStep

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
  Let a b c d ->
      showString "Let "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
    . showChar ' '
    . showsPrec 11 d
  Return a b ->
      showString "Return "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
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
  WithCapability a b ->
      showString "WithCapability "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
  Granting a b c ->
      showString "Granting "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
  HasGrant a b c ->
      showString "HasGrant "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
  MkKsRefGuard a ->
      showString "MkKsRefGuard "
    . showsPrec 11 a
  MkPactGuard a ->
      showString "MkPactGuard "
    . showsPrec 11 a
  MkUserGuard a b ->
      showString "MkUserGuard "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
  MkModuleGuard a ->
      showString "MkModuleGuard "
    . showsPrec 11 a
  GuardPasses a b ->
      showString "GuardPasses "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b

  Read a b c d e f ->
      showString "Read "
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
  Hash a           -> showString "Hash " . showsPrec 11 a
  ReadKeySet  name -> showString "ReadKeySet " . showsPrec 11 name
  ReadDecimal name -> showString "ReadDecimal " . showsPrec 11 name
  ReadInteger name -> showString "ReadInteger " . showsPrec 11 name
  ReadString  name -> showString "ReadString " . showsPrec 11 name
  PactId           -> showString "PactId"
  ChainData a      -> showString "ChainData " . showsPrec 11 a
  Pact steps       -> showString "Pact " . showList steps
  Yield tid a      ->
    showString "Yield " . showsPrec 11 tid . showChar ' ' . singShowsTm ty 11 a
  Resume tid       -> showString "Resume " . showsPrec 11 tid

instance Show PactStep where
  showsPrec _ (Step (exec , execTy) path mEntity mCancelVid mRollback) =
     showString "Step "
   . withSing execTy (showsTm 11 exec)
   . showChar ' '
   . showsPrec 11 path
   . showChar ' '
   . showsPrec 11 mEntity
   . showChar ' '
   . showsPrec 11 mCancelVid
   . showChar ' '
   . showsPrec 11 mRollback

showsProp :: SingTy ty -> Int -> Prop ty -> ShowS
showsProp ty p = withSing ty $ \case
  PropSpecific tm -> showsPrec p tm
  CoreProp     tm -> showsPrecCore ty p tm

eqProp :: SingTy ty -> Prop ty -> Prop ty -> Bool
eqProp ty (CoreProp     a) (CoreProp     b) = eqCoreTm ty a b
eqProp ty (PropSpecific a) (PropSpecific b) = withSing ty $ a == b
eqProp _  _                _                = False

prettyTerm :: SingTy ty -> Term ty -> Doc
prettyTerm ty = \case
  CoreTerm tm -> prettyCore ty tm
  IfThenElse ty' x (_, y) (_, z) -> parensSep
    [ "if"
    , prettyTm x
    , singPrettyTm ty' y
    , singPrettyTm ty' z
    ]
  Let var _ x y -> parensSep
    [ "let"
    , parensSep
      [ parensSep
        [ pretty var
        , pretty x
        ]
      ]
    , prettyTerm ty y
    ]
  Return _ t -> prettyTerm ty t
  Sequence x y -> vsep [pretty x, prettyTerm ty y]

  EnforceOne (Left _)        -> parensSep
    [ "enforce-one"
    , "\"(generated enforce-one)\""
    , prettyList ([] :: [Integer])
    ]
  EnforceOne (Right x)       -> parensSep
    [ "enforce-one"
    , "\"(generated enforce-one)\""
    , prettyList $ fmap snd x
    ]

  Enforce _ (GuardPasses _ x) -> parensSep ["enforce-guard", pretty x]
  Enforce _ (HasGrant _ c vs) -> parensSep ["require-capability", parensSep (pretty c : map (pretty . fst) vs)]
  Enforce _ x                 -> parensSep ["enforce", pretty x]
  GuardPasses _ _
    -> error "GuardPasses should only appear inside of an Enforce"

  WithCapability a x    -> parensSep ["with-capability", pretty a, prettyTerm ty x]
  Granting _ _ x        -> prettyTerm ty x
  HasGrant _ _ _        -> error "HasGrant should only appear inside of an Enforce"
  Read _ _ Nothing    _ tab x -> parensSep ["read", pretty tab, pretty x]
  Read _ _ (Just def) _ tab x -> parensSep ["with-default-read", singPrettyTm ty def, pretty tab, pretty x]
  Write ty' _ _ tab x y -> parensSep ["write", pretty tab, pretty x, singPrettyTm ty' y]
  PactVersion           -> parensSep ["pact-version"]
  Format x y            -> parensSep ["format", pretty x, prettyList y]
  FormatTime x y        -> parensSep ["format", pretty x, pretty y]
  ParseTime Nothing y   -> parensSep ["parse-time", pretty y]
  ParseTime (Just x) y  -> parensSep ["parse-time", pretty x, pretty y]
  Hash x                -> parensSep ["hash", pretty x]
  ReadKeySet name       -> parensSep ["read-keyset", pretty name]
  ReadDecimal name      -> parensSep ["read-decimal", pretty name]
  ReadInteger name      -> parensSep ["read-integer", pretty name]
  ReadString name       -> parensSep ["read-string", pretty name]
  PactId                -> parensSep ["pact-id"]
  ChainData _           -> parensSep ["chain-data"]
  MkKsRefGuard name     -> parensSep ["keyset-ref-guard", pretty name]
  MkPactGuard name      -> parensSep ["create-pact-guard", pretty name]
  MkUserGuard g t       -> parensSep ["create-user-guard", pretty g, pretty t]
  MkModuleGuard name    -> parensSep ["create-module-guard", pretty name]
  Pact steps            -> vsep (pretty <$> steps)
  Yield _tid tm         -> parensSep [ "yield", singPrettyTm ty tm ]
  Resume _tid           -> "resume"

instance Pretty PactStep where
  pretty (Step (exec , execTy) _ mEntity _ mRollback) = parensSep $
    maybe ["step"] (\_ -> ["step-with-rollback"]) mRollback
      ++ maybe [] (\entity -> [prettyTm entity]) mEntity
      ++ [singPrettyTm execTy exec]
      ++ maybe [] (\(_path, Some  ty' tm) -> [singPrettyTm ty' tm]) mRollback

eqTerm :: SingTy ty -> Term ty -> Term ty -> Bool
eqTerm ty (CoreTerm a1) (CoreTerm a2) = singEqTm ty a1 a2
eqTerm ty (IfThenElse _ty1 a1 (b1, c1) (d1, e1))
          (IfThenElse _ty2 a2 (b2, c2) (d2, e2))
  = eqTm a1 a2 && b1 == b2 && singEqTm ty c1 c2 && d1 == d2 && singEqTm ty e1 e2
eqTerm ty (Let a1 b1 c1 d1) (Let a2 b2 c2 d2)
  = a1 == a2 && b1 == b2 && c1 == c2 && singEqTm ty d1 d2
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
eqTerm _ty (ReadString a) (ReadString b)
  = a == b
eqTerm _ty (GuardPasses a1 b1) (GuardPasses a2 b2)
  = a1 == a2 && b1 == b2
eqTerm _ty (Read _ _ Nothing a1 b1 c1) (Read _ _ Nothing a2 b2 c2)
  = a1 == a2 && b1 == b2 && c1 == c2
eqTerm ty (Read _ _ (Just a1) b1 c1 d1) (Read _ _ (Just a2) b2 c2 d2)
  = singEqTm ty a1 a2 && b1 == b2 && c1 == c2 && d1 == d2
eqTerm _ty Read{} Read{}
  = False
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
  subP' = prism' (CoreTerm . Sym) $ \case
    CoreTerm (Sym a) -> Just a
    _                -> Nothing

instance Core Term :<: Term where
  subP = prism' CoreTerm $ \case
    CoreTerm a -> Just a
    _          -> Nothing

instance Numerical Term :<: Term where
  subP = prism' (Inj . Numerical) $ \case
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

constantToProp :: ETerm -> Either String EProp
constantToProp = \case
  Some ty (CoreTerm (Lit l))
    -> Right $ Some ty (CoreProp (Lit l))
  Some ty t -> Left $
    "encountered unexpected non-literal for constant: " ++ withSing ty (show t)

-- Note [instances]:
-- The following nine instances seem like they should be
--
--     instance (IsTerm tm, SingI ty) => Pretty (tm ty) where
--
-- Unfortunately, that's a nightmare in terms of overlap. So we implement an
-- instance for each term type.

instance SingI ty => Pretty (Term ty) where
  pretty = singPrettyTm sing

instance SingI ty => Pretty (Prop ty) where
  pretty = singPrettyTm sing

instance SingI ty => Pretty (Invariant ty) where
  pretty = singPrettyTm sing

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
  singEqTm        = eqTerm
  singShowsTm     = showsTerm
  singPrettyTm ty = prettyTerm ty

instance IsTerm Prop where
  singEqTm        = eqProp
  singShowsTm     = showsProp
  singPrettyTm ty = \case
    PropSpecific tm -> pretty tm
    CoreProp     tm -> singPrettyTm ty tm

instance IsTerm Invariant where
  singEqTm ty (CoreInvariant tm1) (CoreInvariant tm2) = singEqTm ty tm1 tm2
  singShowsTm ty p (CoreInvariant tm)                 = singShowsTm ty p tm
  singPrettyTm ty (CoreInvariant tm)                  = singPrettyTm ty tm

instance IsTerm tm => IsTerm (Core tm) where
  singEqTm        = eqCoreTm
  singShowsTm     = showsPrecCore
  singPrettyTm ty = prettyCore ty

propToInvariant :: Prop t -> Either String (Invariant t)
propToInvariant (PropSpecific ps) = Left $
  "encountered a term that's only valid in properties, and not invariants: " ++
    renderPrettyString' RPlain (pretty ps)
propToInvariant (CoreProp core) = CoreInvariant <$> case core of
  Lit x ->
    pure $ Lit x
  Sym s ->
    pure $ Sym s
  Var vid nm ->
    pure $ Var vid nm
  Identity ty tm ->
    Identity ty <$> f tm
  Constantly ty t1 t2 ->
    Constantly ty <$> f t1 <*> f t2
  Compose ty1 ty2 ty3 tm o1 o2 ->
    Compose ty1 ty2 ty3 <$> f tm <*> openF o1 <*> openF o2
  StrConcat tm1 tm2 ->
    StrConcat <$> f tm1 <*> f tm2
  StrLength tm ->
    StrLength <$> f tm
  StrTake tm1 tm2 ->
    StrTake <$> f tm1 <*> f tm2
  StrDrop tm1 tm2 ->
    StrDrop <$> f tm1 <*> f tm2
  StrToInt tm ->
    StrToInt <$> f tm
  StrToIntBase tm1 tm2 ->
    StrToIntBase <$> f tm1 <*> f tm2
  StrContains tm1 tm2 ->
    StrContains <$> f tm1 <*> f tm2
  Numerical num ->
    Numerical <$> numF num
  IntAddTime tm1 tm2 ->
    IntAddTime <$> f tm1 <*> f tm2
  DecAddTime tm1 tm2 ->
    DecAddTime <$> f tm1 <*> f tm2
  Comparison ty op tm1 tm2 ->
    Comparison ty op <$> f tm1 <*> f tm2
  GuardEqNeq op tm1 tm2 ->
    GuardEqNeq op <$> f tm1 <*> f tm2
  ObjectEqNeq ty1 ty2 op tm1 tm2 ->
    ObjectEqNeq ty1 ty2 op <$> f tm1 <*> f tm2
  ObjAt ty tm1 tm2 ->
    ObjAt ty <$> f tm1 <*> f tm2
  ObjContains ty tm1 tm2 ->
    ObjContains ty <$> f tm1 <*> f tm2
  ObjLength ty tm ->
    ObjLength ty <$> f tm
  ObjDrop ty tm1 tm2 ->
    ObjDrop ty <$> f tm1 <*> f tm2
  ObjTake ty tm1 tm2 ->
    ObjTake ty <$> f tm1 <*> f tm2
  ObjMerge ty1 ty2 tm1 tm2 ->
    ObjMerge ty1 ty2 <$> f tm1 <*> f tm2
  LiteralObject ty obj ->
    LiteralObject ty <$> objF obj
  Logical op tms ->
    Logical op <$> traverse f tms
  ListEqNeq ty op tm1 tm2 ->
    ListEqNeq ty op <$> f tm1 <*> f tm2
  ListAt ty tm1 tm2 ->
    ListAt ty <$> f tm1 <*> f tm2
  ListContains ty tm1 tm2 ->
    ListContains ty <$> f tm1 <*> f tm2
  ListLength ty tm ->
    ListLength ty <$> f tm
  ListReverse ty tm ->
    ListReverse ty <$> f tm
  ListSort ty tm ->
    ListSort ty <$> f tm
  ListConcat ty tm1 tm2 ->
    ListConcat ty <$> f tm1 <*> f tm2
  ListDrop ty tm1 tm2 ->
    ListDrop ty <$> f tm1 <*> f tm2
  ListTake ty tm1 tm2 ->
    ListTake ty <$> f tm1 <*> f tm2
  MakeList ty tm1 tm2 ->
    MakeList ty <$> f tm1 <*> f tm2
  LiteralList ty tms ->
    LiteralList ty <$> traverse f tms
  ListMap ty1 ty2 o tm ->
    ListMap ty1 ty2 <$> openF o <*> f tm
  ListFilter ty o tm ->
    ListFilter ty <$> openF o <*> f tm
  ListFold ty1 ty2 o tm1 tm2 ->
    ListFold ty1 ty2 <$> mapOpen openF o <*> f tm1 <*> f tm2
  AndQ ty o1 o2 tm ->
    AndQ ty <$> openF o1 <*> openF o2 <*> f tm
  OrQ ty o1 o2 tm ->
    OrQ ty <$> openF o1 <*> openF o2 <*> f tm
  Where ty1 ty2 tm1 o tm2 ->
    Where ty1 ty2 <$> f tm1 <*> openF o <*> f tm2
  Typeof ty tm ->
    Typeof ty <$> f tm

  where
    f = propToInvariant
    openF = openPropToInv
    numF = numPropToInv
    objF = objPropToInv

objPropToInv :: Object Prop a -> Either String (Object Invariant a)
objPropToInv (Object hlist) = Object <$> hListPropToInv hlist
  where
    hListPropToInv
      :: HList (Column Prop) a
      -> Either String (HList (Column Invariant) a)
    hListPropToInv SNil = pure SNil
    hListPropToInv (SCons ty tm rest) = SCons ty
      <$> colPropToInvariant tm
      <*> hListPropToInv rest

    colPropToInvariant :: Column Prop ty -> Either String (Column Invariant ty)
    colPropToInvariant (Column ty tm) = Column ty <$> propToInvariant tm

mapOpen
  :: forall f s t a b
   . Functor f
  => (forall c. s c -> f (t c))
  -> Open a s b
  -> f (Open a t b)
mapOpen f (Open vid nm tm) = Open vid nm <$> f tm

openPropToInv :: Open a Prop b -> Either String (Open a Invariant b)
openPropToInv = mapOpen propToInvariant

numPropToInv :: Numerical Prop a -> Either String (Numerical Invariant a)
numPropToInv num = case num of
  DecArithOp op tm1 tm2 ->
    DecArithOp op <$> f tm1 <*> f tm2
  IntArithOp op tm1 tm2 ->
    IntArithOp op <$> f tm1 <*> f tm2
  DecUnaryArithOp op tm ->
    DecUnaryArithOp op <$> f tm
  IntUnaryArithOp op tm ->
    IntUnaryArithOp op <$> f tm
  DecIntArithOp op tm1 tm2 ->
    DecIntArithOp op <$> f tm1 <*> f tm2
  IntDecArithOp op tm1 tm2 ->
    IntDecArithOp op <$> f tm1 <*> f tm2
  ModOp tm1 tm2 ->
    ModOp <$> f tm1 <*> f tm2
  RoundingLikeOp1 op tm ->
    RoundingLikeOp1 op <$> f tm
  RoundingLikeOp2 op tm1 tm2 ->
    RoundingLikeOp2 op <$> f tm1 <*> f tm2
  BitwiseOp op tms ->
    BitwiseOp op <$> traverse f tms

  where
    f = propToInvariant
