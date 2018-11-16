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

module Pact.Analyze.Types.Languages
  ( (:<:)(inject, project)
  , EInvariant
  , EProp
  , ETerm
  , Core(..)
  , Invariant(..)
  , Prop(..)
  , PropSpecific(..)
  , Term(..)
  , BeforeOrAfter(..)

  , lit
  , toPact
  , fromPact
  , valueToProp

  , pattern ILiteral
  , pattern ILogicalOp
  , pattern Inj
  , pattern PAnd
  , pattern PAt
  , pattern PDecAddTime
  , pattern PIntAddTime
  , pattern PKeySetEqNeq
  , pattern PLit
  , pattern PLogical
  , pattern PNot
  , pattern PNumerical
  , pattern PObjectEqNeq
  , pattern POr
  , pattern PStrConcat
  , pattern PStrLength
  , pattern PVar
  ) where

import           Data.Map.Strict              (Map)
import           Data.SBV                     (Boolean (bnot, false, true, (&&&), (|||)))
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable                ((:~:) (Refl))
import           Prelude                      hiding (Float)

import           Pact.Types.Persistence       (WriteType)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Sym, Var, col, str, obj, dec)
import           Pact.Analyze.Types.Model
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.UserShow
import           Pact.Analyze.Util

#define EQ_EXISTENTIAL(tm)                                \
instance Eq (Existential tm) where                        \
  ESimple ta ia == ESimple tb ib = case typeEq ta tb of { \
    Just Refl -> ia == ib;                                \
    Nothing   -> False};                                  \
  EObject sa pa == EObject sb pb = sa == sb && pa == pb;  \
  _ == _ = False;

#define SHOW_EXISTENTIAL(tm)                                                   \
instance Show (Existential tm) where {                                         \
  showsPrec d e = showParen (d > 10) $ case e of                               \
    ESimple ty inv -> showString "ESimple " . showsPrec 11 ty . showString " " \
      . showsPrec 11 inv;                                                      \
    EObject ty obj -> showString "EObject " . showsPrec 11 ty . showString " " \
      . showsPrec 11 obj; };                                                   \
instance UserShow (Existential tm) where                                       \
  userShowsPrec d e = case e of                                                \
    ESimple _ty a -> userShowsPrec d a;                                        \
    EObject _ty a -> userShowsPrec d a;

-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class sub :<: sup where
  inject  :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

instance f :<: f where
  inject  = id
  project = Just

pattern Inj :: sub :<: sup => sub a -> sup a
pattern Inj a <- (project -> Just a) where
  Inj a = inject a

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
data Core t a where
  Lit :: a -> Core t a
  -- | Injects a symbolic value into the language
  Sym :: S a -> Core t a

  -- | Refers to a function argument, universally/existentially-quantified
  -- variable, or column
  Var :: VarId -> Text -> Core t a

  -- string ops
  -- | The concatenation of two 'String' expressions
  StrConcat    :: t String  -> t String  -> Core t String
  -- | The length of a 'String' expression
  StrLength    :: t String  ->              Core t Integer
  -- | Conversion of a base-10 string to an integer
  StrToInt     :: t String  ->              Core t Integer
  -- | Conversion of a base-1-16 string to an integer
  StrToIntBase :: t Integer -> t String  -> Core t Integer

  -- numeric ops
  Numerical    :: Numerical t a -> Core t a

  -- Time
  -- | Adds an 'Integer' expression to a 'Time' expression
  IntAddTime :: t Time -> t Integer -> Core t Time
  -- | Adds a 'Decimal' expression to a 'Time' expression
  DecAddTime :: t Time -> t Decimal -> Core t Time

  -- comparison. Note that while it's cumbersome to define five different
  -- monomorphized comparisons, the alternative is implementing Eq by hand
  -- here.

  -- | A 'ComparisonOp' expression over two 'Integer' expressions
  IntegerComparison :: ComparisonOp -> t Integer -> t Integer -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Decimal' expressions
  DecimalComparison :: ComparisonOp -> t Decimal -> t Decimal -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Time' expressions
  TimeComparison    :: ComparisonOp -> t Time    -> t Time    -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'String' expressions
  StringComparison  :: ComparisonOp -> t String  -> t String  -> Core t Bool
  -- | A 'ComparisonOp' expression over two 'Bool' expressions
  --
  -- note: this is more broad than the set ({=, !=}) of comparisons pact
  -- supports on bools.
  BoolComparison    :: ComparisonOp -> t Bool    -> t Bool    -> Core t Bool

  KeySetEqNeq :: EqNeq -> t KeySet -> t KeySet -> Core t Bool
  ObjectEqNeq :: EqNeq -> t Object -> t Object -> Core t Bool

  At :: Schema -> t String -> t Object -> EType -> Core t a

  ObjectMerge :: t Object -> t Object -> Core t Object

  LiteralObject :: Map Text (Existential t) -> Core t Object

  -- boolean ops
  -- | A 'Logical' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.
  Logical :: LogicalOp -> [t Bool] -> Core t Bool

deriving instance Eq a   => Eq   (Core Prop a)
deriving instance Show a => Show (Core Prop a)

instance
  ( UserShow a
  , UserShow (tm String)
  , UserShow (tm Integer)
  , UserShow (tm Time)
  , UserShow (tm Decimal)
  , UserShow (tm Bool)
  , UserShow (tm KeySet)
  , UserShow (tm Object)
  , UserShow (Existential tm)
  ) => UserShow (Core tm a) where
  userShowsPrec d = \case
    Lit a                    -> userShowsPrec d a
    Sym s                    -> tShow s
    StrConcat x y            -> parenList [SAddition, userShow x, userShow y]
    StrLength str            -> parenList [SStringLength, userShow str]
    StrToInt s               -> parenList ["str-to-int", userShow s]
    StrToIntBase b s         -> parenList ["str-to-int", userShow b, userShow s]
    Numerical tm             -> userShowsPrec d tm
    IntAddTime x y           -> parenList [STemporalAddition, userShow x, userShow y]
    DecAddTime x y           -> parenList [STemporalAddition, userShow x, userShow y]
    IntegerComparison op x y -> parenList [userShow op, userShow x, userShow y]
    DecimalComparison op x y -> parenList [userShow op, userShow x, userShow y]
    TimeComparison op x y    -> parenList [userShow op, userShow x, userShow y]
    StringComparison op x y  -> parenList [userShow op, userShow x, userShow y]
    BoolComparison op x y    -> parenList [userShow op, userShow x, userShow y]
    Logical op args          -> parenList $ userShow op : fmap userShow args
    Var _vid name            -> name
    KeySetEqNeq op x y       -> parenList [userShow op, userShow x, userShow y]
    ObjectEqNeq op x y       -> parenList [userShow op, userShow x, userShow y]
    At _schema k obj _ty     -> parenList [userShow k, userShow obj]
    ObjectMerge x y          -> parenList [SObjectMerge, userShow x, userShow y]
    LiteralObject obj        -> userShow obj


data BeforeOrAfter = Before | After
  deriving (Eq, Show)

instance UserShow BeforeOrAfter where
  userShowsPrec _p = \case
    Before -> "'before"
    After  -> "'after"

-- | Property-specific constructions.
--
-- This encompasses every construction that can appear in a 'Prop' that's not
-- in 'Core'.
data PropSpecific a where

  -- TX success/failure

  --
  -- TODO: remove either Success Or Abort.
  --

  -- | Whether a transaction aborts (does not succeed)
  Abort   :: PropSpecific Bool
  -- | Whether a transaction succeeds (does not abort)
  Success :: PropSpecific Bool
  -- | The return value of the function under examination
  Result  :: PropSpecific a

  -- Abstraction

  -- | Introduces a universally-quantified variable over another property
  Forall :: VarId -> Text -> QType -> Prop Bool -> PropSpecific Bool
  -- | Introduces an existentially-quantified variable over another property
  Exists :: VarId -> Text -> QType -> Prop Bool -> PropSpecific Bool

  -- DB properties

  -- | True when anything in the table is written
  TableWrite :: Prop TableName  ->                PropSpecific Bool
  -- | True when anything in the table is read
  TableRead  :: Prop TableName  ->                PropSpecific Bool

  --
  -- NOTE: it's possible that in a standard library we could implement these in
  --       terms of "CellRead"/"CellWrite" and existential quantification.
  --
  -- | Whether a column is written
  ColumnWritten :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool
  -- | Whether a column is read
  ColumnRead    :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool

  --
  -- TODO: rewrite these in terms of CellBefore, CellAfter, ColumnSumBefore,
  --       ColumnSumAfter:
  --
  -- | The difference (@after-before@) in a cell's integer value across a transaction
  IntCellDelta   :: Prop TableName  -> Prop ColumnName  -> Prop RowKey -> PropSpecific Integer
  -- | The difference (@after-before@) in a cell's decimal value across a transaction
  DecCellDelta   :: Prop TableName  -> Prop ColumnName  -> Prop RowKey -> PropSpecific Decimal
  -- | The difference (@after-before@) in a column's integer sum across a transaction
  IntColumnDelta :: Prop TableName  -> Prop ColumnName                 -> PropSpecific Integer
  -- | The difference (@after-before@) in a column's decimal sum across a transaction
  DecColumnDelta :: Prop TableName  -> Prop ColumnName                 -> PropSpecific Decimal

  -- | Whether a row is read
  RowRead       :: Prop TableName  -> Prop RowKey -> PropSpecific Bool
  -- | Number of times a row is read
  RowReadCount  :: Prop TableName  -> Prop RowKey -> PropSpecific Integer
  -- | Whether a row is written
  RowWrite      :: Prop TableName  -> Prop RowKey -> PropSpecific Bool
  -- | Number of times a row is written
  RowWriteCount :: Prop TableName  -> Prop RowKey -> PropSpecific Integer
  -- | Whether a row exists prior to the transaction
  RowExists     :: Prop TableName  -> Prop RowKey -> BeforeOrAfter -> PropSpecific Bool

  --
  -- TODO: StaleRead?
  --

  -- Authorization

  -- | Whether a transaction contains a signature that satisfied the named key set
  KsNameAuthorized :: KeySetName      ->                                   PropSpecific Bool
  -- | Whether a row has its keyset @enforce@d in a transaction
  RowEnforced      :: Prop TableName  -> Prop ColumnName -> Prop RowKey -> PropSpecific Bool


  PropRead :: BeforeOrAfter -> Schema -> Prop TableName -> Prop RowKey -> PropSpecific Object

deriving instance Eq a   => Eq   (PropSpecific a)
deriving instance Show a => Show (PropSpecific a)


data Prop a
  = PropSpecific (PropSpecific a)
  | CoreProp     (Core Prop a)
  deriving (Show, Eq)

instance UserShow a => UserShow (PropSpecific a) where
  userShowsPrec _d = \case
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

instance UserShow a => UserShow (Prop a) where
  userShowsPrec d = \case
    PropSpecific p -> userShowsPrec d p
    CoreProp     p -> userShowsPrec d p

instance S :<: Prop where
  inject = CoreProp . Sym
  project = \case
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

instance IsString (Prop TableName) where
  fromString = PLit . fromString

instance IsString (Prop ColumnName) where
  fromString = PLit . fromString

instance IsString (Prop RowKey) where
  fromString = PLit . fromString

instance Boolean (Prop Bool) where
  true      = PLit True
  false     = PLit False
  bnot p    = CoreProp $ Logical NotOp [p]
  p1 &&& p2 = PAnd p1 p2
  p1 ||| p2 = POr  p1 p2

instance Num (Prop Integer) where
  fromInteger = PLit . fromInteger
  (+)         = inject ... IntArithOp Add
  (*)         = inject ... IntArithOp Mul
  abs         = inject .   IntUnaryArithOp Abs
  signum      = inject .   IntUnaryArithOp Signum
  negate      = inject .   IntUnaryArithOp Negate

instance Num (Prop Decimal) where
  fromInteger = PLit . fromPact decimalIso . fromInteger
  (+)         = inject ... DecArithOp Add
  (*)         = inject ... DecArithOp Mul
  abs         = inject .   DecUnaryArithOp Abs
  signum      = inject .   DecUnaryArithOp Signum
  negate      = inject .   DecUnaryArithOp Negate

type EProp = Existential Prop
EQ_EXISTENTIAL(Prop)
SHOW_EXISTENTIAL(Prop)

pattern PLit :: a -> Prop a
pattern PLit a = CoreProp (Lit a)

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = CoreProp (Var vid name)

pattern PNumerical :: Numerical Prop t -> Prop t
pattern PNumerical x = CoreProp (Numerical x)

pattern PStrConcat :: Prop String -> Prop String -> Prop String
pattern PStrConcat x y = CoreProp (StrConcat x y)

pattern PIntAddTime :: Prop Time -> Prop Integer -> Prop Time
pattern PIntAddTime x y = CoreProp (IntAddTime x y)

pattern PDecAddTime :: Prop Time -> Prop Decimal -> Prop Time
pattern PDecAddTime x y = CoreProp (DecAddTime x y)

pattern PAt :: Schema -> Prop String -> Prop Object -> EType -> Prop t
pattern PAt a b c d = CoreProp (At a b c d)

pattern PKeySetEqNeq :: EqNeq -> Prop KeySet -> Prop KeySet -> Prop Bool
pattern PKeySetEqNeq op x y = CoreProp (KeySetEqNeq op x y)

pattern PObjectEqNeq :: EqNeq -> Prop Object -> Prop Object -> Prop Bool
pattern PObjectEqNeq op x y = CoreProp (ObjectEqNeq op x y)

pattern PLogical :: LogicalOp -> [Prop Bool] -> Prop Bool
pattern PLogical op args = CoreProp (Logical op args)

pattern PStrLength :: Prop String -> Prop Integer
pattern PStrLength str = CoreProp (StrLength str)

pattern PAnd :: Prop Bool -> Prop Bool -> Prop Bool
pattern PAnd a b = CoreProp (Logical AndOp [a, b])

pattern POr :: Prop Bool -> Prop Bool -> Prop Bool
pattern POr a b = CoreProp (Logical OrOp [a, b])

pattern PNot :: Prop Bool -> Prop Bool
pattern PNot a = CoreProp (Logical NotOp [a])


-- | The schema invariant language.
--
-- This language is pure / stateless. It includes exactly the same
-- constructions as 'Core'.
newtype Invariant a = CoreInvariant (Core Invariant a)
  deriving (Show, Eq)

deriving instance Eq a   => Eq   (Core Invariant a)
deriving instance Show a => Show (Core Invariant a)

instance Core Invariant :<: Invariant where
  inject                    = CoreInvariant
  project (CoreInvariant a) = Just a

instance Numerical Invariant :<: Invariant where
  inject = Inj . Numerical
  project = \case
    Inj (Numerical a) -> Just a
    _                 -> Nothing

instance S :<: Invariant where
  inject = CoreInvariant . Sym
  project = \case
    CoreInvariant (Sym a) -> Just a
    _                     -> Nothing

instance UserShow a => UserShow (Invariant a) where
  userShowsPrec d (CoreInvariant a) = userShowsPrec d a

type EInvariant = Existential Invariant
EQ_EXISTENTIAL(Invariant)
SHOW_EXISTENTIAL(Invariant)

pattern ILiteral :: a -> Invariant a
pattern ILiteral a = CoreInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant Bool] -> Invariant Bool
pattern ILogicalOp op args = CoreInvariant (Logical op args)

type ETerm = Existential Term
EQ_EXISTENTIAL(Term)
SHOW_EXISTENTIAL(Term)

data Term ret where
  CoreTerm        :: Core Term a -> Term a

  -- In principle, this should be a pure term, however, the analyze monad needs
  -- to be `Mergeable`. `Analyze` is, but `Query` isn't, due to having
  -- `Symbolic` in its stack.
  --
  -- TODO(joel): In principle this could be pure and applied to all the
  -- languages. Unfortunately, we can't add this to props because `Query` has
  -- `Symbolic` in its stack, so it can't do an `ite`.
  IfThenElse      :: Term Bool -> (Path, Term a) -> (Path, Term a) -> Term a

  -- Variable binding
  Let             :: Text -> VarId -> TagId -> ETerm -> Term a -> Term a

  -- Control flow
  Sequence        :: ETerm     -> Term a ->           Term a

  -- Conditional transaction abort
  Enforce         :: Maybe TagId -> Term Bool   -> Term Bool -- Only a TagId for an assertion; i.e. not keyset enforcement
  -- Left to be tagged if the list of cases is empty. We do this because we
  -- need a way to signal a failure due to this particular scenario in model
  -- reporting. Right _1 to be tagged if the case fails, Right _2 to be tagged
  -- if the case succeeds:
  EnforceOne      :: Either TagId [((Path, Path), Term Bool)] -> Term Bool

  -- Reading from environment
  ReadKeySet      :: Term String -> Term KeySet
  ReadDecimal     :: Term String -> Term Decimal
  ReadInteger     :: Term String -> Term Integer

  -- TODO: ReadInteger, ReadMsg

  -- Keyset access
  KsAuthorized    :: TagId -> Term KeySet -> Term Bool
  NameAuthorized  :: TagId -> Term String -> Term Bool

  -- Table access
  Read            ::              TagId -> TableName -> Schema -> Term String ->                Term Object
  Write           :: WriteType -> TagId -> TableName -> Schema -> Term String -> Term Object -> Term String

  PactVersion     :: Term String

  Format          :: Term String         -> [ETerm]     -> Term String
  FormatTime      :: Term String         -> Term Time   -> Term String
  ParseTime       :: Maybe (Term String) -> Term String -> Term Time
  Hash            :: ETerm                              -> Term String

instance UserShow a => UserShow (Term a) where
  userShowsPrec _ = \case
    CoreTerm tm                -> userShow tm
    IfThenElse x (_, y) (_, z) -> parenList ["if", userShow x, userShow y, userShow z]
    Let var _ _ x y            -> parenList ["let", userShow var, userShow x, userShow y]
    Sequence x y               -> Text.unlines [userShow x, userShow y]

    EnforceOne (Left _)        -> parenList
      [ "enforce-one"
      , "\"(generated enforce-one)\""
      , userShowList ([] :: [Integer])
      ]
    EnforceOne (Right x)       -> parenList
      [ "enforce-one"
      , "\"(generated enforce-one)\""
      , userShowList $ fmap snd x
      ]

    Enforce _ (KsAuthorized _ x)   -> parenList ["enforce-keyset", userShow x]
    Enforce _ (NameAuthorized _ x) -> parenList ["enforce-keyset", userShow x]
    Enforce _ x                    -> parenList ["enforce", userShow x]
    KsAuthorized   _ _
      -> error "KsAuthorized should only appear inside of an Enforce"
    NameAuthorized _ _
      -> error "NameAuthorized should only appear inside of an Enforce"

    Read _ tab _ x       -> parenList ["read", userShow tab, userShow x]
    Write _ _ tab _ x y  -> parenList ["read", userShow tab, userShow x, userShow y]
    PactVersion          -> parenList ["pact-version"]
    Format x y           -> parenList ["format", userShow x, userShowList y]
    FormatTime x y       -> parenList ["format", userShow x, userShow y]
    ParseTime Nothing y  -> parenList ["parse-time", userShow y]
    ParseTime (Just x) y -> parenList ["parse-time", userShow x, userShow y]
    Hash x               -> parenList ["hash", userShow x]
    ReadKeySet name      -> parenList ["read-keyset", userShow name]
    ReadDecimal name     -> parenList ["read-decimal", userShow name]
    ReadInteger name     -> parenList ["read-integer", userShow name]

deriving instance Eq a   => Eq (Term a)
deriving instance Eq a   => Eq (Core Term a)
deriving instance Show a => Show (Term a)
deriving instance Show a => Show (Core Term a)

instance S :<: Term where
  inject = CoreTerm . Sym
  project = \case
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

instance Num (Term Integer) where
  fromInteger = lit . fromInteger
  (+)    = inject ... IntArithOp Add
  (*)    = inject ... IntArithOp Mul
  abs    = inject .   IntUnaryArithOp Abs
  signum = inject .   IntUnaryArithOp Signum
  negate = inject .   IntUnaryArithOp Negate

instance Num (Term Decimal) where
  fromInteger = lit . fromPact decimalIso . fromInteger
  (+)    = inject ... DecArithOp Add
  (*)    = inject ... DecArithOp Mul
  abs    = inject .   DecUnaryArithOp Abs
  signum = inject .   DecUnaryArithOp Signum
  negate = inject .   DecUnaryArithOp Negate

lit :: a -> Term a
lit = CoreTerm . Lit

valueToProp :: ETerm -> Either String EProp
valueToProp = \case
  EObject{} -> Left "can't (yet) convert objects to props"
  ESimple ty (CoreTerm (Lit l)) -> Right $ ESimple ty (CoreProp (Lit l))
  ESimple _ _ -> Left "can only convert (simple) values terms to props"
