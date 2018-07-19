{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Pact.Analyze.Types.Languages
  ( (:<:)(inject, project)
  , EInvariant
  , EProp
  , Invariant(..)
  , PreProp(..)
  , Prop(..)
  , PropSpecific(..)
  , PureTerm(..)

  , mkDecimal

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

import qualified Data.Decimal               as Decimal
import           Data.Map.Strict            (Map)
import           Data.SBV                   (Boolean (bnot, false, true, (&&&),
                                             (|||)), (%))
import           Data.Semigroup             ((<>))
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Typeable              ((:~:) (Refl))
import           Prelude                    hiding (Float)

import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Util            (tShow)

import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Util

#define EQ_EXISTENTIAL(tm)                               \
instance Eq (Existential tm) where                       \
  ETerm ta ia == ETerm tb ib = case typeEq ta tb of {    \
    Just Refl -> ia == ib;                               \
    Nothing   -> False};                                 \
  EObject sa pa == EObject sb pb = sa == sb && pa == pb; \
  _ == _ = False;

#define SHOW_EXISTENTIAL(tm)                                       \
instance Show (Existential tm) where                               \
  show (ETerm ty inv) = "(" ++ show inv ++ ": " ++ show ty ++ ")"; \
  show (EObject ty obj) = "(" ++ show obj ++ ": " ++ show ty ++ ")";


-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class sub :<: sup where
  inject  :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

pattern Inj :: sub :<: sup => sub a -> sup a
pattern Inj a <- (project -> Just a) where
  Inj a = inject a


-- @PreProp@ stands between @Exp@ and @Prop@.
--
-- The conversion from @Exp@ is light, handled in @expToPreProp@.
data PreProp
  -- literals
  = PreIntegerLit Integer
  | PreStringLit  Text
  | PreDecimalLit Decimal
  | PreTimeLit    Time
  | PreBoolLit    Bool

  -- identifiers
  | PreAbort
  | PreSuccess
  | PreResult
  | PreVar     VarId Text

  -- quantifiers
  | PreForall VarId Text QType PreProp
  | PreExists VarId Text QType PreProp

  -- applications
  | PreApp Text [PreProp]

  | PreAt Text PreProp
  | PreLiteralObject (Map Text PreProp)
  deriving Eq


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
  ColumnWrite :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool
  -- | Whether a column is read
  ColumnRead  :: Prop TableName  -> Prop ColumnName  -> PropSpecific Bool

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

  --
  -- TODO: StaleRead?
  --

  -- Authorization

  -- | Whether a transaction contains a signature that satisfied the named key set
  KsNameAuthorized :: KeySetName      ->                                   PropSpecific Bool
  -- | Whether a row has its keyset @enforce@d in a transaction
  RowEnforced      :: Prop TableName  -> Prop ColumnName -> Prop RowKey -> PropSpecific Bool

deriving instance Eq a => Eq (PropSpecific a)
deriving instance Show a => Show (PropSpecific a)

instance PropSpecific :<: Prop where
  inject = PropSpecific
  project = \case
    PropSpecific a -> Just a
    _              -> Nothing


data PureTerm t a where
  Lit :: a -> PureTerm t a
  -- | Injects a symbolic value into the language
  Sym :: S a -> PureTerm t a

  -- | Refers to a function argument, universally/existentially-quantified
  -- variable, or column
  Var :: VarId -> Text -> PureTerm t a

  -- string ops
  -- | The concatenation of two 'String' expressions
  StrConcat :: t String -> t String -> PureTerm t String
  -- | The length of a 'String' expression
  StrLength :: t String                     -> PureTerm t Integer

  -- numeric ops
  Numerical :: Numerical t a -> PureTerm t a

  -- Time
  -- | Adds an 'Integer' expression to a 'Time' expression
  IntAddTime :: t Time -> t Integer -> PureTerm t Time
  -- | Adds a 'Decimal' expression to a 'Time' expression
  DecAddTime :: t Time -> t Decimal -> PureTerm t Time

  -- comparison. Note that while it's cumbersome to define five different
  -- monomorphized comparisons, the alternative is implementing Eq by hand
  -- here.

  -- | A 'ComparisonOp' expression over two 'Integer' expressions
  IntegerComparison :: ComparisonOp -> t Integer -> t Integer -> PureTerm t Bool
  -- | A 'ComparisonOp' expression over two 'Decimal' expressions
  DecimalComparison :: ComparisonOp -> t Decimal -> t Decimal -> PureTerm t Bool
  -- | A 'ComparisonOp' expression over two 'Time' expressions
  TimeComparison    :: ComparisonOp -> t Time    -> t Time    -> PureTerm t Bool
  -- | A 'ComparisonOp' expression over two 'String' expressions
  StringComparison  :: ComparisonOp -> t String  -> t String  -> PureTerm t Bool
  -- | A 'ComparisonOp' expression over two 'Bool' expressions
  BoolComparison    :: ComparisonOp -> t Bool    -> t Bool    -> PureTerm t Bool

  KeySetEqNeq :: EqNeq -> t KeySet -> t KeySet -> PureTerm t Bool
  ObjectEqNeq :: EqNeq -> t Object -> t Object -> PureTerm t Bool

  At            :: Schema -> t String -> t Object -> EType -> PureTerm t a

  LiteralObject :: Map Text (Existential t) -> PureTerm t Object

  -- boolean ops
  -- | A 'Logical' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.
  Logical :: LogicalOp -> [t Bool] -> PureTerm t Bool

deriving instance Show a => Show (PureTerm Prop a)
deriving instance Eq a => Eq (PureTerm Prop a)

pattern PLit :: a -> Prop a
pattern PLit a = PureProp (Lit a)

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = PureProp (Var vid name)

data Prop a
  = PropSpecific (PropSpecific a)
  | PureProp     (PureTerm Prop a)
  deriving (Show, Eq)

instance S :<: Prop where
  inject = PureProp . Sym
  project = \case
    PureProp (Sym a) -> Just a
    _                -> Nothing

instance Numerical Prop :<: Prop where
  inject = PureProp . Numerical
  project (PureProp (Numerical a)) = Just a
  project _                        = Nothing

instance IsString (Prop TableName) where
  fromString = PLit . fromString

instance IsString (Prop ColumnName) where
  fromString = PLit . fromString

pattern PNumerical :: Numerical Prop t -> Prop t
pattern PNumerical x = PureProp (Numerical x)

pattern PStrConcat :: Prop String -> Prop String -> Prop String
pattern PStrConcat x y = PureProp (StrConcat x y)

pattern PIntAddTime :: Prop Time -> Prop Integer -> Prop Time
pattern PIntAddTime x y = PureProp (IntAddTime x y)

pattern PDecAddTime :: Prop Time -> Prop Decimal -> Prop Time
pattern PDecAddTime x y = PureProp (DecAddTime x y)

pattern PAt :: Schema -> Prop String -> Prop Object -> EType -> Prop t
pattern PAt a b c d = PureProp (At a b c d)

pattern PKeySetEqNeq :: EqNeq -> Prop KeySet -> Prop KeySet -> Prop Bool
pattern PKeySetEqNeq op x y = PureProp (KeySetEqNeq op x y)

pattern PObjectEqNeq :: EqNeq -> Prop Object -> Prop Object -> Prop Bool
pattern PObjectEqNeq op x y = PureProp (ObjectEqNeq op x y)

pattern PLogical :: LogicalOp -> [Prop Bool] -> Prop Bool
pattern PLogical op args = PureProp (Logical op args)

pattern PStrLength :: Prop String -> Prop Integer
pattern PStrLength str = PureProp (StrLength str)

pattern PAnd :: Prop Bool -> Prop Bool -> Prop Bool
pattern PAnd a b = PureProp (Logical AndOp [a, b])

pattern POr :: Prop Bool -> Prop Bool -> Prop Bool
pattern POr a b = PureProp (Logical OrOp [a, b])

pattern PNot :: Prop Bool -> Prop Bool
pattern PNot a = PureProp (Logical NotOp [a])

type EProp = Existential Prop
EQ_EXISTENTIAL(Prop)
SHOW_EXISTENTIAL(Prop)

instance Boolean (Prop Bool) where
  true      = PLit True
  false     = PLit False
  bnot p    = PureProp $ Logical NotOp [p]
  p1 &&& p2 = PAnd p1 p2
  p1 ||| p2 = POr  p1 p2

instance Num (Prop Integer) where
  fromInteger = PLit . fromInteger
  (+)         = inject ... IntArithOp Add
  (*)         = inject ... IntArithOp Mul
  abs         = inject .   IntUnaryArithOp Abs
  signum      = inject .   IntUnaryArithOp Signum
  negate      = inject .   IntUnaryArithOp Negate

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = fromRational $
  mantissa % 10 ^ places

instance Num (Prop Decimal) where
  fromInteger = PLit . mkDecimal . fromInteger
  (+)         = inject ... DecArithOp Add
  (*)         = inject ... DecArithOp Mul
  abs         = inject .   DecUnaryArithOp Abs
  signum      = inject .   DecUnaryArithOp Signum
  negate      = inject .   DecUnaryArithOp Negate

instance UserShow PreProp where
  userShowsPrec prec = \case
    PreIntegerLit i -> tShow i
    PreStringLit t  -> tShow t
    PreDecimalLit d -> tShow d
    PreTimeLit t    -> tShow (Pact.LTime (unMkTime t))
    PreBoolLit b    -> tShow (Pact.LBool b)

    PreAbort        -> "abort"
    PreSuccess      -> "success"
    PreResult       -> "result"
    PreVar _id name -> name

    PreForall _vid name qty prop ->
      "(forall (" <> name <> ":" <> userShow qty <> ") " <> userShow prop <> ")"
    PreExists _vid name qty prop ->
      "(exists (" <> name <> ":" <> userShow qty <> ") " <> userShow prop <> ")"
    PreApp name applicands -> "(" <> name <> " " <> T.unwords
      ((map userShow) applicands) <> ")"

    PreAt objIx obj      -> "(at '" <> objIx <> " " <> userShow obj <> ")"
    PreLiteralObject obj -> userShowsPrec prec obj

-- The schema invariant language consists of:
--
-- * comparisons
--   - { <, >, <=, >= } apply to { integer, decimal, string, time }
--   - { =, != } apply to { integer, decimal, string, time, bool, keyset }
-- * literals
-- * variables
-- * logical operations
--
-- The language is stateless.
newtype Invariant a = PureInvariant (PureTerm Invariant a)
  deriving (Show, Eq)

instance Numerical Invariant :<: Invariant where
  inject = PureInvariant . Numerical
  project (PureInvariant (Numerical a)) = Just a
  project _                             = Nothing

instance S :<: Invariant where
  inject = PureInvariant . Sym
  project = \case
    PureInvariant (Sym a) -> Just a
    _                     -> Nothing

type EInvariant = Existential Invariant
EQ_EXISTENTIAL(Invariant)
SHOW_EXISTENTIAL(Invariant)

deriving instance Show a => Show (PureTerm Invariant a)
deriving instance Eq a => Eq (PureTerm Invariant a)

pattern ILiteral :: a -> Invariant a
pattern ILiteral a = PureInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant Bool] -> Invariant Bool
pattern ILogicalOp op args = PureInvariant (Logical op args)
