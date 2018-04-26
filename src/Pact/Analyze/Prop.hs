{-# language DeriveDataTypeable         #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TypeOperators              #-}

module Pact.Analyze.Prop where

import Control.Lens (Lens', lens)
import Data.Aeson (ToJSON, FromJSON)
import Data.Data (Data)
import qualified Data.Decimal as Decimal
import Data.SBV hiding (Satisfiable)
import qualified Data.SBV.Internals as SBVI
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
import qualified Data.Text as T
import Pact.Types.Util (AsString)

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SBVI.SVal KString
                 . Left
                 . SBVI.CW KString
                 . SBVI.CWString

newtype KeySetName
  = KeySetName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)

instance Show KeySetName where show (KeySetName s) = show s

instance SymWord KeySetName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (KeySetName t) = mkConcreteString $ T.unpack t
  fromCW = wrappedStringFromCW $ KeySetName . T.pack

instance HasKind KeySetName where
  kindOf _ = KString

newtype TableName
  = TableName String
  deriving (Eq, Ord, Show)

instance SymWord TableName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (TableName s) = mkConcreteString s
  fromCW = wrappedStringFromCW TableName

instance HasKind TableName where
  kindOf _ = KString

instance IsString TableName where
  fromString = TableName

newtype ColumnName
  = ColumnName String
  deriving (Eq, Ord, Show)

instance SymWord ColumnName where
  mkSymWord = SBVI.genMkSymVar KString
  literal (ColumnName s) = mkConcreteString s
  fromCW = wrappedStringFromCW ColumnName

instance HasKind ColumnName where
  kindOf _ = KString

instance IsString ColumnName where
  fromString = ColumnName

newtype RowKey
  = RowKey String
  deriving (Eq, Ord, Show)

instance SymWord RowKey where
  mkSymWord = SBVI.genMkSymVar KString
  literal (RowKey s) = mkConcreteString s
  fromCW = wrappedStringFromCW RowKey

instance HasKind RowKey where
  kindOf _ = KString

instance IsString RowKey where
  fromString = RowKey

-- We can't use Proxy because deriving Eq doesn't work
-- We're still 8.0, so we can't use the new TypeRep yet:
data Rep a = Rep deriving (Eq, Show)
data Ty where Ty :: (SymWord t, Typeable t) => Rep t -> Ty
instance Eq Ty where
  Ty (Rep :: Rep a) == Ty (Rep :: Rep b) =
    case eqT :: Maybe (a :~: b) of
      Just Refl -> True
      _ -> False
deriving instance Show Ty

-- Pact uses Data.Decimal which is arbitrary-precision
type Decimal = AlgReal

type Time = Int64

data LogicalOp = AndOp | OrOp | NotOp
  deriving (Show, Eq)

-- Operations that apply to a pair of either integer or decimal, resulting in
-- the same:
-- integer -> integer -> integer
-- decimal -> decimal -> decimal
--
-- Or:
-- integer -> decimal -> integer
-- decimal -> integer -> integer
data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Log
  deriving (Show, Eq)

-- integer -> integer
-- decimal -> decimal
data UnaryArithOp
  = Negate
  | Sqrt
  | Ln
  | Exp
  | Abs

  -- Implemented only for the sake of the Num instance
  | Signum
  deriving (Show, Eq)

-- decimal -> integer -> decimal
-- decimal -> decimal
data RoundingLikeOp
  = Round
  | Ceiling
  | Floor
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

data Prop a where
  -- Literals
  PLit             :: SymWord a => a   -> Prop a
  --
  -- TODO: change this to `S a`, once we move S into Prop
  --
  PSym             ::              SBV a -> Prop a

  -- TX success/failure
  --
  -- TODO: remove one of these.
  --
  Abort            :: Prop Bool
  Success          :: Prop Bool
  Result           :: Prop a

  -- Abstraction
  Forall           :: Text -> Ty -> Prop a -> Prop a
  Exists           :: Text -> Ty -> Prop a -> Prop a
  PVar             :: Text ->                 Prop a

  -- String ops
  PStrConcat       :: Prop String -> Prop String -> Prop String
  PStrLength       :: Prop String ->                Prop Integer
  PStrEmpty        :: Prop String ->                Prop Bool

  -- Numeric ops
  PDecArithOp      :: ArithOp      -> Prop Decimal -> Prop Decimal -> Prop Decimal
  PIntArithOp      :: ArithOp      -> Prop Integer -> Prop Integer -> Prop Integer
  PDecUnaryArithOp :: UnaryArithOp                 -> Prop Decimal -> Prop Decimal
  PIntUnaryArithOp :: UnaryArithOp                 -> Prop Integer -> Prop Integer

  PDecIntArithOp   :: ArithOp -> Prop Decimal -> Prop Integer -> Prop Decimal
  PIntDecArithOp   :: ArithOp -> Prop Integer -> Prop Decimal -> Prop Decimal

  PModOp           :: Prop Integer   -> Prop Integer ->                 Prop Integer
  PRoundingLikeOp1 :: RoundingLikeOp -> Prop Decimal ->                 Prop Integer
  PRoundingLikeOp2 :: RoundingLikeOp -> Prop Decimal -> Prop Integer -> Prop Decimal

  -- Time
  PIntAddTime      :: Prop Time -> Prop Integer -> Prop Time
  PDecAddTime      :: Prop Time -> Prop Decimal -> Prop Time

  -- Comparison
  PComparison      :: (Show a, SymWord a) => ComparisonOp -> Prop a -> Prop a -> Prop Bool

  -- Boolean ops
  PLogical         :: LogicalOp -> [Prop Bool] -> Prop Bool

  -- DB properties
  TableWrite       :: TableName  ->                Prop Bool -- anything in table is written
  TableRead        :: TableName  ->                Prop Bool -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnName  -> Prop Bool -- particular column is written
  CellIncrease     :: TableName  -> ColumnName  -> Prop Bool -- any cell at all in col increases
  ColumnConserve   :: TableName  -> ColumnName  -> Prop Bool -- sum of all changes in col == 0
  ColumnIncrease   :: TableName  -> ColumnName  -> Prop Bool -- sum of all changes in col >  0
  RowRead          :: TableName  -> Prop RowKey -> Prop Bool
  RowWrite         :: TableName  -> Prop RowKey -> Prop Bool
  --
  -- TODO: StaleRead?
  --

  -- Authorization
  KsNameAuthorized :: KeySetName ->                              Prop Bool -- keyset authorized by name
  RowEnforced      :: TableName  -> ColumnName -> Prop RowKey -> Prop Bool

-- NOTE: PComparison's existential currently prevents this:
--deriving instance Eq a => Eq (Prop a)
deriving instance Show a => Show (Prop a)

instance IsString (Prop a) where
  fromString = PVar . fromString

instance Boolean (Prop Bool) where
  true   = PLit True
  false  = PLit False
  bnot p = PLogical NotOp [p]
  p1 &&& p2 = PLogical AndOp [p1, p2]
  p1 ||| p2 = PLogical OrOp [p1, p2]

instance Num (Prop Integer) where
  fromInteger = PLit . fromInteger
  (+)    = PIntArithOp Add
  (*)    = PIntArithOp Mul
  abs    = PIntUnaryArithOp Abs
  signum = PIntUnaryArithOp Signum
  negate = PIntUnaryArithOp Negate

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = fromRational $
  mantissa % 10 ^ places

instance Num (Prop Decimal) where
  fromInteger = PLit . mkDecimal . fromInteger
  (+)    = PDecArithOp Add
  (*)    = PDecArithOp Mul
  abs    = PDecUnaryArithOp Abs
  signum = PDecUnaryArithOp Signum
  negate = PDecUnaryArithOp Negate

data Check where
  Satisfiable :: Prop Bool -> Check
  Valid       :: Prop Bool -> Check
  deriving (Show)

ckProp :: Lens' Check (Prop Bool)
ckProp = lens getter setter
  where
    getter (Satisfiable p) = p
    getter (Valid p) = p

    setter (Satisfiable _) p = Satisfiable p
    setter (Valid _) p = Valid p

data Any = Any
  deriving (Show, Read, Eq, Ord, Data)

instance HasKind Any
instance SymWord Any

-- KeySets are completely opaque to pact programs -- 256 should be enough for
-- symbolic analysis?
newtype KeySet
  = KeySet Word8
  deriving (Eq, Ord, Data, Show, Read)

-- "Giving no instances is ok when defining an uninterpreted/enumerated sort"
instance SymWord KeySet
instance HasKind KeySet where kindOf (KeySet rep) = kindOf rep

-- The type of a simple type
data Type a where
  TInt     :: Type Integer
  TBool    :: Type Bool
  TStr     :: Type String
  TTime    :: Type Time
  TDecimal :: Type Decimal
  TKeySet  :: Type KeySet
  TAny     :: Type Any

deriving instance Show (Type a)
deriving instance Eq (Type a)

typeEq :: Type a -> Type b -> Maybe (a :~: b)
typeEq TInt     TInt     = Just Refl
typeEq TBool    TBool    = Just Refl
typeEq TStr     TStr     = Just Refl
typeEq TTime    TTime    = Just Refl
typeEq TDecimal TDecimal = Just Refl
typeEq TAny     TAny     = Just Refl
typeEq TKeySet  TKeySet  = Just Refl
typeEq _        _        = Nothing

data SchemaInvariant a where
  SchemaDecimalComparison
    :: ComparisonOp
    -> SchemaInvariant Decimal
    -> SchemaInvariant Decimal
    -> SchemaInvariant Bool

  SchemaDecimalLiteral :: Decimal -> SchemaInvariant Decimal
  SchemaVar :: Text -> SchemaInvariant a

deriving instance Eq (SchemaInvariant a)
deriving instance Show (SchemaInvariant a)

data SomeSchemaInvariant where
  SomeSchemaInvariant :: SchemaInvariant a -> Type a -> SomeSchemaInvariant

deriving instance Show SomeSchemaInvariant

instance Eq SomeSchemaInvariant where
  SomeSchemaInvariant a ta == SomeSchemaInvariant b tb = case typeEq ta tb of
    Nothing   -> False
    Just Refl -> a == b

invariantVars :: SchemaInvariant a -> Set Text
invariantVars = \case
  SchemaDecimalComparison _ a b -> invariantVars a <> invariantVars b
  SchemaDecimalLiteral _        -> Set.empty
  SchemaVar v                   -> Set.singleton v
