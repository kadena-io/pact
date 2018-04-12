{-# language DeriveAnyClass      #-}
{-# language DeriveDataTypeable  #-}
{-# language FlexibleInstances   #-}
{-# language GADTs               #-}
{-# language StandaloneDeriving  #-}
{-# language TypeOperators       #-}

module Pact.Analyze.Types where

import Control.Lens hiding (op, (.>), (...))
import Data.Data
import qualified Data.Decimal as Decimal
import Data.Map.Strict (Map)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV.Internals as SBVI
import Data.String (IsString(..))
import Data.Thyme
import Pact.Types.Lang hiding (Term, TableName, Type, TObject, EObject)

newtype Object
  = Object (Map String (EType, AVal))
  deriving (Eq, Show)

newtype Schema
  = Schema (Map String EType)
  deriving (Show, Eq)

-- | Untyped symbolic value.
data AVal
  = AVal SBVI.SVal
  | AnObj Object
  deriving (Eq, Show)

mkSBV :: SBVI.SVal -> SBV a
mkSBV = SBVI.SBV

mkAVal :: SBV a -> AVal
mkAVal (SBVI.SBV sval) = AVal sval

coerceSBV :: SBV a -> SBV b
coerceSBV = SBVI.SBV . SBVI.unSBV

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SBVI.SVal KString
                 . Left
                 . SBVI.CW KString
                 . SBVI.CWString

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

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

type SRowKey
  = SBV RowKey

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind UserType
deriving instance SymWord UserType

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

data LogicalOp = AndOp | OrOp | NotOp
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

-- The type of a simple type
data Type a where
  TInt     :: Type Integer
  TBool    :: Type Bool
  TStr     :: Type String
  TTime    :: Type Time
  TDecimal :: Type Decimal

data EType where
  -- TODO: parametrize over constraint
  EType :: (Show a, SymWord a) => Type a -> EType
  EObjectTy :: Schema -> EType

typeEq :: Type a -> Type b -> Maybe (a :~: b)
typeEq TInt     TInt     = Just Refl
typeEq TBool    TBool    = Just Refl
typeEq TStr     TStr     = Just Refl
typeEq TTime    TTime    = Just Refl
typeEq TDecimal TDecimal = Just Refl
typeEq _        _        = Nothing

instance Eq EType where
  EType a == EType b = case typeEq a b of
    Just _refl -> True
    Nothing    -> False
  EObjectTy a == EObjectTy b = a == b
  _ == _ = False

data ETerm where
  -- TODO: remove Show (add constraint c?)
  ETerm   :: (Show a, SymWord a) => Term a      -> Type a -> ETerm
  EObject ::                        Term Object -> Schema -> ETerm

data Term ret where
  IfThenElse     ::                        Term Bool    -> Term a         -> Term a -> Term a
  Enforce        ::                        Term Bool    ->                             Term Bool
  -- TODO: do we need a noop to handle a sequence of one expression?
  Sequence       :: (Show b, SymWord b) => Term b       -> Term a         ->           Term a
  Literal        ::                        SBV a        ->                             Term a

  --
  -- TODO: we need to allow computed keys here
  --
  LiteralObject  ::                        Map String (EType, ETerm)      ->           Term Object

  -- At holds the schema of the object it's accessing. We do this so we can
  -- determine statically which fields can be accessed.
  At             ::                        Schema      -> Term String              -> Term Object    -> EType -> Term a
  Read           ::                        TableName   -> Schema -> Term String    ->           Term Object
  -- NOTE: pact really does return a string here:
  Write          ::                        TableName -> Term String -> Term Object -> Term String

  Let            ::                        Text         -> ETerm         -> Term a  -> Term a
  -- TODO: not sure if we need a separate `Bind` ctor for object binding. try
  --       just using Let+At first.
  Var            ::                        Text         ->                             Term a

  -- We partition the arithmetic operations in to these classes:
  -- * DecArithOp, IntArithOp: binary operators applied to (and returning) the
  --   same type (either integer or decimal).
  --   - Operations: { + - * / ^ log }
  -- * DecUnaryArithOp, IntUnaryArithOp: unary operators applied to and
  --   returning the same type (integer or decimal).
  --   - Operations: { - (negate) sqrt ln exp abs } (also signum even though
  --     it's not in pact)
  -- * DecIntArithOp, IntDecArithOp: binary operators applied to one integer
  --   and one decimal, returning a decimal. These are overloads of the integer
  --   / decimal binary ops.
  --   - Operations: { + - * / ^ log }
  -- * ModOp: Just modulus (oddly, it's the only operator with signature
  --   `integer -> integer -> integer`.
  --   - Operations: { mod }
  --
  -- * RoundingLikeOp1: Rounding decimals to integers.
  --   - Operations: { round floor ceiling }
  -- * RoundingLikeOp2: Rounding decimals to decimals with a specified level of
  --   precision.
  --   - Operations: { round floor ceiling }
  --
  -- * AddTime: Arguably not an arithmetic op, but under the hood it's just
  --   adding some number of (micro)seconds.

  DecArithOp      :: ArithOp      -> Term Decimal -> Term Decimal -> Term Decimal
  IntArithOp      :: ArithOp      -> Term Integer -> Term Integer -> Term Integer
  DecUnaryArithOp :: UnaryArithOp                 -> Term Decimal -> Term Decimal
  IntUnaryArithOp :: UnaryArithOp                 -> Term Integer -> Term Integer

  DecIntArithOp   :: ArithOp -> Term Decimal -> Term Integer -> Term Decimal
  IntDecArithOp   :: ArithOp -> Term Integer -> Term Decimal -> Term Decimal

  ModOp           :: Term Integer   -> Term Integer -> Term Integer
  RoundingLikeOp1 :: RoundingLikeOp -> Term Decimal -> Term Integer
  RoundingLikeOp2 :: RoundingLikeOp -> Term Decimal -> Term Integer -> Term Decimal

  -- invariant (inaccessible): a ~ Integer or a ~ Decimal
  AddTime         :: Term Time -> ETerm -> Term Time

  Comparison     :: (Show a, SymWord a) => ComparisonOp -> Term a         -> Term a -> Term Bool
  Logical        ::                        LogicalOp    -> [Term Bool]    ->           Term Bool
  NameAuthorized ::                        Term String  ->                             Term Bool
  Concat         ::                        Term String  -> Term String    ->           Term String
  PactVersion    ::                                                                    Term String

  --
  -- TODO: figure out the object representation we use here:
  --
  -- ObjAuthorized  ::                     Term Obj     ->                     Term Bool
  --
  -- TODO: we will also want to handle cases where load a keyset object by its
  -- name, and then use the object: e.g.:
  --
  --   (defconst ADMIN_KEYSET (read-keyset "accounts-admin-keyset"))
  --
  --  and then ADMIN_KEYSET is used in the code
  --

deriving instance Show a => Show (Term a)
deriving instance Show ETerm

deriving instance Show (Type a)
deriving instance Eq (Type a)
deriving instance Show EType

lit :: SymWord a => a -> Term a
lit = Literal . literal

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  (+)    = IntArithOp Add
  (*)    = IntArithOp Mul
  abs    = IntUnaryArithOp Abs
  signum = IntUnaryArithOp Signum
  negate = IntUnaryArithOp Negate

instance Num (Term Decimal) where
  fromInteger = lit . mkDecimal . fromInteger
  (+)    = DecArithOp Add
  (*)    = DecArithOp Mul
  abs    = DecUnaryArithOp Abs
  signum = DecUnaryArithOp Signum
  negate = DecUnaryArithOp Negate

data DomainProperty where
  TableWrite       :: TableName  ->               DomainProperty -- anything in table is written
  TableRead        :: TableName  ->               DomainProperty -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnName -> DomainProperty -- particular column is written
  --
  -- TODO: these properties demonstrate that we probably want to parameterize
  --       by a type (int/bool, etc), and allow appropriate e.g. numeric ops
  --
  CellIncrease     :: TableName  -> ColumnName -> DomainProperty -- any cell at all in col increases
  ColumnConserve   :: TableName  -> ColumnName -> DomainProperty -- sum of all changes in col == 0
  ColumnIncrease   :: TableName  -> ColumnName -> DomainProperty -- sum of all changes in col >  0
  --
  KsNameAuthorized :: KeySetName ->               DomainProperty -- keyset authorized by name
  Abort            ::                             DomainProperty
  Success          ::                             DomainProperty
  --
  -- TODO: row-level keyset enforcement seems like it needs some form of
  --       unification, so that using a variable we can connect >1 domain
  --       property?
  --
  --       e.g.: forall row. RowWrite("balances", r) `Implies` RowKsEnforced(r)
  --
  --       RowKsEnforced  :: RowUid    ->            DomainProperty
  --       RowWrite       :: TableName -> RowUid  -> DomainProperty
  --
  -- TODO: Add DomainProperty/ies for constraining function arguments
  --       - e.g.: x > 10 `Implies` table_write(t0)
  --
  -- TODO: possibly allow use of input as parameter to domain properties
  --       - e.g.: column_increases_by(t0, x)     [where x is function input]
  --
  -- TODO: StaleRead?
  --

data Property where
  Implies :: Property       -> Property -> Property
  Not     :: Property       ->             Property
  And     :: Property       -> Property -> Property
  Or      :: Property       -> Property -> Property
  Occurs  :: DomainProperty ->             Property

data Check where
  Satisfiable :: Property -> Check
  Valid       :: Property -> Check

type Time = Int64

mkTime :: UTCTime -> Time
mkTime utct
  = ((utct ^. _utctDay . to toModifiedJulianDay . to fromIntegral)
    * (1000000 * 60 * 60 * 24))
  + (utct ^. _utctDayTime . microseconds)

-- Pact uses Data.Decimal which is arbitrary-precision
type Decimal = AlgReal

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = fromRational $
  mantissa % 10 ^ places

type SDecimal = (SWord8, SInteger)
