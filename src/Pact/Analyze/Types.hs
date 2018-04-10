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
import Data.Set (Set)
import qualified Data.Set as Set
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

-- a unique column, comprised of table name and column name
-- e.g. accounts__balance
newtype ColumnId
  = ColumnId String
  deriving (Eq, Ord)

instance SymWord ColumnId where
  mkSymWord = SBVI.genMkSymVar KString
  literal (ColumnId cid) = mkConcreteString cid
  fromCW = wrappedStringFromCW ColumnId

instance HasKind ColumnId where
  kindOf _ = KString

instance IsString ColumnId where
  fromString = ColumnId

-- a unique cell, from a column name and a row key
-- e.g. balance__25
newtype CellId
  = CellId String
  deriving (Eq, Ord)

instance SymWord CellId where
  mkSymWord = SBVI.genMkSymVar KString
  literal (CellId cid) = mkConcreteString cid
  fromCW = wrappedStringFromCW CellId

instance HasKind CellId where
  kindOf _ = KString

instance IsString CellId where
  fromString = CellId

data UserType = UserType
  deriving (Eq, Ord, Read, Data, Show)

deriving instance HasKind UserType
deriving instance SymWord UserType

data ArithOp
  = Sub -- "-" Integer / Decimal
  | Add -- "+"
  | Mul -- "*"
  | Div -- "/"

  | Pow -- "^"
  | Sqrt -- "sqrt"
  | Mod -- "mod"
  | Log -- "log"
  | Ln -- "ln"

  | Exp -- "exp"
  | Abs -- "abs"
  | Round -- "round"
  | Ceiling -- "ceiling"
  | Floor -- "floor"

  -- Extras not necessarily in pact
  -- @Signum@ because we want (Term Integer) to be a Num instance
  | Signum
  -- @Negate@ because @-@ is overloaded in pact to mean "minus" or "negate"
  | Negate
  deriving (Show, Eq, Ord)

unsupportedArithOps :: Set ArithOp
unsupportedArithOps = Set.fromList
  -- TODO(joel) support Exp with svExp?
  [Pow, Sqrt, Log, Ln, Exp, Round, Ceiling, Floor]

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

  --
  -- TODO: retire:
  --
  WithRead       :: (Show a) => TableName -> Term String -> [Text] -> Term a -> Term a

  Let            ::                        Text         -> ETerm         -> Term a  -> Term a
  -- TODO: not sure if we need a separate `Bind` ctor for object binding. try
  --       just using Let+At first.
  Var            ::                        Text         ->                             Term a
  Arith          ::                        ArithOp      -> [Term Integer] ->           Term Integer
  Comparison     :: (Show a, SymWord a) => ComparisonOp -> Term a         -> Term a -> Term Bool
  Logical        ::                        LogicalOp    -> [Term Bool]    ->           Term Bool
  AddTimeInt     ::                        Term Time    -> Term Integer   ->           Term Time
  AddTimeDec     ::                        Term Time    -> Term Decimal   ->           Term Time
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

instance Num (Term Integer) where
  fromInteger = Literal . fromInteger
  x + y = Arith Add [x, y]
  x * y = Arith Mul [x, y]
  abs x = Arith Abs [x]
  signum x = Arith Signum [x]
  negate x = Arith Negate [x]

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
mkTime utct = utct ^. _utctDayTime . microseconds

-- Pact uses Data.Decimal which is arbitrary-precision
data Decimal = Decimal
  { decimalPlaces :: !Word8
  , decimalMantissa :: !Integer
  } deriving (Show, Read, Eq, Ord, Data, HasKind, SymWord)

mkDecimal :: Decimal.Decimal -> Decimal
mkDecimal (Decimal.Decimal places mantissa) = Decimal places mantissa
