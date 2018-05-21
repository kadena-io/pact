{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Pact.Analyze.Types where

import           Control.Lens       (Iso, Lens', both, from, iso, lens,
                                     makeLenses, over, to, (%~), (&), (^.))
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Data          (Data)
import qualified Data.Decimal       as Decimal
import           Data.Map.Strict    (Map)
import           Data.SBV           (AlgReal,
                                     Boolean (bnot, false, true, (&&&), (|||)),
                                     EqSymbolic, HasKind, Int64, Kind (KString),
                                     Mergeable (symbolicMerge), OrdSymbolic,
                                     Provable (forAll), SBV,
                                     SDivisible (sDivMod, sQuotRem), SymWord,
                                     Symbolic, Word8, forAll_, forSome,
                                     forSome_, isConcrete, ite, kindOf, literal,
                                     oneIf, sFromIntegral, sRealToSInteger,
                                     unliteral, (%), (.<), (.==))
import qualified Data.SBV.Internals as SBVI
import qualified Data.SBV.String    as SBV
import           Data.Semigroup     ((<>))
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.String        (IsString (..))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Thyme         (UTCTime, microseconds, toModifiedJulianDay,
                                     _utctDay, _utctDayTime)
import           Data.Typeable      ((:~:) (Refl), Typeable, eqT)

import           Pact.Types.Util    (AsString)

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
      _         -> False
deriving instance Show Ty

-- Pact uses Data.Decimal which is arbitrary-precision
type Decimal = AlgReal

type Time = Int64

mkTime :: UTCTime -> Time
mkTime utct
  = ((utct ^. _utctDay . to toModifiedJulianDay . to fromIntegral)
    * (1000000 * 60 * 60 * 24))
  + (utct ^. _utctDayTime . microseconds)

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

data EqNeq = Eq' | Neq'
  deriving (Show, Eq)

data ComparisonOp = Gt | Lt | Gte | Lte | Eq | Neq
  deriving (Show, Eq)

data Provenance
  = Provenance
    { _provTableName  :: TableName
    , _provColumnName :: ColumnName
    , _provRowKey     :: S RowKey
    , _provDirty      :: S Bool
    }
  deriving (Eq, Show)

-- Symbolic value carrying provenance, for tracking if values have come from a
-- particular table+row.
data S a
  = S
    { _sProv :: Maybe Provenance
    , _sSbv  :: SBV a }
  deriving (Eq, Show)

sansProv :: SBV a -> S a
sansProv = S Nothing

withProv :: Provenance -> SBV a -> S a
withProv prov sym = S (Just prov) sym

instance SymWord a => Mergeable (S a) where
  symbolicMerge f t (S mProv1 x) (S mProv2 y)
    | mProv1 == mProv2 = S mProv1 $ symbolicMerge f t x y
    | otherwise        = sansProv $ symbolicMerge f t x y

-- We provide instances for EqSymbolic, OrdSymboic, Boolean because we need
-- these operators for `S a` now that we work with that instead of `SBV a`
-- everywhere:

instance EqSymbolic (S a) where
  S _ x .== S _ y = x .== y

instance SymWord a => OrdSymbolic (S a) where
  S _ x .< S _ y = x .< y

-- We don't care about preserving the provenance value here as we are most
-- interested in tracking `SBV KeySet`s, but really as soon as we apply a
-- transformation to a symbolic value, we are no longer working with the value
-- that was sourced from the database.
instance Boolean (S Bool) where
  true            = sansProv true
  false           = sansProv false
  bnot (S _ x)    = sansProv $ bnot x
  S _ x &&& S _ y = sansProv $ x &&& y
  S _ x ||| S _ y = sansProv $ x ||| y

instance IsString (S String) where
  fromString = sansProv . fromString

instance (Num a, SymWord a) => Num (S a) where
  S _ x + S _ y  = sansProv $ x + y
  S _ x * S _ y  = sansProv $ x * y
  abs (S _ x)    = sansProv $ abs x
  signum (S _ x) = sansProv $ signum x
  fromInteger i  = sansProv $ fromInteger i
  negate (S _ x) = sansProv $ negate x

instance (Fractional a, SymWord a) => Fractional (S a) where
  fromRational = literalS . fromRational
  S _ x / S _ y = sansProv $ x / y

instance SDivisible (S Integer) where
  S _ a `sQuotRem` S _ b = a `sQuotRem` b & both %~ sansProv
  S _ a `sDivMod`  S _ b = a `sDivMod`  b & both %~ sansProv

type PredicateS = Symbolic (S Bool)

instance Provable PredicateS where
  forAll_   = fmap _sSbv
  forAll _  = fmap _sSbv
  forSome_  = fmap _sSbv
  forSome _ = fmap _sSbv

-- Until SBV adds a typeclass for strConcat/(.++):
(.++) :: S String -> S String -> S String
S _ a .++ S _ b = sansProv $ SBV.concat a b

-- Beware: not a law-abiding Iso. Drops provenance info.
sbv2S :: Iso (SBV a) (SBV b) (S a) (S b)
sbv2S = iso sansProv _sSbv

mkProv :: TableName -> ColumnName -> S RowKey -> S Bool -> Provenance
mkProv tn cn sRk sDirty = Provenance tn cn sRk sDirty

symRowKey :: S String -> S RowKey
symRowKey = coerceS

data Object
  = Object (Map String (EType, AVal))
  deriving (Eq, Show)

newtype Schema
  = Schema (Map String EType)
  deriving (Show, Eq)

-- | Untyped symbolic value.
data AVal
  = AVal (Maybe Provenance) SBVI.SVal
  | AnObj Object
  | OpaqueVal
  deriving (Eq, Show)

mkS :: Maybe Provenance -> SBVI.SVal -> S a
mkS mProv sval = S mProv (SBVI.SBV sval)

literalS :: SymWord a => a -> S a
literalS = sansProv . literal

unliteralS :: SymWord a => S a -> Maybe a
unliteralS = unliteral . _sSbv

sbv2SFrom :: Provenance -> Iso (SBV a) (SBV b) (S a) (S b)
sbv2SFrom prov = iso (withProv prov) _sSbv

s2Sbv :: Iso (S a) (S b) (SBV a) (SBV b)
s2Sbv = from sbv2S

mkAVal :: S a -> AVal
mkAVal (S mProv (SBVI.SBV sval)) = AVal mProv sval

mkAVal' :: SBV a -> AVal
mkAVal' (SBVI.SBV sval) = AVal Nothing sval

coerceSBV :: SBV a -> SBV b
coerceSBV = SBVI.SBV . SBVI.unSBV

coerceS :: S a -> S b
coerceS (S mProv a) = S mProv $ coerceSBV a

iteS :: Mergeable a => S Bool -> a -> a -> a
iteS sbool = ite (_sSbv sbool)

fromIntegralS
  :: forall a b
  .  (Integral a, HasKind a, Num a, SymWord a, HasKind b, Num b, SymWord b)
  => S a
  -> S b
fromIntegralS = over s2Sbv sFromIntegral

realToIntegerS :: S Decimal -> S Integer
realToIntegerS = over s2Sbv sRealToSInteger

oneIfS :: (Num a, SymWord a) => S Bool -> S a
oneIfS = over s2Sbv oneIf

isConcreteS :: SymWord a => S a -> Bool
isConcreteS = isConcrete . _sSbv

data EType where
  -- TODO: parametrize over constraint
  EType :: (Show a, SymWord a) => Type a -> EType
  EObjectTy :: Schema -> EType

deriving instance Show EType

instance Eq EType where
  EType a == EType b = case typeEq a b of
    Just _refl -> True
    Nothing    -> False
  EObjectTy a == EObjectTy b = a == b
  _ == _ = False

data Prop a where
  -- Literals
  PLit             :: SymWord a => a   -> Prop a
  PSym             ::              S a -> Prop a

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

  -- Object ops
  -- Note: PAt is the one property we can't yet parse because of the EType it
  -- includes
  PAt              :: Schema -> Prop String -> Prop Object -> EType -> Prop a

  -- String ops
  PStrConcat       :: Prop String -> Prop String -> Prop String
  PStrLength       :: Prop String ->                Prop Integer

  -- Numeric ops
  PDecArithOp      :: ArithOp        -> Prop Decimal -> Prop Decimal -> Prop Decimal
  PIntArithOp      :: ArithOp        -> Prop Integer -> Prop Integer -> Prop Integer
  PDecUnaryArithOp :: UnaryArithOp   -> Prop Decimal ->                 Prop Decimal
  PIntUnaryArithOp :: UnaryArithOp   -> Prop Integer ->                 Prop Integer
  PDecIntArithOp   :: ArithOp        -> Prop Decimal -> Prop Integer -> Prop Decimal
  PIntDecArithOp   :: ArithOp        -> Prop Integer -> Prop Decimal -> Prop Decimal
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
  TableWrite       :: TableName  ->                Prop Bool    -- anything in table is written
  TableRead        :: TableName  ->                Prop Bool    -- anything in table is read
  ColumnWrite      :: TableName  -> ColumnName  -> Prop Bool    -- particular column is written
  CellIncrease     :: TableName  -> ColumnName  -> Prop Bool    -- any cell at all in col increases

  IntCellDelta     :: TableName  -> ColumnName  -> Prop RowKey -> Prop Integer
  DecCellDelta     :: TableName  -> ColumnName  -> Prop RowKey -> Prop Decimal
  IntColumnDelta   :: TableName  -> ColumnName                 -> Prop Integer
  DecColumnDelta   :: TableName  -> ColumnName                 -> Prop Decimal

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
    getter (Valid p)       = p

    setter (Satisfiable _) p = Satisfiable p
    setter (Valid _) p       = Valid p

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

-- The schema invariant language consists of:
--
-- * comparisons
--   - { <, >, <=, >= } apply to { integer, decimal, string, time }
--   - { =, != } apply to { integer, decimal, string, time, bool, keyset }
-- * literals
-- * variables
-- * logical operations
--
-- The language is stateless. Arithmetic could be added if we decide it's
-- useful.
data SchemaInvariant a where

  -- comparisons
  SchemaDecimalComparison
    :: ComparisonOp
    -> SchemaInvariant Decimal
    -> SchemaInvariant Decimal
    -> SchemaInvariant Bool

  SchemaIntComparison
    :: ComparisonOp
    -> SchemaInvariant Integer
    -> SchemaInvariant Integer
    -> SchemaInvariant Bool

  SchemaStringComparison
    :: ComparisonOp
    -> SchemaInvariant String
    -> SchemaInvariant String
    -> SchemaInvariant Bool

  SchemaTimeComparison
    :: ComparisonOp
    -> SchemaInvariant Time
    -> SchemaInvariant Time
    -> SchemaInvariant Bool

  SchemaBoolEqNeq
    :: EqNeq
    -> SchemaInvariant Bool
    -> SchemaInvariant Bool
    -> SchemaInvariant Bool

  SchemaKeySetEqNeq
    :: EqNeq
    -> SchemaInvariant KeySet
    -> SchemaInvariant KeySet
    -> SchemaInvariant Bool

  -- literals
  SchemaDecimalLiteral :: Decimal -> SchemaInvariant Decimal
  SchemaIntLiteral     :: Integer -> SchemaInvariant Integer
  SchemaStringLiteral  :: Text    -> SchemaInvariant String
  SchemaTimeLiteral    :: Time    -> SchemaInvariant Time
  SchemaBoolLiteral    :: Bool    -> SchemaInvariant Bool

  -- variables
  SchemaVar :: Text -> SchemaInvariant a

  -- logical operations
  SchemaLogicalOp
    :: LogicalOp
    -> [SchemaInvariant Bool]
    -> SchemaInvariant Bool

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
  SchemaIntComparison _ a b     -> invariantVars a <> invariantVars b
  SchemaStringComparison _ a b  -> invariantVars a <> invariantVars b
  SchemaTimeComparison _ a b    -> invariantVars a <> invariantVars b
  SchemaBoolEqNeq _ a b         -> invariantVars a <> invariantVars b
  SchemaKeySetEqNeq _ a b       -> invariantVars a <> invariantVars b

  SchemaDecimalLiteral _        -> Set.empty
  SchemaIntLiteral _            -> Set.empty
  SchemaStringLiteral _         -> Set.empty
  SchemaTimeLiteral _           -> Set.empty
  SchemaBoolLiteral _           -> Set.empty

  SchemaVar v                   -> Set.singleton v

  SchemaLogicalOp _ invariants  -> Set.unions (invariantVars <$> invariants)

makeLenses ''S
makeLenses ''Object
