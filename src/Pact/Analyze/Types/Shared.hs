{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Pact.Analyze.Types.Shared where

import           Control.Lens                 (At (at), Index, Iso, Iso',
                                               IxValue, Ixed (ix), Lens',
                                               Prism', both, from, iso, lens,
                                               makeLenses, makePrisms, over,
                                               view, (%~), (&))
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.AffineSpace             ((.+^), (.-.))
import           Data.Coerce                  (Coercible)
import           Data.Data                    (Data)
import qualified Data.Decimal                 as Decimal
import           Data.Function                (on)
import           Data.List                    (sortBy)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.SBV                     (Boolean (bnot, false, true, (&&&), (|||)),
                                               EqSymbolic, HasKind, Int64,
                                               Kind (KString, KUnbounded),
                                               Mergeable (symbolicMerge),
                                               OrdSymbolic, Provable (forAll),
                                               SBV,
                                               SDivisible (sDivMod, sQuotRem),
                                               SymWord, Symbolic, forAll_,
                                               forSome, forSome_, fromBool,
                                               isConcrete, ite, kindOf, literal,
                                               oneIf, sFromIntegral, unliteral,
                                               (%), (.<), (.==))
import           Data.SBV.Control             (SMTValue (..))
import qualified Data.SBV.Internals           as SBVI
import qualified Data.SBV.String              as SBV
import qualified Data.Set                     as Set
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Thyme                   (UTCTime, microseconds)
import           Data.Typeable                ((:~:) (Refl))
import           Prelude                      hiding (Float)

import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Util              (AsString, tShow)

import           Pact.Analyze.Feature         hiding (Type, dec, ks, obj, time)
import           Pact.Analyze.Orphans         ()
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.UserShow


data Located a
  = Located
    { _location :: Pact.Info
    , _located  :: a
    }
  deriving (Eq, Functor, Foldable, Traversable)

deriving instance Show a => Show (Located a)

instance Mergeable a => Mergeable (Located a) where
  symbolicMerge f t (Located i a) (Located i' a') =
    Located (symbolicMerge f t i i') (symbolicMerge f t a a')

data Existential (tm :: * -> *) where
  ESimple :: SimpleType a => Type a -> tm a      -> Existential tm
  EObject ::                 Schema -> tm Object -> Existential tm

-- TODO: when we have quantified constraints we can do this (also for Show):
-- instance (forall a. Eq a => Eq (tm a)) => Eq (Existential tm) where
--   ESimple ta ia == ESimple tb ib = case typeEq ta tb of
--     Just Refl -> ia == ib
--     Nothing   -> False
--   EObject sa pa == EObject sb pb = sa == sb && pa == pb
--   _ == _ = False

transformExistential
  :: (forall a. tm1 a -> tm2 a) -> Existential tm1 -> Existential tm2
transformExistential f term = case term of
  ESimple ty  term' -> ESimple ty  (f term')
  EObject sch term' -> EObject sch (f term')

mapExistential :: (forall a. tm a -> tm a) -> Existential tm -> Existential tm
mapExistential = transformExistential

existentialType :: Existential tm -> EType
existentialType (ESimple ety _) = EType ety
existentialType (EObject sch _) = EObjectTy sch

-- TODO: could implement this stuff generically or add newtype-awareness

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

wrappedIntegerFromCW :: (Integer -> a) -> SBVI.CW -> a
wrappedIntegerFromCW construct (SBVI.CW _ (SBVI.CWInteger i)) = construct i
wrappedIntegerFromCW _ c = error $ "SymWord: Unexpected non-integer value: " ++ show c

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SBVI.SVal KString
                 . Left
                 . SBVI.CW KString
                 . SBVI.CWString

mkConcreteInteger :: Integer -> SBV a
mkConcreteInteger = SBVI.SBV
                  . SBVI.SVal KUnbounded
                  . Left
                  . SBVI.CW KUnbounded
                  . SBVI.CWInteger

newtype PactIso a b = PactIso {unPactIso :: Iso' a b}

decimalIso :: PactIso Decimal.Decimal Decimal
decimalIso = PactIso $ iso mkDecimal unMkDecimal
  where
    unMkDecimal :: Decimal -> Decimal.Decimal
    unMkDecimal (Decimal dec) = case Decimal.eitherFromRational (dec % 10 ^ decimalPrecision) of
      Left err -> error err
      Right d  -> d

    mkDecimal :: Decimal.Decimal -> Decimal
    mkDecimal (Decimal.Decimal places mantissa)
      = lShiftD (decimalPrecision - fromIntegral places) (Decimal mantissa)

fromPact :: PactIso a b -> a -> b
fromPact = view . unPactIso

toPact :: PactIso a b -> b -> a
toPact = view . from . unPactIso

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

instance SMTValue KeySetName where
  sexprToVal = fmap (KeySetName . T.pack) . sexprToVal

instance UserShow KeySetName where
  userShowsPrec _ (KeySetName name) = "'" <> name

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

type RowKey = String

type Time = Int64

timeIso :: PactIso UTCTime Time
timeIso = PactIso $ iso mkTime unMkTime
  where
    mkTime :: UTCTime -> Time
    mkTime utct = view microseconds (utct .-. toEnum 0)

    unMkTime :: Time -> UTCTime
    unMkTime time = toEnum 0 .+^ view (from microseconds) time

data LogicalOp
  = AndOp -- ^ Conjunction
  | OrOp  -- ^ Disjunction
  | NotOp -- ^ Negation
  deriving (Show, Eq, Ord)

logicalOpP :: Prism' Text LogicalOp
logicalOpP = mkOpNamePrism
  [ (SLogicalConjunction, AndOp)
  , (SLogicalDisjunction, OrOp)
  , (SLogicalNegation,    NotOp)
  -- NOTE: that we don't include logical implication here, which only exists in
  -- the invariant and property languages (not term), and is desugared to a
  -- combination of negation and disjunction during parsing.
  ]

instance UserShow LogicalOp where
  userShowsPrec _ = toText logicalOpP

data EqNeq
  = Eq'  -- ^ Equal
  | Neq' -- ^ Not equal
  deriving (Show, Eq, Ord)

eqNeqP :: Prism' Text EqNeq
eqNeqP = mkOpNamePrism
  [ (SEquality,   Eq')
  , (SInequality, Neq')
  ]

instance UserShow EqNeq where
  userShowsPrec _ = toText eqNeqP

data ComparisonOp
  = Gt  -- ^ Greater than
  | Lt  -- ^ Less than
  | Gte -- ^ Greater than or equal to
  | Lte -- ^ Less than or equal to
  | Eq  -- ^ Equal
  | Neq -- ^ Not equal
  deriving (Show, Eq, Ord)

comparisonOpP :: Prism' Text ComparisonOp
comparisonOpP = mkOpNamePrism
  [ (SGreaterThan,        Gt)
  , (SLessThan,           Lt)
  , (SGreaterThanOrEqual, Gte)
  , (SLessThanOrEqual,    Lte)
  , (SEquality,           Eq)
  , (SInequality,         Neq)
  ]

instance UserShow ComparisonOp where
  userShowsPrec _ = toText comparisonOpP

-- | Metadata about a database cell from which a symbolic value originates.
-- This is a separate datatype from 'Provenance' so that we avoid partial field
-- accessors.
data OriginatingCell
  = OriginatingCell
    { _ocTableName  :: TableName
    , _ocColumnName :: ColumnName
    , _ocRowKey     :: S RowKey
    , _ocDirty      :: S Bool
    }
  deriving (Eq, Show)

data Provenance
  = FromCell    OriginatingCell
  | FromNamedKs (S KeySetName)
  | FromInput   Unmunged
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

instance SymbolicDecimal (S Decimal) where
  type IntegerOf (S Decimal) = S Integer
  fromInteger' (S _ a)       = sansProv (fromInteger' a)
  lShiftD  i       (S _ d)   = sansProv (lShiftD  i d)
  lShiftD' (S _ i) (S _ d)   = sansProv (lShiftD' i d)
  rShiftD  i       (S _ d)   = sansProv (rShiftD  i d)
  rShiftD' (S _ i) (S _ d)   = sansProv (rShiftD' i d)
  floorD (S _ d)             = sansProv (floorD d)

-- Caution [OverlappingInstances]: Though it looks like this instance is
-- exactly the same as the one below, do not be deceived, it is not. In this
-- instance `*` resolves to `*` from `instance Num (SBV Decimal)`, which has an
-- `rShift255D`. In the instance below, `*` resolves to `*` from `*` from
-- `instance (Ord a, Num a, SymWord a) => Num (SBV a)`, included in sbv, which
-- does to include the shift. *This instance must be selected for decimals*.
instance {-# OVERLAPPING #-} Num (S Decimal) where
  S _ x + S _ y  = sansProv $ x + y
  S _ x * S _ y  = sansProv $ x * y
  abs (S _ x)    = sansProv $ abs x
  signum (S _ x) = sansProv $ signum x
  fromInteger i  = sansProv $ fromInteger i
  negate (S _ x) = sansProv $ negate x

instance (Num a, SymWord a) => Num (S a) where
  S _ x + S _ y  = sansProv $ x + y
  S _ x * S _ y  = sansProv $ x * y
  abs (S _ x)    = sansProv $ abs x
  signum (S _ x) = sansProv $ signum x
  fromInteger i  = sansProv $ fromInteger i
  negate (S _ x) = sansProv $ negate x

-- Caution: see note [OverlappingInstances] *This instance must be selected for
-- decimals*.
instance {-# OVERLAPPING #-} Fractional (S Decimal) where
  fromRational  = literalS . fromRational
  S _ x / S _ y = sansProv $ x / y

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

fromCell :: TableName -> ColumnName -> S RowKey -> S Bool -> Provenance
fromCell tn cn sRk sDirty = FromCell $ OriginatingCell tn cn sRk sDirty

fromNamedKs :: S KeySetName -> Provenance
fromNamedKs = FromNamedKs

symRowKey :: S String -> S RowKey
symRowKey = coerceS

-- | Typed symbolic value.
type TVal = (EType, AVal)

newtype Object
  = Object (Map Text TVal)
  deriving (Eq, Show, Semigroup)

instance UserShow Object where
  userShowsPrec d (Object m) = userShowsPrec d (fmap snd m)

instance Monoid Object where
  mempty = Object Map.empty

  -- NOTE: left-biased semantics of schemas for Pact's "object merging":
  mappend = (<>)

objFields :: Lens' Object (Map Text TVal)
objFields = lens getter setter
  where
    getter (Object fs) = fs
    setter (Object _) fs' = Object fs'

newtype Schema
  = Schema (Map Text EType)
  deriving (Show, Eq, Semigroup)

instance Monoid Schema where
  mempty = Schema Map.empty

  -- NOTE: left-biased semantics of schemas for Pact's "object merging":
  mappend = (<>)

-- Note: this doesn't exactly match the pact syntax
instance UserShow Schema where
  userShowsPrec d (Schema schema) = userShowsPrec d schema

-- | When given a column mapping, this function gives a canonical way to assign
-- var ids to each column. Also see 'varIdArgs'.
varIdColumns :: Map Text a -> Map Text VarId
varIdColumns m =
  let sortedList = sortBy (compare `on` fst) (Map.toList m)
      reindexedList =
        zipWith (\index (name, _) -> (name, index)) [0..] sortedList
  in Map.fromList reindexedList

-- | Given args representing the columns of a schema, this function gives a
-- canonical assignment of var ids to each column. Also see 'varIdColumns'.
varIdArgs :: [Pact.Arg a] -> [(Pact.Arg a, VarId)]
varIdArgs args =
  let sortedList = sortBy (compare `on` Pact._aName) args
  in zip sortedList [0..]

-- | Untyped symbolic value.
data AVal
  = AVal (Maybe Provenance) SBVI.SVal
  | AnObj Object
  | OpaqueVal
  deriving (Eq, Show)

instance UserShow AVal where
  userShowsPrec _ = \case
    AVal _ sVal -> tShow sVal
    AnObj obj   -> userShow obj
    OpaqueVal   -> "[opaque]"

instance EqSymbolic Object where
  Object fields .== Object fields' =
    let ks  = Map.keysSet fields
        ks' = Map.keysSet fields'
    in if ks == ks'
       then Set.foldl'
              (\acc key -> acc &&&
                ((fields Map.! key) .== (fields' Map.! key)))
              true
              ks
       else false

instance EqSymbolic AVal where
  AVal mProv sv .== AVal mProv' sv' = mkS mProv sv .== mkS mProv' sv'

  AnObj o .== AnObj o' = o .== o'

  -- Not perfect; this would be better if we could easily produce an
  -- uninterpreted bool here. We can't though, because 'uninterpret' takes a
  -- String that must be unique for each allocation.
  OpaqueVal .== OpaqueVal = false

  _ .== _ = false

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

coerceS :: Coercible a b => S a -> S b
coerceS (S mProv a) = S mProv $ coerceSBV a

unsafeCoerceS :: S a -> S b
unsafeCoerceS (S mProv a) = S mProv $ unsafeCoerceSBV a

iteS :: Mergeable a => S Bool -> a -> a -> a
iteS sbool = ite (_sSbv sbool)

fromIntegralS
  :: forall a b
  . (Integral a, SymWord a, Num b, SymWord b)
  => S a
  -> S b
fromIntegralS = over s2Sbv sFromIntegral

oneIfS :: (Num a, SymWord a) => S Bool -> S a
oneIfS = over s2Sbv oneIf

isConcreteS :: SymWord a => S a -> Bool
isConcreteS = isConcrete . _sSbv

data QKind = QType | QAny

-- Integer, Decimal, Bool, String, Time
type SimpleType a = (Show a, SymWord a, SMTValue a, UserShow a)

data Quantifiable :: QKind -> * where
  -- TODO: parametrize over constraint
  EType     :: SimpleType a =>                    Type a -> Quantifiable q
  EObjectTy ::                                    Schema -> Quantifiable q
  QTable    ::                                              Quantifiable 'QAny
  QColumnOf :: TableName                                 -> Quantifiable 'QAny

deriving instance Show (Quantifiable q)

instance Eq (Quantifiable q) where
  EType a == EType b = case typeEq a b of
    Just _refl -> True
    Nothing    -> False
  EObjectTy a == EObjectTy b = a == b
  QTable      == QTable      = True
  QColumnOf a == QColumnOf b = a == b
  _           == _           = False

instance EqSymbolic (Quantifiable q) where
  ety .== ety' = fromBool $ ety == ety'

type EType = Quantifiable 'QType
type QType = Quantifiable 'QAny

coerceQType :: EType -> QType
coerceQType = \case
  EType ty         -> EType ty
  EObjectTy schema -> EObjectTy schema

downcastQType :: QType -> Maybe EType
downcastQType = \case
  EType ty         -> Just $ EType ty
  EObjectTy schema -> Just $ EObjectTy schema
  _                -> Nothing

-- | Unique variable IDs
--
-- 'VarId's are used to represent variables in both the term and property
-- languages. (They should also be used for the schema invariant language).
--
-- These IDs were first introduced to cope with the prenex normalization
-- transform, where we lift quantifiers all the way to the outside of a
-- property, which can easily lead to name confusion (see commit 1f6201).
--
-- But they also serve as a principled connection between the term and property
-- languages, since properties can refer to term variables.
--
-- In translation, unique IDs are generated in three places:
--
-- 1) @checkTopFunction@ generates a unique id for each function argument
-- 2) @genUid@ generates an id for a let-binding
-- 3) @translateBinding@ generates a fresh variable for its synthetic "binding"
--    var
newtype VarId
  = VarId Int
  deriving (Show, Eq, Enum, Num, Ord)

-- | Identifier name that is guaranteed to be unique because it contains a
-- unique identifier. These names are generated upstream in the typechecker
-- (using 'freshId').
newtype Munged
  = Munged Text
  deriving (Eq, Show)

-- | A user-supplied (i.e. non-unique) identifer name.
newtype Unmunged
  = Unmunged Text
  deriving (Eq, Show)

data Binding
  = Binding
    { _bVid   :: VarId
    , _buName :: Unmunged
    , _bmName :: Munged
    , _bType  :: EType
    }
  deriving (Eq, Show)

-- | The type of a pact data type we can't reason about -- see 'TAny'.
data Any = Any
  deriving (Show, Read, Eq, Ord, Data)

instance UserShow Any where
  userShowsPrec _ Any = "*"

instance HasKind Any
instance SymWord Any
instance SMTValue Any

newtype KeySet
  = KeySet Integer
  deriving (Eq, Ord, Data, Show, Read, UserShow)

instance SymWord KeySet where
  mkSymWord = SBVI.genMkSymVar KUnbounded
  literal (KeySet s) = mkConcreteInteger s
  fromCW = wrappedIntegerFromCW KeySet

instance HasKind KeySet where
  kindOf _ = KUnbounded

instance SMTValue KeySet where
  sexprToVal = fmap KeySet . sexprToVal

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

instance UserShow (Type a) where
  userShowsPrec _ = \case
    TInt     -> "integer"
    TBool    -> "bool"
    TStr     -> "string"
    TTime    -> "time"
    TDecimal -> "decimal"
    TKeySet  -> "keyset"
    TAny     -> "*"

columnMapToSchema :: ColumnMap EType -> Schema
columnMapToSchema
  = Schema
  . Map.fromList
  . fmap (\(ColumnName name, ety) -> (fromString name, ety))
  . Map.toList
  . _columnMap

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Semigroup, Monoid)

instance Mergeable a => Mergeable (ColumnMap a) where
  symbolicMerge force test (ColumnMap left) (ColumnMap right) = ColumnMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

newtype TableMap a
  = TableMap { _tableMap :: Map TableName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Mergeable a => Mergeable (TableMap a) where
  symbolicMerge force test (TableMap left) (TableMap right) = TableMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right


instance UserShow (Quantifiable q) where
  userShowsPrec d = \case
    EType ty     -> userShowsPrec d ty
    EObjectTy ty -> userShowsPrec d ty
    QTable       -> "table"
    QColumnOf tn -> "(column-of " <> userShow tn <> ")"

instance UserShow TableName where
  userShowsPrec _ (TableName tn) = T.pack tn

instance UserShow ColumnName where
  userShowsPrec _ (ColumnName cn) = T.pack cn

data DefinedProperty a = DefinedProperty
  { propertyArgs :: [(Text, QType)]
  , propertyBody :: a
  } deriving Show

makeLenses ''Located
makePrisms ''AVal
makeLenses ''ColumnMap
makeLenses ''Object
makeLenses ''OriginatingCell
makePrisms ''Provenance
makeLenses ''S
makeLenses ''Binding
makeLenses ''TableMap

type instance Index (ColumnMap a) = ColumnName
type instance IxValue (ColumnMap a) = a
instance Ixed (ColumnMap a) where ix k = columnMap.ix k
instance At (ColumnMap a) where at k = columnMap.at k

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k
