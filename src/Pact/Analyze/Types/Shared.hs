{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Pact.Analyze.Types.Shared where

import           Control.Lens                 (At (at), Index, Iso, IxValue,
                                               Ixed (ix), Lens', Prism', both,
                                               from, iso, lens, makeLenses,
                                               makePrisms, over, view, (%~),
                                               (&))
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.AffineSpace             ((.+^), (.-.))
import           Data.Coerce                  (Coercible, coerce)
import           Data.Constraint              (Dict (Dict), withDict)
import           Data.Data                    (Data, Typeable, Proxy)
import           Data.Function                (on)
import           Data.Kind                    (Type)
import           Data.List                    (sortBy)
import           Data.Maybe                   (isJust)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.SBV.Trans               (MProvable (..), mkSymWord)
import           Data.SBV                     (EqSymbolic, HasKind, Int64,
                                               Kind (KString, KUnbounded),
                                               Mergeable (symbolicMerge),
                                               OrdSymbolic,
                                               SBV,
                                               SDivisible (sDivMod, sQuotRem),
                                               SymWord(..), Symbolic,
                                               isConcrete, ite, kindOf, literal,
                                               oneIf, sFromIntegral, unliteral,
                                               (.<), (.==), fromCW)
import qualified Data.SBV                     as SBV
import           Data.SBV.Control             (SMTValue (..))
import           Data.SBV.Internals           (CWVal(..), CW(..), genMkSymVar, SVal(SVal), Kind(..))
import qualified Data.SBV.Internals           as SBVI
import qualified Data.SBV.String              as SBV
import qualified Data.Set                     as Set
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Thyme                   (UTCTime, microseconds)
import           Data.Type.Equality           ((:~:) (Refl))
import           GHC.TypeLits
import           Prelude                      hiding (Float)

import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Util              (AsString, tShow)

import           Pact.Analyze.Feature         hiding (Type, dec, ks, obj, str,
                                               time, Constraint)
import           Pact.Analyze.Orphans         ()
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Types
import           Pact.Analyze.Types.UserShow
import           Pact.Analyze.Util            (Boolean(..))


class IsTerm tm where
  singEqTm'       :: SingTy ty -> tm ty -> tm ty -> Bool
  singShowsTm'    :: SingTy ty -> Int   -> tm ty -> ShowS
  singUserShowTm' :: SingTy ty ->          tm ty -> Text

eqTm :: (SingI ty, IsTerm tm) => tm ty -> tm ty -> Bool
eqTm = singEqTm' sing

showsTm :: (SingI ty, IsTerm tm) => Int -> tm ty -> ShowS
showsTm = singShowsTm' sing

showTm :: (SingI ty, IsTerm tm) => tm ty -> String
showTm tm = showsTm 0 tm ""

userShowTm :: (SingI ty, IsTerm tm) => tm ty -> Text
userShowTm = singUserShowTm' sing

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

data Existential (tm :: Ty -> Type) where
  Existential :: SingTy a -> tm a -> Existential tm

instance IsTerm tm => Eq (Existential tm) where
  Existential tya a == Existential tyb b = case singEq tya tyb of
    Nothing   -> False
    Just Refl -> singEqTm' tya a b

instance IsTerm tm => Show (Existential tm) where
  showsPrec p (Existential ty tm) = singShowsTm' ty p tm

instance IsTerm tm => UserShow (Existential tm) where
  userShowPrec _ (Existential ty tm) = singUserShowTm' ty tm

transformExistential
  :: (forall a. tm1 a -> tm2 a) -> Existential tm1 -> Existential tm2
transformExistential f (Existential ty term') = Existential ty (f term')

mapExistential :: (forall a. tm a -> tm a) -> Existential tm -> Existential tm
mapExistential = transformExistential

existentialType :: Existential tm -> EType
existentialType (Existential ety _) = EType ety

-- TODO: could implement this stuff generically or add newtype-awareness

wrappedStringFromCW :: (String -> a) -> SBVI.CW -> a
wrappedStringFromCW construct (SBVI.CW _ (SBVI.CWString s)) = construct s
wrappedStringFromCW _ c = error $ "SymWord: Unexpected non-string value: " ++ show c

wrappedIntegerFromCW :: (Integer -> a) -> SBVI.CW -> a
wrappedIntegerFromCW construct (SBVI.CW _ (SBVI.CWInteger i)) = construct i
wrappedIntegerFromCW _ c = error $ "SymWord: Unexpected non-integer value: " ++ show c

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SVal KString
                 . Left
                 . CW KString
                 . CWString

mkConcreteInteger :: Integer -> SBV a
mkConcreteInteger = SBVI.SBV
                  . SVal KUnbounded
                  . Left
                  . CW KUnbounded
                  . CWInteger

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
  userShowPrec _ (KeySetName name) = "'" <> name

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

newtype Str = Str String
  deriving (Eq, Ord, Show, SMTValue, HasKind, Typeable, IsString)

instance SymWord Str where
  mkSymWord = SBVI.genMkSymVar KString
  literal (Str s) = mkConcreteString s
  fromCW = wrappedStringFromCW Str

instance UserShow Str where
  userShowPrec _ (Str str) = "\"" <> T.pack str <> "\""

type RowKey = Str

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
  userShowPrec _ = toText logicalOpP

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
  userShowPrec _ = toText eqNeqP

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
  userShowPrec _ = toText comparisonOpP

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
data S (a :: Type)
  = S
    { _sProv :: Maybe Provenance
    , _sSbv  :: SBV a }
  deriving (Eq, Show)

sansProv :: SBV a -> S a
sansProv = S Nothing

withProv :: Provenance -> SBV a -> S a
withProv prov sym = S (Just prov) sym

instance (SymWord a) => Mergeable (S a) where
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
  sTrue           = sansProv SBV.sTrue
  sFalse          = sansProv SBV.sFalse
  sNot (S _ x)    = sansProv $ SBV.sNot x
  S _ x .&& S _ y = sansProv $ x SBV..&& y
  S _ x .|| S _ y = sansProv $ x SBV..|| y

instance IsString (S String) where
  fromString = sansProv . fromString

instance IsString (S Str) where
  fromString = coerceS @String @Str . fromString

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

instance MProvable IO PredicateS where
  forAll_   = fmap _sSbv
  forAll _  = fmap _sSbv
  forSome_  = fmap _sSbv
  forSome _ = fmap _sSbv

-- Until SBV adds a typeclass for strConcat/(.++):
(.++) :: S Str -> S Str -> S Str
S _ a .++ S _ b = sansProv $ coerceSBV $ SBV.concat (coerceSBV a) (coerceSBV b)

-- Beware: not a law-abiding Iso. Drops provenance info.
sbv2S :: Iso (SBV a) (SBV b) (S a) (S b)
sbv2S = iso sansProv _sSbv

fromCell :: TableName -> ColumnName -> S RowKey -> S Bool -> Provenance
fromCell tn cn sRk sDirty = FromCell $ OriginatingCell tn cn sRk sDirty

fromNamedKs :: S KeySetName -> Provenance
fromNamedKs = FromNamedKs

symRowKey :: S Str -> S RowKey
symRowKey = coerceS

-- | Typed symbolic value.
type TVal = (EType, AVal)

data Column tm a where
  Column :: IsTerm tm => !(SingTy a) -> !(tm a) -> Column tm a

instance (Eq (SingTy a), Eq (tm a)) => Eq (Column tm a) where
  Column _ a == Column _ b = a == b
instance (Ord (SingTy a), Ord (tm a)) => Ord (Column tm a) where
  Column _ a `compare` Column _ b = a `compare` b

data Object (tm :: Ty -> *) (m :: [(Symbol, Ty)])
  = Object !(HListOf (Column tm) m)

objectLookup :: Object tm m -> String -> Maybe (Existential (Column tm))
objectLookup (Object cols) name = objectLookup' cols where
  objectLookup'
    :: forall tm m. HListOf (Column tm) m -> Maybe (Existential (Column tm))
  objectLookup' NilOf = Nothing
  objectLookup' (ConsOf k c@(Column ty _) cols')
    = if symbolVal k == name
      then Just $ Existential ty c
      else objectLookup' cols'

data EObject tm where
  EObject :: SingList m -> Object tm m -> EObject tm

instance UserShow (tm ('TyObject m)) => UserShow (Object tm m) where
  userShowPrec _ (Object vals)
    = "{" <> T.intercalate ", " (userShowVals vals) <> "}"
      where
      userShowVals :: HListOf (Column tm) m' -> [Text]
      userShowVals NilOf = []
      userShowVals (ConsOf k (Column ty v) m')
        = T.pack (symbolVal k) <> " := " <> singUserShowTm' ty v : userShowVals m'

instance Show (tm ('TyObject m)) => Show (Object tm m) where
  showsPrec p (Object vals) = showParen (p > 10) $
    showString "Object " . showsVals vals
    where showsVals :: HListOf (Column tm) m' -> ShowS
          showsVals NilOf = showString "NilOf"
          showsVals (ConsOf k (Column singv v) m') = showParen True $
              showString "ConsOf "
            . showString (symbolVal k)
            . showString " "
            . singShowsTm' singv 11 v
            . showString " "
            . showsVals m'

instance Eq (tm ('TyObject m)) => Eq (Object tm m) where
  Object vals1 == Object vals2 = eq vals1 vals2 where
    eq :: HListOf (Column tm) m' -> HListOf (Column tm) m' -> Bool
    eq NilOf NilOf = True
    eq (ConsOf k1 (Column singv v1) m1')
       (ConsOf k2 (Column _     v2) m2')
      = eqSymB k1 k2 && singEqTm' singv v1 v2 && eq m1' m2'

data ESchema where
  ESchema :: SingTy ('TyObject m) -> ESchema

instance Eq ESchema where
  -- Since this is a singleton, checking the types match is good enough
  ESchema ty1 == ESchema ty2 = isJust $ singEq ty1 ty2

instance Show ESchema where
  showsPrec p (ESchema ty) = showParen (p > 10) $
      showString "ESchema "
    . showsPrec 11 ty

-- | When given a column mapping, this function gives a canonical way to assign
-- var ids to each column. Also see 'varIdArgs'.
varIdColumns :: Sing (m :: [ (Symbol, Ty) ]) -> Map Text VarId
varIdColumns = Map.fromList . snd . varIdColumns' where
  varIdColumns' :: Sing (m :: [ (Symbol, Ty) ]) -> (VarId, [(Text, VarId)])
  varIdColumns' SNil = (0, [])
  varIdColumns' (SCons k _ty tys) = case varIdColumns' tys of
    (i, tys') -> (succ i, (T.pack (symbolVal k), i) : tys')

-- | Given args representing the columns of a schema, this function gives a
-- canonical assignment of var ids to each column. Also see 'varIdColumns'.
varIdArgs :: [Pact.Arg a] -> [(Pact.Arg a, VarId)]
varIdArgs args =
  let sortedList = sortBy (compare `on` Pact._aName) args
  in zip sortedList [0..]

-- | Untyped object
newtype UObject = UObject (Map.Map Text TVal)
  deriving (Eq, Show, Semigroup, Monoid)

instance UserShow UObject where
  userShowPrec d (UObject m) = userShowPrec d (fmap snd m)

objFields :: Lens' UObject (Map.Map Text TVal)
objFields = lens getter setter
  where
    getter (UObject fs)    = fs
    setter (UObject _) fs' = UObject fs'

-- | Untyped symbolic value.
data AVal
  = AVal (Maybe Provenance) SBVI.SVal
  | OpaqueVal
  deriving (Eq, Show)

instance UserShow AVal where
  userShowPrec _ = \case
    AVal _ sVal -> tShow sVal
    OpaqueVal   -> "[opaque]"

instance EqSymbolic UObject where
  UObject fields .== UObject fields' =
    let ks  = Map.keysSet fields
        ks' = Map.keysSet fields'
    in if ks == ks'
       then Set.foldl'
              (\acc key -> acc .&&
                ((fields Map.! key) .== (fields' Map.! key)))
              sTrue
              ks
       else sFalse

instance EqSymbolic AVal where
  AVal mProv sv .== AVal mProv' sv' = mkS mProv sv .== mkS mProv' sv'

  -- Not perfect; this would be better if we could easily produce an
  -- uninterpreted bool here. We can't though, because 'uninterpret' takes a
  -- String that must be unique for each allocation.
  OpaqueVal .== OpaqueVal = sFalse

  _ .== _ = sFalse

mkS :: Maybe Provenance -> SVal -> S a
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

coerceS :: forall a b. Coercible a b => S a -> S b
coerceS (S mProv a) = S mProv $ coerceSBV a

unsafeCoerceS :: S a -> S b
unsafeCoerceS (S mProv a) = S mProv $ unsafeCoerceSBV a

iteS :: Mergeable a => S Bool -> a -> a -> a
iteS sbool = ite (_sSbv sbool)

fromIntegralS
  :: forall a b. (Integral a, SymWord a, Num b, SymWord b)
  => S a
  -> S b
fromIntegralS = over s2Sbv sFromIntegral

oneIfS :: (Num a, SymWord a) => S Bool -> S a
oneIfS = over s2Sbv oneIf

isConcreteS :: SymWord a => S a -> Bool
isConcreteS = isConcrete . _sSbv

data QKind = QType | QAny

data Quantifiable :: QKind -> Type where
  EType     :: SingTy a  -> Quantifiable q
  QTable    ::              Quantifiable 'QAny
  QColumnOf :: TableName -> Quantifiable 'QAny

deriving instance Show (Quantifiable q)

instance Eq (Quantifiable q) where
  EType a == EType b = case singEq a b of
    Just Refl -> True
    Nothing   -> False
  QTable      == QTable      = True
  QColumnOf a == QColumnOf b = a == b
  _           == _           = False

instance EqSymbolic (Quantifiable q) where
  ety .== ety' = fromBool $ ety == ety'

type EType = Quantifiable 'QType
type QType = Quantifiable 'QAny

coerceQType :: EType -> QType
coerceQType (EType ty) = EType ty

downcastQType :: QType -> Maybe EType
downcastQType = \case
  EType ty -> Just $ EType ty
  _        -> Nothing

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
  userShowPrec _ Any = "*"

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

type family Concrete (a :: Ty) where
  Concrete 'TyInteger     = Integer
  Concrete 'TyBool        = Bool
  Concrete 'TyStr         = Str
  Concrete 'TyTime        = Time
  Concrete 'TyDecimal     = Decimal
  Concrete 'TyKeySet      = KeySet
  Concrete 'TyAny         = Any
  Concrete ('TyList a)    = [Concrete a]
  Concrete ('TyObject ty) = ConcreteObj ty

type family ConcreteObj (a :: [(Symbol, Ty)]) where
  ConcreteObj '[]               = ()
  ConcreteObj ('(k', v) ': kvs) = (Concrete v, ConcreteObj kvs)

newtype AConcrete ty = AConcrete (Concrete ty)

instance (Eq (Concrete ty)) => Eq (AConcrete ty) where
  AConcrete a == AConcrete b = a == b

instance (Show (Concrete ty)) => Show (AConcrete ty) where
  showsPrec p (AConcrete a) = showParen (p > 10) $
    showString "AConcrete " . showsPrec 11 a

instance (UserShow (Concrete ty)) => UserShow (AConcrete ty) where
  userShowPrec p (AConcrete a) = userShowPrec p a

instance SMTValue (Concrete ty) => SMTValue (AConcrete ty) where
  sexprToVal = fmap AConcrete . sexprToVal

instance IsTerm AConcrete where
  singEqTm' ty (AConcrete a) (AConcrete b) = withEq ty $ a == b
  singShowsTm' ty p tm = withShow ty $ showsPrec p tm
  singUserShowTm' ty tm = withUserShow ty $ userShowPrec 0 tm

instance Ord (Concrete ty) => Ord (AConcrete ty) where
  compare (AConcrete a) (AConcrete b) = compare a b

instance HasKind (Concrete ty) => HasKind (AConcrete ty) where
  kindOf (AConcrete a) = kindOf a

instance
  ( Typeable ty
  , SymWord (Concrete ty)
  , SingI ty
  ) => SymWord (AConcrete ty) where
  mkSymWord q name = coerce @(SBV (Concrete ty)) @(SBV (AConcrete ty))
    <$> withSymWord (sing :: SingTy ty) (mkSymWord q name)
  literal (AConcrete a) = coerce $ literal a
  fromCW    = AConcrete . fromCW

newtype AnSBV ty = AnSBV (SBV (Concrete ty))

type family ConcreteList (a :: [Ty]) where
  ConcreteList '[]         = '[]
  ConcreteList (ty ': tys) = Concrete ty ': ConcreteList tys

-- | 'withSing' is emblematic of a tradeoff we deal with repeatedly in
-- evaluation. We must _always_ maintain evidence that _all_ of the types we
-- deal with are in the closed universe we know how to deal with. Sometimes
-- it's easiest to pass this information explicitly, as @SingTy a@. Other times
-- it's easiest to pass it implicitly, as (constraint) @SingI a@. It's easy to
-- go from a constraint to an explicit singleton type, just use 'sing'.
-- 'withSing' allows us to go the other way as well.
--
--     explicit -- withSing -> implicit
--
--     SingTy a <--- sing ---- SingI a
withSing :: SingTy a -> (SingI a => b) -> b
withSing = withDict . singMkSing where

    singMkSing :: SingTy a -> Dict (SingI a)
    singMkSing = \case
      SInteger    -> Dict
      SBool       -> Dict
      SStr        -> Dict
      STime       -> Dict
      SDecimal    -> Dict
      SKeySet     -> Dict
      SAny        -> Dict
      SList ty'   -> withSing ty' Dict
      SObject tys -> withSingListDict tys Dict

    withSingListDict :: SingList tys -> (SingI tys => b) -> b
    withSingListDict SNil f               = f
    withSingListDict (SCons _k _ty tys) f = withSingListDict tys f

withEq :: SingTy a -> (Eq (Concrete a) => b) -> b
withEq = withDict . singMkEq
  where

    singMkEq :: SingTy a -> Dict (Eq (Concrete a))
    singMkEq = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SKeySet      -> Dict
      SAny         -> Dict
      SList ty'    -> withEq ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withEq ty' $ withDict (singMkEq (SObject tys)) Dict

withShow :: SingTy a -> (Show (Concrete a) => b) -> b
withShow = withDict . singMkShow
  where

    singMkShow :: SingTy a -> Dict (Show (Concrete a))
    singMkShow = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SKeySet      -> Dict
      SAny         -> Dict
      SList ty'    -> withShow ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withShow ty' $ withDict (singMkShow (SObject tys)) Dict

withUserShow :: SingTy a -> (UserShow (Concrete a) => b) -> b
withUserShow = withDict . singMkUserShow
  where

    singMkUserShow :: SingTy a -> Dict (UserShow (Concrete a))
    singMkUserShow = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SKeySet      -> Dict
      SAny         -> Dict
      SList ty'    -> withUserShow ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withUserShow ty' $ withDict (singMkUserShow (SObject tys)) Dict

withTypeable :: SingTy a -> ((Typeable a, Typeable (Concrete a)) => b) -> b
withTypeable = withDict . singMkTypeable
  where

    singMkTypeable :: SingTy a -> Dict (Typeable a, Typeable (Concrete a))
    singMkTypeable = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SKeySet      -> Dict
      SAny         -> Dict
      SList   ty'  -> withTypeable ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withTypeable ty' $ withTypeableListDict tys $ Dict

    withTypeableListDict
      :: SingList tys -> ((Typeable tys, Typeable (ConcreteObj tys)) => b) -> b
    withTypeableListDict SNil f
      = f
    withTypeableListDict (SCons _k ty tys) f
      = withTypeableListDict tys $ withTypeable ty f

withSMTValue :: SingTy a -> (SMTValue (Concrete a) => b) -> b
withSMTValue = withDict . singMkSMTValue
  where

    singMkSMTValue :: SingTy a -> Dict (SMTValue (Concrete a))
    singMkSMTValue = \case
      SInteger  -> Dict
      SBool     -> Dict
      SStr      -> Dict
      STime     -> Dict
      SDecimal  -> Dict
      SKeySet   -> Dict
      SAny      -> Dict
      SList ty' -> withSMTValue ty' $ withTypeable ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withSMTValue ty' $ withDict (singMkSMTValue (SObject tys)) Dict

withHasKind :: SingTy a -> (HasKind (Concrete a) => b) -> b
withHasKind = withDict . singMkHasKind
  where

    singMkHasKind :: SingTy a -> Dict (HasKind (Concrete a))
    singMkHasKind = \case
      SInteger  -> Dict
      SBool     -> Dict
      SStr      -> Dict
      STime     -> Dict
      SDecimal  -> Dict
      SKeySet   -> Dict
      SAny      -> Dict
      SList ty' -> withHasKind ty' $ withTypeable ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withHasKind ty' $ withDict (singMkHasKind (SObject tys)) Dict

instance SMTValue (Object AConcrete '[]) where
  sexprToVal _ = Just $ Object NilOf

instance
  ( SMTValue (Concrete ty)
  , SMTValue (Object AConcrete tys)
  , KnownSymbol k
  , SingI ty
  , Typeable ty
  ) => SMTValue (Object AConcrete ('(k, ty) ': tys)) where
  sexprToVal sexpr = case sexprToVal sexpr of
    Nothing             -> Nothing
    Just (a, Object as) -> Just $ Object $ ConsOf SSymbol (Column sing a) as

withSymWord :: SingTy a -> (SymWord (Concrete a) => b) -> b
withSymWord = withDict . singMkSymWord
  where

    singMkSymWord :: SingTy a -> Dict (SymWord (Concrete a))
    singMkSymWord = \case
      SInteger    -> Dict
      SBool       -> Dict
      SStr        -> Dict
      STime       -> Dict
      SDecimal    -> Dict
      SKeySet     -> Dict
      SAny        -> Dict
      SList ty'   -> withSymWord ty' Dict
      SObject SNil -> Dict
      SObject (SCons _ ty' tys)
        -> withSymWord ty' $ withDict (singMkSymWord (SObject tys)) Dict

instance Eq (tm ('TyObject '[])) => Ord (Object tm '[]) where
  compare _ _ = EQ

instance HasKind (Object tm '[]) where
  kindOf _ = KTuple []

instance (Eq (tm ('TyObject '[])), Typeable tm) => SymWord (Object tm '[]) where
  mkSymWord = genMkSymVar $ KTuple []

  literal (Object NilOf) =
    let k = KTuple []
    in SBVI.SBV . SVal k . Left . CW k $ CWTuple []

  fromCW (CW _ (CWTuple [])) = Object NilOf
  fromCW c = error $ "invalid (Object '[]): " ++ show c

instance
  ( Ord (tm ty)
  , Ord (Object tm tys)
  , Eq (tm ('TyObject ('(k, ty) : tys)))
  ) => Ord (Object tm ('(k, ty) ': tys)) where
  compare (Object (ConsOf _ a tys1)) (Object (ConsOf _ b tys2))
    = compare a b <> compare (Object tys1) (Object tys2)

instance
  ( HasKind (tm ty)
  , HasKind (Object tm tys)
  ) => HasKind (Object tm ('(k, ty) ': tys)) where
  kindOf _ = case kindOf (undefined :: Object tm tys) of
    KTuple ks -> KTuple $ kindOf (undefined :: tm ty) : ks
    k         -> error $ "unexpected object kind: " ++ show k

instance
  ( SingI ty
  , Typeable ty
  , Typeable tys
  , SymWord (tm ty)
  , SymWord (Object tm tys)
  , KnownSymbol k
  , Eq (tm ('TyObject ('(k, ty) : tys)))
  , Typeable tm
  , IsTerm tm
  ) => SymWord (Object tm ('(k, ty) ': tys)) where

  mkSymWord = genMkSymVar (kindOf (undefined :: (Object tm ('(k, ty) ': tys))))

  literal (Object (ConsOf _k (Column _ x) xs)) = case literal x of
    SBVI.SBV (SVal _ (Left (CW _ xval))) -> case literal (Object xs) of
      SBVI.SBV (SVal (KTuple kxs) (Left (CW _ (CWTuple xsval)))) ->
        let k = SBVI.KTuple (kindOf x : kxs)
        in SBVI.SBV $ SVal k $ Left $ CW k $ CWTuple $ xval : xsval
      _ -> error "SymWord.literal (Object tm ('(k, ty) ': tys)): Cannot construct a literal value!"
    _ -> error "SymWord.literal (Object tm ('(k, ty) ': tys)): Cannot construct a literal value!"

  fromCW (CW (KTuple (k:ks)) (CWTuple (x:xs))) =
    case fromCW (CW (KTuple ks) (CWTuple xs)) of
      Object vals
        -> Object $ ConsOf SSymbol (Column sing (fromCW (CW k x))) vals
  fromCW c = error $ "invalid (Object tm ('(k, ty) ': tys)): " ++ show c

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map.Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Semigroup, Monoid)

instance Mergeable a => Mergeable (ColumnMap a) where
  symbolicMerge force test (ColumnMap left) (ColumnMap right) = ColumnMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

columnMapToSchema :: ColumnMap EType -> EType
columnMapToSchema (ColumnMap colMap) = go (Map.toList colMap) where
  go [] = EType (SObject SNil)
  go ((ColumnName colName, EType ty) : tys) = case go tys of
    EType (SObject tys') -> case someSymbolVal colName of
      SomeSymbol (_ :: Proxy k) -> withSing ty $
        EType $ SObject $ SCons (SSymbol @k) ty tys'
    _ -> error "TODO"
  go _ = error "TODO"
  -- = Schema
  -- . Map.fromList
  -- . fmap (\(ColumnName name, ety) -> (fromString name, ety))
  -- . Map.toList
  -- . _columnMap

newtype TableMap a
  = TableMap { _tableMap :: Map.Map TableName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Mergeable a => Mergeable (TableMap a) where
  symbolicMerge force test (TableMap left) (TableMap right) = TableMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right


instance UserShow (Quantifiable q) where
  userShowPrec d = \case
    EType ty     -> userShowPrec d ty
    QTable       -> "table"
    QColumnOf tn -> "(column-of " <> userShow tn <> ")"

instance UserShow TableName where
  userShowPrec _ (TableName tn) = T.pack tn

instance UserShow ColumnName where
  userShowPrec _ (ColumnName cn) = T.pack cn

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
