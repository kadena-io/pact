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
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Pact.Analyze.Types.Shared where

import           Control.Lens                 (At (at), Index, Iso, IxValue,
                                               Ixed (ix), Lens', Prism', both,
                                               from, iso, lens, makeLenses,
                                               makePrisms, over, view, (%~),
                                               (&))
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.AffineSpace             ((.+^), (.-.))
import           Data.Coerce                  (Coercible)
import           Data.Constraint              (Dict (Dict), withDict)
import           Data.Data                    (Data, Typeable)
import           Data.Function                (on)
import           Data.Kind                    (Type)
import           Data.List                    (sortBy)
import           Data.Maybe                   (isJust)
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
                                               (.<), (.==), fromCW)
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
  true            = sansProv true
  false           = sansProv false
  bnot (S _ x)    = sansProv $ bnot x
  S _ x &&& S _ y = sansProv $ x &&& y
  S _ x ||| S _ y = sansProv $ x ||| y

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

instance Provable PredicateS where
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

data ConcreteCol a = ConcreteCol !(SingTy a) !(Concrete a)

instance (Eq (SingTy a), Eq (Concrete a)) => Eq (ConcreteCol a) where
  ConcreteCol _ a == ConcreteCol _ b = a == b
instance (Ord (SingTy a), Ord (Concrete a)) => Ord (ConcreteCol a) where
  ConcreteCol _ a `compare` ConcreteCol _ b = a `compare` b

data Object (m :: [(Symbol, Ty)]) = Object !(HListOf ConcreteCol m)

objectLookup :: Object m -> String -> Maybe (Existential ConcreteCol)
objectLookup (Object cols) name = objectLookup' cols where
  objectLookup'
    :: forall m. HListOf ConcreteCol m -> Maybe (Existential ConcreteCol)
  objectLookup' NilOf = Nothing
  objectLookup' (ConsOf k c@(ConcreteCol ty _) cols')
    = if symbolVal k == name
      then Just $ Existential ty c
      else objectLookup' cols'

data EObject where
  EObject :: SingList m -> Object m -> EObject

instance UserShow (Object m) where
  userShowPrec _ (Object vals)
    = "{" <> T.intercalate ", " (userShowVals vals) <> "}"
      where
      userShowVals :: HListOf ConcreteCol m' -> [Text]
      userShowVals NilOf = []
      userShowVals (ConsOf k (ConcreteCol ty v) m')
        = T.pack (symbolVal k) <> " := " <> withUserShow ty (userShow v) : userShowVals m'

instance Show (Object m) where
  showsPrec p (Object vals) = showParen (p > 10) $
    showString "Object " . showsVals vals
    where showsVals :: HListOf ConcreteCol m' -> ShowS
          showsVals NilOf = showString "NilOf"
          showsVals (ConsOf k (ConcreteCol singv v) m') = showParen True $
              showString "ConsOf "
            . showString (symbolVal k)
            . showString " "
            . withShow singv (showsPrec 11 v)
            . showString " "
            . showsVals m'

instance Eq (Object m) where
  Object vals1 == Object vals2 = eq vals1 vals2 where
    eq :: HListOf ConcreteCol m' -> HListOf ConcreteCol m' -> Bool
    eq NilOf NilOf = True
    eq (ConsOf k1 (ConcreteCol singv v1) m1')
       (ConsOf k2 (ConcreteCol _     v2) m2')
      = eqSymB k1 k2 && withEq singv (v1 == v2) && eq m1' m2'

-- | Wrapper for @SingTy@ so it can be used (unsaturated) as an argument to
-- @SMap.Map@
data ColumnTy a = ColumnTy !String !(SingTy a)

data Schema (m :: [(Symbol, Ty)]) = Schema !(HListOf ColumnTy m)

schemaTy :: Schema tys -> SingTy ('TyObject tys)
schemaTy (Schema hlist) = SObject $ hListTys hlist

hListTys :: HListOf f tys -> Sing tys
hListTys NilOf = SNil
hListTys (ConsOf sym _ty tys) = SCons sym sing (hListTys tys)

-- Note: this doesn't exactly match the pact syntax
instance UserShow (Schema m) where
  userShowPrec _ (Schema tys) = "{" <> T.intercalate ", " (userShowTys tys) <> "}"
    where userShowTys :: HListOf ColumnTy m' -> [Text]
          userShowTys NilOf = []
          userShowTys (ConsOf key (ColumnTy _ singv) m')
            = T.pack (symbolVal key) <> " : " <> userShow singv : userShowTys m'

instance Show (Schema m) where
  showsPrec p (Schema tys) = showParen (p > 11) $
    showString "Schema " . showString " " . showTys tys
    where showTys :: HListOf ColumnTy m' -> ShowS
          showTys NilOf = showString "NilOf"
          showTys (ConsOf key (ColumnTy _ singv) tys') = showParen True $
              showString "ConsOf "
            . showString "TODO"
            . showParen True (
                showString "ColumnTy "
              . showsPrec 11 (symbolVal key)
              . showString " "
              . showsPrec 11 singv)
            . showTys tys'

data ESchema where
  ESchema :: SingTy ('TyObject m) -> Schema m -> ESchema

instance Eq ESchema where
  -- Since this is a singleton, checking the types match is good enough
  ESchema ty1 _ == ESchema ty2 _ = isJust $ singEq ty1 ty2

instance Show ESchema where
  showsPrec p (ESchema ty schema) = showParen (p > 10) $
      showString "ESchema "
    . showsPrec 11 ty
    . showString " "
    . showsPrec 11 schema

-- -- | When given a column mapping, this function gives a canonical way to assign
-- -- var ids to each column. Also see 'varIdArgs'.
-- varIdColumns :: SMap.Map f m -> Map Text VarId
-- varIdColumns smap = Map.fromList
--   $ zipWith (\index name -> (T.pack name, index)) [0..]
--   $ SMap.keys smap

-- varIdColumns' :: SingList m -> Map Text VarId
-- varIdColumns' smap = Map.fromList
--   $ zipWith (\index name -> (T.pack name, index)) [0..]
--   $ mappingKeys smap

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
              (\acc key -> acc &&&
                ((fields Map.! key) .== (fields' Map.! key)))
              true
              ks
       else false

instance EqSymbolic AVal where
  AVal mProv sv .== AVal mProv' sv' = mkS mProv sv .== mkS mProv' sv'

  -- Not perfect; this would be better if we could easily produce an
  -- uninterpreted bool here. We can't though, because 'uninterpret' takes a
  -- String that must be unique for each allocation.
  OpaqueVal .== OpaqueVal = false

  _ .== _ = false

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
  EType     :: SingTy a -> Quantifiable q
  QTable    ::                Quantifiable 'QAny
  QColumnOf :: TableName   -> Quantifiable 'QAny

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

type family Concrete (a :: Ty) = r | r -> a where
  Concrete 'TyInteger     = Integer
  Concrete 'TyBool        = Bool
  Concrete 'TyStr         = Str
  Concrete 'TyTime        = Time
  Concrete 'TyDecimal     = Decimal
  Concrete 'TyKeySet      = KeySet
  Concrete 'TyAny         = Any
  Concrete ('TyList a)    = [Concrete a]
  Concrete ('TyObject ty) = Object ty

type family ConcreteList (a :: [Ty]) = r | r -> a where
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
      SInteger  -> Dict
      SBool     -> Dict
      SStr      -> Dict
      STime     -> Dict
      SDecimal  -> Dict
      SKeySet   -> Dict
      SAny      -> Dict
      SList ty' -> withEq ty' Dict
      SObject _ -> Dict

withShow :: SingTy a -> (Show (Concrete a) => b) -> b
withShow = withDict . singMkShow
  where

    singMkShow :: SingTy a -> Dict (Show (Concrete a))
    singMkShow = \case
      SInteger  -> Dict
      SBool     -> Dict
      SStr      -> Dict
      STime     -> Dict
      SDecimal  -> Dict
      SKeySet   -> Dict
      SAny      -> Dict
      SList ty' -> withShow ty' Dict
      SObject _ -> Dict

withUserShow :: SingTy a -> (UserShow (Concrete a) => b) -> b
withUserShow = withDict . singMkUserShow
  where

    singMkUserShow :: SingTy a -> Dict (UserShow (Concrete a))
    singMkUserShow = \case
      SInteger  -> Dict
      SBool     -> Dict
      SStr      -> Dict
      STime     -> Dict
      SDecimal  -> Dict
      SKeySet   -> Dict
      SAny      -> Dict
      SList ty' -> withUserShow ty' Dict
      SObject _ -> Dict

withTypeable :: SingTy a -> (Typeable (Concrete a) => b) -> b
withTypeable = withDict . singMkTypeable
  where

    singMkTypeable :: SingTy a -> Dict (Typeable (Concrete a))
    singMkTypeable = \case
      SInteger    -> Dict
      SBool       -> Dict
      SStr        -> Dict
      STime       -> Dict
      SDecimal    -> Dict
      SKeySet     -> Dict
      SAny        -> Dict
      SList   ty' -> withTypeable ty' Dict
      SObject tys -> withTypeableListDict tys Dict

withTypeableListDict :: SingList tys -> (Typeable tys => b) -> b
withTypeableListDict SNil f            = f
withTypeableListDict (SCons _k _ty tys) f = withTypeableListDict tys f

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
      SObject tys -> withSMTValueListDict tys Dict

    withSMTValueListDict :: SingList tys -> (SMTValue (Object tys) => b) -> b
    withSMTValueListDict SNil f = f
    withSMTValueListDict (SCons _k ty tys) f
      = withSMTValue ty $ withSMTValueListDict tys f

instance SMTValue (Object '[]) where
  sexprToVal _ = Just $ Object NilOf

instance
  ( SMTValue (Concrete ty)
  , SMTValue (Object tys)
  , KnownSymbol k
  , SingI ty
  , Typeable ty
  ) => SMTValue (Object ('(k, ty) ': tys)) where
  sexprToVal = error "TODO"
  -- sexprToVal sexpr = case sexprToVal sexpr of
  --   Nothing             -> Nothing
  --   Just (a, Object as) -> Just $ Object $ ConsOf SSymbol (ConcreteCol sing a) as

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
      SObject tys -> withSymWordListDict tys Dict

withSymWordListDict :: SingList tys -> (SymWord (Object tys) => b) -> b
withSymWordListDict SNil f = f
withSymWordListDict (SCons _k ty tys) f
  = withTypeableListDict tys $ withSymWord ty $ withSymWordListDict tys f

instance Ord (Object '[]) where
  compare _ _ = EQ

instance HasKind (Object '[]) where
  kindOf _ = KTuple []

instance SymWord (Object '[]) where
  mkSymWord = genMkSymVar $ KTuple []

  literal (Object NilOf) =
    let k = KTuple []
    in SBVI.SBV . SVal k . Left . CW k $ CWTuple []

  fromCW (CW _ (CWTuple [])) = Object NilOf
  fromCW c = error $ "invalid (Object '[]): " ++ show c

instance (Ord (Concrete ty), Ord (Object tys)) => Ord (Object ('(k, ty) ': tys)) where
  compare (Object (ConsOf _ a tys1)) (Object (ConsOf _ b tys2))
    = compare a b <> compare (Object tys1) (Object tys2)

instance (HasKind (Concrete ty), HasKind (Object tys)) => HasKind (Object ('(k, ty) ': tys)) where
  kindOf _ = case kindOf (undefined :: Object tys) of
    KTuple ks -> KTuple $ kindOf (undefined :: Concrete ty) : ks
    k         -> error $ "unexpected object kind: " ++ show k

instance (SingI ty, Typeable ty, Typeable tys, SymWord (Concrete ty), SymWord (Object tys), KnownSymbol k)
  => SymWord (Object ('(k, ty) ': tys)) where

  mkSymWord = genMkSymVar (kindOf (undefined :: (Object ('(k, ty) ': tys))))

  literal (Object (ConsOf _k (ConcreteCol _ x) xs)) = case literal x of
    SBVI.SBV (SVal _ (Left (CW _ xval))) -> case literal (Object xs) of
      SBVI.SBV (SVal (KTuple kxs) (Left (CW _ (CWTuple xsval)))) ->
        let k = SBVI.KTuple (kindOf x : kxs)
        in SBVI.SBV $ SVal k $ Left $ CW k $ CWTuple $ xval : xsval
      _ -> error "SymWord.literal (Object ('(k, ty) ': tys)): Cannot construct a literal value!"
    _ -> error "SymWord.literal (Object ('(k, ty) ': tys)): Cannot construct a literal value!"

  fromCW (CW (KTuple (k:ks)) (CWTuple (x:xs))) =
    case fromCW (CW (KTuple ks) (CWTuple xs)) of
      Object vals
        -> Object $ ConsOf SSymbol (ConcreteCol sing (fromCW (CW k x))) vals
  fromCW c = error $ "invalid (Object ('(k, ty) ': tys)): " ++ show c

-- columnMapToSchema :: ColumnMap EType -> Schema
-- columnMapToSchema
--   = Schema
--   . Map.fromList
--   . fmap (\(ColumnName name, ety) -> (fromString name, ety))
--   . Map.toList
--   . _columnMap

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map.Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Semigroup, Monoid)

instance Mergeable a => Mergeable (ColumnMap a) where
  symbolicMerge force test (ColumnMap left) (ColumnMap right) = ColumnMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

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
