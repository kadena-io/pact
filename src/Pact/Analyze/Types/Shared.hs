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
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Type definitions and constructors that cross-cut symbolic analysis.
module Pact.Analyze.Types.Shared where

import           Control.Lens                 (At (at), Index, Iso, IxValue,
                                               Ixed (ix), Lens', Prism', both,
                                               from, iso, lens, makeLenses,
                                               makePrisms, over, view, (%~),
                                               (&), (<&>))
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.AffineSpace             ((.+^), (.-.))
import           Data.Coerce                  (Coercible, coerce)
import           Data.Constraint              (Dict (Dict), withDict)
import           Data.Data                    (Data, Proxy, Typeable)
import           Data.Function                (on)
import           Data.Kind                    (Type)
import           Data.List                    (sort)
import           Data.List                    (sortBy)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (isJust)
import           Data.Monoid                  (First (..))
import           Data.SBV                     (EqSymbolic, HasKind, Int64,
                                               Kind (KString, KUnbounded),
                                               Mergeable (symbolicMerge),
                                               OrdSymbolic, SBV,
                                               SDivisible (sDivMod, sQuotRem),
                                               SymVal (..), Symbolic, fromCV,
                                               isConcrete, ite, kindOf, literal,
                                               oneIf, sFromIntegral, unliteral,
                                               (.<), (.==))
import qualified Data.SBV                     as SBV
import           Data.SBV.Control             (SMTValue (..))
import           Data.SBV.Internals           (CV (..), CVal (..), Kind (..),
                                               SVal (SVal), genMkSymVar)
import qualified Data.SBV.Internals           as SBVI
import qualified Data.SBV.String              as SBV
import           Data.SBV.Trans               (MProvable (..), mkSymVal)
import           Data.SBV.Tuple               (_1, _2)
import qualified Data.Set                     as Set
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Thyme                   (UTCTime, microseconds)
import           Data.Type.Equality           ((:~:) (Refl))
import           GHC.TypeLits
import           Prelude                      hiding (Float)

import           Pact.Types.Pretty            hiding (list)
import qualified Pact.Types.Pretty            as Pretty
import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Util              (AsString)

import           Pact.Analyze.LegacySFunArray (SFunArray)
import           Pact.Analyze.Feature         hiding (Constraint, Doc, Type,
                                               dec, ks, obj, str, time)
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.ObjUtil
import           Pact.Analyze.Types.Types
import           Pact.Analyze.Util            (Boolean (..), vacuousMatch)

class IsTerm tm where
  singEqTm     :: SingTy ty -> tm ty -> tm ty -> Bool
  singShowsTm  :: SingTy ty -> Int   -> tm ty -> ShowS
  singPrettyTm :: SingTy ty ->          tm ty -> Doc

eqTm :: (SingI ty, IsTerm tm) => tm ty -> tm ty -> Bool
eqTm = singEqTm sing

showsTm :: (SingI ty, IsTerm tm) => Int -> tm ty -> ShowS
showsTm = singShowsTm sing

showTm :: (SingI ty, IsTerm tm) => tm ty -> String
showTm tm = showsTm 0 tm ""

prettyTm :: (SingI ty, IsTerm tm) => tm ty -> Doc
prettyTm = singPrettyTm sing

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
  Some :: SingTy a -> tm a -> Existential tm

instance IsTerm tm => Eq (Existential tm) where
  Some tya a == Some tyb b = case singEq tya tyb of
    Nothing   -> False
    Just Refl -> singEqTm tya a b

instance IsTerm tm => Show (Existential tm) where
  showsPrec p (Some ty tm)
    = showParen (p > 10)
    $ showString "Some "
    . showsPrec 11 ty
    . showChar ' '
    . singShowsTm ty 11 tm

instance IsTerm tm => Pretty (Existential tm) where
  pretty (Some ty tm) = singPrettyTm ty tm

transformExistential
  :: (forall a. tm1 a -> tm2 a) -> Existential tm1 -> Existential tm2
transformExistential f (Some ty term') = Some ty (f term')

mapExistential :: (forall a. tm a -> tm a) -> Existential tm -> Existential tm
mapExistential = transformExistential

existentialType :: Existential tm -> EType
existentialType (Some ety _) = EType ety

-- TODO: could implement this stuff generically or add newtype-awareness

wrappedStringFromCV :: (String -> a) -> SBVI.CV -> a
wrappedStringFromCV construct (SBVI.CV _ (SBVI.CString s)) = construct s
wrappedStringFromCV _ c = error $ "SymVal: Unexpected non-string value: " ++ show c

wrappedIntegerFromCV :: (Integer -> a) -> SBVI.CV -> a
wrappedIntegerFromCV construct (SBVI.CV _ (SBVI.CInteger i)) = construct i
wrappedIntegerFromCV _ c = error $ "SymVal: Unexpected non-integer value: " ++ show c

mkConcreteString :: String -> SBV a
mkConcreteString = SBVI.SBV
                 . SVal KString
                 . Left
                 . CV KString
                 . SBVI.CString

mkConcreteInteger :: Integer -> SBV a
mkConcreteInteger = SBVI.SBV
                  . SVal KUnbounded
                  . Left
                  . CV KUnbounded
                  . CInteger

newtype RegistryName
  = RegistryName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON)

instance Show RegistryName where show (RegistryName s) = show s

instance SymVal RegistryName where
  mkSymVal = SBVI.genMkSymVar KString
  literal (RegistryName t) = mkConcreteString $ T.unpack t
  fromCV = wrappedStringFromCV $ RegistryName . T.pack

instance HasKind RegistryName where
  kindOf _ = KString

instance SMTValue RegistryName where
  sexprToVal = fmap (RegistryName . T.pack) . sexprToVal

instance Pretty RegistryName where
  pretty (RegistryName name) = "'" <> pretty name

newtype TableName
  = TableName String
  deriving (Eq, Ord, Show)

instance SymVal TableName where
  mkSymVal = SBVI.genMkSymVar KString
  literal (TableName s) = mkConcreteString s
  fromCV = wrappedStringFromCV TableName

instance HasKind TableName where
  kindOf _ = KString

instance IsString TableName where
  fromString = TableName

instance Pretty TableName where
  pretty (TableName tn) = dquotes $ prettyString tn

newtype ColumnName
  = ColumnName String
  deriving (Eq, Ord, Show)

instance SymVal ColumnName where
  mkSymVal = SBVI.genMkSymVar KString
  literal (ColumnName s) = mkConcreteString s
  fromCV = wrappedStringFromCV ColumnName

instance HasKind ColumnName where
  kindOf _ = KString

instance IsString ColumnName where
  fromString = ColumnName

newtype CapName
  = CapName String
  deriving (Eq, Ord, Show)

instance IsString CapName where
  fromString = CapName

newtype Str = Str { unStr :: String }
  deriving (Eq, Ord, Show, SMTValue, HasKind, Typeable, IsString)

strToText :: Str -> Text
strToText (Str str) = T.pack str

instance SymVal Str where
  mkSymVal = SBVI.genMkSymVar KString
  literal (Str s) = mkConcreteString s
  fromCV = wrappedStringFromCV Str

instance Pretty Str where
  pretty (Str str) = dquotes $ prettyString str

type RowKey = Str

type Time = Int64

timeIso :: PactIso UTCTime Time
timeIso = PactIso $ iso mkTime unMkTime
  where
    mkTime :: UTCTime -> Time
    mkTime utct = view microseconds (utct .-. toEnum 0)

    unMkTime :: Time -> UTCTime
    unMkTime time = toEnum 0 .+^ view (from microseconds) time

isGuardTy :: Pact.Type v -> Bool
isGuardTy (Pact.TyPrim (Pact.TyGuard _)) = True
isGuardTy _                              = False

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

instance Pretty LogicalOp where
  pretty = toDoc logicalOpP

data EqNeq
  = Eq'  -- ^ Equal
  | Neq' -- ^ Not equal
  deriving (Show, Eq, Ord)

eqNeqP :: Prism' Text EqNeq
eqNeqP = mkOpNamePrism
  [ (SEquality,   Eq')
  , (SInequality, Neq')
  ]

instance Pretty EqNeq where
  pretty = toDoc eqNeqP

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

instance Pretty ComparisonOp where
  pretty = toDoc comparisonOpP

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
  = FromCell     OriginatingCell
  | FromRow      (Map ColumnName OriginatingCell)
  | FromRegistry (S RegistryName)
  | FromInput    Unmunged
  | FromMetadata (S Str)
  --
  -- TODO: in the future, probably have FromYield?
  --
  deriving (Eq, Show)

-- Symbolic value carrying provenance, for tracking if values have come from a
-- particular source of data (db, tx metadata, keyset registry, arg, etc).
data S (a :: Type)
  = S
    { _sProv :: Maybe Provenance
    , _sSbv  :: SBV a }
  deriving (Eq, Show)

sansProv :: SBV a -> S a
sansProv = S Nothing

withProv :: Provenance -> SBV a -> S a
withProv prov sym = S (Just prov) sym

instance (SymVal a) => Mergeable (S a) where
  symbolicMerge f t (S mProv1 x) (S mProv2 y)
    | mProv1 == mProv2 = S mProv1 $ symbolicMerge f t x y
    | otherwise        = sansProv $ symbolicMerge f t x y

-- We provide instances for EqSymbolic, OrdSymboic, Boolean because we need
-- these operators for `S a` now that we work with that instead of `SBV a`
-- everywhere:

instance EqSymbolic (S a) where
  S _ x .== S _ y = x .== y

instance (Ord a, SymVal a) => OrdSymbolic (S a) where
  S _ x .< S _ y = x .< y

instance Boolean (S Bool) where
  sTrue           = sansProv SBV.sTrue
  sFalse          = sansProv SBV.sFalse
  sNot (S prov x) = S prov $ SBV.sNot x
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
-- `instance (Ord a, Num a, SymVal a) => Num (SBV a)`, included in sbv, which
-- does to include the shift. *This instance must be selected for decimals*.
instance {-# OVERLAPPING #-} Num (S Decimal) where
  S _ x + S _ y  = sansProv $ x + y
  S _ x * S _ y  = sansProv $ x * y
  abs (S _ x)    = sansProv $ abs x
  signum (S _ x) = sansProv $ signum x
  fromInteger i  = sansProv $ fromInteger i
  negate (S _ x) = sansProv $ negate x

instance (Num a, Ord a, SymVal a) => Num (S a) where
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

instance (Fractional a, Ord a, SymVal a) => Fractional (S a) where
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

fromRegistry :: S RegistryName -> Provenance
fromRegistry = FromRegistry

fromMetadata :: S Str -> Provenance
fromMetadata = FromMetadata

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
  = Object !(HList (Column tm) m)

pattern ObjectNil :: () => schema ~ '[] => Object tm schema
pattern ObjectNil = Object SNil

-- pattern ObjectCons
--   :: ()
--   => (schema ~ '(k, ty) ': schema', SingI ty, Typeable ty, KnownSymbol k)
--   => Object tm schema
-- pattern ObjectCons k v obj = Object (SCons k (Column

objectLookup :: Object tm m -> String -> Maybe (Existential (Column tm))
objectLookup (Object cols) name = objectLookup' cols where
  objectLookup' = getFirst . foldHList
    (\k c@(Column ty _) -> First $
      if symbolVal k == name then Just $ Some ty c else Nothing)

data EObject tm where
  EObject :: SingList m -> Object tm m -> EObject tm

instance Pretty (tm ('TyObject m)) => Pretty (Object tm m) where
  pretty (Object vals) = commaBraces (prettyVals vals) where
    prettyVals = foldHList $ \k (Column ty v) ->
      [prettyString (symbolVal k) <> " := " <> singPrettyTm ty v]

instance Show (tm ('TyObject m)) => Show (Object tm m) where
  showsPrec p (Object vals) = showParen (p > 10) $
    showString "Object " . showsVals vals (11 :: Int) where
      showsVals = foldrHList
        (\_p -> showString "SNil")
        (\k (Column singv v) showRest p' -> showParen (p' > 10) $
           showString "SCons "
         . showString (symbolVal k)
         . showChar ' '
         . singShowsTm singv 11 v
         . showChar ' '
         . showRest 11)

instance Eq (tm ('TyObject m)) => Eq (Object tm m) where
  Object vals1 == Object vals2 = eq vals1 vals2 where
    eq :: HList (Column tm) m' -> HList (Column tm) m' -> Bool
    eq SNil SNil = True
    eq (SCons k1 (Column singv v1) m1')
       (SCons k2 (Column _     v2) m2')
      = eqSymB k1 k2 && singEqTm singv v1 v2 && eq m1' m2'

data ESchema where
  ESchema :: SingList schema -> ESchema

instance Eq ESchema where
  -- Since this is a singleton, checking the types match is good enough
  ESchema ty1 == ESchema ty2 = isJust $ singListEq ty1 ty2

instance Show ESchema where
  showsPrec p (ESchema ty) = showParen (p > 10) $
      showString "ESchema "
    . showsPrec 11 ty

mkESchema :: [(Text, EType)] -> ESchema
mkESchema tys = case go tys of
                  ESchema unsorted -> ESchema $ normalizeSchema unsorted
  where
    go :: [(Text, EType)] -> ESchema
    go [] = ESchema SNil'
    go ((name, (EType ty)):rest) =
      case go rest of
        ESchema restSchema ->
          case someSymbolVal (T.unpack name) of
            SomeSymbol (_ :: Proxy k) ->
              withSing ty $ withTypeable ty $
                ESchema $ SCons' (SSymbol @k) ty restSchema

-- | When given a column mapping, this function gives a canonical way to assign
-- var ids to each column. Also see 'varIdArgs'.
varIdColumns :: SingList m -> Map Text VarId
varIdColumns
  = Map.fromList
  . flip zip [0..]
  . sort
  . foldSingList (\name _ -> [T.pack (symbolVal name)])

-- | Given args representing the columns of a schema, this function gives a
-- canonical assignment of var ids to each column. Also see 'varIdColumns'.
varIdArgs :: [Pact.Arg a] -> [(Pact.Arg a, VarId)]
varIdArgs
  = flip zip [0..]
  . sortBy (compare `on` Pact._aName)

-- | Untyped object
newtype UObject = UObject (Map.Map Text TVal)
  deriving (Eq, Show, Semigroup, Monoid)

instance Pretty UObject where
  pretty (UObject m) = Pretty.list $ Map.toList m <&> \(k, v) ->
    pretty k <> " := " <> pretty v

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

instance Pretty AVal where
  pretty = \case
    AVal _ sVal -> viaShow sVal
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

literalS :: SymVal a => a -> S a
literalS = sansProv . literal

unliteralS :: SymVal a => S a -> Maybe a
unliteralS = unliteral . _sSbv

uninterpretS :: HasKind a => String -> S a
uninterpretS = sansProv . SBV.uninterpret

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
  :: forall a b. (Integral a, SymVal a, Num b, SymVal b)
  => S a
  -> S b
fromIntegralS = over s2Sbv sFromIntegral

oneIfS :: (Num a, Ord a, SymVal a) => S Bool -> S a
oneIfS = over s2Sbv oneIf

isConcreteS :: SymVal a => S a -> Bool
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
  = Munged { _mungedName :: Text }
  deriving (Eq, Show)

instance Pretty Munged where
  pretty (Munged nm) = pretty nm

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

instance Pretty Any where
  pretty Any = "*"

instance HasKind Any
instance SymVal Any
instance SMTValue Any

newtype Guard
  = Guard Integer
  deriving (Eq, Ord, Enum, Data, Show, Read, Pretty)

instance SymVal Guard where
  mkSymVal = SBVI.genMkSymVar KUnbounded
  literal (Guard s) = mkConcreteInteger s
  fromCV = wrappedIntegerFromCV Guard

instance HasKind Guard where
  kindOf _ = KUnbounded

instance SMTValue Guard where
  sexprToVal = fmap Guard . sexprToVal

type family Concrete (a :: Ty) where
  Concrete 'TyInteger     = Integer
  Concrete 'TyBool        = Bool
  Concrete 'TyStr         = Str
  Concrete 'TyTime        = Time
  Concrete 'TyDecimal     = Decimal
  Concrete 'TyGuard       = Guard
  Concrete 'TyAny         = Any
  Concrete ('TyList a)    = [Concrete a]
  Concrete ('TyObject ty) = ConcreteObj ty

type family ConcreteObj (a :: [(Symbol, Ty)]) where
  ConcreteObj '[]               = ()
  ConcreteObj ('(k', v) ': kvs) = (Concrete v, ConcreteObj kvs)

-- | Eliminator for objects
foldrObject
  :: (SBV (ConcreteObj schema) :< SingList schema)
  -> a
  -> (forall k b.
       KnownSymbol k
    => SingSymbol k -> SBV (Concrete b) -> SingTy b -> a -> a)
  -> a
foldrObject (_   :< SNil')              base _f = base
foldrObject (obj :< SCons' k ty schema) base f
  = withSymVal ty $ withSymVal (SObjectUnsafe schema) $
  f k (_1 obj) ty (foldrObject (_2 obj :< schema) base f)

foldObject
  :: Monoid a
  => (SBV (ConcreteObj schema) :< SingList schema)
  -> (forall k b.
       KnownSymbol k
    => SingSymbol k -> SBV (Concrete b) -> SingTy b -> a)
  -> a
foldObject objSchema f
  = foldrObject objSchema mempty (\sym val ty accum -> f sym val ty <> accum)

newtype AConcrete ty = AConcrete (Concrete ty)

instance (Eq (Concrete ty)) => Eq (AConcrete ty) where
  AConcrete a == AConcrete b = a == b

instance (Show (Concrete ty)) => Show (AConcrete ty) where
  showsPrec p (AConcrete a) = showParen (p > 10) $
    showString "AConcrete " . showsPrec 11 a

instance (Pretty (Concrete ty)) => Pretty (AConcrete ty) where
  pretty (AConcrete a) = pretty a

instance SMTValue (Concrete ty) => SMTValue (AConcrete ty) where
  sexprToVal = fmap AConcrete . sexprToVal

instance IsTerm AConcrete where
  singEqTm ty (AConcrete a) (AConcrete b) = withEq ty $ a == b
  singShowsTm ty p tm                     = withShow ty $ showsPrec p tm
  singPrettyTm ty tm                      = withPretty ty $ pretty tm

instance Ord (Concrete ty) => Ord (AConcrete ty) where
  compare (AConcrete a) (AConcrete b) = compare a b

instance HasKind (Concrete ty) => HasKind (AConcrete ty) where
  kindOf (AConcrete a) = kindOf a

instance
  ( Typeable ty
  , SymVal (Concrete ty)
  , SingI ty
  ) => SymVal (AConcrete ty) where
  mkSymVal q name = coerce @(SBV (Concrete ty)) @(SBV (AConcrete ty))
    <$> withSymVal (sing :: SingTy ty) (mkSymVal q name)
  literal (AConcrete a) = coerce $ literal a
  fromCV    = AConcrete . fromCV

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
      SInteger               -> Dict
      SBool                  -> Dict
      SStr                   -> Dict
      STime                  -> Dict
      SDecimal               -> Dict
      SGuard                 -> Dict
      SAny                   -> Dict
      SList ty'              -> withSing ty' Dict
      SObjectUnsafe (SingList tys) -> withHListDict tys Dict

    withHListDict :: HList Sing tys -> (SingI tys => b) -> b
    withHListDict SNil f               = f
    withHListDict (SCons _k _ty tys) f = withHListDict tys f

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
      SGuard       -> Dict
      SAny         -> Dict
      SList ty'    -> withEq ty' Dict
      SObjectUnsafe SNil'   -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withEq ty' $ withDict (singMkEq (SObjectUnsafe tys)) Dict

withOrd :: SingTy a -> (Ord (Concrete a) => b) -> b
withOrd = withDict . singMkOrd
  where

    singMkOrd :: SingTy a -> Dict (Ord (Concrete a))
    singMkOrd = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SGuard       -> Dict
      SAny         -> Dict
      SList ty'    -> withOrd ty' Dict
      SObjectUnsafe SNil'   -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withOrd ty' $ withDict (singMkOrd (SObjectUnsafe tys)) Dict

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
      SGuard       -> Dict
      SAny         -> Dict
      SList ty'    -> withShow ty' Dict
      SObjectUnsafe SNil'   -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withShow ty' $ withDict (singMkShow (SObjectUnsafe tys)) Dict

withPretty :: SingTy a -> (Pretty (Concrete a) => b) -> b
withPretty = withDict . singMkPretty
  where

    singMkPretty :: SingTy a -> Dict (Pretty (Concrete a))
    singMkPretty = \case
      SInteger     -> Dict
      SBool        -> Dict
      SStr         -> Dict
      STime        -> Dict
      SDecimal     -> Dict
      SGuard       -> Dict
      SAny         -> Dict
      SList ty'    -> withPretty ty' Dict
      SObjectUnsafe SNil'   -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withPretty ty' $
           withDict (singMkPretty (SObjectUnsafe tys)) Dict

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
      SGuard       -> Dict
      SAny         -> Dict
      SList   ty'  -> withTypeable ty' Dict
      SObjectUnsafe (SingList tys)
        -> withTypeableListDict tys $ Dict

    withTypeableListDict
      :: HList Sing tys
      -> ((Typeable tys, Typeable (ConcreteObj tys)) => b)
      -> b
    withTypeableListDict SNil f
      = f
    withTypeableListDict (SCons _k ty tys) f
      = withTypeableListDict tys $ withTypeable ty f

withSMTValue :: SingTy a -> (SMTValue (Concrete a) => b) -> b
withSMTValue = withDict . singMkSMTValue
  where

    singMkSMTValue :: SingTy a -> Dict (SMTValue (Concrete a))
    singMkSMTValue = \case
      SInteger   -> Dict
      SBool      -> Dict
      SStr       -> Dict
      STime      -> Dict
      SDecimal   -> Dict
      SGuard     -> Dict
      SAny       -> Dict
      SList ty'  -> withSMTValue ty' $ withTypeable ty' Dict
      SObjectUnsafe SNil' -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withSMTValue ty' $
           withDict (singMkSMTValue (SObjectUnsafe tys)) Dict

withHasKind :: SingTy a -> (HasKind (Concrete a) => b) -> b
withHasKind = withDict . singMkHasKind
  where

    singMkHasKind :: SingTy a -> Dict (HasKind (Concrete a))
    singMkHasKind = \case
      SInteger   -> Dict
      SBool      -> Dict
      SStr       -> Dict
      STime      -> Dict
      SDecimal   -> Dict
      SGuard     -> Dict
      SAny       -> Dict
      SList ty'  -> withHasKind ty' $ withTypeable ty' Dict
      SObjectUnsafe SNil' -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withHasKind ty' $
           withDict (singMkHasKind (SObjectUnsafe tys)) Dict

instance SMTValue (Object AConcrete '[]) where
  sexprToVal _ = Just $ Object SNil

instance
  ( SMTValue (Concrete ty)
  , SMTValue (Object AConcrete tys)
  , KnownSymbol k
  , SingI ty
  , Typeable ty
  ) => SMTValue (Object AConcrete ('(k, ty) ': tys)) where
  sexprToVal sexpr = case sexprToVal sexpr of
    Nothing             -> Nothing
    Just (a, Object as) -> Just $ Object $ SCons SSymbol (Column sing a) as

withSymVal :: SingTy a -> (SymVal (Concrete a) => b) -> b
withSymVal = withDict . singMkSymVal
  where

    singMkSymVal :: SingTy a -> Dict (SymVal (Concrete a))
    singMkSymVal = \case
      SInteger    -> Dict
      SBool       -> Dict
      SStr        -> Dict
      STime       -> Dict
      SDecimal    -> Dict
      SGuard      -> Dict
      SAny        -> Dict
      SList ty'   -> withSymVal ty' Dict
      SObjectUnsafe SNil' -> Dict
      SObjectUnsafe (SCons' _ ty' tys)
        -> withSymVal ty' $
           withDict (singMkSymVal (SObjectUnsafe tys)) Dict

instance Eq (tm ('TyObject '[])) => Ord (Object tm '[]) where
  compare _ _ = EQ

instance HasKind (Object tm '[]) where
  kindOf _ = KTuple []

instance (Eq (tm ('TyObject '[])), Typeable tm) => SymVal (Object tm '[]) where
  mkSymVal = genMkSymVar $ KTuple []

  literal (Object SNil) =
    let k = KTuple []
    in SBVI.SBV . SVal k . Left . CV k $ CTuple []

  fromCV (CV _ (CTuple [])) = Object SNil
  fromCV c                  = error $ "invalid (Object '[]): " ++ show c

instance
  ( Ord (tm ty)
  , Ord (Object tm tys)
  , Eq (tm ('TyObject ('(k, ty) : tys)))
  ) => Ord (Object tm ('(k, ty) ': tys)) where
  compare (Object (SCons _ a tys1)) (Object (SCons _ b tys2))
    = compare a b <> compare (Object tys1) (Object tys2)

instance
  ( HasKind (tm ty)
  , HasKind (Object tm tys)
  ) => HasKind (Object tm ('(k, ty) ': tys)) where
  kindOf _ = KTuple
    [ kindOf (undefined :: tm ty)
    , kindOf (undefined :: Object tm tys)
    ]

instance
  ( SingI ty
  , Typeable ty
  , Typeable tys
  , SymVal (tm ty)
  , SymVal (Object tm tys)
  , KnownSymbol k
  , Eq (tm ('TyObject ('(k, ty) : tys)))
  , Typeable tm
  , IsTerm tm
  ) => SymVal (Object tm ('(k, ty) ': tys)) where

  mkSymVal = genMkSymVar (kindOf (undefined :: Object tm ('(k, ty) ': tys)))

  literal (Object (SCons _k (Column _ x) xs)) = case literal x of
    SBVI.SBV (SVal kx (Left (CV _ xval))) -> case literal (Object xs) of
      SBVI.SBV (SVal kxs (Left (CV _ xsval))) ->
        let k = KTuple [kx, kxs]
        in SBVI.SBV $ SVal k $ Left $ CV k $ CTuple [xval, xsval]
      _ -> error "SymVal.literal (Object tm ('(k, ty) ': tys)): Cannot construct a literal value!"
    _ -> error "SymVal.literal (Object tm ('(k, ty) ': tys)): Cannot construct a literal value!"

  fromCV (CV (KTuple (k:ks)) (CTuple [x, xs])) =
    case fromCV (CV (KTuple ks) xs) of
      Object vals
        -> Object $ SCons SSymbol (Column sing (fromCV (CV k x))) vals
  fromCV c = error $ "invalid (Object tm ('(k, ty) ': tys)): " ++ show c

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map.Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Semigroup, Monoid)

instance Mergeable a => Mergeable (ColumnMap a) where
  symbolicMerge force test (ColumnMap left) (ColumnMap right) = ColumnMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

columnMapToSchema :: ColumnMap EType -> EType
columnMapToSchema (ColumnMap colMap) = go (Map.toList colMap) where
  go [] = EType SObjectNil
  go ((ColumnName colName, EType ty) : tys) = case someSymbolVal colName of
    SomeSymbol (_ :: Proxy k) -> withSing ty $ withTypeable ty $ case go tys of
      EType (SObject tys') -> EType $ mkSObject $ SCons' (SSymbol @k) ty tys'
      _ -> vacuousMatch "columnMapToSchema always returns (EType SObject)"
  go _ = vacuousMatch "both list constructors already covered"

schemaToColumns :: SingTy ('TyObject schema) -> [(String, EType)]
schemaToColumns (SObjectUnsafe schema) = case schema of
  SNil'
    -> []
  SCons' k v vs
    -> (symbolVal k, EType v) : schemaToColumns (SObjectUnsafe vs)

objTypeFilter :: (String -> Bool) -> SingTy ('TyObject schema) -> EType
objTypeFilter f objTy
  = columnMapToSchema
  $ ColumnMap
  $ Map.fromList
  $ fmap   (\(k,  v) -> (ColumnName k, v))
  $ filter (\(k, _v) -> f k)
  $ schemaToColumns objTy

objTypeDrop :: [String] -> SingTy ('TyObject schema) -> EType
objTypeDrop dropFields = objTypeFilter (`notElem` dropFields)

objTypeTake :: [String] -> SingTy ('TyObject schema) -> EType
objTypeTake keepFields = objTypeFilter (`elem` keepFields)

newtype TableMap a
  = TableMap { _tableMap :: Map.Map TableName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Mergeable a => Mergeable (TableMap a) where
  symbolicMerge force test (TableMap left) (TableMap right) = TableMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right


instance Pretty (Quantifiable q) where
  pretty = \case
    EType ty     -> pretty ty
    QTable       -> "table"
    QColumnOf tn -> "(column-of " <> pretty tn <> ")"

instance Pretty ColumnName where
  pretty (ColumnName cn) = dquotes $ prettyString cn

data DefinedProperty a = DefinedProperty
  { propertyArgs :: [(Text, QType)]
  , propertyBody :: a
  } deriving Show

-- | SFunArray with existential value type
data EValSFunArray k where
  EValSFunArray :: HasKind k => SingTy v -> SFunArray k (Concrete v) -> EValSFunArray k

instance Show (EValSFunArray k) where
  showsPrec p (EValSFunArray ty sfunarr) = showParen (p > 10) $
      showString "EValSFunArray "
    . showsPrec 11 ty
    . showChar ' '
    . withHasKind ty (showsPrec 11 sfunarr)

eVArrayAt
  :: forall k v
   . SingTy v
  -> S k
  -> Lens' (EValSFunArray k) (SBV (Concrete v))
eVArrayAt ty (S _ symKey) = lens getter setter where

  getter :: EValSFunArray k -> SBV (Concrete v)
  getter (EValSFunArray ty' arr) = case singEq ty ty' of
    Just Refl -> SBV.readArray arr symKey
    Nothing   -> error $
      "eVArrayAt: bad getter access: " ++ show ty ++ " vs " ++ show ty'

  setter :: EValSFunArray k -> SBV (Concrete v) -> EValSFunArray k
  setter (EValSFunArray ty' arr) val = case singEq ty ty' of
    Just Refl -> withSymVal ty $ EValSFunArray ty $ SBV.writeArray arr symKey val
    Nothing   -> error $
      "eVArrayAt: bad setter access: " ++ show ty ++ " vs " ++ show ty'

instance Mergeable (EValSFunArray k) where
  symbolicMerge force test (EValSFunArray ty1 arr1) (EValSFunArray ty2 arr2)
    = case singEq ty1 ty2 of
      Nothing   -> error "mismatched types when merging two EValSFunArrays"
      Just Refl -> withSymVal ty1 $
        EValSFunArray ty1 $ symbolicMerge force test arr1 arr2

-- | SFunArray with existential key type
data EKeySFunArray v where
  EKeySFunArray :: SingTy k -> SFunArray (Concrete k) v -> EKeySFunArray v

instance SymVal v => Show (EKeySFunArray v) where
  showsPrec p (EKeySFunArray ty sfunarr) = showParen (p > 10) $
      showString "EKeySFunArray "
    . showsPrec 11 ty
    . showChar ' '
    . withHasKind ty (showsPrec 11 sfunarr)

eKArrayAt
  :: forall k v
   . SymVal v
  => SingTy k
  -> S (Concrete k)
  -> Lens' (EKeySFunArray v) (SBV v)
eKArrayAt ty (S _ symKey) = lens getter setter
  where
    getter :: EKeySFunArray v -> SBV v
    getter (EKeySFunArray ty' arr) = case singEq ty ty' of
      Just Refl -> SBV.readArray arr symKey
      Nothing   -> error $
        "eKArrayAt: bad getter access: " ++ show ty ++ " vs " ++ show ty'

    setter :: EKeySFunArray v -> SBV v -> EKeySFunArray v
    setter (EKeySFunArray ty' arr) val = case singEq ty ty' of
      Just Refl -> EKeySFunArray ty $ SBV.writeArray arr symKey val
      Nothing   -> error $
        "eKArrayAt: bad setter access: " ++ show ty ++ " vs " ++ show ty'

instance SymVal v => Mergeable (EKeySFunArray v) where
  symbolicMerge f t (EKeySFunArray ty1 arr1) (EKeySFunArray ty2 arr2) =
    case singEq ty1 ty2 of
      Nothing   -> error "mismatched types when merging two EKeySFunArrays"
      Just Refl -> EKeySFunArray ty1 $ symbolicMerge f t arr1 arr2

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

data Governance
  = KsGovernance RegistryName
  | CapGovernance CapName
  deriving (Eq, Ord, Show)
