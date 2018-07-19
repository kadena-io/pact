{-# LANGUAGE ConstraintKinds            #-}
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

module Pact.Analyze.Types where

import           Control.Lens               (At (at), Index, Iso, Iso', IxValue,
                                             Ixed (ix), Lens', both, from, iso,
                                             lens, makeLenses, makePrisms, over,
                                             use, view, (%~), (&), (+=))
import           Control.Monad.State.Strict (MonadState)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.AffineSpace           ((.+^), (.-.))
import           Data.Data                  (Data)
import qualified Data.Decimal               as Decimal
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (Boolean (bnot, false, true, (&&&), (|||)),
                                             EqSymbolic, HasKind, Int64,
                                             Kind (KString, KUnbounded),
                                             Mergeable (symbolicMerge),
                                             OrdSymbolic, Provable (forAll),
                                             SBV,
                                             SDivisible (sDivMod, sQuotRem),
                                             SymWord, Symbolic, forAll_,
                                             forSome, forSome_, fromBool,
                                             isConcrete, ite, kindOf, literal,
                                             oneIf, sFromIntegral,
                                             sRealToSInteger, unliteral, (%),
                                             (.<), (.==))
import           Data.SBV.Control           (SMTValue (..))
import qualified Data.SBV.Internals         as SBVI
import qualified Data.SBV.String            as SBV
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as Set
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Thyme                 (UTCTime, microseconds)
import           Data.Typeable              ((:~:) (Refl))
import           GHC.Natural                (Natural)
import           Prelude                    hiding (Float)

import qualified Pact.Types.Lang            as Pact
import qualified Pact.Types.Typecheck       as TC
import           Pact.Types.Util            (AsString, tShow)

import           Pact.Analyze.Numerical
import           Pact.Analyze.Orphans       ()
import           Pact.Analyze.Util

import Data.Function (on)
import Data.List (sortBy)

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

mkTime :: UTCTime -> Time
mkTime utct = view microseconds (utct .-. toEnum 0)

unMkTime :: Time -> UTCTime
unMkTime time = toEnum 0 .+^ view (from microseconds) time

timeIso :: Iso' UTCTime Time
timeIso = iso mkTime unMkTime

data LogicalOp
  = AndOp -- ^ Conjunction
  | OrOp  -- ^ Disjunction
  | NotOp -- ^ Negation
  deriving (Show, Eq)

data EqNeq
  = Eq'  -- ^ Equal
  | Neq' -- ^ Not equal
  deriving (Show, Eq)

data ComparisonOp
  = Gt  -- ^ Greater than
  | Lt  -- ^ Less than
  | Gte -- ^ Greater than or equal to
  | Lte -- ^ Less than or equal to
  | Eq  -- ^ Equal
  | Neq -- ^ Not equal
  deriving (Show, Eq)

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
  | FromInput   Text
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
  deriving (Eq, Show)

objFields :: Lens' Object (Map Text TVal)
objFields = lens getter setter
  where
    getter (Object fs) = fs
    setter (Object _) fs' = Object fs'

newtype Schema
  = Schema (Map Text EType)
  deriving (Show, Eq)

-- | When given a column mapping, this function gives a canonical way to assign
-- var ids to each column. Also see 'varIdArgs'.
varIdColumns :: ColumnMap a -> Map VarId a
varIdColumns (ColumnMap m) = varIdColumns' (Map.mapKeys
  (\(ColumnName name) -> T.pack name) m)

varIdColumns' :: Map Text a -> Map VarId a
varIdColumns' m =
  let sortedList = sortBy (compare `on` fst) (Map.toList m)
      reindexedList =
        zipWith (\index (_name, val) -> (index, val)) [0..] sortedList
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

data QKind = QType | QAny

-- Integer, Decimal, Bool, String, Time
type SimpleType a = (Float a, Show a, SymWord a, SMTValue a)

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

pattern TableNameLit :: String -> Prop TableName
pattern TableNameLit str = PLit (TableName str)

pattern ColumnNameLit :: String -> Prop ColumnName
pattern ColumnNameLit str = PLit (ColumnName str)

instance IsString (Prop TableName) where
  fromString = PLit . fromString

instance IsString (Prop ColumnName) where
  fromString = PLit . fromString

data Quantifier
  = Forall' VarId Text QType
  | Exists' VarId Text QType

class Float a where
  float :: Prop a -> ([Quantifier], Prop a)

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

class sub :<: sup where
  inject  :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

pattern Inj :: sub :<: sup => sub a -> sup a
pattern Inj a <- (project -> Just a) where
  Inj a = inject a

instance PropSpecific :<: Prop where
  inject = PropSpecific
  project = \case
    PropSpecific a -> Just a
    _              -> Nothing

instance Numerical Prop :<: Prop where
  inject = PureProp . Numerical
  project (PureProp (Numerical a)) = Just a
  project _                        = Nothing

instance Numerical Invariant :<: Invariant where
  inject = PureInvariant . Numerical
  project (PureInvariant (Numerical a)) = Just a
  project _                             = Nothing

data PureTerm et t a where
  Lit :: a -> PureTerm et t a
  -- | Injects a symbolic value into the language
  Sym :: S a -> PureTerm et t a

  -- | Refers to a function argument, universally/existentially-quantified
  -- variable, or column
  Var :: VarId -> Text -> PureTerm et t a

  -- string ops
  -- | The concatenation of two 'String' expressions
  StrConcat :: t String -> t String -> PureTerm et t String
  -- | The length of a 'String' expression
  StrLength :: t String                     -> PureTerm et t Integer

  -- numeric ops
  Numerical :: Numerical t a -> PureTerm et t a

  -- Time
  -- | Adds an 'Integer' expression to a 'Time' expression
  IntAddTime      :: t Time -> t Integer -> PureTerm et t Time
  -- | Adds a 'Decimal' expression to a 'Time' expression
  DecAddTime      :: t Time -> t Decimal -> PureTerm et t Time

  -- comparison
  -- | A 'ComparisonOp' expression over two 'Integer' expressions
  IntegerComparison :: ComparisonOp -> t Integer -> t Integer -> PureTerm et t Bool
  -- | A 'ComparisonOp' expression over two 'Decimal' expressions
  DecimalComparison :: ComparisonOp -> t Decimal -> t Decimal -> PureTerm et t Bool
  -- | A 'ComparisonOp' expression over two 'Time' expressions
  TimeComparison    :: ComparisonOp -> t Time    -> t Time    -> PureTerm et t Bool
  -- | A 'ComparisonOp' expression over two 'String' expressions
  StringComparison  :: ComparisonOp -> t String  -> t String  -> PureTerm et t Bool
  -- | A 'ComparisonOp' expression over two 'Bool' expressions
  BoolComparison    :: ComparisonOp -> t Bool    -> t Bool    -> PureTerm et t Bool

  KeySetEqNeq :: EqNeq -> t KeySet -> t KeySet -> PureTerm et t Bool
  ObjectEqNeq :: EqNeq -> t Object -> t Object -> PureTerm et t Bool

  At             :: Schema -> t String -> t Object -> EType -> PureTerm et t a

  LiteralObject  :: Map Text et -> PureTerm et t Object

  -- boolean ops
  -- | A 'Logical' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.
  Logical :: LogicalOp -> [t Bool] -> PureTerm et t Bool

deriving instance Show a => Show (PureTerm EProp Prop a)
deriving instance Show a => Show (PureTerm EInvariant Invariant a)

deriving instance Eq a => Eq (PureTerm EProp Prop a)
deriving instance Eq a => Eq (PureTerm EInvariant Invariant a)

pattern PLit :: a -> Prop a
pattern PLit a = PureProp (Lit a)

pattern PVar :: VarId -> Text -> Prop t
pattern PVar vid name = PureProp (Var vid name)

data Prop a
  = PropSpecific (PropSpecific a)
  | PureProp     (PureTerm EProp Prop a)
  deriving (Show, Eq)

instance S :<: Prop where
  inject = PureProp . Sym
  project = \case
    PureProp (Sym a) -> Just a
    _                -> Nothing

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

data EProp where
  EProp       :: SimpleType a => Type a -> Prop a      -> EProp
  EObjectProp ::                 Schema -> Prop Object -> EProp

deriving instance Show EProp

instance Eq EProp where
  EProp ta pa == EProp tb pb = case typeEq ta tb of
    Just Refl -> pa == pb
    Nothing   -> False
  EObjectProp sa pa == EObjectProp sb pb = sa == sb && pa == pb
  _ == _ = False

ePropToEType :: EProp -> EType
ePropToEType = \case
  EProp ty _            -> EType ty
  EObjectProp schema' _ -> EObjectTy schema'

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

-- | An argument to a function
data Arg = Arg
  { argName  :: Text
  , argVarId :: VarId
  , argNode  :: TC.Node
  , argType  :: EType
  }

data Table = Table
  { _tableName       :: Text
  , _tableType       :: TC.UserType
  , _tableInvariants :: [Located (Invariant Bool)]
  } deriving (Show)

data Goal
  = Satisfaction -- ^ Find satisfying model
  | Validation   -- ^ Prove no invalidating model exists

deriving instance Eq Goal

data Check
  = PropertyHolds (Prop Bool) -- valid, assuming success
  | Satisfiable   (Prop Bool) -- sat,   not assuming success
  | Valid         (Prop Bool) -- valid, not assuming success
  --
  -- TODO: potentially another case for satisfiable, assuming success?
  --
  deriving Show

checkGoal :: Check -> Goal
checkGoal (PropertyHolds _) = Validation
checkGoal (Satisfiable _)   = Satisfaction
checkGoal (Valid _)         = Validation

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

newtype TagId
  = TagId Natural
  deriving (Num, Show, Ord, Eq)

data ModelTags
  = ModelTags
    { _mtArgs   :: Map VarId (Located (Text, TVal))
    -- ^ one per input to the function
    , _mtVars   :: Map VarId (Located (Text, TVal))
    -- ^ each intermediate variable binding
    , _mtReads  :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each read, in traversal order
    , _mtWrites :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each write, in traversal order
    , _mtAuths  :: Map TagId (Located (SBV Bool))
    -- ^ one per each enforce/auth check, in traversal order. note that this
    -- includes all (enforce ks) and (enforce-keyset "ks") calls.
    , _mtResult :: Located TVal
    -- ^ return value of the function being checked
    }
  deriving (Eq, Show)

data Model
  = Model
    { _modelTags    :: ModelTags
    , _modelKsProvs :: Map TagId Provenance
    }
  deriving (Eq, Show)

-- | The type of a pact data type we can't reason about -- see 'TAny'.
data Any = Any
  deriving (Show, Read, Eq, Ord, Data)

instance HasKind Any
instance SymWord Any
instance SMTValue Any

newtype KeySet
  = KeySet Integer
  deriving (Eq, Ord, Data, Show, Read)

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

class UserShow a where
  userShowsPrec :: Int -> a -> Text

  userShowList :: [a] -> Text
  userShowList as = "[" <> T.intercalate ", " (userShow <$> as) <> "]"

instance UserShow (Type a) where
  userShowsPrec _ = \case
    TInt     -> "integer"
    TBool    -> "bool"
    TStr     -> "string"
    TTime    -> "time"
    TDecimal -> "decimal"
    TKeySet  -> "keyset"
    TAny     -> "*"

instance UserShow ArithOp where
  userShowsPrec _d = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Pow -> "^"
    Log -> "log"

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

instance UserShow Pact.Exp where
  userShowsPrec _ = tShow

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

instance UserShow a => UserShow (Map Text a) where
  userShowsPrec _ m =
    let go result k a = result <> ", " <> k <> ": " <> userShow a
    in "{ " <> T.drop 2 (Map.foldlWithKey go "" m) <> " }"

-- Note: this doesn't exactly match the pact syntax
instance UserShow Schema where
  userShowsPrec d (Schema schema) = userShowsPrec d schema

userShow :: UserShow a => a -> Text
userShow = userShowsPrec 0

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
data Invariant a = PureInvariant (PureTerm EInvariant Invariant a)
  deriving (Show, Eq)

instance S :<: Invariant where
  inject = PureInvariant . Sym
  project = \case
    PureInvariant (Sym a) -> Just a
    _                     -> Nothing

data EInvariant where
  EInvariant       :: SimpleType a => Type a -> Invariant a      -> EInvariant
  EObjectInvariant ::                 Schema -> Invariant Object -> EInvariant

instance Eq EInvariant where
  EInvariant ta ia == EInvariant tb ib = case typeEq ta tb of
    Just Refl -> ia == ib
    Nothing   -> False
  EObjectInvariant sa pa == EObjectInvariant sb pb = sa == sb && pa == pb
  _ == _ = False

instance Show EInvariant where
  show (EInvariant ty inv) = "(" ++ show inv ++ ": " ++ show ty ++ ")"
  show (EObjectInvariant ty obj) = "(" ++ show obj ++ ": " ++ show ty ++ ")"

pattern ILiteral :: a -> Invariant a
pattern ILiteral a = PureInvariant (Lit a)

pattern ILogicalOp :: LogicalOp -> [Invariant Bool] -> Invariant Bool
pattern ILogicalOp op args = PureInvariant (Logical op args)

data SomeSchemaInvariant where
  SomeSchemaInvariant :: (Show a, Eq a) => Invariant a -> Type a -> SomeSchemaInvariant

deriving instance Show SomeSchemaInvariant

instance Eq SomeSchemaInvariant where
  SomeSchemaInvariant a ta == SomeSchemaInvariant b tb = case typeEq ta tb of
    Nothing   -> False
    Just Refl -> a == b

genId :: (MonadState s m, Num i) => Lens' s i -> m i
genId l = do
  i <- use l
  l += 1
  pure i

class HasVarId s where
  varId :: Lens' s VarId

instance HasVarId VarId where
  varId = id

genVarId :: (MonadState s m, HasVarId s) => m VarId
genVarId = genId varId

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Monoid)

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

makeLenses ''S
makeLenses ''Object
makeLenses ''Table
makeLenses ''ModelTags
makeLenses ''Model
makeLenses ''Located
makeLenses ''ColumnMap
makeLenses ''TableMap
makeLenses ''OriginatingCell
makePrisms ''Provenance
makePrisms ''AVal

type instance Index (ColumnMap a) = ColumnName
type instance IxValue (ColumnMap a) = a
instance Ixed (ColumnMap a) where ix k = columnMap.ix k
instance At (ColumnMap a) where at k = columnMap.at k

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k
