{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}

module Pact.Analyze.Types where

import           Control.Lens               (At (at), Index, IxValue, Ixed (ix),
                                             Iso, Iso', Lens', both, from, iso,
                                             lens, makeLenses, over, use, view,
                                             (%~), (&), (+=))
import           Control.Monad.State.Strict (MonadState)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.AffineSpace           ((.+^), (.-.))
import           Data.Data                  (Data)
import qualified Data.Decimal               as Decimal
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (AlgReal, Boolean (bnot, false, true, (&&&), (|||)),
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

import qualified Pact.Types.Lang            as Pact
import qualified Pact.Types.Typecheck       as TC
import           Pact.Types.Util            (AsString, tShow)

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

-- Pact uses Data.Decimal which is arbitrary-precision
type Decimal = AlgReal

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

-- Operations that apply to a pair of either integer or decimal, resulting in
-- the same:
-- integer -> integer -> integer
-- decimal -> decimal -> decimal
--
-- Or:
-- integer -> decimal -> integer
-- decimal -> integer -> integer
data ArithOp
  = Add -- ^ Addition
  | Sub -- ^ Subtraction
  | Mul -- ^ Multiplication
  | Div -- ^ Division
  | Pow -- ^ Exponentiation
  | Log -- ^ Logarithm
  deriving (Show, Eq)

-- integer -> integer
-- decimal -> decimal
data UnaryArithOp
  = Negate -- ^ Negation
  | Sqrt   -- ^ Square root
  | Ln     -- ^ Natural logarithm
  | Exp    -- ^ e raised to a power
  | Abs    -- ^ Absolute value

  | Signum -- ^ Sign of a number; implemented only for the sake of the Num
           -- instance.
  deriving (Show, Eq)

-- decimal -> integer -> decimal
-- decimal -> decimal
data RoundingLikeOp
  = Round   -- ^ Banker's method; reals exactly between two integers are
            -- rounded to the nearest even.
  | Ceiling -- ^ Round to the next integer
  | Floor   -- ^ Round to the previous integer
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

data EType where
  -- TODO: parametrize over constraint
  EType     :: (Show a, SymWord a, SMTValue a) => Type a -> EType
  EObjectTy ::                                    Schema -> EType

deriving instance Show EType

instance Eq EType where
  EType a == EType b = case typeEq a b of
    Just _refl -> True
    Nothing    -> False
  EObjectTy a == EObjectTy b = a == b
  _ == _ = False

instance EqSymbolic EType where
  ety .== ety' = fromBool $ ety == ety'

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
  | PreForall VarId Text EType PreProp
  | PreExists VarId Text EType PreProp

  -- applications
  | PreApp Text [PreProp]

  -- -- TODO: parse
  -- -- | PreAt {- Schema -} PreProp PreProp -- EType
  deriving Eq

data Prop a where
  -- Literals

  PLit :: SymWord a => a   -> Prop a
  -- ^ Injects a literal into the property language
  PSym ::              S a -> Prop a
  -- ^ Injects a symbolic value into the property language

  -- TX success/failure

  --
  -- TODO: remove either Success Or Abort.
  --

  Abort   :: Prop Bool
  -- ^ Whether a transaction aborts (does not succeed)
  Success :: Prop Bool
  -- ^ Whether a transaction succeeds (does not abort)
  Result  :: Prop a
  -- ^ The return value of the function under examination

  -- Abstraction

  Forall :: VarId -> Text -> EType -> Prop a -> Prop a
  -- ^ Introduces a universally-quantified variable over another property
  Exists :: VarId -> Text -> EType -> Prop a -> Prop a
  -- ^ Introduces an existentially-quantified variable over another property
  PVar   :: VarId -> Text                 -> Prop a
  -- ^ Refers to a function argument or universally/existentially-quantified variable

  -- Object ops

  --
  -- Note: PAt is the one property we can't yet parse because of the EType it
  -- includes
  --
  PAt :: Schema -> Prop String -> Prop Object -> EType -> Prop a
  -- ^ Projects from an object at a key

  -- String ops

  PStrConcat :: Prop String -> Prop String -> Prop String
  -- ^ Concatenates two strings
  PStrLength :: Prop String ->                Prop Integer
  -- ^ Produces the length of a string

  -- Numeric ops

  PDecArithOp      :: ArithOp        -> Prop Decimal -> Prop Decimal -> Prop Decimal
  -- ^ An 'ArithOp' expression over two 'Decimal' expressions
  PIntArithOp      :: ArithOp        -> Prop Integer -> Prop Integer -> Prop Integer
  -- ^ An 'ArithOp' expression over two 'Integer' expressions
  PDecUnaryArithOp :: UnaryArithOp   -> Prop Decimal ->                 Prop Decimal
  -- ^ A 'UnaryArithOp' expression over a 'Decimal' expression
  PIntUnaryArithOp :: UnaryArithOp   -> Prop Integer ->                 Prop Integer
  -- ^ A 'UnaryArithOp' expression over an 'Integer expression
  PDecIntArithOp   :: ArithOp        -> Prop Decimal -> Prop Integer -> Prop Decimal
  -- ^ An 'ArithOp' expression over a 'Decimal' and an 'Integer' expression
  PIntDecArithOp   :: ArithOp        -> Prop Integer -> Prop Decimal -> Prop Decimal
  -- ^ An 'ArithOp' expression over an 'Integer' and a 'Decimal' expression
  PModOp           :: Prop Integer   -> Prop Integer ->                 Prop Integer
  -- ^ Integer modulus
  PRoundingLikeOp1 :: RoundingLikeOp -> Prop Decimal ->                 Prop Integer
  -- ^ Rounds a 'Decimal' expression to an 'Integer' expression per the
  -- 'RoundingLikeOp' strategy
  PRoundingLikeOp2 :: RoundingLikeOp -> Prop Decimal -> Prop Integer -> Prop Decimal
  -- ^ Rounds a 'Decimal' expression to a 'Decimal' expression with the
  -- specified 'Integer' level of precision and per the 'RoundingLikeOp'
  -- strategy

  -- Time

  PIntAddTime :: Prop Time -> Prop Integer -> Prop Time
  -- ^ Adds an 'Integer' expression to a 'Time' expression
  PDecAddTime :: Prop Time -> Prop Decimal -> Prop Time
  -- ^ Adds a 'Decimal' expression to a 'Time' expression

  -- Comparison

  PIntegerComparison :: ComparisonOp -> Prop Integer -> Prop Integer -> Prop Bool
  -- ^ A 'ComparisonOp' expression over two 'Integer' expressions
  PDecimalComparison :: ComparisonOp -> Prop Decimal -> Prop Decimal -> Prop Bool
  -- ^ A 'ComparisonOp' expression over two 'Decimal' expressions
  PTimeComparison    :: ComparisonOp -> Prop Time    -> Prop Time    -> Prop Bool
  -- ^ A 'ComparisonOp' expression over two 'Time' expressions
  PStringComparison  :: ComparisonOp -> Prop String  -> Prop String  -> Prop Bool
  -- ^ A 'ComparisonOp' expression over two 'String' expressions
  PBoolComparison    :: ComparisonOp -> Prop Bool    -> Prop Bool    -> Prop Bool
  -- ^ A 'ComparisonOp' expression over two 'Bool' expressions
  PKeySetEqNeq       :: EqNeq        -> Prop KeySet  -> Prop KeySet  -> Prop Bool
  -- ^ A boolean comparison expression over two 'KeySet' expressions

  -- Boolean ops

  PLogical :: LogicalOp -> [Prop Bool] -> Prop Bool
  -- ^ A 'LogicalOp' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.

  -- DB properties

  TableWrite :: TableName  ->                Prop Bool
  -- ^ True when anything in the table is written
  TableRead  :: TableName  ->                Prop Bool
  -- ^ True when anything in the table is read

  --
  -- NOTE: it's possible that in a standard library we could implement these in
  --       terms of "CellRead"/"CellWrite" and existential quantification.
  --
  ColumnWrite :: TableName  -> ColumnName  -> Prop Bool
  -- ^ Whether a column is written
  ColumnRead  :: TableName  -> ColumnName  -> Prop Bool -- particular column is read
  -- ^ Whether a column is read

  --
  -- TODO: rewrite these in terms of CellBefore, CellAfter, ColumnSumBefore,
  --       ColumnSumAfter:
  --
  IntCellDelta   :: TableName  -> ColumnName  -> Prop RowKey -> Prop Integer
  -- ^ The difference (@after-before@) in a cell's integer value across a transaction
  DecCellDelta   :: TableName  -> ColumnName  -> Prop RowKey -> Prop Decimal
  -- ^ The difference (@after-before@) in a cell's decimal value across a transaction
  IntColumnDelta :: TableName  -> ColumnName                 -> Prop Integer
  -- ^ The difference (@after-before@) in a column's integer sum across a transaction
  DecColumnDelta :: TableName  -> ColumnName                 -> Prop Decimal
  -- ^ The difference (@after-before@) in a column's decimal sum across a transaction

  RowRead  :: TableName  -> Prop RowKey -> Prop Bool
  -- ^ Whether a row is read
  RowWrite :: TableName  -> Prop RowKey -> Prop Bool
  -- ^ Whether a row is written

  --
  -- TODO: StaleRead?
  --

  -- Authorization

  KsNameAuthorized :: KeySetName ->                              Prop Bool
  -- ^ Whether a transaction contains a signature that satisfied the named key set
  RowEnforced      :: TableName  -> ColumnName -> Prop RowKey -> Prop Bool
  -- ^ Whether a row has its keyset @enforce@d in a transaction

pattern PAnd :: Prop Bool -> Prop Bool -> Prop Bool
pattern PAnd a b = PLogical AndOp [a, b]

pattern POr :: Prop Bool -> Prop Bool -> Prop Bool
pattern POr a b = PLogical OrOp [a, b]

pattern PNot :: Prop Bool -> Prop Bool
pattern PNot a = PLogical NotOp [a]

deriving instance Eq a => Eq (Prop a)
deriving instance Show a => Show (Prop a)

instance Boolean (Prop Bool) where
  true   = PLit True
  false  = PLit False
  bnot p = PLogical NotOp [p]
  p1 &&& p2 = PAnd p1 p2
  p1 ||| p2 = POr  p1 p2

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

--
-- TODO: extract data type
--
type Arg
  = (Text, VarId, TC.Node, EType)

data Table = Table
  { _tableName       :: Text
  , _tableType       :: TC.UserType
  , _tableInvariants :: [Invariant Bool]
  } deriving (Show)

data Goal
  = Satisfaction -- ^ Find satisfying model
  | Validation   -- ^ Prove no invalidating model exists

deriving instance Eq Goal

data Check
  = InvariantsHold             -- valid, assuming success
  | PropertyHolds  (Prop Bool) -- valid, assuming success
  | Satisfiable    (Prop Bool) -- sat,   not assuming success
  | Valid          (Prop Bool) -- valid, not assuming success
  deriving Show

checkGoal :: Check -> Goal
checkGoal InvariantsHold    = Validation
checkGoal (PropertyHolds _) = Validation
checkGoal (Satisfiable _)   = Satisfaction
checkGoal (Valid _)         = Validation

data Located a
  = Located
    { _location :: Pact.Info
    , _located  :: a
    }
  deriving Functor

deriving instance Show a => Show (Located a)

newtype TagId
  = TagId Natural
  deriving (Num, Show, Ord, Eq)

data Model
  = Model
    { _modelArgs   :: Map VarId (Located (Text, TVal))
    -- ^ one per input to the function
    , _modelVars   :: Map VarId (Located (Text, TVal))
    -- ^ each intermediate variable binding
    , _modelReads  :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each read, in traversal order
    , _modelWrites :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each write, in traversal order
    , _modelAuths  :: Map TagId (Located (SBV Bool))
    -- ^ one per each enforce/auth check, in traversal order. note that for
    -- now, we just treat all (enforce ks) and (enforce-keyset "ks") calls
    -- equally, and in the future we can try to connect keysets with their
    -- names for better tooling / reporting.
    , _modelResult :: Located TVal
    -- ^ return value of the function being checked
    }
  deriving Show

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
    TInt     -> "int"
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
  userShowsPrec _d = \case
    PreIntegerLit i -> tShow i
    PreStringLit t  -> tShow t
    PreDecimalLit d -> tShow d
    PreTimeLit t    -> tShow (Pact.LTime (unMkTime t))
    PreBoolLit b    -> tShow (Pact.LBool b)

    PreAbort        -> "abort"
    PreSuccess      -> "success"
    PreResult       -> "result"
    PreVar _id name -> name

    PreForall _vid name (EType ty) prop ->
      "(forall (" <> name <> ":" <> userShow ty <> ") " <> userShow prop <> ")"
    PreExists _vid name (EType ty) prop ->
      "(exists (" <> name <> ":" <> userShow ty <> ") " <> userShow prop <> ")"
    PreApp name applicands -> "(" <> name <> " " <> T.unwords
      ((map userShow) applicands) <> ")"

    PreForall _vid _name (EObjectTy _) _prop ->
      "ERROR: quantification over an object type is not currently allowed (issue 139)"
    PreExists _vid _name (EObjectTy _) _prop ->
      "ERROR: quantification over an object type is not currently allowed (issue 139)"

instance UserShow Pact.Exp where
  userShowsPrec _ = tShow

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
-- The language is stateless. Arithmetic could be added if we decide it's
-- useful.
data Invariant a where

  -- literals
  IDecimalLiteral :: Decimal -> Invariant Decimal
  -- ^ A 'Decimal' literal
  IIntLiteral     :: Integer -> Invariant Integer
  -- ^ An 'Integer' literal
  IStringLiteral  :: Text    -> Invariant String
  -- ^ A 'String' literal
  ITimeLiteral    :: Time    -> Invariant Time
  -- ^ A 'Time' literal
  IBoolLiteral    :: Bool    -> Invariant Bool
  -- ^ A 'Bool' literal

  ISym :: S a -> Invariant a
  -- ^ Creates an invariant expression from a symbolic value

  -- variables
  IVar :: Text -> Invariant a
  -- ^ Refers to a variable with the provided name

  -- string ops
  IStrConcat :: Invariant String -> Invariant String -> Invariant String
  -- ^ The concatenation of two 'String' expressions
  IStrLength :: Invariant String                     -> Invariant Integer
  -- ^ The length of a 'String' expression

  -- numeric ops
  IDecArithOp      :: ArithOp             -> Invariant Decimal -> Invariant Decimal -> Invariant Decimal
  -- ^ An 'ArithOp' expression over two 'Decimal' expressions
  IIntArithOp      :: ArithOp             -> Invariant Integer -> Invariant Integer -> Invariant Integer
  -- ^ An 'ArithOp' expression over two 'Integer' expressions
  IDecUnaryArithOp :: UnaryArithOp        -> Invariant Decimal ->                      Invariant Decimal
  -- ^ A 'UnaryArithOp' expression over a 'Decimal' expression
  IIntUnaryArithOp :: UnaryArithOp        -> Invariant Integer ->                      Invariant Integer
  -- ^ A 'UnaryArithOp' expression over an 'Integer expression
  IDecIntArithOp   :: ArithOp             -> Invariant Decimal -> Invariant Integer -> Invariant Decimal
  -- ^ An 'ArithOp' expression over a 'Decimal' and an 'Integer' expression
  IIntDecArithOp   :: ArithOp             -> Invariant Integer -> Invariant Decimal -> Invariant Decimal
  -- ^ An 'ArithOp' expression over an 'Integer' and a 'Decimal' expression
  IModOp           :: Invariant Integer   -> Invariant Integer ->                      Invariant Integer
  -- ^ Integer modulus
  IRoundingLikeOp1 :: RoundingLikeOp      -> Invariant Decimal ->                      Invariant Integer
  -- ^ Rounds a 'Decimal' expression to an 'Integer' expression per the
  -- 'RoundingLikeOp' strategy
  IRoundingLikeOp2 :: RoundingLikeOp      -> Invariant Decimal -> Invariant Integer -> Invariant Decimal
  -- ^ Rounds a 'Decimal' expression to a 'Decimal' expression with the
  -- specified 'Integer' level of precision and per the 'RoundingLikeOp'
  -- strategy

  -- Time
  IIntAddTime      :: Invariant Time -> Invariant Integer -> Invariant Time
  -- ^ Adds an 'Integer' expression to a 'Time' expression
  IDecAddTime      :: Invariant Time -> Invariant Decimal -> Invariant Time
  -- ^ Adds a 'Decimal' expression to a 'Time' expression

  -- comparison
  IDecimalComparison :: ComparisonOp -> Invariant Decimal -> Invariant Decimal -> Invariant Bool
  -- ^ A 'ComparisonOp' expression over two 'Decimal' expressions
  IIntComparison     :: ComparisonOp -> Invariant Integer -> Invariant Integer -> Invariant Bool
  -- ^ A 'ComparisonOp' expression over two 'Integer' expressions
  IStringComparison  :: ComparisonOp -> Invariant String  -> Invariant String  -> Invariant Bool
  -- ^ A 'ComparisonOp' expression over two 'String' expressions
  ITimeComparison    :: ComparisonOp -> Invariant Time    -> Invariant Time    -> Invariant Bool
  -- ^ A 'ComparisonOp' expression over two 'Time' expressions
  IBoolComparison    :: ComparisonOp -> Invariant Bool    -> Invariant Bool    -> Invariant Bool
  -- ^ A 'ComparisonOp' expression over two 'Bool' expressions
  IKeySetEqNeq       :: EqNeq        -> Invariant KeySet  -> Invariant KeySet  -> Invariant Bool
  -- ^ A boolean comparison expression over two 'KeySet' expressions

  -- boolean ops
  ILogicalOp :: LogicalOp -> [Invariant Bool] -> Invariant Bool
  -- ^ A 'LogicalOp' expression over one or two 'Bool' expressions; one operand
  -- for NOT, and two operands for AND or OR.

deriving instance Eq (Invariant a)
deriving instance Show (Invariant a)

data SomeSchemaInvariant where
  SomeSchemaInvariant :: Invariant a -> Type a -> SomeSchemaInvariant

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
  deriving (Show, Functor, Foldable, Traversable)

instance Mergeable a => Mergeable (TableMap a) where
  symbolicMerge force test (TableMap left) (TableMap right) = TableMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

makeLenses ''S
makeLenses ''Object
makeLenses ''Table
makeLenses ''Model
makeLenses ''Located
makeLenses ''ColumnMap
makeLenses ''TableMap

type instance Index (ColumnMap a) = ColumnName
type instance IxValue (ColumnMap a) = a
instance Ixed (ColumnMap a) where ix k = columnMap.ix k
instance At (ColumnMap a) where at k = columnMap.at k

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k
