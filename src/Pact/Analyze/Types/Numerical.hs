{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

{-# LANGUAGE InstanceSigs                  #-}
{-# LANGUAGE PolyKinds                  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- coerceSBV requires Coercible
module Pact.Analyze.Types.Numerical where

import Data.Constraint (Dict(Dict))
import Data.Constraint.Extras
import Data.Semigroup ((<>))
import           Data.Type.Equality           ((:~:) (Refl), apply)
import           Control.Lens                (Prism')
import           Data.Coerce                 (Coercible)
import qualified Data.Decimal                as Decimal
import           Data.SBV                    (HasKind (kindOf),
                                              SDivisible (..), SymWord (..),
                                              bnot, oneIf, (&&&), (.==), (.>),
                                              (.^), (|||))
import           Data.SBV.Control            (SMTValue (sexprToVal))
import           Data.SBV.Dynamic            (svAbs, svPlus, svTimes, svUNeg)
import           Data.SBV.Internals          (CW (..), CWVal (CWInteger),
                                              SBV (SBV), SVal (SVal),
                                              genMkSymVar, normCW)
import qualified Data.SBV.Internals          as SBVI
import           Data.Text                   (Text)
import           GHC.Real                    ((%))

import           Pact.Types.Util             (tShow)

import           Pact.Analyze.Feature        hiding (dec, Constraint)
import           Pact.Analyze.Types.UserShow


-- TODO: merge with PrimType?
data Ty
  = TyInteger
  | TyBool
  | TyStr
  | TyTime
  | TyDecimal
  | TyKeySet
  | TyAny
  | TyList Ty
  | TyObject

type TyTableName  = 'TyStr
type TyColumnName = 'TyStr
type TyRowKey     = 'TyStr
type TyKeySetName = 'TyStr

data Kind = SimpleK | ListK | ObjectK

data SingTy :: Kind -> Ty -> * where
  SInteger ::                      SingTy 'SimpleK 'TyInteger
  SBool    ::                      SingTy 'SimpleK 'TyBool
  SStr     ::                      SingTy 'SimpleK 'TyStr
  STime    ::                      SingTy 'SimpleK 'TyTime
  SDecimal ::                      SingTy 'SimpleK 'TyDecimal
  SKeySet  ::                      SingTy 'SimpleK 'TyKeySet
  SAny     ::                      SingTy 'SimpleK 'TyAny
  SList    :: SingTy 'SimpleK a -> SingTy 'ListK   ('TyList a)
  SObject  ::                      SingTy 'ObjectK 'TyObject

instance Show (SingTy k ty) where
  showsPrec p = \case
    SInteger -> showString "SInteger"
    SBool    -> showString "SBool"
    SStr     -> showString "SStr"
    STime    -> showString "STime"
    SDecimal -> showString "SDecimal"
    SKeySet  -> showString "SKeySet"
    SAny     -> showString "SAny"
    SList a  -> showParen (p > 10) $ showString "SList " . showsPrec 11 a
    SObject  -> showString "SObject"

instance UserShow (SingTy k ty) where
  userShowPrec _ = \case
    SInteger     -> "integer"
    SBool    -> "bool"
    SStr     -> "string"
    STime    -> "time"
    SDecimal -> "decimal"
    SKeySet  -> "keyset"
    SAny     -> "*"
    SList a  -> "[" <> userShow a <> "]"
    SObject  -> "object"

instance ArgDict (SingTy k) where
  type ConstraintsFor (SingTy k) c =
    ( c 'TyInteger
    , c 'TyBool
    , c 'TyStr
    , c 'TyTime
    , c 'TyDecimal
    , c 'TyKeySet
    , c 'TyAny
    , c ('TyList 'TyInteger)
    , c ('TyList 'TyBool)
    , c ('TyList 'TyStr)
    , c ('TyList 'TyTime)
    , c ('TyList 'TyDecimal)
    , c ('TyList 'TyKeySet)
    , c ('TyList 'TyAny)
    , c 'TyObject
    )

  type ConstraintsFor' (SingTy k) c g =
    ( c (g 'TyInteger)
    , c (g 'TyBool)
    , c (g 'TyStr)
    , c (g 'TyTime)
    , c (g 'TyDecimal)
    , c (g 'TyKeySet)
    , c (g 'TyAny)
    , c (g ('TyList 'TyInteger))
    , c (g ('TyList 'TyBool))
    , c (g ('TyList 'TyStr))
    , c (g ('TyList 'TyTime))
    , c (g ('TyList 'TyDecimal))
    , c (g ('TyList 'TyKeySet))
    , c (g ('TyList 'TyAny))
    , c (g 'TyObject)
    )

  argDict :: ConstraintsFor (SingTy k) c => SingTy k a -> Dict (c a)
  argDict = \case
    SInteger       -> Dict
    SBool          -> Dict
    SStr           -> Dict
    STime          -> Dict
    SDecimal       -> Dict
    SKeySet        -> Dict
    SAny           -> Dict
    SList SInteger -> Dict
    SList SBool    -> Dict
    SList SStr     -> Dict
    SList STime    -> Dict
    SList SDecimal -> Dict
    SList SKeySet  -> Dict
    SList SAny     -> Dict
    SObject        -> Dict

  -- argDict' :: ConstraintsFor' (SingTy k) c g => (SingTy k) a -> Dict (c (g a))
  argDict' = \case
    SInteger       -> Dict
    SBool          -> Dict
    SStr           -> Dict
    STime          -> Dict
    SDecimal       -> Dict
    SKeySet        -> Dict
    SAny           -> Dict
    SList SInteger -> Dict
    SList SBool    -> Dict
    SList SStr     -> Dict
    SList STime    -> Dict
    SList SDecimal -> Dict
    SList SKeySet  -> Dict
    SList SAny     -> Dict
    SObject        -> Dict

singEq :: SingTy k1 a -> SingTy k2 b -> Maybe (a :~: b)
singEq SInteger  SInteger  = Just Refl
singEq SBool     SBool     = Just Refl
singEq SStr      SStr      = Just Refl
singEq STime     STime     = Just Refl
singEq SDecimal  SDecimal  = Just Refl
singEq SKeySet   SKeySet   = Just Refl
singEq SAny      SAny      = Just Refl
singEq (SList a) (SList b) = apply Refl <$> singEq a b
singEq SObject   SObject   = Just Refl
singEq _         _         = Nothing

-- We model decimals as integers. The value of a decimal is the value of the
-- integer, shifted right 255 decimal places.
newtype Decimal = Decimal { unDecimal :: Integer }
  deriving (Enum, Eq, Ord, Show)

forceConcrete :: SymWord a => SBV a -> a
forceConcrete sbva = case unliteral sbva of
  Just result -> result
  Nothing     -> error "this computation must be concrete"

liftSBV :: (SymWord a, SymWord b) => (SBV a -> SBV b) -> a -> b
liftSBV f = forceConcrete . f . literal

liftSBV2 :: (SymWord a, SymWord b, SymWord c)
  => (SBV a -> SBV b -> SBV c)
  -> a -> b -> c
liftSBV2 f a b = forceConcrete $ f (literal a) (literal b)

instance Num Decimal where
  negate                = liftSBV negate
  (+)                   = liftSBV2 (+)
  (-)                   = liftSBV2 (-)
  Decimal a * Decimal b = Decimal $ liftSBV banker'sMethod $ Decimal $ a * b
  fromInteger           = lShift255D . Decimal
  abs                   =              Decimal . abs    . unDecimal
  signum                = lShift255D . Decimal . signum . unDecimal

instance Fractional Decimal where
  (/)          = liftSBV2 (/)
  fromRational = forceConcrete . fromRational

class SymbolicDecimal d where
  type IntegerOf d :: *
  fromInteger' :: IntegerOf d      -> d
  lShiftD      :: Int         -> d -> d
  lShiftD'     :: IntegerOf d -> d -> d
  rShiftD      :: Int         -> d -> d
  rShiftD'     :: IntegerOf d -> d -> d
  floorD       ::      d -> IntegerOf d

instance SymbolicDecimal Decimal where
  type IntegerOf Decimal = Integer
  fromInteger'          = fromInteger
  lShiftD n (Decimal d) = Decimal (d * 10 ^ n)
  lShiftD' n            = lShiftD (fromIntegral n)
  rShiftD n (Decimal d) = Decimal (d `div` 10 ^ n)
  rShiftD' n            = rShiftD (fromIntegral n)
  floorD                = unDecimal . rShift255D

-- Caution: see note [OverlappingInstances] *This instance must be selected for
-- decimals*.
instance {-# OVERLAPPING #-} Num (SBV Decimal) where
  negate (SBVI.SBV a)     = SBVI.SBV $ svUNeg a
  SBVI.SBV a + SBVI.SBV b = SBVI.SBV $ svPlus a b
  SBVI.SBV a - SBVI.SBV b = SBVI.SBV $ svPlus a $ svUNeg b
  SBVI.SBV a * SBVI.SBV b = coerceSBV $ banker'sMethod $ SBVI.SBV $ svTimes a b
  fromInteger             = literal . fromInteger
  abs (SBVI.SBV a)        = SBVI.SBV $ svAbs a
  signum = lShift255D . coerceSBV . signum . coerceSBV @Decimal @Integer

-- Caution: see note [OverlappingInstances] *This instance must be selected for
-- decimals*.
instance {-# OVERLAPPING #-} Fractional (SBV Decimal) where
  -- Note that we need to round to the nearest decimal in the same way that the
  -- banker's method rounds to the nearest int. Also note we need to make the
  -- numerator larger by a factor of 10^255 to offset the larger denominator (1
  -- * 10^255 represents 1.0).
  a / b = coerceSBV @Integer @Decimal $ roundingDiv
    (coerceSBV @_ @Integer (lShift255D a))
    (coerceSBV @_ @Integer b)

  fromRational rat = literal $
    let (Decimal.Decimal places mantissa) = fromRational rat
    in lShiftD (decimalPrecision - fromIntegral places) (Decimal mantissa)

-- Convert from decimal to integer by the banker's method. This rounds to the
-- nearest integer when the decimal happens to land exactly between two
-- integers.
banker'sMethod :: SBV Decimal -> SBV Integer
banker'sMethod d
  = roundingDiv (coerceSBV @Decimal @Integer d) (coerceSBV @Decimal @Integer 1)

-- (Banker's method) rounding division for integers.
roundingDiv :: SBV Integer -> SBV Integer -> SBV Integer
roundingDiv num denom =
  let -- @wholePart@ is always less than (or equal to) the answer, so we may
      -- add a positive adjustment to it.
      -- @fractionalPart@ is negative when the denominator is negative. So when
      -- we use it we take the absolute value.
      (wholePart, fractionalPart) = num `sDivMod` denom

      exactlyBetweenNumbers = abs fractionalPart * 2 .== abs denom
      roundsUp              = abs fractionalPart * 2 .>  abs denom
      wholePartIsOdd        = bnot $ wholePart `sMod` 2 .== 0

    -- We're working in the space of integers 10^255 times bigger than the
    -- decimals they represent. Possibly add an adjustment to jump to the next
    -- decimal, then convert to a decimal.
  in wholePart + oneIf (roundsUp ||| exactlyBetweenNumbers &&& wholePartIsOdd)

instance SymbolicDecimal (SBV Decimal) where
  type IntegerOf (SBV Decimal) = SBV Integer

  fromInteger' = lShift255D . coerceSBV

  lShiftD  n = coerceSBV . (*       10  ^ n)  . coerceSBV @Decimal @Integer
  lShiftD' n = coerceSBV . (*       10 .^ n)  . coerceSBV @Decimal @Integer
  rShiftD  n = coerceSBV . (`sDiv` (10  ^ n)) . coerceSBV @Decimal @Integer
  rShiftD' n = coerceSBV . (`sDiv` (10 .^ n)) . coerceSBV @Decimal @Integer
  floorD     = coerceSBV . rShift255D

decimalPrecision :: Int
decimalPrecision = 255

lShift255D :: SymbolicDecimal d => d -> d
lShift255D = lShiftD decimalPrecision

-- Caution: This is only used for the floor operation. Do not use for dropping
-- digits. The banker's method must be used instead.
rShift255D :: SymbolicDecimal d => d -> d
rShift255D = rShiftD decimalPrecision

-- Note: we require a redundant `Coercible` constraint here. `a` and `b` are
-- both phantom, so this is not actually required to do the conversion, but it
-- seems right morally. We also provide the unsafe version for conversions like
-- `Text` -> `String`.
coerceSBV :: Coercible a b => SBV a -> SBV b
coerceSBV = SBVI.SBV . SBVI.unSBV

unsafeCoerceSBV :: SBV a -> SBV b
unsafeCoerceSBV = SBVI.SBV . SBVI.unSBV

instance HasKind Decimal where kindOf _ = SBVI.KUnbounded
instance SymWord Decimal where
  mkSymWord  = genMkSymVar SBVI.KUnbounded
  literal a  = SBV . SVal SBVI.KUnbounded . Left . normCW $ CW SBVI.KUnbounded (CWInteger (unDecimal a))
  fromCW (CW _ (CWInteger x)) = Decimal x
  fromCW x = error $ "in instance SymWord Decimal: expected CWInteger, found: " ++ show x

instance SMTValue Decimal where sexprToVal = fmap Decimal . sexprToVal

instance UserShow Decimal where
  userShowPrec _ (Decimal dec) =
    case Decimal.eitherFromRational (dec % (10 ^ decimalPrecision)) of
      Left err                    -> error err
      -- Make sure to show ".0":
      Right (Decimal.Decimal 0 i) -> tShow $ Decimal.Decimal 1 (i * 10 :: Integer)
      Right d                     -> tShow d

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
  deriving (Show, Eq, Ord)

arithOpP :: Prism' Text ArithOp
arithOpP = mkOpNamePrism
  [ (SAddition,       Add)
  , (SSubtraction,    Sub)
  , (SMultiplication, Mul)
  , (SDivision,       Div)
  , (SExponentiation, Pow)
  , (SLogarithm,      Log)
  ]

instance UserShow ArithOp where
  userShowPrec _ = toText arithOpP

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
  deriving (Show, Eq, Ord)

unaryArithOpP :: Prism' Text UnaryArithOp
unaryArithOpP = mkOpNamePrism
  [ (SNumericNegation,  Negate)
  , (SSquareRoot,       Sqrt)
  , (SNaturalLogarithm, Ln)
  , (SExponential,      Exp)
  , (SAbsoluteValue,    Abs)
  -- explicitly no signum
  ]

instance UserShow UnaryArithOp where
  userShowPrec _ = toText unaryArithOpP

-- decimal -> integer -> decimal
-- decimal -> decimal
data RoundingLikeOp
  = Round   -- ^ Banker's method; reals exactly between two integers are
            -- rounded to the nearest even.
  | Ceiling -- ^ Round to the next integer
  | Floor   -- ^ Round to the previous integer
  deriving (Show, Eq, Ord)

roundingLikeOpP :: Prism' Text RoundingLikeOp
roundingLikeOpP = mkOpNamePrism
  [ (SBankersRound, Round)
  , (SCeilingRound, Ceiling)
  , (SFloorRound,   Floor)
  ]

instance UserShow RoundingLikeOp where
  userShowPrec _ = toText roundingLikeOpP

-- | Arithmetic ops
--
-- We partition the arithmetic operations in to these classes:
-- - DecArithOp, IntArithOp: binary operators applied to (and returning) the
--   same type (either integer or decimal).
--   - Operations: { + - * / ^ log }
-- - DecUnaryArithOp, IntUnaryArithOp: unary operators applied to and
--   returning the same type (integer or decimal).
--   - Operations: { - (negate) sqrt ln exp abs } (also signum even though
--     it's not in pact)
-- - DecIntArithOp, IntDecArithOp: binary operators applied to one integer
--   and one decimal, returning a decimal. These are overloads of the integer
--   / decimal binary ops.
--   - Operations: { + - * / ^ log }
-- - ModOp: Just modulus (oddly, it's the only operator with signature
--   `integer -> integer -> integer`.
--   - Operations: { mod }
--
-- - RoundingLikeOp1: Rounding decimals to integers.
--   - Operations: { round floor ceiling }
-- - RoundingLikeOp2: Rounding decimals to decimals with a specified level of
--   precision.
--   - Operations: { round floor ceiling }
data Numerical t (a :: Ty) where
  DecArithOp      :: ArithOp         -> t 'TyDecimal -> t 'TyDecimal -> Numerical t 'TyDecimal
  IntArithOp      :: ArithOp         -> t 'TyInteger -> t 'TyInteger -> Numerical t 'TyInteger
  DecUnaryArithOp :: UnaryArithOp    -> t 'TyDecimal ->                 Numerical t 'TyDecimal
  IntUnaryArithOp :: UnaryArithOp    -> t 'TyInteger ->                 Numerical t 'TyInteger
  DecIntArithOp   :: ArithOp         -> t 'TyDecimal -> t 'TyInteger -> Numerical t 'TyDecimal
  IntDecArithOp   :: ArithOp         -> t 'TyInteger -> t 'TyDecimal -> Numerical t 'TyDecimal
  ModOp           :: t 'TyInteger    -> t 'TyInteger ->                 Numerical t 'TyInteger
  RoundingLikeOp1 :: RoundingLikeOp  -> t 'TyDecimal ->                 Numerical t 'TyInteger
  RoundingLikeOp2 :: RoundingLikeOp  -> t 'TyDecimal -> t 'TyInteger -> Numerical t 'TyDecimal

instance (UserShow (t 'TyInteger), UserShow (t 'TyDecimal))
  => UserShow (Numerical t a) where
  userShowPrec _ = parenList . \case
    DecArithOp op a b      -> [userShow op, userShow a, userShow b]
    IntArithOp op a b      -> [userShow op, userShow a, userShow b]
    DecUnaryArithOp op a   -> [userShow op, userShow a]
    IntUnaryArithOp op a   -> [userShow op, userShow a]
    DecIntArithOp op a b   -> [userShow op, userShow a, userShow b]
    IntDecArithOp op a b   -> [userShow op, userShow a, userShow b]
    ModOp a b              -> ["mod", userShow a, userShow b]
    RoundingLikeOp1 op a   -> [userShow op, userShow a]
    RoundingLikeOp2 op a b -> [userShow op, userShow a, userShow b]

deriving instance (Eq   (t 'TyDecimal), Eq   (t 'TyInteger)) => Eq   (Numerical t a)
deriving instance (Show (t 'TyDecimal), Show (t 'TyInteger)) => Show (Numerical t a)
