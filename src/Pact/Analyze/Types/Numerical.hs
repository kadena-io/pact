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
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- coerceSBV requires Coercible
module Pact.Analyze.Types.Numerical where

import           Control.Lens                (Prism')
import           Data.Coerce                 (Coercible)
import qualified Data.Decimal                as Decimal
import           Data.SBV                    (HasKind (kindOf),
                                              Kind (KUnbounded),
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

import           Pact.Analyze.Feature        hiding (dec)
import           Pact.Analyze.Types.UserShow


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

instance HasKind Decimal where kindOf _ = KUnbounded
instance SymWord Decimal where
  mkSymWord  = genMkSymVar KUnbounded
  literal a  = SBV . SVal KUnbounded . Left . normCW $ CW KUnbounded (CWInteger (unDecimal a))
  fromCW (CW _ (CWInteger x)) = Decimal x
  fromCW x = error $ "in instance SymWord Decimal: expected CWInteger, found: " ++ show x

instance SMTValue Decimal where sexprToVal = fmap Decimal . sexprToVal

instance UserShow Decimal where
  userShowsPrec _ (Decimal dec) =
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
  userShowsPrec _ = toText arithOpP

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
  userShowsPrec _ = toText unaryArithOpP

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
  userShowsPrec _ = toText roundingLikeOpP

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
data Numerical t a where
  DecArithOp      :: ArithOp        -> t Decimal -> t Decimal -> Numerical t Decimal
  IntArithOp      :: ArithOp        -> t Integer -> t Integer -> Numerical t Integer
  DecUnaryArithOp :: UnaryArithOp   -> t Decimal ->              Numerical t Decimal
  IntUnaryArithOp :: UnaryArithOp   -> t Integer ->              Numerical t Integer
  DecIntArithOp   :: ArithOp        -> t Decimal -> t Integer -> Numerical t Decimal
  IntDecArithOp   :: ArithOp        -> t Integer -> t Decimal -> Numerical t Decimal
  ModOp           :: t Integer      -> t Integer ->              Numerical t Integer
  RoundingLikeOp1 :: RoundingLikeOp -> t Decimal ->              Numerical t Integer
  RoundingLikeOp2 :: RoundingLikeOp -> t Decimal -> t Integer -> Numerical t Decimal

instance (UserShow (t Integer), UserShow (t Decimal))
  => UserShow (Numerical t a) where
  userShowsPrec _ = parenList . \case
    DecArithOp op a b      -> [userShow op, userShow a, userShow b]
    IntArithOp op a b      -> [userShow op, userShow a, userShow b]
    DecUnaryArithOp op a   -> [userShow op, userShow a]
    IntUnaryArithOp op a   -> [userShow op, userShow a]
    DecIntArithOp op a b   -> [userShow op, userShow a, userShow b]
    IntDecArithOp op a b   -> [userShow op, userShow a, userShow b]
    ModOp a b              -> ["mod", userShow a, userShow b]
    RoundingLikeOp1 op a   -> [userShow op, userShow a]
    RoundingLikeOp2 op a b -> [userShow op, userShow a, userShow b]

deriving instance (Show (t Decimal), Show (t Integer), Show a) => Show (Numerical t a)
deriving instance (Eq (t Decimal), Eq (t Integer), Eq a) => Eq (Numerical t a)
