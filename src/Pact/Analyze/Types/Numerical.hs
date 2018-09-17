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
                                              oneIf, (.>=), (.^))
import           Data.SBV.Control            (SMTValue (sexprToVal))
import           Data.SBV.Dynamic            (svPlus, svTimes, svUNeg)
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

-- Note [SymbolicDecimal]:
--
-- Decimal used to be an instance of Num, and it almost still could be. The
-- only problem is that sbv automatically builds an instance `instance (Ord a,
-- Num a, SymWord a) => Num (SBV a)`, which ignores your own `Num` instance. In
-- our case we need to do shifting aound multiplications and divisions so the
-- default instance doesn't work.
class SymbolicDecimal d where
  type IntegerOf d :: *
  negateD       ::                d -> d
  (.+)          ::           d -> d -> d
  (.-)          ::           d -> d -> d
  (.*)          ::           d -> d -> d
  (./)          ::           d -> d -> d
  fromIntegerD  :: Integer          -> d
  fromIntegerD' :: IntegerOf d      -> d
  literalD      :: Decimal.Decimal  -> d
  lShiftD       :: Int         -> d -> d
  lShiftD'      :: IntegerOf d -> d -> d
  rShiftD       :: Int         -> d -> d
  rShiftD'      :: IntegerOf d -> d -> d
  floorD        ::      d -> IntegerOf d

instance SymbolicDecimal Decimal where
  type IntegerOf Decimal = Integer
  negateD (Decimal d)    = Decimal (negate d)
  Decimal a .+ Decimal b = Decimal (a + b)
  Decimal a .- Decimal b = Decimal (a - b)
  Decimal a .* Decimal b = rShift255D (Decimal (a * b))
  a         ./ Decimal b =
    -- first make the numerator 10^255 times larger
    let Decimal a'     = lShift255D a
        -- now get the quotient and remainder
        (divAb, modAb) = divMod a' b
        -- if the remainder is more than half of the divisor, round up
        adjustment     = if modAb * 2 >= b then 1 else 0
    in Decimal $ divAb + adjustment
  fromIntegerD i        = lShift255D (Decimal i)
  fromIntegerD'         = fromIntegerD
  literalD (Decimal.Decimal places mantissa)
    = lShiftD (decimalPrecision - fromIntegral places) (Decimal mantissa)
  lShiftD n (Decimal d) = Decimal (d * 10 ^ n)
  lShiftD' n            = lShiftD (fromIntegral n)
  rShiftD n (Decimal d) = Decimal (d `div` 10 ^ n)
  rShiftD' n            = rShiftD (fromIntegral n)
  floorD                = unDecimal . rShift255D

instance SymbolicDecimal (SBV Decimal) where
  type IntegerOf (SBV Decimal) = SBV Integer
  negateD (SBVI.SBV a)         = SBVI.SBV (svUNeg a)
  SBVI.SBV a .+ SBVI.SBV b     = SBVI.SBV (svPlus a b)
  SBVI.SBV a .- SBVI.SBV b     = SBVI.SBV (svPlus a (svUNeg b))
  SBVI.SBV a .* SBVI.SBV b     = rShift255D (SBVI.SBV (svTimes a b))

  a ./ b =
    let (divAb, modAb) = sDivMod
          (coerceSBV @_ @Integer (lShift255D a))
          (coerceSBV @_ @Integer b)
        adjustment = oneIf $ coerceSBV modAb * 2 .>= coerceSBV @_ @Integer b
    in coerceSBV $ divAb + adjustment

  fromIntegerD  = literal . fromIntegerD
  fromIntegerD' = lShift255D . coerceSBV
  literalD      = literal . literalD

  lShiftD  n = coerceSBV . (*       10  ^ n)  . coerceSBV @Decimal @Integer
  lShiftD' n = coerceSBV . (*       10 .^ n)  . coerceSBV @Decimal @Integer
  rShiftD  n = coerceSBV . (`sDiv` (10  ^ n)) . coerceSBV @Decimal @Integer
  rShiftD' n = coerceSBV . (`sDiv` (10 .^ n)) . coerceSBV @Decimal @Integer
  floorD     = coerceSBV . rShift255D

decimalPrecision :: Int
decimalPrecision = 255

lShift255D :: SymbolicDecimal d => d -> d
lShift255D = lShiftD decimalPrecision

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
