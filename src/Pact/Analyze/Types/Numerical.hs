{-# language FlexibleInstances  #-}
{-# language GADTs              #-}
{-# language StandaloneDeriving #-}
module Pact.Analyze.Types.Numerical where

import Data.SBV          (AlgReal)

-- Pact uses Data.Decimal which is arbitrary-precision
type Decimal = AlgReal

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

deriving instance (Show (t Decimal), Show (t Integer), Show a) => Show (Numerical t a)
deriving instance (Eq (t Decimal), Eq (t Integer), Eq a) => Eq (Numerical t a)
