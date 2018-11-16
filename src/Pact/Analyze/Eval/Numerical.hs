{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Pact.Analyze.Eval.Numerical where

import           Data.Coerce             (Coercible)
import           Data.SBV                (EqSymbolic ((.==)), SymWord, sDiv,
                                          sMod, (.<))

import           Pact.Analyze.Errors
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval

-- When doing binary arithmetic involving:
-- * decimal x decimal
-- * integer x decimal
-- * decimal x integer
--
-- We widen / downcast the integer to be a decimal. This just requires shifting
-- the number left 255 decimal digits in the case of an integer.
class DecimalRepresentable a where
  widenToDecimal :: S a -> S Decimal

instance DecimalRepresentable Integer where
  widenToDecimal = lShift255D . coerceS

instance DecimalRepresentable Decimal where
  widenToDecimal = id

-- This module handles evaluation of unary and binary integers and decimals.
-- Two tricky details to watch out for:
--
-- 1. See Note [Rounding]
--
-- 2. Errors. Division can fail if the denominator is 0. Rounding can fail if
--    the precision is negative. In either case we mark failure via
--    @markFailure@. Thus our codomain is extended with failure.
--
-- Note [Rounding]
--
-- The key is this quote from "Data.Decimal":
--
-- "Decimal numbers are represented as m*10^(-e) where m and e are integers. The
-- exponent e is an unsigned Word8. Hence the smallest value that can be
-- represented is 10^-255."

evalNumerical
  :: Analyzer m
  => Numerical (TermOf m) a -> m (S a)
evalNumerical (IntArithOp op x y)      = evalIntArithOp op x y
evalNumerical (DecArithOp op x y)      = evalDecArithOp op x y
evalNumerical (IntDecArithOp op x y)   = evalDecArithOp op x y
evalNumerical (DecIntArithOp op x y)   = evalDecArithOp op x y
evalNumerical (IntUnaryArithOp op x)   = evalUnaryArithOp op x
evalNumerical (DecUnaryArithOp op x)   = evalUnaryArithOp op x
evalNumerical (ModOp x y)              = evalModOp x y
evalNumerical (RoundingLikeOp1 op x)   = evalRoundingLikeOp1 op x
evalNumerical (RoundingLikeOp2 op x p) = evalRoundingLikeOp2 op x p

-- In principle this could share an implementation with evalDecArithOp. In
-- practice, evaluation can be slower because you're multiplying both inputs by
-- (10 ^ 255), so we stick to this more efficient implementation.
evalIntArithOp
  :: Analyzer m
  => ArithOp
  -> TermOf m Integer
  -> TermOf m Integer
  -> m (S Integer)
evalIntArithOp op xT yT = do
  x <- eval xT
  y <- eval yT
  case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> do
      markFailure $ y .== 0
      pure $ x `sDiv` y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

evalDecArithOp
  :: ( Analyzer m
     , DecimalRepresentable a, Show a, SymWord a
     , DecimalRepresentable b, Show b, SymWord b
     )
  => ArithOp
  -> TermOf m a
  -> TermOf m b
  -> m (S Decimal)
evalDecArithOp op xT yT = do
  x <- widenToDecimal <$> eval xT
  y <- widenToDecimal <$> eval yT
  coerceS <$> case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> do
      markFailure $ y .== 0
      pure $ x / y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

-- In practice (a ~ Decimal) or (a ~ Integer).
evalUnaryArithOp
  :: forall m a
   . (Analyzer m, Show a, SymWord a, Coercible a Integer)
  => UnaryArithOp
  -> TermOf m a
  -> m (S a)
evalUnaryArithOp op term = do
  x <- coerceS @a @Integer <$> eval term
  coerceS @Integer @a <$> case op of
    Negate -> pure $ negate x
    Sqrt   -> throwErrorNoLoc $ UnsupportedUnaryOp op
    Ln     -> throwErrorNoLoc $ UnsupportedUnaryOp op
    Exp    -> throwErrorNoLoc $ UnsupportedUnaryOp op -- TODO: use svExp
    Abs    -> pure $ abs x
    Signum -> pure $ signum x

evalModOp
  :: Analyzer m
  => TermOf m Integer
  -> TermOf m Integer
  -> m (S Integer)
evalModOp xT yT = do
  x <- eval xT
  y <- eval yT
  markFailure $ y .== 0
  pure $ sMod x y

evalRoundingLikeOp1
  :: Analyzer m
  => RoundingLikeOp
  -> TermOf m Decimal
  -> m (S Integer)
evalRoundingLikeOp1 op x = do
  x' <- eval x
  pure $ case op of
    Floor   -> floorD x'

    -- For ceiling we use the identity:
    -- ceil(x) = -floor(-x)
    Ceiling -> negate (floorD (negate x'))

    -- Round is much more complicated because pact uses the banker's method,
    -- where a real exactly between two integers (_.5) is rounded to the
    -- nearest even.
    Round   -> banker'sMethodS x'

-- In the decimal rounding operations we shift the number left by `precision`
-- digits, round using the integer method, and shift back right.
--
-- x: SReal            := -100.15234
-- precision: SInteger := 2
-- return: SReal       := -100.15
evalRoundingLikeOp2
  :: forall m
   . Analyzer m
  => RoundingLikeOp
  -> TermOf m Decimal
  -> TermOf m Integer
  -> m (S Decimal)
evalRoundingLikeOp2 op xT precisionT = do
  x         <- eval xT
  precision <- eval precisionT
  -- Precision must be >= 0
  markFailure (precision .< 0)
  rShiftD' precision . fromInteger' <$>
    evalRoundingLikeOp1 op (inject (lShiftD' precision x))

-- Round a real exactly between two integers (_.5) to the nearest even
banker'sMethodS :: S Decimal -> S Integer
banker'sMethodS (S prov x) = S prov $ banker'sMethod x
