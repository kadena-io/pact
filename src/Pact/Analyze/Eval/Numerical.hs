{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Pact.Analyze.Eval.Numerical where

import           Control.Lens            (over)
import           Data.SBV                (EqSymbolic ((.==)), SymWord, sDiv,
                                          sMod, (.^))

import           Pact.Analyze.Errors
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Types

evalNumerical
  :: (Analyzer m, S :<: TermOf m)
  => Numerical (TermOf m) a -> m (S a)
evalNumerical (DecArithOp op x y)      = evalDecArithOp op x y
evalNumerical (IntArithOp op x y)      = evalIntArithOp op x y
evalNumerical (IntDecArithOp op x y)   = evalIntDecArithOp op x y
evalNumerical (DecIntArithOp op x y)   = evalDecIntArithOp op x y
evalNumerical (IntUnaryArithOp op x)   = evalUnaryArithOp op x
evalNumerical (DecUnaryArithOp op x)   = evalUnaryArithOp op x
evalNumerical (ModOp x y)              = evalModOp x y
evalNumerical (RoundingLikeOp1 op x)   = evalRoundingLikeOp1 op x
evalNumerical (RoundingLikeOp2 op x p) = evalRoundingLikeOp2 op x p

-- | Implementation of decimal arithmetic.
--
-- The key is this quote from "Data.Decimal":
--
-- "Decimal numbers are represented as m*10^(-e) where m and e are integers. The
-- exponent e is an unsigned Word8. Hence the smallest value that can be
-- represented is 10^-255."
--
-- Thus in this implementation, after any decimal arithmetic operation, we
-- shift left 255 decimal digits, drop the remainder, then shift back right.
dropRemainder :: S Decimal -> S Decimal
dropRemainder =
  let digitShift = 10 ^ (255 :: Int)
  in (/ digitShift) . fromIntegralS . decRound . (* digitShift)

-- round decimals by taking the floor of `x + 0.5`
decRound :: S Decimal -> S Integer
decRound = realToIntegerS . (+ 0.5)

evalDecArithOp
  :: Analyzer m
  => ArithOp
  -> TermOf m Decimal
  -> TermOf m Decimal
  -> m (S Decimal)
evalDecArithOp op xT yT = do
  x <- eval xT
  y <- eval yT
  case op of
    Add -> pure $ dropRemainder $ x + y
    Sub -> pure $ dropRemainder $ x - y
    Mul -> pure $ dropRemainder $ x * y
    Div -> pure $ dropRemainder $ x / y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

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
    Div -> pure $ x `sDiv` y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

evalIntDecArithOp
  :: Analyzer m
  => ArithOp
  -> TermOf m Integer
  -> TermOf m Decimal
  -> m (S Decimal)
evalIntDecArithOp op xT yT = do
  x <- eval xT
  y <- eval yT
  case op of
    Add -> pure $ dropRemainder $ fromIntegralS x + y
    Sub -> pure $ dropRemainder $ fromIntegralS x - y
    Mul -> pure $ dropRemainder $ fromIntegralS x * y
    Div -> pure $ dropRemainder $ fromIntegralS x / y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

evalDecIntArithOp
  :: Analyzer m
  => ArithOp
  -> TermOf m Decimal
  -> TermOf m Integer
  -> m (S Decimal)
evalDecIntArithOp op xT yT = do
  x <- eval xT
  y <- eval yT
  case op of
    Add -> pure $ dropRemainder $ x + fromIntegralS y
    Sub -> pure $ dropRemainder $ x - fromIntegralS y
    Mul -> pure $ dropRemainder $ x * fromIntegralS y
    Div -> pure $ dropRemainder $ x / fromIntegralS y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

evalUnaryArithOp
  :: (Analyzer m, Num a, Show a, SymWord a)
  => UnaryArithOp
  -> TermOf m a
  -> m (S a)
evalUnaryArithOp op term = do
  x <- eval term
  case op of
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
evalModOp xT yT = sMod <$> eval xT <*> eval yT

-- Round a real exactly between two integers (_.5) to the nearest even
banker'sMethod :: S Decimal -> S Integer
banker'sMethod x =
  let wholePart      = realToIntegerS x
      wholePartIsOdd = sansProv $ wholePart `sMod` 2 .== 1
      isExactlyHalf  = sansProv $ fromIntegralS wholePart + 1 / 2 .== x

  in iteS isExactlyHalf
    -- nearest even number!
    (wholePart + oneIfS wholePartIsOdd)
    -- otherwise we round
    (decRound x)

evalRoundingLikeOp1
  :: Analyzer m
  => RoundingLikeOp
  -> TermOf m Decimal
  -> m (S Integer)
evalRoundingLikeOp1 op x = do
  x' <- eval x
  pure $ case op of
    -- The only SReal -> SInteger conversion function that sbv provides is
    -- sRealToSInteger (which realToIntegerS wraps), which computes the floor.
    Floor   -> realToIntegerS x'

    -- For ceiling we use the identity:
    -- ceil(x) = -floor(-x)
    Ceiling -> negate (realToIntegerS (negate x'))

    -- Round is much more complicated because pact uses the banker's method,
    -- where a real exactly between two integers (_.5) is rounded to the
    -- nearest even.
    Round   -> banker'sMethod x'

-- In the decimal rounding operations we shift the number left by `precision`
-- digits, round using the integer method, and shift back right.
--
-- x': SReal            := -100.15234
-- precision': SInteger := 2
-- x'': SReal           := -10015.234
-- x''': SInteger       := -10015
-- return: SReal        := -100.15
evalRoundingLikeOp2
  :: forall m
   . (Analyzer m, S :<: TermOf m)
  => RoundingLikeOp
  -> TermOf m Decimal
  -> TermOf m Integer
  -> m (S Decimal)
evalRoundingLikeOp2 op x precision = do
  x'         <- eval x
  precision' <- eval precision
  let digitShift = over s2Sbv (10 .^) precision' :: S Integer
      x''        = x' * fromIntegralS digitShift
  x''' <- evalRoundingLikeOp1 op (inject x'' :: TermOf m Decimal)
  pure $ fromIntegralS x''' / fromIntegralS digitShift
