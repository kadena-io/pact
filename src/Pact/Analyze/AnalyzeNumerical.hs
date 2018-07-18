{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Pact.Analyze.AnalyzeNumerical where

import           Control.Lens               (over)
import           Control.Monad.Except       (MonadError)
import           Data.SBV                   (EqSymbolic ((.==)), SymWord,
                                             sDiv, sMod, (.^))

import           Pact.Analyze.Errors
import           Pact.Analyze.Numerical
import           Pact.Analyze.Term
import           Pact.Analyze.Types         hiding (tableName)

class (MonadError AnalyzeFailure m) => Analyzer m term | m -> term where
  analyze         :: (Show a, SymWord a) => term a -> m (S a)
  throwErrorNoLoc :: AnalyzeFailureNoLoc -> m a

-- TODO: replace with InjectPure
class SymbolicTerm term where
  injectS :: S a -> term a

instance SymbolicTerm Term      where injectS = PureTerm      . Sym
instance SymbolicTerm Prop      where injectS = PureProp      . Sym
instance SymbolicTerm Invariant where injectS = PureInvariant . Sym

analyzeNumerical
  :: (Analyzer m term, SymbolicTerm term)
  => Numerical term a -> m (S a)
analyzeNumerical (DecArithOp op x y)      = analyzeDecArithOp op x y
analyzeNumerical (IntArithOp op x y)      = analyzeIntArithOp op x y
analyzeNumerical (IntDecArithOp op x y)   = analyzeIntDecArithOp op x y
analyzeNumerical (DecIntArithOp op x y)   = analyzeDecIntArithOp op x y
analyzeNumerical (IntUnaryArithOp op x)   = analyzeUnaryArithOp op x
analyzeNumerical (DecUnaryArithOp op x)   = analyzeUnaryArithOp op x
analyzeNumerical (ModOp x y)              = analyzeModOp x y
analyzeNumerical (RoundingLikeOp1 op x)   = analyzeRoundingLikeOp1 op x
analyzeNumerical (RoundingLikeOp2 op x p) = analyzeRoundingLikeOp2 op x p

analyzeDecArithOp
  :: Analyzer m term
  => ArithOp
  -> term Decimal
  -> term Decimal
  -> m (S Decimal)
analyzeDecArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> pure $ x / y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

analyzeIntArithOp
  :: Analyzer m term
  => ArithOp
  -> term Integer
  -> term Integer
  -> m (S Integer)
analyzeIntArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> pure $ x `sDiv` y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

analyzeIntDecArithOp
  :: Analyzer m term
  => ArithOp
  -> term Integer
  -> term Decimal
  -> m (S Decimal)
analyzeIntDecArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ fromIntegralS x + y
    Sub -> pure $ fromIntegralS x - y
    Mul -> pure $ fromIntegralS x * y
    Div -> pure $ fromIntegralS x / y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

analyzeDecIntArithOp
  :: Analyzer m term
  => ArithOp
  -> term Decimal
  -> term Integer
  -> m (S Decimal)
analyzeDecIntArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + fromIntegralS y
    Sub -> pure $ x - fromIntegralS y
    Mul -> pure $ x * fromIntegralS y
    Div -> pure $ x / fromIntegralS y
    Pow -> throwErrorNoLoc $ UnsupportedDecArithOp op
    Log -> throwErrorNoLoc $ UnsupportedDecArithOp op

analyzeUnaryArithOp
  :: (Analyzer m term, Num a, Show a, SymWord a)
  => UnaryArithOp
  -> term a
  -> m (S a)
analyzeUnaryArithOp op term = do
  x <- analyze term
  case op of
    Negate -> pure $ negate x
    Sqrt   -> throwErrorNoLoc $ UnsupportedUnaryOp op
    Ln     -> throwErrorNoLoc $ UnsupportedUnaryOp op
    Exp    -> throwErrorNoLoc $ UnsupportedUnaryOp op -- TODO: use svExp
    Abs    -> pure $ abs x
    Signum -> pure $ signum x

analyzeModOp
  :: (Analyzer m term)
  => term Integer
  -> term Integer
  -> m (S Integer)
analyzeModOp xT yT = sMod <$> analyze xT <*> analyze yT

analyzeRoundingLikeOp1
  :: (Analyzer m term)
  => RoundingLikeOp
  -> term Decimal
  -> m (S Integer)
analyzeRoundingLikeOp1 op x = do
  x' <- analyze x
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

-- Round a real exactly between two integers (_.5) to the nearest even
banker'sMethod :: S Decimal -> S Integer
banker'sMethod x =
  let wholePart      = realToIntegerS x
      wholePartIsOdd = sansProv $ wholePart `sMod` 2 .== 1
      isExactlyHalf  = sansProv $ fromIntegralS wholePart + 1 / 2 .== x

  in iteS isExactlyHalf
    -- nearest even number!
    (wholePart + oneIfS wholePartIsOdd)
    -- otherwise we take the floor of `x + 0.5`
    (realToIntegerS (x + 0.5))

-- In the decimal rounding operations we shift the number left by `precision`
-- digits, round using the integer method, and shift back right.
--
-- x': SReal            := -100.15234
-- precision': SInteger := 2
-- x'': SReal           := -10015.234
-- x''': SInteger       := -10015
-- return: SReal        := -100.15
analyzeRoundingLikeOp2
  :: forall m term
   . (Analyzer m term, SymbolicTerm term)
  => RoundingLikeOp
  -> term Decimal
  -> term Integer
  -> m (S Decimal)
analyzeRoundingLikeOp2 op x precision = do
  x'         <- analyze x
  precision' <- analyze precision
  let digitShift = over s2Sbv (10 .^) precision' :: S Integer
      x''        = x' * fromIntegralS digitShift
  x''' <- analyzeRoundingLikeOp1 op (injectS x'' :: term Decimal)
  pure $ fromIntegralS x''' / fromIntegralS digitShift
