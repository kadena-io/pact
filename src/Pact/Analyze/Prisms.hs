{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE ViewPatterns      #-}

module Pact.Analyze.Prisms where

import           Control.Lens         (Prism')
import           Data.Text            (Text)

import qualified Pact.Types.Persistence as Pact

import           Pact.Analyze.Feature
import           Pact.Analyze.Types.Numerical
import           Pact.Analyze.Types.Shared    hiding (Object)


arithOpP :: Prism' Text ArithOp
arithOpP = mkOpNamePrism
  [ (SAddition,       Add)
  , (SSubtraction,    Sub)
  , (SMultiplication, Mul)
  , (SDivision,       Div)
  , (SExponentiation, Pow)
  , (SLogarithm,      Log)
  ]

unaryArithOpP :: Prism' Text UnaryArithOp
unaryArithOpP = mkOpNamePrism
  [ (SNumericNegation,  Negate)
  , (SSquareRoot,       Sqrt)
  , (SNaturalLogarithm, Ln)
  , (SExponential,      Exp)
  , (SAbsoluteValue,    Abs)
  -- explicitly no signum
  ]

comparisonOpP :: Prism' Text ComparisonOp
comparisonOpP = mkOpNamePrism
  [ (SGreaterThan,        Gt)
  , (SLessThan,           Lt)
  , (SGreaterThanOrEqual, Gte)
  , (SLessThanOrEqual,    Lte)
  , (SEquality,           Eq)
  , (SInequality,         Neq)
  ]

roundingLikeOpP :: Prism' Text RoundingLikeOp
roundingLikeOpP = mkOpNamePrism
  [ (SBankersRound, Round)
  , (SCeilingRound, Ceiling)
  , (SFloorRound,   Floor)
  ]

logicalOpP :: Prism' Text LogicalOp
logicalOpP = mkOpNamePrism
  [ (SLogicalConjunction, AndOp)
  , (SLogicalDisjunction, OrOp)
  , (SLogicalNegation,    NotOp)
  -- NOTE: that we don't include logical implication here, which only exists in
  -- the invariant and property languages (not term), and is desugared to a
  -- combination of negation and disjunction during parsing.
  ]

eqNeqP :: Prism' Text EqNeq
eqNeqP = mkOpNamePrism
  [ (SEquality,   Eq')
  , (SInequality, Neq')
  ]

-- NOTE: we don't (yet?) use symbols here because Feature (currently?) only
-- handles properties and invariants.
writeTypeP :: Prism' Text Pact.WriteType
writeTypeP = mkOpNamePrism
  [ ("insert", Pact.Insert)
  , ("update", Pact.Update)
  , ("write",  Pact.Write)
  ]
