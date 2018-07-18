{-# LANGUAGE CPP               #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Features, availability, and documentation
module Pact.Analyze.Feature where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

data Feature
  = FAddition
  | FSubtraction
  | FMultiplication
  | FDivision
  | FExponentiation
  | FLogarithm
  | FNegation
  | FSquareRoot
  | FNaturalLogarithm
  | FExponential
  | FAbsoluteValue
  | FGreaterThan
  | FLessThan
  | FGreaterThanOrEqual
  | FLessThanOrEqual
  | FEquality
  | FInequality
  | FRound
  deriving (Eq, Ord, Show, Bounded, Enum)

data Availability
  = InvOnly
  | PropOnly
  | InvAndProp
  deriving (Eq, Ord, Show)

data Usage
  = Usage { _usageTemplate :: Text
          , _usageRetType  :: Text
          , _usageArgTypes :: [(Text, Text)]
          }
  deriving (Show)

data Doc
  = Doc { _docSymbol       :: Text
        , _docAvailability :: Availability
        , _docDescription  :: Text
        , _docUsages       :: [Usage]
        }
  deriving (Show)

symbol :: Feature -> Text
symbol = _docSymbol . doc

availability :: Feature -> Availability
availability = _docAvailability . doc

int, dec, str, time, bool, obj, ks :: Text
int  = "integer"
dec  = "decimal"
str  = "string"
time = "time"
bool = "bool"
obj  = "object"
ks   = "keyset"

doc :: Feature -> Doc
doc FAddition = Doc
  "+"
  InvAndProp
  "Addition of integers and decimals."
  [ Usage "(+ x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec] ]
doc FSubtraction = Doc
  "-"
  InvAndProp
  "Subtraction of integers and decimals."
  [ Usage "(- x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec] ]
doc FMultiplication = Doc
  "*"
  InvAndProp
  "Multiplication of integers and decimals."
  [ Usage "(* x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec] ]
doc FDivision = Doc
  "/"
  InvAndProp
  "Division of integers and decimals."
  [ Usage "(/ x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec] ]
doc FExponentiation = Doc
  "^"
  InvAndProp
  "Exponentiation of integers and decimals."
  [ Usage "(^ x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec] ]
doc FLogarithm = Doc
  "log"
  InvAndProp
  "Logarithm of `x` base `b`."
  [ Usage "(log b x)" ty [("b", ty), ("x", ty)]
  | ty <- [int, dec] ]
doc FNegation = Doc
  "-"
  InvAndProp
  "Negation of integers and decimals."
  [ Usage "(- x)" ty [("x", ty)]
  | ty <- [int, dec] ]
doc FSquareRoot = Doc
  "sqrt"
  InvAndProp
  "Square root of integers and decimals."
  [ Usage "(sqrt x)" ty [("x", ty)]
  | ty <- [int, dec] ]
doc FNaturalLogarithm = Doc
  "ln"
  InvAndProp
  "Logarithm of integers and decimals base e."
  [ Usage "(ln x)" ty [("x", ty)]
  | ty <- [int, dec] ]
doc FExponential = Doc
  "exp"
  InvAndProp
  "Exponential of integers and decimals. e raised to the integer or decimal `x`."
  [ Usage "(exp x)" ty [("x", ty)]
  | ty <- [int, dec] ]
doc FAbsoluteValue = Doc
  "abs"
  InvAndProp
  "Absolute value of integers and decimals."
  [ Usage "(abs x)" ty [("x", ty)]
  | ty <- [int, dec] ]
doc FGreaterThan = Doc
  ">"
  InvAndProp
  "True if `x` > `y`"
  [ Usage "(> x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time] ]
doc FLessThan = Doc
  "<"
  InvAndProp
  "True if `x` < `y`"
  [ Usage "(< x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time] ]
doc FGreaterThanOrEqual = Doc
  ">="
  InvAndProp
  "True if `x` >= `y`"
  [ Usage "(>= x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time] ]
doc FLessThanOrEqual = Doc
  "<="
  InvAndProp
  "True if `x` <= `y`"
  [ Usage "(<= x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time] ]
doc FEquality = Doc
  "="
  InvAndProp
  "True if `x` = `y`"
  [ Usage "(= x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time, bool, obj, ks] ]
doc FInequality = Doc
  "!="
  InvAndProp
  "True if `x` != `y`"
  [ Usage "(!= x y)" ty [("x", ty), ("y", ty)]
  | ty <- [int, dec, str, time, bool, obj, ks] ]
doc FRound = Doc
  "round"
  InvAndProp
  "Banker's rounding value of decimal `x` as integer, or to `prec` precision as decimal."
  [ Usage "(round x)" int [("x", dec)]
  , Usage "(round x)" int [("x", dec), ("prec", int)] ]

allFeatures :: [Feature]
allFeatures = enumFrom minBound

featuresBy :: Ord a => (Feature -> a) -> Map a (Set Feature)
featuresBy f = foldl'
  (\acc feat ->
    Map.insertWith Set.union (f feat) (Set.singleton feat) acc)
  Map.empty
  allFeatures

availableFeatures :: Map Availability (Set Feature)
availableFeatures = featuresBy availability

symbolFeatures :: Map Text (Set Feature)
symbolFeatures = featuresBy symbol

-- * Pattern synonyms for matching on symbol names

symIs :: Feature -> Text -> Bool
symIs feat sym = symbol feat == sym

--
-- TODO: generate these using TH once we are on GHC 8.2.1+, which is when
-- template-haskell learned pattern synonym support. It seems that we can not
-- upgrade template-haskell without upgrading GHC because template-haskell is
-- bundled with GHC.
--

#define PAT(sfeat, ffeat) pattern sfeat :: Text ; pattern sfeat <- (symIs ffeat -> True)

PAT(SAddition, FAddition)
PAT(SSubtraction, FSubtraction)
PAT(SMultiplication, FMultiplication)
PAT(SDivision, FDivision)
PAT(SExponentiation, FExponentiation)
PAT(SLogarithm, FLogarithm)
PAT(SNegation, FNegation)
PAT(SSquareRoot, FSquareRoot)
PAT(SNaturalLogarithm, FNaturalLogarithm)
PAT(SExponential, FExponential)
PAT(SAbsoluteValue, FAbsoluteValue)
PAT(SGreaterThan, FGreaterThan)
PAT(SLessThan, FLessThan)
PAT(SGreaterThanOrEqual, FGreaterThanOrEqual)
PAT(SLessThanOrEqual, FLessThanOrEqual)
PAT(SEquality, FEquality)
PAT(SInequality, FInequality)
PAT(SRound, FRound)
