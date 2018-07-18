{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
-- TODO: generate each of these using TH once we are on GHC 8.2.1+, which is
-- when template-haskell learned pattern synonym support. It seems that we can
-- not upgrade template-haskell without upgrading GHC because template-haskell
-- is bundled with GHC.
--

pattern SAddition :: Text
pattern SAddition <- (symIs FAddition -> True)

pattern SSubtraction :: Text
pattern SSubtraction <- (symIs FSubtraction -> True)

pattern SMultiplication :: Text
pattern SMultiplication <- (symIs FMultiplication -> True)

pattern SDivision :: Text
pattern SDivision <- (symIs FDivision -> True)

pattern SExponentiation :: Text
pattern SExponentiation <- (symIs FExponentiation -> True)

pattern SLogarithm :: Text
pattern SLogarithm <- (symIs FLogarithm -> True)

pattern SNegation :: Text
pattern SNegation <- (symIs FNegation -> True)

pattern SSquareRoot :: Text
pattern SSquareRoot <- (symIs FSquareRoot -> True)

pattern SNaturalLogarithm :: Text
pattern SNaturalLogarithm <- (symIs FNaturalLogarithm -> True)

pattern SExponential :: Text
pattern SExponential <- (symIs FExponential -> True)

pattern SAbsoluteValue :: Text
pattern SAbsoluteValue <- (symIs FAbsoluteValue -> True)

pattern SGreaterThan :: Text
pattern SGreaterThan <- (symIs FGreaterThan -> True)

pattern SLessThan :: Text
pattern SLessThan <- (symIs FLessThan -> True)

pattern SGreaterThanOrEqual :: Text
pattern SGreaterThanOrEqual <- (symIs FGreaterThanOrEqual -> True)

pattern SLessThanOrEqual :: Text
pattern SLessThanOrEqual <- (symIs FLessThanOrEqual -> True)

pattern SEquality :: Text
pattern SEquality <- (symIs FEquality -> True)

pattern SInequality :: Text
pattern SInequality <- (symIs FInequality -> True)
