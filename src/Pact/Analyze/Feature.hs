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

doc :: Feature -> Doc
doc FAddition = Doc
  "+"
  InvAndProp
  "Addition of integers and decimals."
  [ Usage "(+ x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(+ x y)" "decimal" [("x", "decimal"), ("y", "decimal")]]
doc FSubtraction = Doc
  "-"
  InvAndProp
  "Subtraction of integers and decimals."
  [ Usage "(- x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(- x y)" "decimal" [("x", "decimal"), ("y", "decimal")]]
doc FMultiplication = Doc
  "*"
  InvAndProp
  "Multiplication of integers and decimals."
  [ Usage "(* x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(* x y)" "decimal" [("x", "decimal"), ("y", "decimal")]]
doc FDivision = Doc
  "/"
  InvAndProp
  "Division of integers and decimals."
  [ Usage "(/ x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(/ x y)" "decimal" [("x", "decimal"), ("y", "decimal")]]
doc FExponentiation = Doc
  "^"
  InvAndProp
  "Exponentiation of integers and decimals."
  [ Usage "(^ x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(^ x y)" "decimal" [("x", "decimal"), ("y", "decimal")]]
doc FLogarithm = Doc
  "log"
  InvAndProp
  "Logarithm of `x` base `b`."
  [ Usage "(log b x)" "integer" [("b", "integer"), ("x", "integer")]
  , Usage "(log b x)" "decimal" [("b", "decimal"), ("x", "decimal")]]
doc FNegation = Doc
  "-"
  InvAndProp
  "Negation of integers and decimals."
  [ Usage "(- x)" "integer" [("x", "integer")]
  , Usage "(- x)" "decimal" [("x", "decimal")]]
doc FSquareRoot = Doc
  "sqrt"
  InvAndProp
  "Square root of integers and decimals."
  [ Usage "(sqrt x)" "integer" [("x", "integer")]
  , Usage "(sqrt x)" "decimal" [("x", "decimal")]]
doc FNaturalLogarithm = Doc
  "ln"
  InvAndProp
  "Logarithm of integers and decimals base e."
  [ Usage "(ln x)" "integer" [("x", "integer")]
  , Usage "(ln x)" "decimal" [("x", "decimal")]]
doc FExponential = Doc
  "exp"
  InvAndProp
  "Exponential of integers and decimals. e raised to the integer or decimal `x`."
  [ Usage "(exp x)" "integer" [("x", "integer")]
  , Usage "(exp x)" "decimal" [("x", "decimal")]]
doc FAbsoluteValue = Doc
  "abs"
  InvAndProp
  "Absolute value of integers and decimals."
  [ Usage "(abs x)" "integer" [("x", "integer")]
  , Usage "(abs x)" "decimal" [("x", "decimal")]]
doc FGreaterThan = Doc
  ">"
  InvAndProp
  "True if `x` > `y`"
  [ Usage "(> x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(> x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(> x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(> x y)" "time"    [("x", "time"),    ("y", "time")]]
doc FLessThan = Doc
  "<"
  InvAndProp
  "True if `x` < `y`"
  [ Usage "(< x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(< x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(< x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(< x y)" "time"    [("x", "time"),    ("y", "time")]]
doc FGreaterThanOrEqual = Doc
  ">="
  InvAndProp
  "True if `x` >= `y`"
  [ Usage "(>= x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(>= x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(>= x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(>= x y)" "time"    [("x", "time"),    ("y", "time")]]
doc FLessThanOrEqual = Doc
  "<="
  InvAndProp
  "True if `x` <= `y`"
  [ Usage "(<= x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(<= x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(<= x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(<= x y)" "time"    [("x", "time"),    ("y", "time")]]
doc FEquality = Doc
  "="
  InvAndProp
  "True if `x` = `y`"
  [ Usage "(= x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(= x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(= x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(= x y)" "time"    [("x", "time"),    ("y", "time")]
  , Usage "(= x y)" "bool"    [("x", "bool"),    ("y", "bool")]
  , Usage "(= x y)" "object"  [("x", "object"),  ("y", "object")]
  , Usage "(= x y)" "keyset"  [("x", "keyset"),  ("y", "keyset")]]
doc FInequality = Doc
  "!="
  InvAndProp
  "True if `x` != `y`"
  [ Usage "(!= x y)" "integer" [("x", "integer"), ("y", "integer")]
  , Usage "(!= x y)" "decimal" [("x", "decimal"), ("y", "decimal")]
  , Usage "(!= x y)" "string"  [("x", "string"),  ("y", "string")]
  , Usage "(!= x y)" "time"    [("x", "time"),    ("y", "time")]
  , Usage "(!= x y)" "bool"    [("x", "bool"),    ("y", "bool")]
  , Usage "(!= x y)" "object"  [("x", "object"),  ("y", "object")]
  , Usage "(!= x y)" "keyset"  [("x", "keyset"),  ("y", "keyset")]]


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
