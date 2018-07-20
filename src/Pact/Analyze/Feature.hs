{-# LANGUAGE CPP                #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Features, availability, and documentation
module Pact.Analyze.Feature where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String (IsString)

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
  = PropOnly
  | InvAndProp
  deriving (Eq, Ord, Show)

data Usage
  = Usage { _usageTemplate    :: Text
          , _usageConstraints :: Map TypeVar [ConcreteType]
          , _usageArgTypes    :: [(Var, Type)]
          , _usageRetType     :: Type
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

newtype Var
  = Var Text
  deriving (Show, IsString)

newtype ConcreteType
  = ConcreteType Text
  deriving (Show, IsString)

newtype TypeVar
  = TypeVar Text
  deriving (Eq, Ord, Show, IsString)

data Type
  = TyCon ConcreteType
  | TyVar TypeVar
  deriving (Show)

int, dec, str, time, bool, obj, ks :: ConcreteType
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
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(+ x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      a
  ]
doc FSubtraction = Doc
  "-"
  InvAndProp
  "Subtraction of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(- x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      a
  ]
doc FMultiplication = Doc
  "*"
  InvAndProp
  "Multiplication of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(* x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      a
  ]
doc FDivision = Doc
  "/"
  InvAndProp
  "Division of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(/ x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      a
  ]
doc FExponentiation = Doc
  "^"
  InvAndProp
  "Exponentiation of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(^ x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      a
  ]
doc FLogarithm = Doc
  "log"
  InvAndProp
  "Logarithm of `x` base `b`."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(log b x)"
      (Map.fromList [("a", [int, dec])])
      [ ("b", a)
      , ("x", a)]
      a
  ]
doc FNegation = Doc
  "-"
  InvAndProp
  "Negation of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(- x)"
      (Map.fromList [("a", [int, dec])])
      [("x", a)]
      a
  ]
doc FSquareRoot = Doc
  "sqrt"
  InvAndProp
  "Square root of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(sqrt x)"
      (Map.fromList [("a", [int, dec])])
      [("x", a)]
      a
  ]
doc FNaturalLogarithm = Doc
  "ln"
  InvAndProp
  "Logarithm of integers and decimals base e."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(ln x)"
      (Map.fromList [("a", [int, dec])])
      [("x", a)]
      a
  ]
doc FExponential = Doc
  "exp"
  InvAndProp
  "Exponential of integers and decimals. e raised to the integer or decimal `x`."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(exp x)"
      (Map.fromList [("a", [int, dec])])
      [("x", a)]
      a
  ]
doc FAbsoluteValue = Doc
  "abs"
  InvAndProp
  "Absolute value of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(abs x)"
      (Map.fromList [("a", [int, dec])])
      [("x", a)]
      a
  ]
doc FGreaterThan = Doc
  ">"
  InvAndProp
  "True if `x` > `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(> x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FLessThan = Doc
  "<"
  InvAndProp
  "True if `x` < `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(< x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FGreaterThanOrEqual = Doc
  ">="
  InvAndProp
  "True if `x` >= `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(>= x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FLessThanOrEqual = Doc
  "<="
  InvAndProp
  "True if `x` <= `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(<= x y)"
      (Map.fromList [("a", [int, dec])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FEquality = Doc
  "="
  InvAndProp
  "True if `x` = `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(= x y)"
      (Map.fromList [("a", [int, dec, str, time, bool, obj, ks])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FInequality = Doc
  "!="
  InvAndProp
  "True if `x` != `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(!= x y)"
      (Map.fromList [("a", [int, dec, str, time, bool, obj, ks])])
      [ ("x", a)
      , ("y", a)]
      (TyCon bool)
  ]
doc FRound = Doc
  "round"
  InvAndProp
  "Banker's rounding value of decimal `x` as integer, or to `prec` precision as decimal."
  [ Usage
      "(round x)"
      Map.empty
      [ ("x", TyCon dec)]
      (TyCon int)
  , Usage
      "(round x)"
      Map.empty
      [ ("x",    TyCon dec)
      , ("prec", TyCon int)]
      (TyCon int)
  ]

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

#define PAT(symbol, feat) pattern symbol :: Text ; pattern symbol <- (symIs feat -> True)

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
