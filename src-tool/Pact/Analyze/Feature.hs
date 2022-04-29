{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Data and functions for generating documentation of the features found in
-- the invariant and property languages. Note that currently this omits
-- features found in the Pact term language that do not appear in either
-- properties or invariants -- e.g. @write@ for modifying a database table.
module Pact.Analyze.Feature where

import           Control.Lens           (Prism', preview, prism', review)
import           Data.Foldable          (foldl')
import qualified Data.Map               as Map
import           Data.Map.Strict        (Map)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.String            (IsString)
import           Data.Text              (Text)
import           Data.Tuple             (swap)

import qualified Pact.Types.Persistence as Pact
import           Pact.Types.Pretty      hiding (Doc, list)
import qualified Pact.Types.Pretty      as Pretty

--
-- NOTE: that in the current factoring, there can be multiple features that
-- share the same symbol -- this is one form of overloading. The "other form"
-- is the more boring instance where e.g. rounding functions have >1 signature
-- and so >1 "Usage". This must be taken into account when we render
-- user-facing docs from this information.
--

data FeatureClass
  = CNumerical
  | CBitwise
  | CLogical
  | CObject
  | CList
  | CString
  | CTemporal
  | CQuantification
  | CTransactional
  | CDatabase
  | CAuthorization
  | CFunction
  | COther
  deriving (Eq, Ord, Show)

classTitle :: FeatureClass -> Text
classTitle CNumerical      = "Numerical"
classTitle CBitwise        = "Bitwise"
classTitle CLogical        = "Logical"
classTitle CObject         = "Object"
classTitle CString         = "String"
classTitle CTemporal       = "Temporal"
classTitle CQuantification = "Quantification"
classTitle CTransactional  = "Transactional"
classTitle CDatabase       = "Database"
classTitle CAuthorization  = "Authorization"
classTitle CList           = "List"
classTitle CFunction       = "Function"
classTitle COther          = "Other"

data Feature
  -- Numerical operators
  = FAddition
  | FSubtraction
  | FMultiplication
  | FDivision
  | FExponentiation
  | FLogarithm
  | FNumericNegation
  | FSquareRoot
  | FNaturalLogarithm
  | FExponential
  | FAbsoluteValue
  | FBankersRound
  | FCeilingRound
  | FFloorRound
  | FModulus
  -- Bitwise operators
  | FBitwiseAnd
  | FBitwiseOr
  | FXor
  | FShift
  | FComplement
  -- Logical operators
  | FGreaterThan
  | FLessThan
  | FGreaterThanOrEqual
  | FLessThanOrEqual
  | FEquality
  | FInequality
  | FLogicalConjunction
  | FLogicalDisjunction
  | FLogicalNegation
  | FLogicalImplication
  | FAndQ
  | FOrQ
  -- Object operators
  | FObjectProjection
  | FObjectMerge
  | FObjectDrop
  | FObjectTake
  | FObjectLength
  -- List operators
  | FListProjection
  | FListLength
  | FContains
  | FReverse
  | FSort
  | FListDrop
  | FListTake
  | FMakeList
  | FMap
  | FFilter
  | FFold
  -- String operators
  | FStringLength
  | FConcatenation
  | FStringToInteger
  | FStringTake
  | FStringDrop
  -- Temporal operators
  | FTemporalAddition
  -- Quantification forms
  | FUniversalQuantification
  | FExistentialQuantification
  | FColumnOf
  -- Transactional operators
  | FTransactionAborts
  | FTransactionSucceeds
  | FGovernancePasses
  | FFunctionResult
  -- Database operators
  | FTableWritten
  | FTableRead
  | FCellDelta
  | FColumnDelta
  | FColumnWritten
  | FColumnRead
  | FRowRead
  | FRowWritten
  | FRowReadCount
  | FRowWriteCount
  | FRowExists
  | FPropRead
  -- Authorization operators
  | FAuthorizedBy
  | FRowEnforced
  -- Function
  | FIdentity
  | FConstantly
  | FCompose
  -- Other
  | FWhere
  | FTypeof
  deriving (Eq, Ord, Show, Bounded, Enum)

data Availability
  = PropOnly
  | InvAndProp
  deriving (Eq, Ord, Show)

data Constraint
  = OneOf [ConcreteType]
  | AnyType
  deriving (Eq, Ord, Show)

data FormType
  = Fun (Maybe Bindings) [(Var, Type)] Type
  | Sym Type
  deriving (Eq, Ord, Show)

data Usage
  = Usage { _usageTemplate    :: Text
          , _usageConstraints :: Map TypeVar Constraint
          , _usageFormType    :: FormType
          }
  deriving (Eq, Ord, Show)

--
-- NOTE: if we so chose, this information could provide a good basis for
-- interactive client-side docs. e.g. a react app, where the user can filter by
-- availability, class, symbol name, etc.
--
data Doc
  = Doc { _docSymbol       :: Text
        , _docClass        :: FeatureClass
        , _docAvailability :: Availability
        , _docDescription  :: Text
        , _docUsages       :: [Usage]
        }
  deriving (Eq, Ord, Show)

pattern Feature
  :: Text
  -> FeatureClass
  -> Availability
  -> Text
  -> [Usage]
  -> Feature
pattern Feature s c a d us <- (doc -> Doc s c a d us)

symbol :: Feature -> Text
symbol = _docSymbol . doc

availability :: Feature -> Availability
availability = _docAvailability . doc

featureClass :: Feature -> FeatureClass
featureClass = _docClass . doc

newtype Var
  = Var Text
  deriving (Eq, Ord, Show, IsString)

newtype ConcreteType
  = ConcreteType Text
  deriving (Eq, Ord, Show, IsString)

newtype TypeVar
  = TypeVar Text
  deriving (Eq, Ord, Show, IsString)

data Type
  = TyCon ConcreteType
  | TyVar TypeVar
  | TyList' Type
  | TyEnum [Text]
  | TyFun [Type] Type
  deriving (Eq, Ord, Show)

data Bindings
  = BindVar Var Type
  | BindObject
  deriving (Eq, Ord, Show)

int, dec, str, time, bool, obj, ks, tbl, col, type', list :: ConcreteType
int   = "integer"
dec   = "decimal"
str   = "string"
time  = "time"
bool  = "bool"
obj   = "object"
ks    = "keyset"
tbl   = "table"
col   = "column"
type' = "type"
list  = "list"

doc :: Feature -> Doc

-- Numeric operators

doc FAddition = Doc
  "+"
  CNumerical
  InvAndProp
  "Addition of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(+ x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        a
  ]
doc FSubtraction = Doc
  "-"
  CNumerical
  InvAndProp
  "Subtraction of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(- x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        a
  ]
doc FMultiplication = Doc
  "*"
  CNumerical
  InvAndProp
  "Multiplication of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(* x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        a
  ]
doc FDivision = Doc
  "/"
  CNumerical
  InvAndProp
  "Division of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(/ x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        a
  ]
doc FExponentiation = Doc
  "^"
  CNumerical
  InvAndProp
  "Exponentiation of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(^ x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
      a
  ]
doc FLogarithm = Doc
  "log"
  CNumerical
  InvAndProp
  "Logarithm of `x` base `b`."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(log b x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("b", a)
        , ("x", a)
        ]
        a
  ]
doc FNumericNegation = Doc
  "-"
  CNumerical
  InvAndProp
  "Negation of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(- x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        ]
        a
  ]
doc FSquareRoot = Doc
  "sqrt"
  CNumerical
  InvAndProp
  "Square root of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(sqrt x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        ]
        a
  ]
doc FNaturalLogarithm = Doc
  "ln"
  CNumerical
  InvAndProp
  "Logarithm of integers and decimals base e."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(ln x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        ]
        a
  ]
doc FExponential = Doc
  "exp"
  CNumerical
  InvAndProp
  "Exponential of integers and decimals. e raised to the integer or decimal `x`."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(exp x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        ]
        a
  ]
doc FAbsoluteValue = Doc
  "abs"
  CNumerical
  InvAndProp
  "Absolute value of integers and decimals."
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(abs x)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        ]
        a
  ]
doc FBankersRound = Doc
  "round"
  CNumerical
  InvAndProp
  "Banker's rounding value of decimal `x` as integer, or to `prec` precision as decimal."
  [ Usage
      "(round x)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon dec)
        ]
        (TyCon int)
  , Usage
      "(round x prec)"
      Map.empty
      $ Fun
        Nothing
        [ ("x",    TyCon dec)
        , ("prec", TyCon int)
        ]
        (TyCon int)
  ]
doc FCeilingRound = Doc
  "ceiling"
  CNumerical
  InvAndProp
  "Rounds the decimal `x` up to the next integer, or to `prec` precision as decimal."
  [ Usage
      "(ceiling x)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon dec)]
        (TyCon int)
  , Usage
      "(ceiling x prec)"
      Map.empty
      $ Fun
        Nothing
        [ ("x",    TyCon dec)
        , ("prec", TyCon int)
        ]
        (TyCon int)
  ]
doc FFloorRound = Doc
  "floor"
  CNumerical
  InvAndProp
  "Rounds the decimal `x` down to the previous integer, or to `prec` precision as decimal."
  [ Usage
      "(floor x)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon dec)]
        (TyCon int)
  , Usage
      "(floor x prec)"
      Map.empty
      $ Fun
        Nothing
        [ ("x",    TyCon dec)
        , ("prec", TyCon int)
        ]
        (TyCon int)
  ]
doc FModulus = Doc
  "mod"
  CNumerical
  InvAndProp
  "Integer modulus"
  [ Usage
      "(mod x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int)
        , ("y", TyCon int)
        ]
        (TyCon int)
  ]

-- Bitwise operators

doc FBitwiseAnd = Doc
  "&"
  CBitwise
  InvAndProp
  "Bitwise and"
  [ Usage
      "(& x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int)
        , ("y", TyCon int)
        ]
        (TyCon int)
  ]

doc FBitwiseOr = Doc
  "|"
  CBitwise
  InvAndProp
  "Bitwise or"
  [ Usage
      "(| x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int)
        , ("y", TyCon int)
        ]
        (TyCon int)
  ]

doc FXor = Doc
  "xor"
  CBitwise
  InvAndProp
  "Bitwise exclusive-or"
  [ Usage
      "(xor x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int)
        , ("y", TyCon int)
        ]
        (TyCon int)
  ]

doc FShift = Doc
  "shift"
  CBitwise
  InvAndProp
  "Shift `x` `y` bits left if `y` is positive, or right by `-y` bits otherwise."
  [ Usage
      "(shift x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int)
        , ("y", TyCon int)
        ]
        (TyCon int)
  ]

doc FComplement = Doc
  "~"
  CBitwise
  InvAndProp
  "Reverse all bits in `x`"
  [ Usage
      "(~ x)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon int) ]
        (TyCon int)
  ]

-- Logical operators

doc FGreaterThan = Doc
  ">"
  CLogical
  InvAndProp
  "True if `x` > `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(> x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FLessThan = Doc
  "<"
  CLogical
  InvAndProp
  "True if `x` < `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(< x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FGreaterThanOrEqual = Doc
  ">="
  CLogical
  InvAndProp
  "True if `x` >= `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(>= x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FLessThanOrEqual = Doc
  "<="
  CLogical
  InvAndProp
  "True if `x` <= `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(<= x y)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FEquality = Doc
  "="
  CLogical
  InvAndProp
  "True if `x` = `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(= x y)"
      (Map.fromList [("a", OneOf [int, dec, str, time, bool, obj, ks])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FInequality = Doc
  "!="
  CLogical
  InvAndProp
  "True if `x` != `y`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(!= x y)"
      (Map.fromList [("a", OneOf [int, dec, str, time, bool, obj, ks])])
      $ Fun
        Nothing
        [ ("x", a)
        , ("y", a)
        ]
        (TyCon bool)
  ]
doc FLogicalConjunction = Doc
  "and"
  CLogical
  InvAndProp
  "Short-circuiting logical conjunction"
  [ Usage
      "(and x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon bool)
        , ("y", TyCon bool)
        ]
        (TyCon bool)
  ]
doc FLogicalDisjunction = Doc
  "or"
  CLogical
  InvAndProp
  "Short-circuiting logical disjunction"
  [ Usage
      "(or x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon bool)
        , ("y", TyCon bool)
        ]
        (TyCon bool)
  ]
doc FLogicalNegation = Doc
  "not"
  CLogical
  InvAndProp
  "Logical negation"
  [ Usage
      "(not x)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon bool)
        ]
        (TyCon bool)
  ]
doc FLogicalImplication = Doc
  "when"
  CLogical
  InvAndProp
  "Logical implication. Equivalent to `(or (not x) y)`."
  [ Usage
      "(when x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon bool)
        , ("y", TyCon bool)
        ]
        (TyCon bool)
  ]
doc FAndQ = Doc
  "and?"
  CLogical
  InvAndProp
  "`and` the results of applying both `f` and `g` to `a`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(and? f g a)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", TyFun [a] (TyCon bool))
        , ("g", TyFun [a] (TyCon bool))
        , ("a", a)
        ]
        (TyCon bool)
  ]
doc FOrQ = Doc
  "or?"
  CLogical
  InvAndProp
  "`or` the results of applying both `f` and `g` to `a`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(or? f g a)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", TyFun [a] (TyCon bool))
        , ("g", TyFun [a] (TyCon bool))
        , ("a", a)
        ]
        (TyCon bool)
  ]

-- Object features

doc FObjectProjection = Doc
  "at"
  CObject
  InvAndProp
  "projection"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(at k o)"
      Map.empty
      $ Fun
        Nothing
        [ ("k", TyCon str)
        , ("o", TyCon obj)
        ]
        a
  , Usage
      "(at i l)"
      Map.empty
      $ Fun
        Nothing
        [ ("i", TyCon int)
        , ("o", TyCon list)
        ]
        (TyCon bool)
  ]

doc FObjectMerge = Doc
  "+"
  CObject
  InvAndProp
  "Object merge"
  [ Usage
      "(+ x y)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", TyCon obj)
        , ("y", TyCon obj)
        ]
        (TyCon obj)
  ]

doc FObjectDrop = Doc
  "drop"
  CObject
  InvAndProp
  "drop entries having the specified keys from an object"
  [ Usage
      "(drop keys o)"
      Map.empty
      $ Fun
        Nothing
        [ ("keys", TyList' (TyCon str))
        , ("o", TyCon obj)
        ]
      (TyCon obj)
  ]

doc FObjectTake = Doc
  "take"
  CObject
  InvAndProp
  "take entries having the specified keys from an object"
  [ Usage
      "(take keys o)"
      Map.empty
      $ Fun
        Nothing
        [ ("keys", TyList' (TyCon str))
        , ("o", TyCon obj)
        ]
      (TyCon obj)
  ]

doc FObjectLength = Doc
  "length"
  CObject
  InvAndProp
  "the number of key-value pairs in the object"
  [ Usage
      "(length o)"
      Map.empty
      $ Fun
        Nothing
        [ ("o", TyCon obj)
        ]
      (TyCon int)
  ]

-- List features

doc FListProjection = Doc
  "at"
  CList
  InvAndProp
  "projection"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(at k l)"
      Map.empty
      $ Fun
        Nothing
        [ ("k", TyCon str)
        , ("l", TyList' a)
        ]
        a
  , Usage
      "(at i l)"
      Map.empty
      $ Fun
        Nothing
        [ ("i", TyCon int)
        , ("o", TyCon list)
        ]
        (TyCon bool)
  ]

doc FListLength = Doc
  "length"
  CList
  InvAndProp -- TODO: double-check that this is true
  "List length"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(length s)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyList' a)
        ]
        (TyCon int)
  ]

doc FContains = Doc
  "contains"
  CList -- TODO: other category?
  InvAndProp
  "List / string / object contains"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(contains x xs)"
      Map.empty
      $ Fun
        Nothing
        [ ("x", a)
        , ("xs", TyList' a)
        ]
        (TyCon bool)
  , Usage
      "(contains k o)"
      Map.empty
      $ Fun
        Nothing
        [ ("k", TyCon str)
        , ("o", TyCon obj)
        ]
      (TyCon bool)
  , Usage
      "(contains value string)"
      Map.empty
      $ Fun
        Nothing
        [ ("value", TyCon str)
        , ("string", TyCon str)
        ]
      (TyCon bool)
  ]

doc FReverse = Doc
  "reverse"
  CList
  InvAndProp
  "reverse a list of values"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(reverse xs)"
      Map.empty
      $ Fun
        Nothing
        [ ("xs", TyList' a)
        ]
      (TyList' a)
  ]

doc FSort = Doc
  "sort"
  CList
  InvAndProp
  "sort a list of values"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(sort xs)"
      Map.empty
      $ Fun
        Nothing
        [ ("xs", TyList' a)
        ]
      (TyList' a)
  ]

doc FListDrop = Doc
  "drop"
  CList
  InvAndProp
  "drop the first `n` values from the beginning of a list (or the end if `n` is negative)"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(drop n xs)"
      Map.empty
      $ Fun
        Nothing
        [ ("n", TyCon int)
        , ("xs", TyList' a)
        ]
      (TyList' a)
  ]

doc FListTake = Doc
  "take"
  CList
  InvAndProp
  "take the first `n` values from `xs` (taken from the end if `n` is negative)"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(take n xs)"
      Map.empty
      $ Fun
        Nothing
        [ ("n", TyCon int)
        , ("xs", TyList' a)
        ]
      (TyList' a)
  ]

doc FMakeList = Doc
  "make-list"
  CList
  InvAndProp
  "create a new list with `n` copies of `a`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(make-list n a)"
      Map.empty
      $ Fun
        Nothing
        [ ("n", TyCon int)
        , ("a", a)
        ]
      (TyList' a)
  ]

doc FMap = Doc
  "map"
  CList
  InvAndProp
  "apply `f` to each element in a list"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(map f as)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", TyFun [a] b)
        , ("as", TyList' a)
        ]
      (TyList' b)
  ]

doc FFilter = Doc
  "filter"
  CList
  InvAndProp
  "filter a list by keeping the values for which `f` returns `true`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(filter f as)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", TyFun [a] (TyCon bool))
        , ("as", TyList' a)
        ]
      (TyList' a)
  ]
doc FFold = Doc
  "fold"
  CList
  InvAndProp
  "reduce a list by applying `f` to each element and the previous result"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(fold f a bs)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", TyFun [a, b] a)
        , ("a", a)
        , ("bs", TyList' b)
        ]
      (TyList' a)
  ]


-- String features

doc FStringTake = Doc
  "take"
  CString
  InvAndProp
  "take the first `n` values from `xs` (taken from the end if `n` is negative)"
  [ Usage
      "(take n s)"
      Map.empty
      $ Fun
        Nothing
        [ ("n", TyCon int)
        , ("s", TyCon str)
        ]
      (TyCon str)
  ]

doc FStringDrop = Doc
  "drop"
  CString
  InvAndProp
  "drop the first `n` values from `xs` (dropped from the end if `n` is negative)"
  [ Usage
      "(drop n s)"
      Map.empty
      $ Fun
        Nothing
        [ ("n", TyCon int)
        , ("s", TyCon str)
        ]
      (TyCon str)
  ]

doc FStringLength = Doc
  "length"
  CString
  InvAndProp -- TODO: double-check that this is true
  "String length"
  [ Usage
      "(length s)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyCon str)
        ]
        (TyCon int)
  ]

doc FConcatenation = Doc
  "+"
  CString
  InvAndProp
  "String / list concatenation"
  [ Usage
      "(+ s t)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyCon str)
        , ("t", TyCon str)
        ]
        (TyCon str)
  , let a = TyVar $ TypeVar "a"
    in Usage
      "(+ s t)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyList' a)
        , ("t", TyList' a)
        ]
        (TyList' a)
  ]

doc FStringToInteger = Doc
  "str-to-int"
  CString
  InvAndProp
  "String to integer conversion"
  [ Usage
      "(str-to-int s)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyCon str)
        ]
        (TyCon int)
  , Usage
      "(str-to-int b s)"
      Map.empty
      $ Fun
        Nothing
        [ ("b", TyCon int)
        , ("s", TyCon str)
        ]
        (TyCon int)
  ]

-- Temporal features

doc FTemporalAddition = Doc
  "add-time"
  CTemporal
  InvAndProp
  "Add seconds to a time"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(add-time t s)"
      (Map.fromList [("a", OneOf [int, dec])])
      $ Fun
        Nothing
        [ ("t", TyCon time)
        , ("s", a)
        ]
        (TyCon time)
  ]

--
-- Property-specific features
--

-- Quantification features

doc FUniversalQuantification = Doc
  "forall"
  CQuantification
  PropOnly
  "Bind a universally-quantified variable"
  [ let a = TyVar $ TypeVar "a"
        r = TyVar $ TypeVar "r"
    in Usage
      "(forall (x:string) y)"
      (Map.fromList [("a", AnyType), ("r", AnyType)])
      $ Fun
        (Just $ BindVar "x" a)
        [ ("y", r)
        ]
        r
  ]
doc FExistentialQuantification = Doc
  "exists"
  CQuantification
  PropOnly
  "Bind an existentially-quantified variable"
  [ let a = TyVar $ TypeVar "a"
        r = TyVar $ TypeVar "r"
    in Usage
      "(exists (x:string) y)"
      (Map.fromList [("a", AnyType), ("r", AnyType)])
      $ Fun
        (Just $ BindVar "x" a)
        [ ("y", r)
        ]
        r
  ]

doc FColumnOf = Doc
  "column-of"
  CQuantification
  PropOnly
  "The *type* of `column`s for a given `table`. Commonly used in conjunction with quantification; e.g.: `(exists (col:(column-of accounts)) (column-written accounts col))`."
  [ Usage
      "(column-of t)"
      Map.empty
      $ Fun
        Nothing
        [ ("t", TyCon tbl)
        ]
        (TyCon type')
  ]

-- Transactional features

doc FTransactionAborts = Doc
  "abort"
  CTransactional
  PropOnly
  "Whether the transaction aborts. This function is only useful when expressing propositions that do not assume transaction success. Propositions defined via `property` implicitly assume transaction success. We will be adding a new mode in which to use this feature in the future -- please let us know if you need this functionality."
  [ Usage
      "abort"
      Map.empty
      (Sym (TyCon bool))
  ]
doc FTransactionSucceeds = Doc
  "success"
  CTransactional
  PropOnly
  "Whether the transaction succeeds. This function is only useful when expressing propositions that do not assume transaction success. Propositions defined via `property` implicitly assume transaction success. We will be adding a new mode in which to use this feature in the future -- please let us know if you need this functionality."
  [ Usage
      "success"
      Map.empty
      (Sym (TyCon bool))
  ]
doc FGovernancePasses = Doc
  "governance-passes"
  CTransactional
  PropOnly
  "Whether the governance predicate passes. For keyset-based governance, this is the same as something like `(authorized-by 'governance-ks-name)`. Pact's property checking system currently does not analyze the body of a capability when it is used for governance due to challenges around capabilities making DB modifications -- the system currently assumes that a capability-based governance predicate is equally capable of succeeding or failing. This feature allows describing the scenarios where the predicate passes or fails."
  [ Usage
      "governance-passes"
      Map.empty
      (Sym (TyCon bool))
  ]
doc FFunctionResult = Doc
  "result"
  CTransactional
  PropOnly
  "The return value of the function under test"
  [ let r = TyVar $ TypeVar "r"
    in Usage
      "result"
      (Map.fromList [("r", AnyType)])
      (Sym r)
  ]

-- Database features

doc FTableWritten = Doc
  "table-written"
  CDatabase
  PropOnly
  "Whether a table is written in the function under analysis"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(table-written t)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        ]
        (TyCon bool)
  ]
doc FTableRead = Doc
  "table-read"
  CDatabase
  PropOnly
  "Whether a table is read in the function under analysis"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(table-read t)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        ]
        (TyCon bool)
  ]
doc FCellDelta = Doc
  "cell-delta"
  CDatabase
  PropOnly
  "The difference in a cell's value before and after the transaction"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
        c = TyVar $ TypeVar "c"
    in Usage
      "(cell-delta t c r)"
      (Map.fromList
        [ ("a", OneOf [tbl, str])
        , ("b", OneOf [col, str])
        , ("c", OneOf [int, dec])
        ])
      $ Fun
        Nothing
        [ ("t", a)
        , ("c", b)
        , ("r", TyCon str)
        ]
        c
  ]
doc FColumnDelta = Doc
  "column-delta"
  CDatabase
  PropOnly
  "The difference in a column's total summed value before and after the transaction"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
        c = TyVar $ TypeVar "c"
    in Usage
      "(column-delta t c)"
      (Map.fromList
        [ ("a", OneOf [tbl, str])
        , ("b", OneOf [col, str])
        , ("c", OneOf [int, dec])
        ])
      $ Fun
        Nothing
        [ ("t", a)
        , ("c", b)
        ]
        c
  ]
doc FColumnWritten = Doc
  "column-written"
  CDatabase
  PropOnly
  "Whether a column is written to in a transaction"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(column-written t c)"
      (Map.fromList
        [ ("a", OneOf [tbl, str])
        , ("b", OneOf [col, str])
        ])
      $ Fun
        Nothing
        [ ("t", a)
        , ("c", b)
        ]
        (TyCon bool)
  ]
doc FColumnRead = Doc
  "column-read"
  CDatabase
  PropOnly
  "Whether a column is read from in a transaction"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(column-read t c)"
      (Map.fromList
        [ ("a", OneOf [tbl, str])
        , ("b", OneOf [col, str])
        ])
      $ Fun
        Nothing
        [ ("t", a)
        , ("c", b)
        ]
        (TyCon bool)
  ]
doc FRowRead = Doc
  "row-read"
  CDatabase
  PropOnly
  "Whether a row is read in the function under analysis"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(row-read t r)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        ]
        (TyCon bool)
  ]
doc FRowWritten = Doc
  "row-written"
  CDatabase
  PropOnly
  "Whether a row is written in the function under analysis"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(row-written t r)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        ]
        (TyCon bool)
  ]
doc FRowReadCount = Doc
  "row-read-count"
  CDatabase
  PropOnly
  "The number of times a row is read during a transaction"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(row-read-count t r)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        ]
        (TyCon int)
  ]
doc FRowWriteCount = Doc
  "row-write-count"
  CDatabase
  PropOnly
  "The number of times a row is written during a transaction"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(row-write-count t r)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        ]
        (TyCon int)
  ]
doc FRowExists = Doc
  "row-exists"
  CDatabase
  PropOnly
  "Whether a row exists before or after a transaction"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(row-exists t r time)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        , ("time", TyEnum ["before", "after"])
        ]
        (TyCon bool)
  ]
doc FPropRead = Doc
  "read"
  CDatabase
  PropOnly
  "The value of a read before or after a transaction"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(read t r)"
      (Map.fromList [("a", OneOf [tbl, str])])
      $ Fun
        Nothing
        [ ("t", a)
        , ("r", TyCon str)
        , ("time", TyEnum ["before", "after"])
        ]
        (TyCon obj)
  ]

-- Authorization features

doc FAuthorizedBy = Doc
  "authorized-by"
  CAuthorization
  PropOnly
  "Whether the named keyset/guard is satisfied by the executing transaction"
  [ Usage
      "(authorized-by k)"
      Map.empty
      $ Fun
        Nothing
        [ ("k", TyCon str)
        ]
        (TyCon bool)
  ]
doc FRowEnforced = Doc
  "row-enforced"
  CAuthorization
  PropOnly
  "Whether the keyset in the row is enforced by the function under analysis"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(row-enforced t c r)"
      (Map.fromList
        [ ("a", OneOf [tbl, str])
        , ("b", OneOf [col, str])
        ])
      $ Fun
        Nothing
        [ ("t", a)
        , ("c", b)
        , ("r", TyCon str)
        ]
        (TyCon bool)
  ]

-- Functions

doc FIdentity = Doc
  "identity"
  CFunction
  InvAndProp
  "identity returns its argument unchanged"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(identity a)"
      (Map.singleton "a" (OneOf [tbl, str]))
      $ Fun
        Nothing
        [ ("a", a)
        ]
        a
  ]

doc FConstantly = Doc
  "constantly"
  CFunction
  InvAndProp
  -- TODO(joel): implement the multivariate form
  "constantly returns its first argument, ignoring the second"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
    in Usage
      "(constantly a)"
      Map.empty
      $ Fun
        Nothing
        [ ("a", a)
        , ("b", b)
        ]
        a
  ]

doc FCompose = Doc
  "compose"
  CFunction
  InvAndProp
  "compose two functions"
  [ let a = TyVar $ TypeVar "a"
        b = TyVar $ TypeVar "b"
        c = TyVar $ TypeVar "c"
        f = TyFun [a] b
        g = TyFun [b] c
    in Usage
      "(compose f g)"
      Map.empty
      $ Fun
        Nothing
        [ ("f", f)
        , ("g", g)
        ]
        c
  ]

-- Other features

doc FWhere = Doc
  "where"
  COther
  InvAndProp
  "utility for use in `filter` and `select` applying `f` to `field` in `obj`"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(where field f obj)"
      Map.empty
      $ Fun
        Nothing
        [ ("field", TyCon str)
        , ("f", TyFun [a] (TyCon bool))
        , ("obj", TyCon obj)
        ]
        (TyCon bool)
  ]

doc FTypeof = Doc
  "typeof"
  COther
  InvAndProp
  "return the type of `a` as a string"
  [ let a = TyVar $ TypeVar "a"
    in Usage
      "(typeof a)"
      Map.empty
      $ Fun Nothing
        [ ("a", a) ]
        (TyCon str)
  ]

allFeatures :: Set Feature
allFeatures = Set.fromList $ enumFrom minBound

by :: (Ord a, Ord k) => Set a -> (a -> k) -> Map k (Set a)
as `by` discrim = foldl'
  (\acc a ->
    Map.insertWith Set.union (discrim a) (Set.singleton a) acc)
  Map.empty
  as

availableFeatures :: Map Availability (Set Feature)
availableFeatures = allFeatures `by` availability

symbolFeatures :: Map Text (Set Feature)
symbolFeatures = allFeatures `by` symbol

classFeatures :: Map FeatureClass (Set Feature)
classFeatures = allFeatures `by` featureClass

-- Pattern synonyms for matching on symbol names

symIs :: Feature -> Text -> Bool
symIs feat sym = symbol feat == sym

--
-- TODO: generate these using TH once we are on GHC 8.2.1+, which is when
-- template-haskell learned pattern synonym support. It seems that we can not
-- upgrade template-haskell without upgrading GHC because template-haskell is
-- bundled with GHC.
--

#define PAT(sym, feat) pattern sym :: Text ; pattern sym <- (symIs feat -> True) where sym = symbol feat

PAT(SAddition, FAddition)
PAT(SSubtraction, FSubtraction)
PAT(SMultiplication, FMultiplication)
PAT(SDivision, FDivision)
PAT(SExponentiation, FExponentiation)
PAT(SLogarithm, FLogarithm)
PAT(SNumericNegation, FNumericNegation)
PAT(SSquareRoot, FSquareRoot)
PAT(SNaturalLogarithm, FNaturalLogarithm)
PAT(SExponential, FExponential)
PAT(SAbsoluteValue, FAbsoluteValue)
PAT(SBankersRound, FBankersRound)
PAT(SCeilingRound, FCeilingRound)
PAT(SFloorRound, FFloorRound)
PAT(SModulus, FModulus)
PAT(SBitwiseAnd, FBitwiseAnd)
PAT(SBitwiseOr, FBitwiseOr)
PAT(SXor, FXor)
PAT(SShift, FShift)
PAT(SComplement, FComplement)
PAT(SGreaterThan, FGreaterThan)
PAT(SLessThan, FLessThan)
PAT(SGreaterThanOrEqual, FGreaterThanOrEqual)
PAT(SLessThanOrEqual, FLessThanOrEqual)
PAT(SEquality, FEquality)
PAT(SInequality, FInequality)
PAT(SLogicalConjunction, FLogicalConjunction)
PAT(SLogicalDisjunction, FLogicalDisjunction)
PAT(SLogicalNegation, FLogicalNegation)
PAT(SLogicalImplication, FLogicalImplication)
PAT(SAndQ, FAndQ)
PAT(SOrQ, FOrQ)
PAT(SObjectProjection, FObjectProjection)
PAT(SListLength, FListLength)
PAT(SContains, FContains)
PAT(SReverse, FReverse)
PAT(SSort, FSort)
PAT(SListDrop, FListDrop)
PAT(SListTake, FListTake)
PAT(SMakeList, FMakeList)
PAT(SMap, FMap)
PAT(SFilter, FFilter)
PAT(SFold, FFold)
PAT(SObjectMerge, FObjectMerge)
PAT(SObjectDrop, FObjectDrop)
PAT(SObjectTake, FObjectTake)
PAT(SObjectLength, FObjectLength)
PAT(SStringLength, FStringLength)
PAT(SStringTake, FStringTake)
PAT(SStringDrop, FStringDrop)
PAT(SConcatenation, FConcatenation)
PAT(SStringToInteger, FStringToInteger)
PAT(STemporalAddition, FTemporalAddition)
PAT(SUniversalQuantification, FUniversalQuantification)
PAT(SExistentialQuantification, FExistentialQuantification)
PAT(SColumnOf, FColumnOf)
PAT(STransactionAborts, FTransactionAborts)
PAT(STransactionSucceeds, FTransactionSucceeds)
PAT(SGovernancePasses, FGovernancePasses)
PAT(SFunctionResult, FFunctionResult)
PAT(STableWritten, FTableWritten)
PAT(STableRead, FTableRead)
PAT(SCellDelta, FCellDelta)
PAT(SColumnDelta, FColumnDelta)
PAT(SColumnWritten, FColumnWritten)
PAT(SColumnRead, FColumnRead)
PAT(SRowRead, FRowRead)
PAT(SRowWritten, FRowWritten)
PAT(SRowReadCount, FRowReadCount)
PAT(SRowWriteCount, FRowWriteCount)
PAT(SRowExists, FRowExists)
PAT(SPropRead, FPropRead)
PAT(SAuthorizedBy, FAuthorizedBy)
PAT(SRowEnforced, FRowEnforced)
PAT(SIdentity, FIdentity)
PAT(SConstantly, FConstantly)
PAT(SCompose, FCompose)
PAT(SWhere, FWhere)
PAT(STypeof, FTypeof)

-- 'Text'/op prisms

mkOpNamePrism :: Ord op => [(Text, op)] -> Prism' Text op
mkOpNamePrism table =
  let mapForward = Map.fromList table
      lookupForward name = Map.lookup name mapForward

      mapReverse = Map.fromList (fmap swap table)
      lookupReverse op = mapReverse Map.! op
  in prism' lookupReverse lookupForward

toOp :: Prism' Text op -> Text -> Maybe op
toOp p = preview p

toText :: Prism' Text op -> op -> Text
toText p = review p

toDoc :: Prism' Text op -> op -> Pretty.Doc
toDoc p op = pretty $ toText p op

-- NOTE: we don't (yet?) use symbols here because Feature (currently?) only
-- handles properties and invariants.
writeTypeP :: Prism' Text Pact.WriteType
writeTypeP = mkOpNamePrism
  [ ("insert", Pact.Insert)
  , ("update", Pact.Update)
  , ("write",  Pact.Write)
  ]
