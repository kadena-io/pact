{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Features, availability, and documentation
module Pact.Analyze.Feature where

import           Control.Lens           (Prism', prism', preview, review)
import           Data.Foldable          (foldl')
import qualified Data.Map               as Map
import           Data.Map.Strict        (Map)
import qualified Data.Set               as Set
import           Data.Set               (Set)
import           Data.String            (IsString)
import           Data.Text              (Text)
import           Data.Tuple             (swap)

import qualified Pact.Types.Persistence as Pact

--
-- NOTE: that in the current factoring, there can be multiple features that
-- share the same symbol -- this is one form of overloading. The "other form"
-- is the more boring instance where e.g. rounding functions have >1 signature
-- and so >1 "Usage". This must be taken into account when we render
-- user-facing docs from this information.
--

data FeatureClass
  = CNumerical
  | CLogical
  | CObject
  | CString
  | CTemporal
  | CQuantification
  | CTransactional
  | CDatabase
  | CAuthorization
  deriving (Eq, Ord, Show)

classTitle :: FeatureClass -> Text
classTitle CNumerical = "Numerical"
classTitle CLogical = "Logical"
classTitle CObject = "Object"
classTitle CString = "String"
classTitle CTemporal = "Temporal"
classTitle CQuantification = "Quantification"
classTitle CTransactional = "Transactional"
classTitle CDatabase = "Database"
classTitle CAuthorization = "Authorization"

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
  -- Object operators
  | FObjectProjection
  | FObjectMerge
  -- String operators
  | FStringLength
  | FStringConcatenation
  -- Temporal operators
  | FTemporalAddition
  -- Quantification forms
  | FUniversalQuantification
  | FExistentialQuantification
  -- Transactional operators
  | FTransactionAborts
  | FTransactionSucceeds
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
  | TyEnum [Text]
  deriving (Eq, Ord, Show)

data Bindings
  = BindVar Var Type
  | BindObject
  deriving (Eq, Ord, Show)

int, dec, str, time, bool, obj, ks, tbl, col :: ConcreteType
int  = "integer"
dec  = "decimal"
str  = "string"
time = "time"
bool = "bool"
obj  = "object"
ks   = "keyset"
tbl  = "table"
col  = "column"

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

-- Object features

doc FObjectProjection = Doc
  "at"
  CObject
  InvAndProp
  "Object projection"
  [ Usage
      "(at k o)"
      Map.empty
      $ Fun
        Nothing
        [ ("k", TyCon str)
        , ("o", TyCon obj)
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

-- String features

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
doc FStringConcatenation = Doc
  "+"
  CString
  InvAndProp
  "String concatenation"
  [ Usage
      "(+ s t)"
      Map.empty
      $ Fun
        Nothing
        [ ("s", TyCon str)
        , ("t", TyCon str)
        ]
        (TyCon str)
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
  "Whether the named keyset is enforced by the function under analysis"
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
PAT(SObjectProjection, FObjectProjection)
PAT(SObjectMerge, FObjectMerge)
PAT(SStringLength, FStringLength)
PAT(SStringConcatenation, FStringConcatenation)
PAT(STemporalAddition, FTemporalAddition)
PAT(SUniversalQuantification, FUniversalQuantification)
PAT(SExistentialQuantification, FExistentialQuantification)
PAT(STransactionAborts, FTransactionAborts)
PAT(STransactionSucceeds, FTransactionSucceeds)
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

-- 'Text'/op prisms

mkOpNamePrism :: Ord op => [(Text, op)] -> Prism' Text op
mkOpNamePrism table =
  let mapForward = Map.fromList table
      lookupForward name = Map.lookup name mapForward

      mapReverse = Map.fromList (fmap swap table)
      lookupReverse op = mapReverse Map.! op
  in prism' lookupReverse lookupForward

toOp :: Prism' Text op -> Text -> Maybe op
toOp = preview

toText :: Prism' Text op -> op -> Text
toText = review

-- NOTE: we don't (yet?) use symbols here because Feature (currently?) only
-- handles properties and invariants.
writeTypeP :: Prism' Text Pact.WriteType
writeTypeP = mkOpNamePrism
  [ ("insert", Pact.Insert)
  , ("update", Pact.Update)
  , ("write",  Pact.Write)
  ]
