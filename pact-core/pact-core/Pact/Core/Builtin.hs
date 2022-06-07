{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Builtin where

import Data.Text(Text)
import Data.Map.Strict(Map)

import qualified Data.Map.Strict as Map

import Pact.Core.Names
import Pact.Core.Pretty(Pretty(..))

data ObjectOp o
  = ObjectAccess Field o
  | ObjectRemove Field o
  | ObjectUpdate Field o o
  deriving (Show, Eq, Functor, Foldable, Traversable)

{-
  [Typeclasses and Instances]
  Builtin operator overloads, grouped by the current type class hierarchy:

  class Add a where
    (+) :: a -> a -> a

  instance Add integer
  instance Add decimal
  instance Add string
  instance Add (list a)

  class Eq a where
    (==) :: a -> a -> bool
    (/=) :: a -> a -> bool

  instance Eq integer
  instance Eq decimal
  instance Eq string
  instance Eq time
  instance Eq unit
  instance Eq bool
  instance (Eq a) => Eq (list a)
  -- todo: rows

  class Ord a where
    (>=) :: a -> a -> bool
    (>) :: a -> a -> bool
    (<) :: a -> a -> bool
    (<=) :: a -> a -> bool

  instance Ord integer
  instance Ord decimal
  instance Ord string
  instance Ord time
  instance Ord unit
  instance Ord a => Ord (list a)

  class Show a where
    show :: a -> string

  instance Show integer
  instance Show decimal
  instance Show string
  instance Show time
  instance Show unit
  instance Show bool
  instance (Show a) => Show (list a)

  class Num a where
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    abs :: a -> a
    negate :: a -> a

  instance Num integer
  instance Num decimal

  class Fractional a where
    ln :: a -> decimal
    exp :: a -> decimal
    sqrt :: a -> decimal
    log-base :: a -> a -> a

  instance Fractional integer
  instance Fractional decimal

  class ListLike a where
    take :: integer -> a -> a
    drop :: integer -> a -> a
    concat :: [a] -> a
    reverse :: a -> a
    length :: a -> integer

  instance ListList string
  instance ListLike (list a)
-}
data RawBuiltin
  -- Operators
  -- Addition/Concatenation
  = RawAdd
  -- Num
  | RawSub
  | RawMultiply
  | RawDivide
  | RawNegate
  | RawAbs
  -- Boolean Ops
  | RawAnd
  | RawOr
  | RawNot
  -- Equality and Comparisons
  | RawEq
  | RawNeq
  -- Ord
  | RawGT
  | RawGEQ
  | RawLT
  | RawLEQ
  -- Bitwise Ops
  | RawBitwiseAnd
  | RawBitwiseOr
  | RawBitwiseXor
  | RawBitwiseFlip
  | RawBitShift
  --  Rounding
  | RawRound
  | RawCeiling
  | RawFloor
  -- Fractional
  | RawExp
  | RawLn
  | RawSqrt
  | RawLogBase
  -- List like
  | RawLength
  | RawTake
  | RawDrop
  | RawConcat
  | RawReverse
  -- General
  | RawMod
  | RawMap
  | RawFilter
  | RawZip
  | RawIf
  | RawIntToStr
  | RawStrToInt
  | RawFold
  | RawDistinct
  | RawEnforce
  | RawEnforceOne
  | RawEnumerate
  | RawEnumerateStepN
  -- Show
  | RawShow
  -- Testing builtin atm
  | RawDummy
  deriving (Eq, Show, Ord, Bounded, Enum)

rawBuiltinToText :: RawBuiltin -> Text
rawBuiltinToText = \case
  -- Addition
  RawAdd -> "(+)"
  -- Num
  RawSub -> "(-)"
  RawMultiply -> "(*)"
  RawDivide -> "(/)"
  RawNegate -> "(-)"
  RawAbs -> "abs"
  -- Bolean ops
  RawAnd -> "(&&)"
  RawOr -> "||"
  RawNot -> "not"
  -- Eq
  RawEq -> "(==)"
  RawNeq -> "(!=)"
  -- Ord
  RawGT -> "(>)"
  RawGEQ -> "(>=)"
  RawLT -> "(<)"
  RawLEQ -> "(<=)"
  -- Int ops
  RawBitwiseAnd -> "(&)"
  RawBitwiseOr -> "(|)"
  RawBitwiseXor -> "xor"
  RawBitwiseFlip -> "(~)"
  RawBitShift -> "shift"
  RawMod -> "mod"
  -- roundings
  RawRound -> "round"
  RawCeiling -> "ceiling"
  RawFloor -> "floor"
  -- Fractional
  RawExp -> "exp"
  RawLn -> "ln"
  RawSqrt -> "sqrt"
  RawLogBase -> "logBase"
  -- ListLike
  RawLength -> "length"
  RawTake -> "take"
  RawDrop -> "drop"
  RawConcat -> "concat"
  RawReverse -> "reverse"
  -- general
  RawMap -> "map"
  RawFilter -> "filter"
  RawIf -> "if"
  RawIntToStr -> "int-to-str"
  RawStrToInt -> "str-to-int"
  RawFold -> "fold"
  RawZip -> "zip"
  RawDistinct -> "distinct"
  RawEnforce -> "enforce"
  RawEnforceOne -> "enforce-one"
  RawEnumerate -> "enumerate"
  RawEnumerateStepN -> "enumerate-step"
  RawShow -> "show"
  RawDummy -> "dummy"

rawBuiltinNames :: [Text]
rawBuiltinNames = fmap rawBuiltinToText [minBound .. maxBound]

rawBuiltinMap :: Map Text RawBuiltin
rawBuiltinMap = Map.fromList $ (\b -> (rawBuiltinToText b, b)) <$> [minBound .. maxBound]

-- monomorphised builtin operations
-- TODO: TIME
data CoreBuiltin
  -- IntOps
  -- Integer Add
  = AddInt
  -- Int Num functions
  | SubInt
  | DivInt
  | MulInt
  | NegateInt
  | AbsInt
  -- Int fractional
  | ExpInt
  | LnInt
  | SqrtInt
  | LogBaseInt
  -- General int ops
  | ModInt
  | BitAndInt
  | BitOrInt
  | BitXorInt
  | BitShiftInt
  | BitComplementInt
  -- Int show instance
  | ShowInt
  -- Int Equality
  | EqInt
  | NeqInt
  | GTInt
  | GEQInt
  | LTInt
  | LEQInt
  -- If
  | IfElse
  -- Decimal ops
  -- Decimal add
  | AddDec
  -- Decimal num
  | SubDec
  | DivDec
  | MulDec
  | NegateDec
  | AbsDec
  -- Decimal rounding ops
  | RoundDec
  | CeilingDec
  | FloorDec
  -- Decimal rounding ops
  | ExpDec
  | LnDec
  | LogBaseDec
  | SqrtDec
  -- Decimal Show
  | ShowDec
  -- Decimal Equality
  | EqDec
  | NeqDec
  -- Decimal ord
  | GTDec
  | GEQDec
  | LTDec
  | LEQDec
  -- Bool Comparisons
  | AndBool
  | OrBool
  | NotBool
  -- other bool ops
  | EqBool
  | NeqBool
  | ShowBool
  -- String Equality
  | EqStr
  | NeqStr
  -- String Ord
  | GTStr
  | GEQStr
  | LTStr
  | LEQStr
   -- String Add
  | AddStr
  -- String ListLike
  | ConcatStr
  | DropStr
  | TakeStr
  | LengthStr
  | ReverseStr
  -- String Show
  | ShowStr
  -- Object equality
  | EqObj
  | NeqObj
  -- List Equality
  | EqList
  | NeqList
  -- List Ord
  | GTList
  | GEQList
  | LTList
  | LEQList
  -- List Show
  | ShowList
  -- List Add
  | AddList
  -- ListLike List
  | TakeList
  | DropList
  | LengthList
  | ConcatList
  | ReverseList
  -- Misc list ops
  | FilterList
  | DistinctList
  | MapList
  | ZipList
  | FoldList
  -- Unit ops
  | EqUnit
  | NeqUnit
  | ShowUnit
  -- Others
  | Enforce
  | EnforceOne
  | Enumerate
  | EnumerateStepN
  | Dummy
  deriving (Eq, Show, Ord, Bounded, Enum)

coreBuiltinToText :: CoreBuiltin -> Text
coreBuiltinToText = \case
-- IntOps
  AddInt -> "addInt"
  -- Int Num functions
  SubInt -> "subInt"
  DivInt -> "divInt"
  MulInt -> "mulInt"
  NegateInt -> "negateInt"
  AbsInt -> "absInt"
  -- Int fractional
  ExpInt -> "expInt"
  LnInt -> "lnInt"
  SqrtInt -> "sqrtInt"
  LogBaseInt -> "logBaseInt"
  -- General int ops
  ModInt -> "modInt"
  BitAndInt -> "bitAndInt"
  BitOrInt -> "bitOrInt"
  BitXorInt -> "bitXorInt"
  BitShiftInt -> "bitShiftInt"
  BitComplementInt -> "bitComplementInt"
  -- Int show instance
  ShowInt -> "showInt"
  -- Int Equality
  EqInt -> "eqInt"
  NeqInt -> "neqInt"
  GTInt -> "gtInt"
  GEQInt -> "geqInt"
  LTInt -> "ltInt"
  LEQInt -> "leqInt"
  -- If
  IfElse -> "ifElse"
  -- Decimal ops
  -- Decimal add
  AddDec -> "addDec"
  -- Decimal num
  SubDec -> "subDec"
  DivDec -> "divDec"
  MulDec -> "mulDec"
  NegateDec -> "negateDec"
  AbsDec -> "absDec"
  -- Decimal rounding ops
  RoundDec -> "roundDec"
  CeilingDec -> "ceilingDec"
  FloorDec -> "floorDec"
  -- Decimal rounding ops
  ExpDec -> "expDec"
  LnDec -> "lnDec"
  LogBaseDec -> "logBaseDec"
  SqrtDec -> "sqrtDec"
  -- Decimal Show
  ShowDec -> "showDec"
  -- Decimal Equality
  EqDec -> "eqDec"
  NeqDec -> "neqDec"
  -- Decimal ord
  GTDec -> "gtDec"
  GEQDec -> "geqDec"
  LTDec -> "ltDec"
  LEQDec -> "leqDec"
  -- Bool Comparisons
  AndBool -> "andBool"
  OrBool -> "orBool"
  NotBool -> "notBool"
  -- other bool ops
  EqBool -> "eqBool"
  NeqBool -> "neqBool"
  ShowBool -> "showBool"
  -- String Equality
  EqStr -> "eqStr"
  NeqStr -> "neqStr"
  -- String Ord
  GTStr -> "gtStr"
  GEQStr -> "gtStr"
  LTStr -> "gtStr"
  LEQStr -> "gtStr"
   -- String Add
  AddStr -> "addStr"
  -- String ListLike
  ConcatStr -> "concatStr"
  DropStr -> "dropStr"
  TakeStr -> "takeStr"
  LengthStr -> "lengthStr"
  ReverseStr -> "reverseStr"
  -- String Show
  ShowStr -> "showStr"
  -- Object equality
  EqObj -> "eqObj"
  NeqObj -> "neqObj"
  -- List Equality
  EqList -> "eqList"
  NeqList -> "neqList"
  -- List Ord
  GTList -> "gtList"
  GEQList -> "geqList"
  LTList -> "ltList"
  LEQList -> "leqList"
  -- List Show
  ShowList -> "showList"
  -- List Add
  AddList -> "addList"
  -- ListLike List
  TakeList -> "takeList"
  DropList -> "dropList"
  LengthList -> "lengthList"
  ConcatList -> "concatList"
  ReverseList -> "reverseList"
  -- Misc list ops
  FilterList -> "filterList"
  DistinctList -> "distinctList"
  MapList -> "mapList"
  ZipList -> "zipList"
  FoldList -> "foldList"
  -- Unit ops
  EqUnit -> "eqUnit"
  NeqUnit -> "neqUnit"
  ShowUnit -> "showUnit"
  -- Others
  Enforce -> "enforce"
  EnforceOne -> "enforceOn"
  Enumerate -> "enumerate"
  EnumerateStepN -> "enumerateStep"
  Dummy -> "dummy"

instance Pretty RawBuiltin where
  pretty b = pretty (rawBuiltinToText b)

instance Pretty CoreBuiltin where
  pretty = pretty . coreBuiltinToText
