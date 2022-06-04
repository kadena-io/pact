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


data RawBuiltin
  -- Operators
  -- BasicArith
  = RawAdd
  | RawSub
  | RawMultiply
  | RawDivide
  | RawNegate
  -- Boolean Ops
  | RawAnd
  | RawOr
  | RawNot
  -- Equality and Comparisons
  | RawEq
  | RawNeq
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
    -- Other Numerics
  | RawAbs
  | RawRound
  | RawCeiling
  | RawExp
  | RawFloor
  | RawLn
  | RawLogBase
  | RawMod
  -- General
  | RawMap
  | RawFilter
  | RawIf
  | RawIntToStr
  | RawConcat
  | RawStrToInt
  | RawTake
  | RawDrop
  | RawLength
  | RawFold
  | RawDistinct
  | RawEnforce
  | RawEnforceOne
  | RawEnumerate
  | RawEnumerateStepN
  | RawDummy
  | RawShow
  deriving (Eq, Show, Ord, Bounded, Enum)

rawBuiltinToText :: RawBuiltin -> Text
rawBuiltinToText = \case
  RawAdd -> "+"
  RawSub -> "-"
  RawMultiply -> "*"
  RawDivide -> "/"
  RawNegate -> "-"
  RawAnd -> "&&"
  RawOr -> "||"
  RawNot -> "not"
  RawEq -> "=="
  RawNeq -> "!="
  RawGT -> ">"
  RawGEQ -> ">="
  RawLT -> "<"
  RawLEQ -> "<="
  RawBitwiseAnd -> "&"
  RawBitwiseOr -> "|"
  RawBitwiseXor -> "xor"
  RawBitwiseFlip -> "~"
  RawBitShift -> "shift"
  RawAbs -> "abs"
  RawRound -> "round"
  RawCeiling -> "ceiling"
  RawExp -> "exp"
  RawFloor -> "floor"
  RawLn -> "ln"
  RawLogBase -> "logBase"
  RawMod -> "mod"
  RawMap -> "map"
  RawFilter -> "filter"
  RawIf -> "if"
  RawIntToStr -> "int-to-str"
  RawConcat -> "concat"
  RawStrToInt -> "str-to-int"
  RawTake -> "take"
  RawDrop -> "drop"
  RawLength -> "length"
  RawFold -> "fold"
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
  = AddInt
  | SubInt
  | DivInt
  | MulInt
  | NegateInt
  | AbsInt
  | LogBaseInt
  | ModInt
  | ExpInt
  | LnInt
  | BitAndInt
  | BitOrInt
  | BitXorInt
  | BitShiftInt
  | BitComplementInt
  | ShowInt
  -- If
  | IfElse
  -- Decimal ops
  | AddDec
  | SubDec
  | DivDec
  | MulDec
  | NegateDec
  | AbsDec
  | RoundDec
  | CeilingDec
  | ExpDec
  | FloorDec
  | LnDec
  | LogBaseDec
  | ShowDec
  -- Bool Comparisons
  | AndBool
  | OrBool
  | NotBool
  -- Int Equality
  | EqInt
  | NeqInt
  | GTInt
  | GEQInt
  | LTInt
  | LEQInt
  -- Decimal Equality
  | EqDec
  | NeqDec
  | GTDec
  | GEQDec
  | LTDec
  | LEQDec
  -- String Equality
  | EqStr
  | NeqStr
  | GTStr
  | GEQStr
  | LTStr
  | LEQStr
  -- Object equality
  | EqObj
  | NeqObj
  -- List Equality
  | EqList
  | ShowList
  -- String Ops
  | AddStr
  | ConcatStr
  | DropStr
  | TakeStr
  | LengthStr
  | ShowStr
  -- Unit ops
  | EqUnit
  | ShowUnit
  | GTLTUnit
  -- ListOps
  | AddList
  | DistinctList
  | TakeList
  | DropList
  | LengthList
  | FilterList
  | MapList
  | FoldList
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
  SubInt -> "subInt"
  DivInt -> "divInt"
  MulInt -> "mulInt"
  NegateInt -> "negateInt"
  AbsInt -> "absInt"
  LogBaseInt -> "logBaseInt"
  ModInt -> "modInt"
  ExpInt -> "expInt"
  LnInt -> "lnInt"
  BitAndInt -> "bitAndInt"
  BitOrInt -> "bitOrInt"
  BitXorInt -> "bitXorInt"
  BitShiftInt -> "bitShiftInt"
  BitComplementInt -> "bitFlipInt"
  ShowInt -> "showInt"
 -- If
  IfElse -> "if"
  -- Decimal ops
  AddDec -> "addDec"
  SubDec -> "subDec"
  DivDec -> "divDec"
  MulDec -> "mulDec"
  NegateDec -> "negateDec"
  AbsDec -> "absDec"
  RoundDec -> "roundDec"
  CeilingDec -> "ceilingDec"
  ExpDec -> "expDec"
  FloorDec -> "floorDec"
  LnDec -> "lnDec"
  LogBaseDec -> "logBaseDec"
  ShowDec -> "showDec"
  -- Bool Comparisons
  AndBool -> "andBool"
  OrBool -> "orBool"
  NotBool -> "notBool"
  -- Int Equality
  EqInt -> "eqInt"
  NeqInt -> "neqInt"
  GTInt -> "gtInt"
  GEQInt -> "geqInt"
  LTInt -> "ltInt"
  LEQInt -> "leqInt"
  -- Decimal Equality
  EqDec -> "eqInt"
  NeqDec -> "neqDec"
  GTDec -> "gtDec"
  GEQDec -> "geqDec"
  LTDec -> "ltDec"
  LEQDec -> "leqDec"
  -- String Equality
  EqStr -> "eqStr"
  NeqStr -> "neqStr"
  GTStr -> "gtStr"
  GEQStr -> "geqStr"
  LTStr -> "ltStr"
  LEQStr -> "leqStr"
  -- Object equality
  EqObj -> "eqObj"
  NeqObj -> "neqObj"
  -- List Equality
  EqList -> "eqList"
  ShowList -> "showList"
  -- String Ops
  AddStr -> "addStr"
  ConcatStr -> "concatStr"
  DropStr -> "dropStr"
  TakeStr -> "takeStr"
  LengthStr -> "lengthStr"
  ShowStr -> "showStr"
  -- Unit ops
  EqUnit -> "eqUnit"
  ShowUnit -> "showUnit"
  GTLTUnit -> "gtltUnit"
  -- Unit ops
  AddList -> "addList"
  DistinctList -> "distinctList"
  TakeList -> "takeList"
  DropList -> "dropList"
  LengthList -> "lengthList"
  FilterList -> "filter"
  MapList -> "map"
  FoldList -> "fold"
  Enforce -> "enforce"
  EnforceOne -> "enforce-one"
  Enumerate -> "enumerate"
  EnumerateStepN -> "enumerateStepN"
  Dummy -> "dummy"

instance Pretty RawBuiltin where
  pretty b = pretty (rawBuiltinToText b)

instance Pretty CoreBuiltin where
  pretty = pretty . coreBuiltinToText
