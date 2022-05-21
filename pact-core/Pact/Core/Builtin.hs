{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pact.Core.Builtin where

import Data.Text(Text)
import Data.Map.Strict(Map)

import qualified Data.Map.Strict as Map

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
  | RawDistinct
  | RawEnforce
  | RawEnforceOne
  | RawEnumerate
  | RawEnumerateStepN
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
  RawDistinct -> "distinct"
  RawEnforce -> "enforce"
  RawEnforceOne -> "enforce-one"
  RawEnumerate -> "enumerate"
  RawEnumerateStepN -> "enumerate-step"

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
  | CielingDec
  | ExpDec
  | FloorDec
  | LnDec
  | LogBaseDec
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
  -- List Equaliry
  | EqList
  -- String Ops
  | AddStr
  | ConcatStr
  | DropStr
  | TakeStr
  | LengthStr
  -- ListOps
  | AddList
  | ListLength
  | DistinctList
  | TakeList
  | DropList
  | LengthList
  | FilterList
  | Enforce
  | EnforceOne
  | Enumerate
  | EnumerateStepN
  deriving (Eq, Show, Ord, Bounded, Enum)
