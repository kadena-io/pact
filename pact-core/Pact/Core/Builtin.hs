
module Pact.Core.Builtin where


data RawBuiltin
  -- Operators
  -- BasicArith
  = RawAdd
  | RawSub
  | RawMultiply
  | RawDivide
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
  | RawLTEQ
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
  deriving (Eq, Show, Ord)

-- monomorphised builtin operations
data ResolvedBuiltin
  = AddInt
  | SubInt
  | DivInt
  | AddDecimal
  | SubDecimal
  | AddStr
  | EqInt
  | EqString
  | EqDecimal
  | ConcatStr
  | DropStr
  | DropObj
  | Enforce
  | EnforceOne
  | Enumerate
  | EnumerateStepN
  deriving (Eq, Show, Ord)
