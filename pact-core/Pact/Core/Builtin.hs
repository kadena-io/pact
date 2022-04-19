module Pact.Core.Builtin where

data RawBuiltin
  = RawAdd
  | RawSub
  | RawMultiply
  | RawDivide
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
  deriving (Eq, Show, Ord, Enum)

-- monomorphised builtin operations
data ResolvedBuiltin
  = AddInt
  | SubInt
  | AddDecimal
  | SubDecimal
  | EqInt
  | EqString
  | EqDecimal
  | ConcatStr
  | Dro
  deriving (Eq, Show, Ord, Enum)
