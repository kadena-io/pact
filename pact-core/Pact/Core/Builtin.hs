{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Builtin
 ( RawBuiltin(..)
 , rawBuiltinToText
 , rawBuiltinMap
 , rawBuiltinNames
 , ReplBuiltin(..)
 , replRawBuiltinNames
 , replRawBuiltinMap
 , BuiltinArity(..)
--  , CapabilityOp(..)
--  , CapType(..)
--  , DefType(..)
 , CoreBuiltin(..)
 , ReplRawBuiltin
 , ReplCoreBuiltin
 )where

import Data.Text(Text)
import Data.Map.Strict(Map)

import qualified Data.Map.Strict as Map

import Pact.Core.Pretty(Pretty(..))

type ReplRawBuiltin = ReplBuiltin RawBuiltin
type ReplCoreBuiltin = ReplBuiltin CoreBuiltin

-- Todo: Objects to be added later @ a later milestone
-- data ObjectOp o
--   = ObjectAccess Field o
--   -- access[f](o)
--   -- For some {f:a|r}, access f
--   | ObjectRemove Field o
--   -- remove[f](o)
--   -- For some {f:a|r}, remove f
--   | ObjectExtend Field o o
--   -- extend[k:=v](o)
--   -- For some {r}, extend with
--   deriving (Show, Eq, Functor, Foldable, Traversable)

data DefType
  = DTDefun
  | DTDefcap
  | DTDefConst
  deriving Show

-- data CapabilityOp name o
--   = WithCapability name [o] o
--   | RequireCapability name [o]
--   | InstallCapability name [o]
--   | ComposeCapability name [o]
--   deriving (Show, Eq, Functor, Foldable, Traversable)

-- data CapType name
--   = ManagedCap Int (Type Void) name
--   | AutomanagedCap
--   | Unmanaged
--   deriving  (Show, Eq, Functor, Foldable, Traversable)

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
  | RawReadInteger
  | RawReadDecimal
  | RawReadString
  -- | RawReadKeyset
  -- | RawEnforceGuard
  -- | RawKeysetRefGuard
  -- | RawCreateUserGuard
  | RawListAccess
  | RawB64Encode
  | RawB64Decode
  deriving (Eq, Show, Ord, Bounded, Enum)

rawBuiltinToText :: RawBuiltin -> Text
rawBuiltinToText = \case
  -- Addition
  RawAdd -> "(+)"
  -- Num
  RawSub -> "(-)"
  RawMultiply -> "(*)"
  RawDivide -> "(/)"
  RawNegate -> "negate"
  RawAbs -> "abs"
  -- Bolean ops
  RawAnd -> "and"
  RawOr -> "or"
  RawNot -> "not"
  -- Eq
  RawEq -> "(=)"
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
  RawReadInteger -> "read-integer"
  RawReadDecimal -> "read-decimal"
  RawReadString -> "read-string"
  -- RawReadKeyset -> "read-keyset"
  -- RawEnforceGuard -> "enforce-guard"
  -- RawKeysetRefGuard -> "keyset-ref-guard"
  -- RawCreateUserGuard -> "create-user-guard"
  RawListAccess -> "at"
  RawB64Encode -> "base64-encode"
  RawB64Decode -> "base64-decode"

instance BuiltinArity RawBuiltin where
  builtinArity = \case
    RawAdd -> 2
    -- Num ->
    RawSub -> 2
    RawMultiply -> 2
    RawDivide -> 2
    RawNegate -> 1
    RawAbs -> 1
    -- Boolean Ops ->
    RawAnd -> 2
    RawOr -> 2
    RawNot -> 1
    -- Equality and Comparisons ->
    RawEq -> 2
    RawNeq -> 2
    -- Ord ->
    RawGT -> 2
    RawGEQ -> 2
    RawLT -> 2
    RawLEQ -> 2
    -- Bitwise Ops ->
    RawBitwiseAnd -> 2
    RawBitwiseOr -> 2
    RawBitwiseXor -> 2
    RawBitwiseFlip -> 1
    RawBitShift -> 2
    --  Rounding ->
    RawRound -> 1
    RawCeiling -> 1
    RawFloor -> 1
    -- Fractional ->
    RawExp -> 1
    RawLn -> 1
    RawSqrt -> 1
    RawLogBase -> 1
    -- List like ->
    RawLength -> 1
    RawTake -> 2
    RawDrop -> 2
    RawConcat -> 1
    RawReverse -> 1
    -- General ->
    RawMod -> 2
    RawMap -> 2
    RawFilter -> 2
    RawZip -> 3
    RawIf -> 3
    RawIntToStr -> 2
    RawStrToInt -> 2
    RawFold -> 3
    RawDistinct -> 1
    RawEnforce -> 2
    RawEnforceOne -> 2
    RawEnumerate -> 2
    RawEnumerateStepN -> 3
    -- Show ->
    RawShow -> 1
    RawReadInteger -> 1
    RawReadDecimal -> 1
    RawReadString -> 1
    -- RawReadKeyset -> 1
    -- RawEnforceGuard -> 1
    -- RawKeysetRefGuard -> 1
    -- RawCreateUserGuard -> 1
    RawListAccess -> 2
    RawB64Encode -> 1
    RawB64Decode -> 1

rawBuiltinNames :: [Text]
rawBuiltinNames = fmap rawBuiltinToText [minBound .. maxBound]

rawBuiltinMap :: Map Text RawBuiltin
rawBuiltinMap = Map.fromList $ (\b -> (rawBuiltinToText b, b)) <$> [minBound .. maxBound]


-- Note: commented out natives are
-- to be implemented later
data ReplBuiltin b
  = RBuiltinWrap b
  -- | RBeginTx
  -- | RBench
  -- | RCommitTx
  -- | RContinuePact
  -- | REnvChainData
  -- | REnvData
  -- | REnvDynRef
  -- | REnvEnableReplNatives
  -- | REnvEntity
  -- | REnvEvents
  -- | REnvExecConfig
  -- | REnvGas
  -- | REnvGasLimit
  -- | REnvGasLog
  -- | REnvGasModel
  -- | REnvGasPrice
  -- | REnvGasRate
  -- | REnvHash
  -- | REnvKeys
  -- | REnvNamespacePolicy
  -- | REnvSigs
  | RExpect
  | RExpectFailure
  | RExpectThat
  -- | RFormatAddress
  -- | RPactState
  | RPrint
  -- | RRollbackTx
  -- | RSigKeyset
  -- | RTestCapability
  -- | RVerify
  -- | RWithAppliedEnv
  -- | RLoad
  deriving (Eq, Show)

instance BuiltinArity b => BuiltinArity (ReplBuiltin b) where
  builtinArity = \case
    RBuiltinWrap b -> builtinArity b
    RExpect -> 5
    RExpectFailure -> 2
    RExpectThat -> 3
    RPrint -> 2
    -- RLoad -> 1

instance Bounded b => Bounded (ReplBuiltin b) where
  minBound = RBuiltinWrap minBound
  maxBound = RPrint

instance (Enum b, Bounded b) => Enum (ReplBuiltin b) where
  toEnum  = replBToEnum
  {-# SPECIALISE toEnum :: Int -> ReplBuiltin RawBuiltin #-}

  fromEnum = replBFromEnum
  {-# SPECIALISE fromEnum :: ReplBuiltin RawBuiltin -> Int #-}

replBToEnum :: forall b. (Bounded b, Enum b) => Int -> ReplBuiltin b
replBToEnum i =
  if i <= mbound then RBuiltinWrap (toEnum i)
  else case i - mbound of
    1 -> RExpect
    2 -> RExpectFailure
    3 -> RExpectThat
    4 -> RPrint
    -- 5 -> RLoad
    _ -> error "invalid"
  where
  mbound = fromEnum (maxBound :: b)
{-# INLINE replBToEnum #-}

replBFromEnum :: forall b. (Bounded b, Enum b) => ReplBuiltin b -> Int
replBFromEnum e =
  let maxContained = fromEnum (maxBound :: b)
  in case e of
    RBuiltinWrap b -> fromEnum b
    RExpect -> maxContained + 1
    RExpectFailure -> maxContained + 2
    RExpectThat -> maxContained + 3
    RPrint -> maxContained + 4
    -- RLoad -> maxContained + 5
{-# INLINE replBFromEnum #-}

replBuiltinToText :: (b -> Text) -> ReplBuiltin b -> Text
replBuiltinToText f = \case
  RBuiltinWrap b -> f b
  RExpect -> "expect"
  RExpectFailure -> "expect-failure"
  RExpectThat -> "expect-that"
  RPrint -> "print"
  -- RLoad -> "load"

replRawBuiltinNames :: [Text]
replRawBuiltinNames = fmap (replBuiltinToText rawBuiltinToText) [minBound .. maxBound]

replRawBuiltinMap :: Map Text (ReplBuiltin RawBuiltin)
replRawBuiltinMap =
  Map.fromList $ (\b -> (replBuiltinToText rawBuiltinToText b, b)) <$> [minBound .. maxBound]

-- Todo: is not a great abstraction.
-- In particular: the arity could be gathered from the type.
class BuiltinArity b where
  builtinArity :: b -> Int


instance Pretty RawBuiltin where
  pretty b = pretty (rawBuiltinToText b)

instance (Pretty b) => Pretty (ReplBuiltin b) where
  pretty = \case
    RBuiltinWrap b -> pretty b
    t -> pretty (replBuiltinToText (const "") t)

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
  -- | EqObj
  -- | NeqObj
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
  | ReadInteger
  | ReadDecimal
  | ReadString
  -- | ReadKeyset
  -- | EnforceGuard
  -- | KeysetRefGuard
  -- | CreateUserGuard
  | ListAccess
  | B64Encode
  | B64Decode
  deriving (Eq, Show, Ord, Bounded, Enum)


instance Pretty CoreBuiltin where
  pretty = pretty . coreBuiltinToText

instance BuiltinArity CoreBuiltin where
  builtinArity = \case
    AddInt -> 2
    SubInt -> 2
    DivInt -> 2
    MulInt -> 2
    NegateInt -> 1
    AbsInt -> 1
    ExpInt -> 1
    LnInt -> 1
    SqrtInt -> 1
    LogBaseInt -> 2
    ModInt -> 2
    BitAndInt -> 2
    BitOrInt -> 2
    BitXorInt -> 2
    BitShiftInt -> 2
    BitComplementInt -> 1
    ShowInt -> 1
    EqInt -> 2
    NeqInt -> 2
    GTInt -> 2
    GEQInt -> 2
    LTInt -> 2
    LEQInt -> 2
    IfElse -> 3
    AddDec -> 2
    SubDec -> 2
    DivDec -> 2
    MulDec -> 2
    NegateDec -> 1
    AbsDec -> 1
    RoundDec -> 1
    CeilingDec -> 1
    FloorDec -> 1
    ExpDec -> 1
    LnDec -> 1
    LogBaseDec -> 2
    SqrtDec -> 1
    ShowDec -> 1
    EqDec -> 2
    NeqDec -> 2
    GTDec -> 2
    GEQDec -> 2
    LTDec -> 2
    LEQDec -> 2
    AndBool -> 2
    OrBool -> 2
    NotBool -> 1
    EqBool -> 2
    NeqBool -> 2
    ShowBool -> 1
    EqStr -> 2
    NeqStr -> 2
    GTStr -> 2
    GEQStr -> 2
    LTStr -> 2
    LEQStr -> 2
    AddStr -> 2
    ConcatStr -> 1
    DropStr -> 2
    TakeStr -> 2
    LengthStr -> 1
    ReverseStr -> 1
    ShowStr -> 1
    -- EqObj -> 2
    -- NeqObj -> 2
    EqList -> 3
    NeqList -> 2
    GTList -> 2
    GEQList -> 2
    LTList -> 2
    LEQList -> 2
    ShowList -> 2
    AddList -> 2
    TakeList -> 2
    DropList -> 2
    LengthList -> 1
    ConcatList -> 1
    ReverseList -> 1
    FilterList -> 2
    DistinctList -> 1
    MapList -> 2
    ZipList -> 3
    FoldList -> 3
    EqUnit -> 2
    NeqUnit -> 2
    ShowUnit -> 1
    Enforce -> 2
    EnforceOne -> 2
    Enumerate -> 2
    EnumerateStepN -> 3
    ReadInteger -> 1
    ReadDecimal -> 1
    ReadString -> 1
    -- ReadKeyset -> 1
    -- EnforceGuard -> 1
    -- KeysetRefGuard -> 1
    -- CreateUserGuard -> 1
    ListAccess -> 2
    B64Encode -> 1
    B64Decode -> 1

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
  -- EqObj -> "eqObj"
  -- NeqObj -> "neqObj"
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
  ReadInteger -> "read-integer"
  ReadDecimal -> "read-decimal"
  ReadString -> "read-string"
  -- ReadKeyset -> "read-keyset"
  -- EnforceGuard -> "enforce-guard"
  -- KeysetRefGuard -> "keyset-ref-guard"
  -- CreateUserGuard -> "create-user-guard"
  ListAccess -> "at"
  B64Encode -> "base64-encode"
  B64Decode -> "base64-decode"
