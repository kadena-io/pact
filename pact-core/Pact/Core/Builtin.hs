{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Builtin
 ( ObjectOp(..)
 , RawBuiltin(..)
 , rawBuiltinToText
 , rawBuiltinMap
 , rawBuiltinNames
 , ReplBuiltin(..)
 , replRawBuiltinNames
 , replRawBuiltinMap
 , BuiltinArity(..)
 , CapabilityOp(..)
 , CapType(..)
 , DefType(..)
 )where

import Data.Void(Void)
import Data.Text(Text)
import Data.Map.Strict(Map)

import qualified Data.Map.Strict as Map

import Pact.Core.Names
import Pact.Core.Pretty(Pretty(..))
import Pact.Core.Type

data ObjectOp o
  = ObjectAccess Field o
  -- access[f](o)
  -- For some {f:a|r}, access f
  | ObjectRemove Field o
  -- remove[f](o)
  -- For some {f:a|r}, remove f
  | ObjectExtend Field o o
  -- extend[k:=v](o)
  -- For some {r}, extend with
  | ReadEnvObject (Row Void) o
  deriving (Show, Eq, Functor, Foldable, Traversable)

data DefType
  = DTDefun
  | DTDefcap
  | DTDefConst
  deriving Show

data CapabilityOp name o
  = WithCapability name [o] o
  | RequireCapability name [o]
  | InstallCapability name [o]
  | ComposeCapability name [o]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data CapType name
  = ManagedCap Int (Type Void) name
  | AutomanagedCap
  | Unmanaged
  deriving  (Show, Eq, Functor, Foldable, Traversable)

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
  | RawReadKeyset
  | RawEnforceGuard
  | RawKeysetRefGuard
  | RawCreateUserGuard
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
  RawReadInteger -> "read-integer"
  RawReadDecimal -> "read-decimal"
  RawReadString -> "read-string"
  RawReadKeyset -> "read-keyset"
  RawEnforceGuard -> "enforce-guard"
  RawKeysetRefGuard -> "keyset-ref-guard"
  RawCreateUserGuard -> "create-user-guard"
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
    RawNot -> 2
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
    RawReadKeyset -> 1
    RawEnforceGuard -> 1
    RawKeysetRefGuard -> 1
    RawCreateUserGuard -> 1
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
  | RLoad
  deriving (Eq, Show)

instance Bounded b => Bounded (ReplBuiltin b) where
  minBound = RBuiltinWrap minBound
  maxBound = RLoad

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
    5 -> RLoad
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
    RLoad -> maxContained + 5
{-# INLINE replBFromEnum #-}

replBuiltinToText :: (b -> Text) -> ReplBuiltin b -> Text
replBuiltinToText f = \case
  RBuiltinWrap b -> f b
  RExpect -> "expect"
  RExpectFailure -> "expect-failure"
  RExpectThat -> "expect"
  RPrint -> "print"
  RLoad -> "load"

replRawBuiltinNames :: [Text]
replRawBuiltinNames = fmap (replBuiltinToText rawBuiltinToText) [minBound .. maxBound]

replRawBuiltinMap :: Map Text (ReplBuiltin RawBuiltin)
replRawBuiltinMap = Map.fromList $ (\b -> (replBuiltinToText rawBuiltinToText b, b)) <$> [minBound .. maxBound]

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
