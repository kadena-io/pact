{-# LANGUAGE OverloadedStrings #-}

module Pact.Types.Native where

import Pact.Types.Util
import Pact.Types.Runtime
import Pact.Compile (Reserved(RWithCapability))
import qualified Data.Map.Strict as M
import Control.Arrow

data SpecialForm =
  WithRead |
  WithDefaultRead |
  Bind |
  Select |
  Where |
  WithCapability |
  YieldSF |
  Resume
  deriving (Eq,Enum,Ord,Bounded)

instance AsString SpecialForm where
  asString WithRead = "with-read"
  asString WithDefaultRead = "with-default-read"
  asString Bind = "bind"
  asString Select = "select"
  asString Where = "where"
  asString WithCapability = asString RWithCapability
  asString YieldSF = "yield"
  asString Resume = "resume"

instance Show SpecialForm where show = show . asString

specialForm :: SpecialForm -> NativeDefName
specialForm = NativeDefName . asString

sfLookup :: M.Map NativeDefName SpecialForm
sfLookup = M.fromList $ map (specialForm &&& id) [minBound .. maxBound]

isSpecialForm :: NativeDefName -> Maybe SpecialForm
isSpecialForm = (`M.lookup` sfLookup)


data NativeFunType = NativeFunType | GasRNativeFunType | RNativeFunType
  deriving (Eq)


-- | Native function with un-reduced arguments that computes gas.
--   Opertations with cost dependent on amount of computation performed.
type NativeFun e = FunApp -> [Term Ref] -> Eval e (Gas,Term Name)

-- | Native function with reduced arguments, initial gas pre-compute that computes final gas.
--   Gas = Cost of Opertation * How Many Times Operation Occurs
--   Database functions (i.e. read) are examples of this.
type GasRNativeFun e = Gas -> FunApp -> [Term Name] -> Eval e (Gas,Term Name)

-- | Native function with reduced arguments, final gas pre-computed.
--   Operations with fixed cost.
type RNativeFun e = FunApp -> [Term Name] -> Eval e (Term Name)

type NativeDef = (NativeDefName,Term Name,NativeFunType)
type NativeModule = (ModuleName,[NativeDef])
