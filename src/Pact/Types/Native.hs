{-# LANGUAGE OverloadedStrings #-}

module Pact.Types.Native where

import Pact.Types.Util
import Pact.Types.Runtime
import qualified Data.Map.Strict as M
import Control.Arrow

data SpecialForm =
  WithRead |
  WithDefaultRead |
  Bind |
  Select |
  Where
  deriving (Eq,Enum,Ord,Bounded)

instance AsString SpecialForm where
  asString WithRead = "with-read"
  asString WithDefaultRead = "with-default-read"
  asString Bind = "bind"
  asString Select = "select"
  asString Where = "where"

instance Show SpecialForm where show = show . asString

specialForm :: SpecialForm -> NativeDefName
specialForm = NativeDefName . asString

sfLookup :: M.Map NativeDefName SpecialForm
sfLookup = M.fromList $ map (specialForm &&& id) [minBound .. maxBound]

isSpecialForm :: NativeDefName -> Maybe SpecialForm
isSpecialForm = (`M.lookup` sfLookup)


-- | Native function with un-reduced arguments that computes gas. Must fire call stack.
type NativeFun e = FunApp -> [Term Ref] -> Eval e (Gas,Term Name)

-- | Native function with reduced arguments, initial gas pre-compute that computes final gas.
-- Call stack fired.
type GasRNativeFun e = Gas -> FunApp -> [Term Name] -> Eval e (Gas,Term Name)

-- | Native function with reduced arguments, final gas pre-compute, call stack fired.
type RNativeFun e = FunApp -> [Term Name] -> Eval e (Term Name)

type NativeDef = (NativeDefName,Term Name)
type NativeModule = (ModuleName,[NativeDef])
