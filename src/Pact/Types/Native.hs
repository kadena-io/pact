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
  Map |
  Filter |
  Fold |
  Compose |
  Select |
  Where |
  AndF |
  OrF |
  NotF
  deriving (Eq,Enum,Ord,Bounded)

instance AsString SpecialForm where
  asString WithRead = "with-read"
  asString WithDefaultRead = "with-default-read"
  asString Bind = "bind"
  asString Map = "map"
  asString Filter = "filter"
  asString Fold = "fold"
  asString Compose = "compose"
  asString Select = "select"
  asString Where = "where"
  asString AndF = "and?"
  asString OrF = "or?"
  asString NotF = "not?"

instance Show SpecialForm where show = show . asString

specialForm :: SpecialForm -> NativeDefName
specialForm = NativeDefName . asString

sfLookup :: M.Map NativeDefName SpecialForm
sfLookup = M.fromList $ map (specialForm &&& id) [minBound .. maxBound]

isSpecialForm :: NativeDefName -> Maybe SpecialForm
isSpecialForm = (`M.lookup` sfLookup)


-- | Native function with un-reduced arguments. Must fire call stack.
type NativeFun e = FunApp -> [Term Ref] -> Eval e (Term Name)

-- | Native function with pre-reduced arguments, call stack fired.
type RNativeFun e = FunApp -> [Term Name] -> Eval e (Term Name)


type NativeDef = (NativeDefName,Term Name)
type NativeModule = (ModuleName,[NativeDef])
