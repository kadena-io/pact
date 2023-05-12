{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Pact.Native.ModularArithmetic (egcdDefs, egcd) where

import Pact.Types.Runtime
import Pact.Native.Internal
import qualified Data.Vector as V

egcdDefs :: NativeModule
egcdDefs = ("ModularArithmetic",
  [ egcdDef
  ])

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, x, y) = egcd (b `mod` a) a
       in (g, y - (b `div` a) * x, x)

egcd' :: NativeFun e
egcd' i [TLitInteger a, TLitInteger m] = do
  let (g, x, y) = egcd a m
  return (Gas 0, TList (V.fromList [toTerm g, toTerm x, toTerm y]) (TyList (TyPrim TyInteger)) (_faInfo i))
egcd' i as = argsError' i as

egcdDef :: NativeDef
egcdDef = defNative
  "egcd"
  egcd'
  (funType (TyList tTyInteger) [("x", tTyInteger), ("y", tTyInteger)])
  ["Computes the extended euclidean algorithm of two integers."]
  "Computes the extended euclidean algorithm of two integers and returns the result as a tuple. The function also calculates the gas cost for the operation."