{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Pact.Native.ModularArithmetic (egcdDefs, egcd, keccak256bs) where

import Crypto.Number.Serialize (i2ospOf_)
import Pact.Types.ECDSA (keccak256Hash, bsToInteger)
import Data.ByteString  (ByteString)
import Pact.Types.Runtime
import Pact.Native.Internal
import qualified Data.Vector as Vector

integerToBS :: Int -> Integer -> ByteString
integerToBS = i2ospOf_

egcdDefs :: NativeModule
egcdDefs = ("ModularArithmetic",
  [ egcdDef,
    keccak256bsDef
   ])

-- egcd

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, x, y) = egcd (b `mod` a) a
       in (g, y - (b `div` a) * x, x)

egcd' :: RNativeFun e
egcd' i [TLitInteger a, TLitInteger m] = do
  let (g, x, y) = egcd a m
  return (TList (Vector.fromList [toTerm g, toTerm x, toTerm y]) (TyList (TyPrim TyInteger)) (_faInfo i))
egcd' i as = argsError i as

egcdDef :: NativeDef
egcdDef = defRNative
  "egcd"
  egcd'
  (funType (TyList tTyInteger) [("x", tTyInteger), ("y", tTyInteger)])
  ["Computes the extended euclidean algorithm of two integers."]
  "Computes the extended euclidean algorithm of two integers and returns the result as a tuple."

-- keccak256bs

keccak256bs :: Int -> Integer -> Integer
keccak256bs i j = bsToInteger $ keccak256Hash $ integerToBS i j

keccak256bs' :: RNativeFun e
keccak256bs' _ [TLitInteger i, TLitInteger j] = return $ toTerm $ keccak256bs (fromIntegral i) j
keccak256bs' i as = argsError i as

keccak256bsDef :: NativeDef
keccak256bsDef = defRNative
  "keccak256-bs"
  keccak256bs'
  (funType tTyInteger [("i", tTyInteger), ("j", tTyInteger)])
  ["Computes the keccak256 hash of a given size and number, and returns the result as a integer."]
  "Computes the keccak256 hash of a given size and number, which is converted to a byte string, and returns the result as a integer"