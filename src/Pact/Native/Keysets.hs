{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Native.Keysets
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with keysets.
--

module Pact.Native.Keysets
  ( keyDefs
  , readKeysetDef
  )

where

import Control.Lens
import Data.Text (Text)

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.KeySet
import Pact.Types.Purity
import Pact.Types.Runtime

readKeysetDef :: NativeDef
readKeysetDef =
  defRNative "read-keyset" readKeySet (funType tTyKeySet [("key",tTyString)])
    [LitExample "(read-keyset \"admin-keyset\")"] $
    "Read KEY from message data body as keyset ({ \"keys\": KEYLIST, \"pred\": PREDFUN }). " <>
    "PREDFUN should resolve to a keys predicate."
  where
    readKeySet :: RNativeFun e
    readKeySet i [TLiteral (LString key) ki] = ((`TGuard` ki) . GKeySet) <$> readKeySet' i key
    readKeySet i as = argsError i as


keyDefs :: NativeModule
keyDefs =
    let keysN n _ m = m >= n
        defKeyPred kp pf examples docs =
          defRNative (NativeDefName $ asString kp) (keyPred pf)
          (funType tTyBool [("count",tTyInteger),("matched",tTyInteger)])
          examples docs
    in
    ("Keysets",[
     readKeysetDef
    ,setTopLevelOnly $ defGasRNative "define-keyset" defineKeyset
     (funType tTyString [("name",tTyString),("keyset",tTyString)] <>
      funType tTyString [("name",tTyString)])
     [LitExample "(define-keyset 'admin-keyset (read-keyset \"keyset\"))"]
     "Define keyset as NAME with KEYSET, or if unspecified, read NAME from message payload as keyset, \
     \similarly to 'read-keyset'. \
     \If keyset NAME already exists, keyset will be enforced before updating to new value."
    ,enforceGuardDef "enforce-keyset"
    ,defKeyPred KeysAll (==)
     ["(keys-all 3 3)"] "Keyset predicate function to match all keys in keyset."
    ,defKeyPred KeysAny (keysN 1)
     ["(keys-any 10 1)"] "Keyset predicate function to match any (at least 1) key in keyset."
    ,defKeyPred Keys2 (keysN 2)
     ["(keys-2 3 1)"] "Keyset predicate function to match at least 2 keys in keyset."
    ])


readKeySet' :: FunApp -> Text -> Eval e KeySet
readKeySet' i key = do
  ks <- parseMsgKey i "read-keyset" key
  whenExecutionFlagSet FlagEnforceKeyFormats $
      enforceKeyFormats (const $ evalError' i "Invalid keyset") ks
  return ks

defineKeyset :: GasRNativeFun e
defineKeyset g0 fi as = case as of
  [TLitString name,TGuard (GKeySet ks) _] -> go name ks
  [TLitString name] -> readKeySet' fi name >>= go name
  _ -> argsError fi as
  where
    go name ks = do
      let ksn = KeySetName name
          i = _faInfo fi
      old <- readRow i KeySets ksn
      case old of
        Nothing -> do
          computeGas' g0 fi (GPreWrite (WriteKeySet ksn ks)) $
            writeRow i Write KeySets ksn ks & success "Keyset defined"
        Just oldKs -> do
          (g1,_) <- computeGas' g0 fi (GPostRead (ReadKeySet ksn oldKs)) $ return ()
          computeGas' g1 fi (GPreWrite (WriteKeySet ksn ks)) $ do
            runSysOnly $ enforceKeySet i (Just ksn) oldKs
            writeRow i Write KeySets ksn ks & success "Keyset defined"

keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
