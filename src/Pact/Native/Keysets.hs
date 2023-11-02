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
import Pact.Types.Namespace

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
    whenExecutionFlagSet FlagDisablePact410 $
      enforceKeyFormats (const $ evalError' i "Invalid keyset") ks
  pure ks

defineKeyset :: GasRNativeFun e
defineKeyset fi as = case as of
  [TLitString name,TGuard (GKeySet ks) _] -> go name ks
  [TLitString name] -> readKeySet' fi name >>= go name
  _ -> argsError fi as
  where
    go name ks = do
      let i = _faInfo fi

      ksn <- ifExecutionFlagSet FlagDisablePact44
        (pure $ KeySetName name Nothing)
        (case parseAnyKeysetName name of
           Left {} ->
             evalError' fi "incorrect keyset name format"
           Right ksn -> pure ksn)


      mNs <- use $ evalRefs . rsNamespace
      old <- readRow i KeySets ksn
      szVer <- getSizeOfVersion

      case old of
        Nothing -> case mNs of
          Nothing -> do
            unlessExecutionFlagSet FlagDisablePact44 $
              evalError' fi "Cannot define a keyset outside of a namespace"

            computeGas' fi (GPreWrite (WriteKeySet ksn ks) szVer) $
              writeRow i Write KeySets ksn ks & success "Keyset defined"
          Just (Namespace nsn ug _ag) -> do
            ksn' <- ifExecutionFlagSet FlagDisablePact44
              (pure ksn)
              (do
                enforceGuard i ug
                if Just nsn == _ksnNamespace ksn
                  -- if namespaces match, leave the keyset name alone
                  then pure ksn
                  -- otherwise, assume mismatching keysets
                  else evalError' fi "Mismatching keyset namespace")

            computeGas' fi (GPreWrite (WriteKeySet ksn' ks) szVer) $
              writeRow i Write KeySets ksn' ks & success "Keyset defined"

        Just oldKs -> do
          computeGas (Right fi) (GPostRead (ReadKeySet ksn oldKs))
          computeGas' fi (GPreWrite (WriteKeySet ksn ks) szVer) $ do
            runSysOnly $ enforceKeySet i (Just ksn) oldKs
            writeRow i Write KeySets ksn ks & success "Keyset defined"

keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
