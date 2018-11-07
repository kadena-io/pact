{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Native.Keysets
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with keysets.
--

module Pact.Native.Keysets where

import Control.Lens
import Control.Monad
import Data.Default
import Prelude

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Runtime

readKeysetDef :: NativeDef
readKeysetDef =
  defRNative "read-keyset" readKeySet (funType tTyKeySet [("key",tTyString)]) $
    "Read KEY from message data body as keyset ({ \"keys\": KEYLIST, \"pred\": PREDFUN }). " <>
    "PREDFUN should resolve to a keys predicate. `$(read-keyset \"admin-keyset\")`"
  where

    readKeySet :: RNativeFun e
    readKeySet i [TLitString key]
      = (`TKeySet` def) <$> parseMsgKey i "read-keyset" key
    readKeySet i as = argsError i as

keyDefs :: NativeModule
keyDefs =
    let keysN n _ m = m >= n
        defKeyPred kp pf docs =
          defRNative (NativeDefName $ asString kp) (keyPred pf)
          (funType tTyBool [("count",tTyInteger),("matched",tTyInteger)])
          docs
    in
    ("Keysets",[
     readKeysetDef
    ,setTopLevelOnly $ defRNative "define-keyset" defineKeyset
     (funType tTyString [("name",tTyString),("keyset",tTyString)])
     "Define keyset as NAME with KEYSET. \
     \If keyset NAME already exists, keyset will be enforced before updating to new value.\
     \`$(define-keyset 'admin-keyset (read-keyset \"keyset\"))`"
    ,defRNative "enforce-keyset" enforceKeyset' (funType tTyBool [("keyset-or-name",mkTyVar "k" [tTyString,tTyKeySet])])
     "Special form to enforce KEYSET-OR-NAME against message keys before running BODY. \
     \KEYSET-OR-NAME can be a symbol of a keyset name or a keyset object. \
     \`$(with-keyset 'admin-keyset ...)` `$(with-keyset (read-keyset \"keyset\") ...)`"
    ,defKeyPred KeysAll (==)
     "Keyset predicate function to match all keys in keyset. `(keys-all 3 3)`"
    ,defKeyPred KeysAny (keysN 1)
     "Keyset predicate function to match any (at least 1) key in keyset. `(keys-any 10 1)`"
    ,defKeyPred Keys2 (keysN 2)
     "Keyset predicate function to match at least 2 keys in keyset. `(keys-2 3 1)`"
    ])


defineKeyset :: RNativeFun e
defineKeyset fi [TLitString name,TKeySet ks _] = do
  let ksn = KeySetName name
      i = _faInfo fi
  old <- readRow i KeySets ksn
  case old of
    Nothing -> writeRow i Write KeySets ksn ks & success "Keyset defined"
    Just oldKs -> do
      runPure $ enforceKeySet i (Just ksn) oldKs
      writeRow i Write KeySets ksn ks & success "Keyset defined"
defineKeyset i as = argsError i as


enforceKeyset' :: RNativeFun e
enforceKeyset' i [t] = do
  (ksn,ks) <- case t of
    TLitString name -> do
      let ksn = KeySetName name
      ksm <- readRow (_faInfo i) KeySets ksn
      case ksm of
        Nothing -> evalError' i $ "Keyset not found: " ++ show name
        Just ks -> return (Just ksn,ks)
    TKeySet ks _ -> return (Nothing,ks)
    _ -> argsError i [t,toTerm ("[body...]" :: Text)]
  runPure $ enforceKeySet (_faInfo i) ksn ks
  return $ toTerm True
enforceKeyset' i _as = argsError i []


keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
