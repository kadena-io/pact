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


import Control.Lens hiding (from,to,parts)
import Control.Monad
import Data.Default
import Data.String
import Prelude hiding (exp)


import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Runtime

keyDefs :: NativeModule
keyDefs =
    let keyPredArgs = funType tTyBool [("count",tTyInteger),("matched",tTyInteger)]
        keysN n _ m = m >= n
    in
    ("Keysets",[
     defRNative "read-keyset" readKeySet (funType tTyKeySet [("key",tTyString)]) $
         "Read KEY from message data body as keyset ({ \"keys\": KEYLIST, \"pred\": PREDFUN }). " ++
         "PREDFUN should resolve to a keys predicate. `$(read-keyset \"admin-keyset\")`"
    ,defRNative "define-keyset" defineKeyset (funType tTyString [("name",tTyString),("keyset",tTyString)])
     "Define keyset as NAME with KEYSET. \
     \If keyset NAME already exists, keyset will be enforced before updating to new value.\
     \`$(define-keyset 'admin-keyset (read-keyset \"keyset\"))`"
    ,defNative "enforce-keyset" enforceKeyset' (funType tTyBool [("keyset-or-name",mkTyVar "k" [tTyString,tTyKeySet])])
     "Special form to enforce KEYSET-OR-NAME against message keys before running BODY. \
     \KEYSET-OR-NAME can be a symbol of a keyset name or a keyset object. \
     \`$(with-keyset 'admin-keyset ...)` `$(with-keyset (read-keyset \"keyset\") ...)`"
    ,defRNative "keys-all" (keyPred (==)) keyPredArgs
     "Keyset predicate function to match all keys in keyset. `(keys-all 3 3)`"
    ,defRNative "keys-any" (keyPred (keysN 1)) keyPredArgs
     "Keyset predicate function to match all keys in keyset. `(keys-any 10 1)`"
    ,defRNative "keys-2" (keyPred (keysN 2)) keyPredArgs
     "Keyset predicate function to match at least 2 keys in keyset. `(keys-2 3 1)`"
    ])

readKeySet :: RNativeFun e
readKeySet i [TLitString key] = (`TKeySet` def) <$> parseMsgKey i "read-keyset" key
readKeySet i as = argsError i as


defineKeyset :: RNativeFun e
defineKeyset i [TLitString name,TKeySet ks _] = do
  let ksn = fromString name
  old <- readRow KeySets ksn
  case old of
    Nothing -> writeRow Write KeySets ksn ks & success "Keyset defined"
    Just _ -> do
             enforceKeySet (_faInfo i) (Just ksn) ks
             writeRow Write KeySets ksn ks & success "Keyset defined"
defineKeyset i as = argsError i as


enforceKeyset' :: NativeFun e
enforceKeyset' i [k] = do
  t <- reduce k
  (ksn,ks) <- case t of
    TLitString name -> do
      let ksn = fromString name
      ksm <- readRow KeySets ksn
      case ksm of
        Nothing -> evalError' i $ "Keyset not found: " ++ name
        Just ks -> return (Just ksn,ks)
    TKeySet ks _ -> return (Nothing,ks)
    _ -> argsError i [t,toTerm ("[body...]" :: String)]
  enforceKeySet (_faInfo i) ksn ks
  return $ toTerm True
enforceKeyset' i _as = argsError i []


keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
