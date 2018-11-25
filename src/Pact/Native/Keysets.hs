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
      = ((`TGuard` def) . GKeySet) <$> parseMsgKey i "read-keyset" key
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
    ,withCapabilityDef
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

tvA :: Type n
tvA = mkTyVar "a" []


withCapabilityDef :: NativeDef
withCapabilityDef =
  defNative "with-capability" withCapability
  (funType tvA [("capability",TyFun $ funType' tTyBool []),("body",TyList TyAny)])
  "Specifies and requests grant of CAPABILITY which is an application of a 'defcap' \
   \production; given the unique token specified by this application, ensure \
   \that the token is granted in the environment during execution of BODY. If token is not \
   \present, the CAPABILITY is applied, with \
   \successful completion resulting in the installation/granting of the token, which \
   \will then be revoked upon completion of BODY. \
   \Nested 'with-capability' calls for the same token will detect the presence of \
   \the token, and will not re-apply CAPABILITY, but simply execute BODY. \
   \`$(with capability (update-users id) (update users id { salary: new-salary }))`"
  where
    withCapability :: NativeFun e
    withCapability i [c@TApp{},body@TList{}] = gasUnreduced i [] $ do
      grantedCap <- evalCap (_tApp c)
      r <- reduceBody body
      mapM_ revokeCapability grantedCap
      return r
    withCapability i as = argsError' i as

evalCap :: App (Term Ref) -> Eval e (Maybe Capability)
evalCap = undefined


defineKeyset :: RNativeFun e
defineKeyset fi [TLitString name,TGuard (GKeySet ks) _] = do
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
    TGuard (GKeySet ks) _ -> return (Nothing,ks)
    _ -> argsError i [t,toTerm ("[body...]" :: Text)]
  runPure $ enforceKeySet (_faInfo i) ksn ks
  return $ toTerm True
enforceKeyset' i _as = argsError i []


keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
