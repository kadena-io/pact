{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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

import Data.Default
import Data.Text (Text)

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Pretty
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
    ,setTopLevelOnly $ defGasRNative "describe-keyset" descKeySet
     (funType tTyObjectAny [("keyset",tTyString)])
     ["(env-data {\"k\": { \"pred\": \"keys-all\", \"keys\": [\"abc\"] }) (describe-keyset (read-keyset 'k))"]
     "Get metadata for KEYSET."
    ,setTopLevelOnly $ defGasRNative "describe-guard" descGuard
     (funType tTyObjectAny [("guard",tTyString)])
     ["(env-data {'k: ['abc]}) (define-keyset 'ks (read-keyset 'k)) (describe-guard (keyset-ref-guard 'ks))"]
     "Get metadata for GUARD."
    ,enforceGuardDef "enforce-keyset"
    ,defKeyPred KeysAll (==)
     ["(keys-all 3 3)"] "Keyset predicate function to match all keys in keyset."
    ,defKeyPred KeysAny (keysN 1)
     ["(keys-any 10 1)"] "Keyset predicate function to match any (at least 1) key in keyset."
    ,defKeyPred Keys2 (keysN 2)
     ["(keys-2 3 1)"] "Keyset predicate function to match at least 2 keys in keyset."
    ])


readKeySet' :: FunApp -> Text -> Eval e KeySet
readKeySet' i key = parseMsgKey i "read-keyset" key

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

descKeySet :: GasRNativeFun e
descKeySet g i [TLitString t] = do
  r <- readRow (_faInfo i) KeySets (KeySetName t)
  case r of
    Just v -> computeGas' g i (GPostRead (ReadKeySet (KeySetName t) v)) $
              return $ toTerm v
    Nothing -> evalError' i $ "Keyset not found: " <> pretty t
descKeySet _ i as = argsError i as

descGuard :: GasRNativeFun e
descGuard gas i [TGuard g _] = do
  case g of
    GKeySet ks -> computeGas' gas i (GUserApp Defun) $
      return $ toTObject TyAny def
        [ ("type", tStr "KeySet")
        , ("keyset", toTerm ks)
        ]
    GKeySetRef (KeySetName t) -> do
      r <- readRow (_faInfo i) KeySets (KeySetName t)
      case r of
        Just v -> computeGas' gas i (GPostRead (ReadKeySet (KeySetName t) v)) $
          return $ toTObject TyAny def
            [ ("type", tStr "KeySetRef")
            , ("name", tStr t)
            , ("keyset", toTerm v)
            ]
        Nothing -> evalError' i $ "Keyset not found: " <> pretty t

      --descKeySet gas i [TLiteral (LString t) info]
    GPact (PactGuard (PactId pid) nm) -> computeGas' gas i (GUserApp Defun) $
      return $ toTObject TyAny def
        [ ("type", tStr "Pact")
        , ("name", tStr nm)
        , ("pactId", tStr pid)
        ]
    GModule mg -> computeGas' gas i (GUserApp Defun) $
      return $ toTObject TyAny def
        [ ("type", tStr "Module")
        , ("module", toTObject TyAny def $
            ("name", tStr $ _mnName $ _mgModuleName mg) :
            maybe [] (\(NamespaceName t) -> [("namespace", tStr t)]) (_mnNamespace $ _mgModuleName mg)
          )
        , ("name", tStr $ _mgName mg)
        ]
    GUser ug -> computeGas' gas i (GUserApp Defun) $
      return $ toTObject TyAny def
        [ ("type", tStr "User")
        , ("fun", tStr $ renderCompactText $ _ugFun ug) -- Meh
        , ("args", toTList TyAny def $ _ugArgs ug) -- ???
        ]
descGuard _ i as = argsError i as

keyPred :: (Integer -> Integer -> Bool) -> RNativeFun e
keyPred predfun _ [TLitInteger count,TLitInteger matched] =
    return $ toTerm (predfun count matched)
keyPred _ i as = argsError i as
