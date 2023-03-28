-- |
-- Module      :  Pact.Native.Session
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with sessions.
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Native.Session (sessionDefs, enforceSessionDef) where


import Pact.Eval (enforceKeySetSession)
import Pact.Native.Internal(NativeDef, NativeModule, defRNative, funType, tTyBool, tTyGuard, tTyString)
import Pact.Types.KeySet (KeySetName(..), parseAnyKeysetName)
import Pact.Types.Native (RNativeFun)
import Pact.Types.Pretty (pretty)
import Pact.Types.Purity (PureSysOnly, runSysOnly)
import Pact.Types.Runtime (getInfo, evalError, evalError', ifExecutionFlagSet, ExecutionFlag(FlagDisablePact44), readRow, Domain(KeySets), argsError)
import Pact.Types.Term (Example(LitExample), Guard(GKeySet, GKeySetRef), pattern TLitString, Term(TGuard), _tGuard, toTerm)
import Pact.Types.Type (GuardType(GTyKeySet))

sessionDefs :: NativeModule
sessionDefs =
  ("Session",[enforceSessionDef])

enforceSessionDef :: NativeDef
enforceSessionDef =
  defRNative "enforce-session" (\i as -> runSysOnly $ enforceSession' i as)
  (funType tTyBool [("keyset", tTyGuard (Just GTyKeySet))]
   <> funType tTyBool [("keysetname",tTyString)]
  )
  [LitExample "(enforce-session keyset)"]
  "Enforce that the current environment contains a session signer with a key \
  \that satisfies the keyset parameter. The execution environment is \
  \responsible for setting the session signer, usually in response to an \
  \authorization flow."
  where

    lookupEnvironmentKeyset i keySetName = do
      readRow (getInfo i) KeySets keySetName >>= \case
        Nothing -> evalError (getInfo i) $ "No such keyset: " <> pretty keySetName
        Just keySet -> pure keySet

    enforceSession' :: PureSysOnly e => RNativeFun e
    enforceSession' i [TGuard{_tGuard}] = case _tGuard of
      GKeySetRef (ksr) -> do
        ks <- lookupEnvironmentKeyset i ksr
        enforceKeySetSession (getInfo i) Nothing ks >> return (toTerm True)
      GKeySet ks -> enforceKeySetSession (getInfo i) Nothing ks >> return (toTerm True)
      _ -> evalError' i "incorrect guard type, must be keyset ref or keyset"
    enforceSession' i [TLitString k] = do
      keySetName <- ifExecutionFlagSet FlagDisablePact44
        (pure $ KeySetName k Nothing)
        (case parseAnyKeysetName k of
           Left{} -> evalError' i "incorrect keyset name format"
           Right ksn -> return ksn
        )
      ks <- readRow (getInfo i) KeySets keySetName >>= \case
        Nothing -> evalError (getInfo i) $ "No such keyset: " <> pretty keySetName
        Just ks -> pure ks
      enforceKeySetSession (getInfo i) (Just keySetName) ks >> return (toTerm True)

    enforceSession' i as = argsError i as
