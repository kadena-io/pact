{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Pact.Native.Guards
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>,
--                Jose Cardona <jose@kadena.io>
--
-- Builtins for working with Guards and Principals.
--

module Pact.Native.Guards
( guardDefs
) where

import Data.Attoparsec.Text
import Data.Functor (void)
import Data.Text (Text)

import Pact.Eval
import Pact.Native.Internal
import Pact.Runtime.Utils
import Pact.Types.Capability
import Pact.Types.KeySet
import Pact.Types.Pretty
import Pact.Types.Principal
import Pact.Types.Runtime


guardDefs :: NativeModule
guardDefs =
  ( "Guards",
    [ createUserGuard
    , createPactGuard
    , createModuleGuard
    , createCapabilityGuard
    , createCapabilityPactGuard
    , keysetRefGuard
    , createPrincipalDef
    , validatePrincipalDef
    , isPrincipleDef
    , typeOfPrincipalDef
    ]
  )



createPactGuard :: NativeDef
createPactGuard =
  defRNative "create-pact-guard" createPactGuard'
  (funType (tTyGuard (Just GTyPact)) [("name",tTyString)])
  []
  "Defines a guard predicate by NAME that captures the results of 'pact-id'. \
  \At enforcement time, the success condition is that at that time 'pact-id' must \
  \return the same value. In effect this ensures that the guard will only succeed \
  \within the multi-transaction identified by the pact id."
  where
    createPactGuard' :: RNativeFun e
    createPactGuard' i [TLitString name] = do
      emitPactWarning $ DeprecatedNative "create-pact-guard"
                        "Pact guards have been deprecate and will be removed in a future release, please switch to capability guards"
      pid <- getPactId i
      return $ (`TGuard` (_faInfo i)) $ GPact $ PactGuard pid name
    createPactGuard' i as = argsError i as


createModuleGuard :: NativeDef
createModuleGuard =
  defRNative "create-module-guard" createModuleGuard'
  (funType (tTyGuard (Just GTyModule)) [("name",tTyString)])
  []
  "Defines a guard by NAME that enforces the current module admin predicate."
  where
    createModuleGuard' :: RNativeFun e
    createModuleGuard' i [TLitString name] = do
      emitPactWarning $ DeprecatedNative "create-module-guard"
                        "Module guards have been deprecate and will be removed in a future release, please switch to capability guards"
      findCallingModule >>= \case
        Just mn ->
          return $ (`TGuard` (_faInfo i)) $ GModule $ ModuleGuard mn name
        Nothing -> evalError' i "create-module-guard: must call within module"
    createModuleGuard' i as = argsError i as

keysetRefGuard :: NativeDef
keysetRefGuard =
  defRNative "keyset-ref-guard" keysetRefGuard'
  (funType (tTyGuard (Just GTyKeySetName)) [("keyset-ref",tTyString)])
  []
  "Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'. \
  \Concrete keysets are themselves guard types; this function is specifically to \
  \store references alongside other guards in the database, etc."
  where
    keysetRefGuard' :: RNativeFun e
    keysetRefGuard' fa [TLitString kref] = do
      n <- ifExecutionFlagSet FlagDisablePact44
        (pure $ KeySetName kref Nothing)
        (case parseAnyKeysetName kref of
           Left {} -> evalError' fa "incorrect keyset name format"
           Right k -> pure k)

      let i = _faInfo fa

      readRow i KeySets n >>= \case
        Nothing -> evalError i $ "Keyset reference cannot be found: " <> pretty kref
        Just _ -> return $ (`TGuard` i) $ GKeySetRef n
    keysetRefGuard' i as = argsError i as


createUserGuard :: NativeDef
createUserGuard =
  defNative "create-user-guard" createUserGuard'
  (funType (tTyGuard (Just GTyUser)) [("closure",TyFun $ funType' tTyBool [])])
  []
  "Defines a custom guard CLOSURE whose arguments are strictly evaluated at definition time, \
  \to be supplied to indicated function at enforcement time."
  where
    createUserGuard' :: NativeFun e
    createUserGuard' i [TApp App {..} _] = gasUnreduced i [] $ do
      args <- mapM reduce _appArgs
      appFun' <- lookupFullyQualifiedTerm _appInfo _appFun
      fun <- case appFun' of
        (TVar (Ref (TDef Def{..} _)) _) -> case _dDefType of
          Defun -> return (QName $ QualifiedName _dModule (asString _dDefName) _dInfo)
          _ -> evalError _appInfo $ "User guard closure must be defun, found: " <> pretty _dDefType
        t -> evalError (_tInfo t) $ "User guard closure function must be def: " <> pretty _appFun
      return $ (`TGuard` (_faInfo i)) $ GUser (UserGuard fun args)
    createUserGuard' i as = argsError' i as

createCapabilityGuard :: NativeDef
createCapabilityGuard =
  defNative "create-capability-guard" createCapabilityGuard'
  (funType (tTyGuard (Just GTyCapability)) [("capability",TyFun $ funType' tTyBool [])])
  [LitExample "(create-capability-guard (BANK_DEBIT 10.0))"]
  "Creates a guard that will enforce that CAPABILITY is acquired."

  where
    createCapabilityGuard' :: NativeFun e
    createCapabilityGuard' i [TApp app _] = gasUnreduced i [] $ do
      (cap,_,(args,_)) <- appToCap app
      return $ (`TGuard` (_faInfo i)) $
          GCapability $ CapabilityGuard (_scName cap) args Nothing
    createCapabilityGuard' i as = argsError' i as


createCapabilityPactGuard :: NativeDef
createCapabilityPactGuard =
  defNative "create-capability-pact-guard" createCapabilityPactGuard'
  (funType (tTyGuard (Just GTyCapability)) [("capability",TyFun $ funType' tTyBool [])])
  [LitExample "(create-capability-pact-guard (ESCROW owner))"]
  ("Creates a guard that will enforce that CAPABILITY is acquired and " <>
   "that the currently-executing defpact is operational.")

  where
    createCapabilityPactGuard' :: NativeFun e
    createCapabilityPactGuard' i [TApp app _] = gasUnreduced i [] $ do
      (cap,_,(args,_)) <- appToCap app
      pid <- getPactId i
      return $ (`TGuard` (_faInfo i)) $
          GCapability $ CapabilityGuard (_scName cap) args (Just pid)
    createCapabilityPactGuard' i as = argsError' i as


createPrincipalDef :: NativeDef
createPrincipalDef =
  defRNative "create-principal" createPrincipal'
  (funType tTyString
    [ ("guard", tTyGuard Nothing) ] )
  [ LitExample "(create-principal (read-keyset 'keyset))"]
  "Create a principal which unambiguously identifies GUARD."
  where
    createPrincipal' :: RNativeFun e
    createPrincipal' i [TGuard g _] =
      toTerm <$> createPrincipal (getInfo i) g
    createPrincipal' i as = argsError i as

createPrincipal :: Info -> Guard (Term Name) -> Eval e Text
createPrincipal i g = do
  g' <- traverse enforcePactValue g
  mkPrincipalIdent <$> guardToPrincipal chargeGas g'
  where
    chargeGas amt =
      void $ computeGasCommit i "createPrincipal" (GPrincipal amt)


validatePrincipalDef :: NativeDef
validatePrincipalDef =
  defRNative "validate-principal" validatePrincipal'
  (funType tTyBool
    [ ("guard", tTyGuard Nothing)
    , ("principal", tTyString) ] )
  [LitExample
   "(enforce (validate-principal (read-keyset 'keyset) account) \"Invalid account ID\")"]
  "Validate that PRINCIPAL unambiguously identifies GUARD."
  where
    validatePrincipal' :: RNativeFun e
    validatePrincipal' i [TGuard g _, TLitString p] = do
      q <- createPrincipal (getInfo i) g
      pure $ toTerm (p == q)
    validatePrincipal' i as = argsError i as

isPrincipleDef :: NativeDef
isPrincipleDef = defRNative "is-principal" isPrincipal
  (funType tTyBool [("principal", tTyString)])
  [LitExample
   "(enforce \
   \  (is-principal \"k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69\") \
   \  \"Invalid account structure: non-principal account\")"]
  "Tell whether PRINCIPAL string conforms to the principal format without proving validity."
  where
    isPrincipal :: RNativeFun e
    isPrincipal i as = case as of
      [TLitString p] -> case parseOnly (principalParser i) p of
        Left{} -> pure $ toTerm False
        Right{} -> pure $ toTerm True
      _ -> argsError i as

typeOfPrincipalDef :: NativeDef
typeOfPrincipalDef = defRNative "typeof-principal" typeOfPrincipal
  (funType tTyString [("principal", tTyString)])
    [LitExample
     "(typeof-principal \"k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69\")"]
  "Return the protocol type of a given PRINCIPAL value. If input value is not a principal type, \
  \then the empty string is returned."
  where
    typeOfPrincipal :: RNativeFun e
    typeOfPrincipal i as = case as of
      [TLitString p] -> case parseOnly (principalParser i) p of
        Left{} -> pure $ tStr ""
        Right ty -> pure $ tStr $ showPrincipalType ty
      _ -> argsError i as
