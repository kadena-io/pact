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
import Data.Text (Text)

import Pact.Native.Internal
import Pact.Types.Principal
import Pact.Types.Runtime


guardDefs :: NativeModule
guardDefs =
  ( "Guards"
  , [ createPrincipalDef
    , validatePrincipalDef
    , isPrincipleDef
    , typeOfPrincipalDef
    ]
  )

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
createPrincipal i = fmap mkPrincipalIdent . guardToPrincipal i

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
   \  (is-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69) \
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
     "(typeof-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)"]
  "Return the protocol type of a given PRINCIPAL value. If input value is not a principal type, \
  \then the empty string is returned."
  where
    typeOfPrincipal :: RNativeFun e
    typeOfPrincipal i as = case as of
      [TLitString p] -> case parseOnly (principalParser i) p of
        Left{} -> pure $ tStr ""
        Right ty -> pure $ tStr $ showPrincipalType ty
      _ -> argsError i as
