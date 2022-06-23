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

import Data.Aeson (encode)
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Functor (void)
import Data.Text (Text)

import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Hash
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
createPrincipal i = \case
  GPact (PactGuard pid n) -> do
    chargeGas 1
    pure $ "p:" <> asString pid <> ":" <> n
  GKeySet (KeySet ks pf) -> case (toList ks,asString pf) of
    ([k],"keys-all") -> do
      chargeGas 1
      pure $ "k:" <> asString k
    (l,fun) -> do
      a <- mkHash $ map _pubKey l
      pure $ "w:" <> asString a <> ":" <> fun
  GKeySetRef (KeySetName n) -> do
    chargeGas 1
    pure $ "r:" <> n
  GModule (ModuleGuard mn n) -> do
    chargeGas 1
    pure $ "m:" <> asString mn <> ":" <> n
  GUser (UserGuard uf args) -> do
    args' <- enforcePactValue' args
    a <- mkHash $ map toJSONPactValue args'
    pure $ "u:" <> asString uf <> ":" <> asString a
  where
    chargeGas amt = void $ computeGasCommit i "createPrincipal" (GPrincipal amt)
    mkHash bss = do
      let bs = mconcat bss
      chargeGas $ 1 + (BS.length bs `quot` 64) -- charge for 64 bytes of hashing
      return $ pactHash bs
    toJSONPactValue = toStrict . encode

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
