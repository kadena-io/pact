{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Json.Compat
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Json.Compat
( spec
, spec_compat
, spec_roundtrip
, checkCompat
, checkRoundtrip
) where

import Data.Aeson hiding (Object)
import Data.Aeson.Encoding

import Test.Hspec hiding (Example, Arg)
import Test.QuickCheck

-- internal modules

import Pact.Types.Type
import Pact.Types.Exp
import Pact.Types.Names
import Pact.Types.Info
import Pact.Types.Term

spec :: Spec
spec = describe "JSON encodings" $ do
    spec_compat
    -- spec_roundtrip

-- -------------------------------------------------------------------------- --
-- toJSON ~ toEncoding compatiblity

spec_compat :: Spec
spec_compat = describe "toJSON and toEncoding are equivalent" $ do

  -- ---------------------------------------------- --
  describe "Pact.Types.Info" $ do
    it "Info" $ property $ checkCompat @Info
    it "Code" $ property $ checkCompat @Code

  -- ---------------------------------------------- --
  describe "Pact.Types.Names" $ do
    it "NamespaceName" $ property $ checkCompat @NamespaceName
    it "ModuleName" $ property $ checkCompat @ModuleName
    it "DefName" $ property $ checkCompat @DefName
    it "FullyQualifiedName" $ property $ checkCompat @FullyQualifiedName
    it "QualifiedName" $ property $ checkCompat @QualifiedName
    -- NO_JSON it "BareName" $ property $ checkCompat @BareName
    it "DynamicName" $ property $ checkCompat @DynamicName
    it "Name" $ property $ checkCompat @Name
    it "NativeDefName" $ property $ checkCompat @NativeDefName
    it "TableName" $ property $ checkCompat @TableName

  -- ---------------------------------------------- --
  describe "Pact.Types.Exp" $ do
    it "Literal" $ property $ checkCompat @Literal
    it "ListDelimiter" $ property $ checkCompat @ListDelimiter
    it "Separator" $ property $ checkCompat @Separator
    it "LiteralExp" $ property $ checkCompat @(LiteralExp ())
    it "AtomExp" $ property $ checkCompat @(AtomExp ())
    it "ListExp" $ property $ checkCompat @(ListExp ())
    it "SeparatorExp" $ property $ checkCompat @(SeparatorExp ())
    it "Exp" $ property $ checkCompat @(Exp ())

  -- ---------------------------------------------- --
  describe "Pact.Types.Type" $ do
    it "TypeName" $ property $ checkCompat @TypeName
    it "Arg" $ property $ checkCompat @(Arg ())
    it "FunType" $ property $ checkCompat @(FunType ())
    it "GuardType" $ property $ checkCompat @GuardType
    it "PrimType" $ property $ checkCompat @PrimType
    it "SchemaType" $ property $ checkCompat @SchemaType
    it "TypeVarName" $ property $ checkCompat @TypeVarName
    it "TypeVar" $ property $ checkCompat @(TypeVar ())
    it "SchemaPartial" $ property $ checkCompat @SchemaPartial
    it "Type" $ property $ checkCompat @(Type ())

  -- ---------------------------------------------- --
  describe "Pact.Types.Term" $ do
    it "Meta" $ property $ checkCompat @Meta
    it "PactId" $ property $ checkCompat @PactId
    it "UserGuard" $ property $ checkCompat @(UserGuard ())
    it "DefType" $ property $ checkCompat @DefType
    it "Gas" $ property $ checkCompat @Gas
    it "BindType" $ property $ checkCompat @(BindType ())
    -- NO_JSON it "BindPair" $ property $ checkCompat @(BindPair ())
    it "App" $ property $ checkCompat @(App ())
    it "Governance" $ property $ checkCompat @(Governance ())
    it "ModuleHash" $ property $ checkCompat @ModuleHash
    it "DefcapMeta" $ property $ checkCompat @(DefcapMeta ())
    it "DefMeta" $ property $ checkCompat @(DefMeta ())
    it "ConstVal" $ property $ checkCompat @(ConstVal ())
    -- NO_JSON it "Example" $ property $ checkCompat @Example
    it "FieldKey" $ property $ checkCompat @FieldKey
    it "Step" $ property $ checkCompat @(Step ())
    it "ModRef" $ property $ checkCompat @ModRef
    it "ModuleGuard" $ property $ checkCompat @ModuleGuard
    it "PactGuard" $ property $ checkCompat @PactGuard
    it "ObjectMap" $ property $ checkCompat @(ObjectMap ())
    it "Use" $ property $ checkCompat @Use
    it "Guard" $ property $ checkCompat @(Guard ())
    it "Module" $ property $ checkCompat @(Module ())
    it "Interface" $ property $ checkCompat @Interface
    it "ModuleDef" $ property $ checkCompat @(ModuleDef ())
    it "FunApp" $ property $ checkCompat @FunApp
    -- NO_JSON it "Ref" $ property $ checkCompat @Ref
    -- NO_JSON it "NativeDFun" $ property $ checkCompat @NativeDFun
    it "Def" $ property $ checkCompat @(Def ())
    it "Lam" $ property $ checkCompat @(Lam ())
    it "Object" $ property $ checkCompat @(Object ())
    it "Term" $ property $ checkCompat @(Term ())

checkCompat :: ToJSON a => a -> Property
checkCompat a = encode (toJSON a) === encodingToLazyByteString (toEncoding a)

-- -------------------------------------------------------------------------- --
-- JSON Encoding Roundtrips
--
-- Currently broken

spec_roundtrip :: Spec
spec_roundtrip = describe "JSON encoding roundtrips" $ do

  -- ---------------------------------------------- --
  describe "Pact.Types.Info" $ do
    it "Info" $ property $ checkRoundtrip @Info
    it "Code" $ property $ checkRoundtrip @Code

  -- ---------------------------------------------- --
  describe "Pact.Types.Names" $ do
    it "NamespaceName" $ property $ checkRoundtrip @NamespaceName
    it "ModuleName" $ property $ checkRoundtrip @ModuleName
    it "DefName" $ property $ checkRoundtrip @DefName
    it "FullyQualifiedName" $ property $ checkRoundtrip @FullyQualifiedName
    it "QualifiedName" $ property $ checkRoundtrip @QualifiedName
    -- NO_JSON it "BareName" $ property $ checkRoundtrip @BareName
    it "DynamicName" $ property $ checkRoundtrip @DynamicName

    -- BROKEN
    it "Name" $ property $ checkRoundtrip @Name
    it "NativeDefName" $ property $ checkRoundtrip @NativeDefName
    it "TableName" $ property $ checkRoundtrip @TableName

  -- ---------------------------------------------- --
  describe "Pact.Types.Exp" $ do
    it "Literal" $ property $ checkRoundtrip @Literal
    it "ListDelimiter" $ property $ checkRoundtrip @ListDelimiter
    it "Separator" $ property $ checkRoundtrip @Separator
    it "LiteralExp" $ property $ checkRoundtrip @(LiteralExp ())
    it "AtomExp" $ property $ checkRoundtrip @(AtomExp ())
    it "ListExp" $ property $ checkRoundtrip @(ListExp ())
    it "SeparatorExp" $ property $ checkRoundtrip @(SeparatorExp ())
    it "Exp" $ property $ checkRoundtrip @(Exp ())

  -- ---------------------------------------------- --
  describe "Pact.Types.Type" $ do
    it "TypeName" $ property $ checkRoundtrip @TypeName
    it "Arg" $ property $ checkRoundtrip @(Arg ())
    it "FunType" $ property $ checkRoundtrip @(FunType ())
    it "GuardType" $ property $ checkRoundtrip @GuardType
    it "PrimType" $ property $ checkRoundtrip @PrimType
    it "SchemaType" $ property $ checkRoundtrip @SchemaType
    it "TypeVarName" $ property $ checkRoundtrip @TypeVarName
    it "TypeVar" $ property $ checkRoundtrip @(TypeVar ())
    it "SchemaPartial" $ property $ checkRoundtrip @SchemaPartial
    it "Type" $ property $ checkRoundtrip @(Type ())

  -- ---------------------------------------------- --
  describe "Pact.Types.Term" $ do
    it "Meta" $ property $ checkRoundtrip @Meta
    it "PactId" $ property $ checkRoundtrip @PactId
    it "UserGuard" $ property $ checkRoundtrip @(UserGuard ())
    it "DefType" $ property $ checkRoundtrip @DefType
    it "Gas" $ property $ checkRoundtrip @Gas
    it "BindType" $ property $ checkRoundtrip @(BindType ())
    -- NO_JSON it "BindPair" $ property $ checkRoundtrip @(BindPair ())
    it "App" $ property $ checkRoundtrip @(App ())
    it "Governance" $ property $ checkRoundtrip @(Governance ())
    it "ModuleHash" $ property $ checkRoundtrip @ModuleHash
    it "DefcapMeta" $ property $ checkRoundtrip @(DefcapMeta ())
    it "DefMeta" $ property $ checkRoundtrip @(DefMeta ())
    it "ConstVal" $ property $ checkRoundtrip @(ConstVal ())
    -- NO_JSON it "Example" $ property $ checkRoundtrip @Example
    it "FieldKey" $ property $ checkRoundtrip @FieldKey
    it "Step" $ property $ checkRoundtrip @(Step ())
    it "ModRef" $ property $ checkRoundtrip @ModRef
    it "ModuleGuard" $ property $ checkRoundtrip @ModuleGuard
    it "PactGuard" $ property $ checkRoundtrip @PactGuard
    it "ObjectMap" $ property $ checkRoundtrip @(ObjectMap ())
    it "Use" $ property $ checkRoundtrip @Use
    it "Guard" $ property $ checkRoundtrip @(Guard ())
    it "Module" $ property $ checkRoundtrip @(Module ())
    it "Interface" $ property $ checkRoundtrip @Interface
    it "ModuleDef" $ property $ checkRoundtrip @(ModuleDef ())
    it "FunApp" $ property $ checkRoundtrip @FunApp
    -- NO_JSON it "Ref" $ property $ checkRoundtrip @Ref
    -- NO_JSON it "NativeDFun" $ property $ checkRoundtrip @NativeDFun
    it "Def" $ property $ checkRoundtrip @(Def ())
    it "Lam" $ property $ checkRoundtrip @(Lam ())
    it "Object" $ property $ checkRoundtrip @(Object ())
    it "Term" $ property $ checkRoundtrip @(Term ())

checkRoundtrip :: Eq a => Show a => ToJSON a => FromJSON a => a -> Property
checkRoundtrip a = eitherDecode (encode a) === Right a
