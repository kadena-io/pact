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

import Pact.Parse
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Names
import Pact.Types.Namespace
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Scheme
import Pact.Types.SigData
import Pact.Types.SPV

spec :: Spec
spec = describe "JSON encodings" $ do
    spec_compat
    -- spec_roundtrip

-- -------------------------------------------------------------------------- --
-- toJSON ~ toEncoding compatiblity

spec_compat :: Spec
spec_compat = describe "toJSON and toEncoding are equivalent" $ do

  -- ---------------------------------------------- --
  describe "Pact.Types.PactError" $ do
    it "PactErrorType" $ property $ checkCompat @PactErrorType
    it "StackFrame" $ property $ checkCompat @StackFrame
    it "PactError" $ property $ checkCompat @PactError
    it "OutputType" $ property $ checkCompat @OutputType
    it "RenderedOutput" $ property $ checkCompat @RenderedOutput

  -- ---------------------------------------------- --
  describe "Pact.Types.ChainId" $ do
    it "ChainId" $ property $ checkCompat @ChainId
    it "NetworkId" $ property $ checkCompat @NetworkId

  -- ---------------------------------------------- --
  describe "Pact.Types.Namespace" $ do
    it "Namespace" $ property $ checkCompat @(Namespace ())

  -- ---------------------------------------------- --
  describe "Pact.Types.Gas" $ do
    it "GasLimit" $ property $ checkCompat @GasLimit
    it "GasPrice" $ property $ checkCompat @GasPrice

  -- ---------------------------------------------- --
  describe "Pact.Parse" $ do
    it "ParsedInteger" $ property $ checkCompat @ParsedInteger
    it "ParsedDecimal" $ property $ checkCompat @ParsedDecimal

  -- ---------------------------------------------- --
  describe "Pact.Types.ChainMeta" $ do
    it "EntityName" $ property $ checkCompat @EntityName
    it "TTLSeconds" $ property $ checkCompat @TTLSeconds
    it "TxCreationTime" $ property $ checkCompat @TxCreationTime
    it "Address" $ property $ checkCompat @Address
    it "PrivateMeta" $ property $ checkCompat @PrivateMeta
    it "PublicMeta" $ property $ checkCompat @PublicMeta
    it "PublicData" $ property $ checkCompat @PublicData

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

  -- ---------------------------------------------- --
  describe "Pact.Types.PactValue" $
    it "PactValue" $ property $ checkCompat @PactValue

  -- ---------------------------------------------- --
  describe "Pact.Types.Continuation" $ do
    it "Provenance" $ property $ checkCompat @Provenance
    it "Yield" $ property $ checkCompat @Yield
    it "PactContinuation" $ property $ checkCompat @PactContinuation
    it "NestedPactExec" $ property $ checkCompat @NestedPactExec
    it "PactExec" $ property $ checkCompat @PactExec

  -- ---------------------------------------------- --
  describe "Pact.Types.Hash" $ do
    it "Hash" $ property $ checkCompat @Hash
    it "PactHash" $ property $ checkCompat @PactHash

  -- ---------------------------------------------- --
  describe "Pact.Types.Scheme" $ do
    it "PPKScheme" $ property $ checkCompat @PPKScheme

  -- ---------------------------------------------- --
  describe "Pact.Types.Persistence" $ do
    it "PersistDirect" $ property $ checkCompat @PersistDirect
    it "ModuleData" $ property $ checkCompat @(ModuleData ())
    it "PersistModuleData" $ property $ checkCompat @PersistModuleData
    -- NO_JSON it "RowKey" $ property $ checkCompat @RowKey
    it "(Ref' PersistDirect)" $ property $ checkCompat @(Ref' PersistDirect)
    it "TxLog" $ property $ checkCompat @(TxLog ())
    it "TxId" $ property $ checkCompat @TxId

  -- ---------------------------------------------- --
  describe "Pact.Types.Runtime" $ do
    it "PactEvent" $ property $ checkCompat @PactEvent
    it "ExecutionFlag" $ property $ checkCompat @ExecutionFlag

  -- ---------------------------------------------- --
  describe "Pact.Types.SPV" $ do
    it "ContProof" $ property $ checkCompat @ContProof

  -- ---------------------------------------------- --
  describe "Pact.Types.Capability" $ do
    it "SigCapability" $ property $ checkCompat @SigCapability

  -- ---------------------------------------------- --
  describe "Pact.Types.RPC" $ do
    it "PactRPC" $ property $ checkCompat @(PactRPC ())
    it "ExecMsg" $ property $ checkCompat @(ExecMsg ())
    it "ContMsg" $ property $ checkCompat @ContMsg

  -- ---------------------------------------------- --
  describe "Pact.Types.Command" $ do
    it "UserSig" $ property $ checkCompat @UserSig
    it "Command" $ property $ checkCompat @(Command ())
    it "Signer" $ property $ checkCompat @Signer
    it "Payload" $ property $ checkCompat @(Payload () ())
    it "PactResult" $ property $ checkCompat @PactResult
    it "CommandResult" $ property $ checkCompat @(CommandResult ())
    it "RequestKey" $ property $ checkCompat @RequestKey

  -- ---------------------------------------------- --
  describe "Pact.Types.SigData" $ do
    it "PublicKeyHex" $ property $ checkCompat @PublicKeyHex
    it "SigData" $ property $ checkCompat @(SigData ())

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
