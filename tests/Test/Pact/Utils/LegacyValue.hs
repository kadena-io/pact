{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Json.Compat
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
--
module Test.Pact.Utils.LegacyValue
( spec
) where

import Bound

import Control.Monad

import Data.Aeson hiding (Object)
import Data.Aeson.Encoding
import Data.Proxy
import Data.Typeable

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
import Pact.Types.KeySet
import Pact.Types.Names
import Pact.Types.Namespace
import Pact.Types.Orphans ()
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.PactValue.Arbitrary ()
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Scheme
import Pact.Types.SigData
import Pact.Types.SPV
import Pact.Types.Term.Arbitrary ()

import Pact.Utils.LegacyValue

-- -------------------------------------------------------------------------- --
-- Roundtrips


-- | These roundtrip checks do not check backward compatibility directly.
-- However, they are useful for ensuring that the (old) parsing code is
-- compatbile with the new encoding code paths.
--
-- It is also documents which JSON instances do not roundtrip.
--
prop_checkRoundtrip :: Eq a => Show a => ToJSON a => FromJSON a => a -> Property
prop_checkRoundtrip a = eitherDecode (encode a) === Right a

checkRoundtrip :: Eq a => Show a => ToJSON a => FromJSON a => (String, a -> Property)
checkRoundtrip = ("roundtrip", prop_checkRoundtrip)

-- -------------------------------------------------------------------------- --
-- LegacyValue Compat

-- | The purpose of this test is to confirm that the direct encoding via
-- 'toEncoding' is compatible with the indirect encoding via 'toJSON' and
-- 'LegacyValue'.
--
-- 'encode' internally calls 'toEncoding' which is supposed to be compabile with
-- the encoding of 'toLegacyJson'.
--
-- 'toLegacyJson' does not call 'toEncoding'. Instead it first creates a 'Value'
-- value via 'toJSON' which is then encoded using the legacy sorting of
-- properties provided by 'LegacyHashMap'. This test checks that those code
-- paths produce the same result.
--
prop_checkLegacyValueCompat :: ToJSON a => a -> Property
prop_checkLegacyValueCompat a = encode a === encode (toLegacyJson a)

checkLegacyValueCompat :: ToJSON a => (String, a -> Property)
checkLegacyValueCompat =
  ("encoding is compatible with toLegacyJson encoding", prop_checkLegacyValueCompat)

-- -------------------------------------------------------------------------- --
-- LegacyHashable Compat

-- | This test only succeeds when it is build with hashable <1.3.1 and
-- unordered-containers <0.2.16.0.
--
-- It validates that the implementation of toJSON and toEncoding are compatible.
-- For hashable >=1.3.1 or unordered-containers >=0.2.16.0 the behavior of
-- 'toJSON' is different and it CAN NOT be used for encoding 'Value' to JSON.
--
prop_checkLegacyHashableCompat :: ToJSON a => a -> Property
prop_checkLegacyHashableCompat a =
  encode (toJSON a) === encodingToLazyByteString (toEncoding a)

checkLegacyHashableCompat :: ToJSON a => (String, a -> Property)
checkLegacyHashableCompat =
  ("toJSON and toEncoding are equivalent", prop_checkLegacyHashableCompat)

-- -------------------------------------------------------------------------- --
-- Tools

data Case a
  = Case !(Eq a => Show a => FromJSON a => ToJSON a => (String, a -> Property))
  | Pending !(Eq a => Show a => FromJSON a => ToJSON a => (String, a -> Property))
  | CaseOldHashable !(Eq a => Show a => FromJSON a => ToJSON a => (String, a -> Property))

spec_case
  :: forall a
  . ToJSON a
  => FromJSON a
  => Show a
  => Eq a
  => Typeable a
  => Arbitrary a
  => [Case a]
  -> Spec
spec_case props =
  describe (show $ typeRep (Proxy @a)) $
    forM_ props $ \case
      Case (n, p) -> it n $ property p
      Pending (n, p) -> xit n $ property p
      CaseOldHashable (n, _p) ->
#if ! MIN_VERSION_hashable(1,3,1) && !MIN_VERSION_unordered_containers(0,2,16)
        xit n $ property _p
#else
        xit n $ pendingWith "test only succeeds for hashable <1.3.1 and unordered-containers <0.2.16.0"
#endif

-- -------------------------------------------------------------------------- --
-- Spec

spec :: Spec
spec = describe "JSON encoding backward compatibility" $ do

  -- ---------------------------------------------- --
  describe "Pact.Types.Orphans" $ do
   spec_case @(Var Int ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Var () ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Scope Int [] ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Scope Int (Var ()) ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.PactError" $ do
   spec_case @PactErrorType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @StackFrame
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactError
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @OutputType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @RenderedOutput
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.ChainId" $ do
   spec_case @ChainId
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @NetworkId
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Namespace" $ do
   spec_case @(Namespace ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Gas" $ do
   spec_case @GasLimit
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @GasPrice
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Parse" $ do
   spec_case @ParsedInteger
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ParsedDecimal
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.ChainMeta" $ do
   spec_case @EntityName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @TTLSeconds
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @TxCreationTime
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Address
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PrivateMeta
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PublicMeta
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PublicData
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Info" $ do
   spec_case @Info
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Code
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Names" $ do
   spec_case @NamespaceName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ModuleName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @DefName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @FullyQualifiedName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @QualifiedName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   -- spec_case @BareName [ ]
   spec_case @DynamicName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

   spec_case @Name
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @NativeDefName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @TableName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.KeySet" $ do
   spec_case @KeySet
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @KeySetName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Exp" $ do
   spec_case @Literal
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ListDelimiter
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Separator
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(LiteralExp ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(AtomExp ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ListExp ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(SeparatorExp ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Exp ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Type" $ do
   spec_case @TypeName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Arg ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(FunType ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @GuardType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PrimType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @SchemaType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @TypeVarName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(TypeVar ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @SchemaPartial
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Type ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Term" $ do
   spec_case @Meta
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactId
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(UserGuard ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @DefType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Gas
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(BindType ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(BindPair ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(App ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Governance ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ModuleHash
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(DefcapMeta ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(DefMeta ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ConstVal ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   -- spec_case @Example []
   spec_case @FieldKey
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Step ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ModRef
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ModuleGuard
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactGuard
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ObjectMap ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Use
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Guard ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Module ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Interface
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ModuleDef ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @FunApp
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   -- spec_case @Ref [ ]
   -- spec_case @NativeDFun [ ]
   spec_case @(Def ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Lam ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Object ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Term ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.PactValue" $ do
   spec_case @PactValue
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Continuation" $ do
   spec_case @Provenance
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Yield
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactContinuation
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @NestedPactExec
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactExec
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Hash" $ do
   spec_case @Hash
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactHash
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Scheme" $ do
   spec_case @PPKScheme
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Persistence" $ do
   spec_case @PersistDirect
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ModuleData ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PersistModuleData
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   -- spec_case @RowKey [ ]
   spec_case @(Ref' PersistDirect)
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(TxLog ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @TxId
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Runtime" $ do
   spec_case @PactEvent
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ExecutionFlag
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.SPV" $ do
   spec_case @ContProof
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Capability" $ do
   spec_case @SigCapability
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.RPC" $ do
   spec_case @(PactRPC ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(ExecMsg ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @ContMsg
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Command" $ do
   spec_case @UserSig
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Command ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @Signer
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(Payload () ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @PactResult
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(CommandResult ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @RequestKey
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.SigData" $ do
   spec_case @PublicKeyHex
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]
   spec_case @(SigData ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      , CaseOldHashable checkLegacyHashableCompat
      ]

