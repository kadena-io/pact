{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.Proxy
import Data.Typeable

import Test.Hspec hiding (Example, Arg)
import Test.QuickCheck

-- internal modules

import Pact.ApiReq
import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Parse
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Crypto
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
import Pact.Types.RowData
import Pact.Types.Runtime
-- import Pact.Types.Scheme -- imported via Pact.Types.Crypto
import Pact.Types.SigData
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Types.Term.Arbitrary ()
import Pact.Types.Verifier
import Pact.PersistPactDb

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- Roundtrips


-- | These roundtrip checks do not check backward compatibility directly.
-- However, they are useful for ensuring that the (old) parsing code is
-- compatbile with the new encoding code paths.
--
-- It also documents which JSON instances do not roundtrip.
--
checkRoundtrip :: (String, a -> Property)
checkRoundtrip = ("roundtrip disabled", \_ -> once True)

prop_checkRoundtrip2 :: Eq a => Show a => J.Encode a => FromJSON a => a -> Property
prop_checkRoundtrip2 a = eitherDecode (J.encode a) === Right a

checkRoundtrip2 :: Eq a => Show a => J.Encode a => FromJSON a => (String, a -> Property)
checkRoundtrip2 = ("roundtrip2", prop_checkRoundtrip2)

-- -------------------------------------------------------------------------- --
-- Roundtripable

newtype Roundtripable a = Roundtripable a
  deriving newtype (Show, Eq, ToJSON, FromJSON, J.Encode)

instance Arbitrary a => Arbitrary (Roundtripable (SigData a)) where
  arbitrary = Roundtripable <$> arbitraryRoundtripableSigData

-- -------------------------------------------------------------------------- --
-- LegacyValue Compat

-- | The purpose of this test is to confirm that the direct encoding via
-- 'toEncoding' is compatible with the indirect encoding via 'toJSON' and
-- 'LegacyValue'.
--
-- 'encode' internally calls 'toEncoding' which is supposed to be compatible with
-- the encoding of 'toLegacyJson'.
--
-- 'toLegacyJson' does not call 'toEncoding'. Instead it first creates a 'Value'
-- value via 'toJSON' which is then encoded using the legacy sorting of
-- properties provided by 'LegacyHashMap'. This test checks that those code
-- paths produce the same result.
--
checkLegacyValueCompat :: (String, a -> Property)
checkLegacyValueCompat = ("toLegacyJson is disabled", \_ -> once True)

-- -------------------------------------------------------------------------- --
--

checkAesonCompat :: (String, a -> Property)
checkAesonCompat =
  ("aeson encoding compat is disabled", \_ -> once True)
-- -------------------------------------------------------------------------- --
-- Tools

data Case a
  = Case !(Eq a => Show a => FromJSON a => (String, a -> Property))
  | Pending !(Eq a => Show a => FromJSON a => (String, a -> Property))

spec_case
  :: forall a
  . FromJSON a
  => Show a
  => Eq a
  => Typeable a
  => Arbitrary a
  => HasCallStack
  => [Case a]
  -> Spec
spec_case props =
  describe (show $ typeRep (Proxy @a)) $
    forM_ props $ \case
      Case (n, p) -> it n $ property p
      Pending (n, p) -> xit n $ property p

-- -------------------------------------------------------------------------- --
-- Orphans

deriving newtype instance ToJSON a => ToJSON (J.Aeson a)
deriving newtype instance FromJSON a => FromJSON (J.Aeson a)
deriving newtype instance Show a => Show (J.Aeson a)
deriving newtype instance Eq a => Eq (J.Aeson a)
deriving newtype instance Arbitrary a => Arbitrary (J.Aeson a)
deriving instance Functor J.Aeson
deriving instance Foldable J.Aeson
deriving instance Traversable J.Aeson

instance J.Encode [J.Aeson ()] where
  build a = J.build $ J.Array a

instance J.Encode [Var Int [J.Aeson ()]] where
  build a = J.build $ J.Array a

-- -------------------------------------------------------------------------- --
-- Specs

type A = J.Aeson

spec_pact_types_orphans :: Spec
spec_pact_types_orphans =
  describe "Pact.Types.Orphans" $ do
   spec_case @(Var Int (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Var () (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Scope Int [] (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Scope Int (Var ()) (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_namespace :: Spec
spec_pact_types_namespace =
  describe "Pact.Types.Namespace" $ do
   spec_case @(Namespace (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_chainid :: Spec
spec_pact_types_chainid =
  describe "Pact.Types.ChainId" $ do
   spec_case @ChainId
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @NetworkId
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_parse :: Spec
spec_pact_types_parse =
  describe "Pact.Parse" $ do
   spec_case @ParsedInteger
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ParsedDecimal
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_gas :: Spec
spec_pact_types_gas =
  describe "Pact.Types.Gas" $ do
   spec_case @GasLimit
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @GasPrice
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_chainmeta :: Spec
spec_pact_types_chainmeta =
  describe "Pact.Types.ChainMeta" $ do
   spec_case @EntityName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @TTLSeconds
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @TxCreationTime
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Address
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PrivateMeta
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PublicMeta
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PublicData
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_info :: Spec
spec_pact_types_info =
  describe "Pact.Types.Info" $ do
   spec_case @Info
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Code
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

  -- ---------------------------------------------- --
spec_pact_types_names :: Spec
spec_pact_types_names =
  describe "Pact.Types.Names" $ do
   spec_case @NamespaceName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ModuleName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @DefName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @FullyQualifiedName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @QualifiedName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @BareName [ ]
   spec_case @DynamicName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

   spec_case @Name
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @NativeDefName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @TableName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
-- ---------------------------------------------- --

spec_pact_types_keyset :: Spec
spec_pact_types_keyset =
  describe "Pact.Types.KeySet" $ do
   spec_case @KeySet
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @KeySetName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_exp :: Spec
spec_pact_types_exp =
  describe "Pact.Types.Exp" $ do
   spec_case @Literal
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ListDelimiter
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Separator
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(LiteralExp (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(AtomExp (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ListExp (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(SeparatorExp (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Exp (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_type :: Spec
spec_pact_types_type =
  describe "Pact.Types.Type" $ do
   spec_case @TypeName
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Arg (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(FunType (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @GuardType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PrimType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @SchemaType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @TypeVarName
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      , Case checkAesonCompat
      , Case checkRoundtrip2
      ]
   spec_case @(TypeVar (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @SchemaPartial
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Type (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_term :: Spec
spec_pact_types_term =
  describe "Pact.Types.Term" $ do
   spec_case @Meta
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactId
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(UserGuard (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @DefType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Gas
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(BindType (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(BindPair (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(App (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Governance (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ModuleHash
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(DefcapMeta (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(DefMeta (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ConstVal (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @Example []
   spec_case @FieldKey
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Step (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ModRef
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ModuleGuard
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactGuard
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ObjectMap (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Use
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(CapabilityGuard (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Guard (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Module (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Interface
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ModuleDef (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @FunApp
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @Ref [ ]
   -- spec_case @NativeDFun [ ]
   spec_case @(Def (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Lam (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Object (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Term (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_pactvalue :: Spec
spec_pact_types_pactvalue =
  describe "Pact.Types.PactValue" $ do
   spec_case @PactValue
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_continuation :: Spec
spec_pact_types_continuation =
  describe "Pact.Types.Continuation" $ do
   spec_case @Provenance
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Yield
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactContinuation
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @NestedPactExec
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactExec
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_hash :: Spec
spec_pact_types_hash =
  describe "Pact.Types.Hash" $ do
   spec_case @Hash
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactHash
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_scheme :: Spec
spec_pact_types_scheme =
  describe "Pact.Types.Scheme" $ do
   spec_case @PPKScheme
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_persistence :: Spec
spec_pact_types_persistence =
  describe "Pact.Types.Persistence" $ do
   spec_case @PersistDirect
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ModuleData (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PersistModuleData
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @RowKey [ ]
   spec_case @(Ref' PersistDirect)
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(TxLog (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @TxId
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_runtime :: Spec
spec_pact_types_runtime =
  describe "Pact.Types.Runtime" $ do
   spec_case @PactEvent
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ExecutionConfig
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @ExecutionFlag
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_spv :: Spec
spec_pact_types_spv =
  describe "Pact.Types.SPV" $ do
   spec_case @ContProof
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_capability :: Spec
spec_pact_types_capability =
  describe "Pact.Types.Capability" $ do
   spec_case @SigCapability
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_analyze_remote_types :: Spec
spec_pact_analyze_remote_types =
  describe "Pact.Analyze.Remote.Types" $ do
   spec_case @Analyze.Request
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Analyze.Response
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Analyze.ClientError
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_pactError :: Spec
spec_pact_types_pactError =
  describe "Pact.Types.PactError" $ do
   spec_case @PactErrorType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @StackFrame
   --    [ Case checkAesonCompat
   --    , Case checkLegacyValueCompat
   --    ]
   spec_case @UxStackFrame
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @PactError
   --    [ Case checkAesonCompat
   --    , Case checkLegacyValueCompat
   --    ]
   spec_case @UxPactError
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @OutputType
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @RenderedOutput
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_rowData :: Spec
spec_pact_types_rowData =
  describe "Pact.Types.RowData" $ do
    spec_case @RowDataValue
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @RowDataVersion
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @RowData
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

    -- Not  exported
    -- spec_case @OldPactValue
    --   [ Case checkRoundtrip
    --   , Case checkRoundtrip2
    --   , Case checkAesonCompat
    --   , Case checkLegacyValueCompat
    --   ]

-- ---------------------------------------------- --
spec_pact_persistPactDb :: Spec
spec_pact_persistPactDb =
  describe "Pact.PersistPactDb" $ do
    spec_case @UserTableInfo
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_sqlite :: Spec
spec_pact_types_sqlite =
  describe "Pact.Types.SQLite" $ do
   spec_case @Pragma
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @SQLiteConfig
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_command :: Spec
spec_pact_types_command =
  describe "Pact.Types.Command" $ do
   spec_case @UserSig
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Command (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Signer
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Payload (A ()) (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactResult
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(CommandResult (A ()))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @RequestKey
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Verifier ParsedVerifierArgs)
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_sigdata :: Spec
spec_pact_types_sigdata =
  describe "Pact.Types.SigData" $ do
   spec_case @PublicKeyHex
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(SigData (A ()))
      [ Pending checkRoundtrip -- See FIXME in Pact.Types.SigData
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(Roundtripable(SigData (A ())))
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- -------------------------------------------------------------------------- --
--

spec :: Spec
spec = describe "JSON encoding backward compatibility" $ do

  spec_pact_types_orphans

  spec_pact_analyze_remote_types
  spec_pact_types_pactError
  spec_pact_types_namespace
  spec_pact_types_chainid
  spec_pact_types_parse
  spec_pact_types_gas
  spec_pact_types_chainmeta
  spec_pact_types_info
  spec_pact_types_names
  spec_pact_types_keyset
  spec_pact_types_exp
  spec_pact_types_type
  spec_pact_types_term
  spec_pact_types_pactvalue
  spec_pact_types_continuation
  spec_pact_types_hash
  spec_pact_types_scheme
  spec_pact_types_persistence
  spec_pact_types_runtime
  spec_pact_types_spv
  spec_pact_types_capability

  -- ---------------------------------------------- --
  describe "Pact.Types.RPC" $ do
   spec_case @(PactRPC ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @(ExecMsg ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @ContMsg
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]

  spec_pact_types_command
  spec_pact_types_sigdata
  spec_pact_types_rowData
  spec_pact_persistPactDb
  spec_pact_types_sqlite

  -- ---------------------------------------------- --
  describe "Pact.ApiReq" $ do
    spec_case @ApiKeyPair
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @ApiSigner
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @ApiPublicMeta
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @ApiReq
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      -- , Case checkLegacyValueCompat not required
      ]
    spec_case @AddSigsReq
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.Crypto" $ do
    spec_case @PublicKeyBS
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @PrivateKeyBS
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
    spec_case @SignatureBS
      [ Case checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
