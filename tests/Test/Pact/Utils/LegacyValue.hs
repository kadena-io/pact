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
import Pact.Types.RowData
import Pact.Types.Runtime
import Pact.Types.Scheme
import Pact.Types.SigData
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Types.Term.Arbitrary ()
import Pact.PersistPactDb

import Pact.JSON.Legacy.Value

import qualified Pact.JSON.Encode as J

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

prop_checkRoundtrip2 :: Eq a => Show a => J.Encode a => FromJSON a => a -> Property
prop_checkRoundtrip2 a = eitherDecode (J.encode a) === Right a

checkRoundtrip2 :: Eq a => Show a => J.Encode a => FromJSON a => (String, a -> Property)
checkRoundtrip2 = ("roundtrip2", prop_checkRoundtrip2)

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
prop_checkLegacyValueCompat :: ToJSON a => a -> Property
prop_checkLegacyValueCompat a = encode a === J.encode (toLegacyJson a)

checkLegacyValueCompat :: ToJSON a => (String, a -> Property)
checkLegacyValueCompat =
  ("encoding is compatible with toLegacyJson encoding", prop_checkLegacyValueCompat)

-- -------------------------------------------------------------------------- --
--

prop_checkAesonCompat :: ToJSON a => J.Encode a => a -> Property
prop_checkAesonCompat a = encode a === J.encode a

checkAesonCompat :: ToJSON a => J.Encode a => (String, a -> Property)
checkAesonCompat =
  ("encoding is compatible with Aeson encoding", prop_checkAesonCompat)

-- -------------------------------------------------------------------------- --
-- Tools

data Case a
  = Case !(Eq a => Show a => FromJSON a => ToJSON a => (String, a -> Property))
  | Pending !(Eq a => Show a => FromJSON a => ToJSON a => (String, a -> Property))

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
      [ Pending checkRoundtrip
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
      [ Pending checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @GasPrice
      [ Pending checkRoundtrip
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
      [ Pending checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PublicData
      [ Pending checkRoundtrip
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
      [ Pending checkRoundtrip
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- ---------------------------------------------- --
spec_pact_types_continuation :: Spec
spec_pact_types_continuation =
  describe "Pact.Types.Continuation" $ do
   spec_case @Provenance
      [ Case checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @Yield
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactContinuation
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @NestedPactExec
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PactExec
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @(ModuleData (A ()))
      [ Pending checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @PersistModuleData
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   -- spec_case @RowKey [ ]
   spec_case @(Ref' PersistDirect)
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]


-- ---------------------------------------------- --
spec_pact_types_rowData :: Spec
spec_pact_types_rowData =
  describe "Pact.Types.RowData" $ do
    spec_case @RowDataValue
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Pending checkRoundtrip2
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
      [ Pending checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]
   spec_case @SQLiteConfig
      [ Pending checkRoundtrip
      , Case checkRoundtrip2
      , Case checkAesonCompat
      , Case checkLegacyValueCompat
      ]

-- -------------------------------------------------------------------------- --
--

spec :: Spec
spec = describe "JSON encoding backward compatibility" $ do

  spec_pact_types_orphans

  -- ---------------------------------------------- --
  describe "Pact.Types.PactError" $ do
   spec_case @PactErrorType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @StackFrame
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @PactError
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @OutputType
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @RenderedOutput
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]

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

  -- ---------------------------------------------- --
  describe "Pact.Types.Command" $ do
   spec_case @UserSig
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @(Command ())
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @Signer
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @(Payload () ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @PactResult
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @(CommandResult ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @RequestKey
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]

  -- ---------------------------------------------- --
  describe "Pact.Types.SigData" $ do
   spec_case @PublicKeyHex
      [ Case checkRoundtrip
      , Case checkLegacyValueCompat
      ]
   spec_case @(SigData ())
      [ Pending checkRoundtrip
      , Case checkLegacyValueCompat
      ]

  spec_pact_types_rowData
  spec_pact_persistPactDb
  spec_pact_types_sqlite
