{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Types.Namespace
( Namespace(..), nsName, nsUser, nsAdmin
, NamespacePolicy(..)
, permissiveNamespacePolicy
) where

import Control.DeepSeq
import Control.Lens

import Data.Aeson


import Pact.Types.Names
import Pact.Types.Term
import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Types.Util

import GHC.Generics

import Test.QuickCheck

-- -------------------------------------------------------------------------- --
-- Namespace

data Namespace a = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard a)
  , _nsAdmin :: !(Guard a)
  } deriving (Eq, Show, Generic)
makeLenses ''Namespace

instance (Arbitrary a) => Arbitrary (Namespace a) where
  arbitrary = Namespace <$> arbitrary <*> arbitrary <*> arbitrary

instance Pretty (Namespace a) where
  pretty Namespace{..} = "(namespace " <> prettyString (asString' _nsName) <> ")"

instance (SizeOf n) => SizeOf (Namespace n) where
  sizeOf ver (Namespace name ug ag) =
    (constructorCost 3) + (sizeOf ver name) + (sizeOf ver ug) + (sizeOf ver ag)

instance (ToJSON a, FromJSON a) => ToJSON (Namespace a) where toJSON = lensyToJSON 3
instance (FromJSON a, ToJSON a) => FromJSON (Namespace a) where parseJSON = lensyParseJSON 3

instance (NFData a) => NFData (Namespace a)

-- | Governance of namespace use. Policy dictates:
-- 1. Whether a namespace can be created.
-- 2. Whether the default namespace can be used.
data NamespacePolicy
  = SimpleNamespacePolicy (Maybe (Namespace (Term Name)) -> Bool)
  -- ^ if namespace is Nothing/root, govern usage; otherwise govern creation.
  | SmartNamespacePolicy Bool QualifiedName
  -- ^ Bool governs root usage, Name governs ns creation.
  -- Def is (defun xxx:bool (ns:string ns-admin:guard))

permissiveNamespacePolicy :: NamespacePolicy
permissiveNamespacePolicy = SimpleNamespacePolicy $ const True
