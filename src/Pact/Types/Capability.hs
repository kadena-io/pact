{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Pact.Types.Capability
-- Copyright   :  (C) 2019 Stuart Popejoy, Kadena LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Capability and related types.
--

module Pact.Types.Capability
  ( Capability(..)
  , CapabilityType(..), capType
  , CapAcquireResult(..)
  , SigCapability(..)
  , Capabilities(..), capStack, capManaged, capManagedSeen
  , CapScope(..)
  , CapSlot(..), csCap, csComposed, csScope
  ) where

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=),DefName)
import Data.Aeson
import Data.Default
import Data.Set (Set)

import GHC.Generics

import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.PactValue
import Pact.Types.Pretty



data Capability
  = ModuleAdminCapability ModuleName
  | UserCapability QualifiedName [PactValue]
  deriving (Eq,Show,Ord,Generic)
instance NFData Capability

data CapabilityType
  = CapTypeModuleAdmin ModuleName
  | CapTypeUser QualifiedName -- TODO add args for full type
  deriving (Eq,Show,Ord)

capType :: Capability -> CapabilityType
capType (ModuleAdminCapability m) = CapTypeModuleAdmin m
capType (UserCapability qn _) = CapTypeUser qn

instance Pretty Capability where
  pretty (ModuleAdminCapability mn) = pretty mn
  pretty (UserCapability n tms)  = parensSep (pretty n : fmap pretty tms)

data SigCapability = SigCapability
  { _scName :: !QualifiedName
  , _scArgs :: ![PactValue]
  } deriving (Eq,Show,Generic,Ord)
instance NFData SigCapability

instance Pretty SigCapability where
  pretty SigCapability{..} = parens $ hsep (pretty _scName:map pretty _scArgs)

instance ToJSON SigCapability where
  toJSON (SigCapability n args) = object $
    [ "name" .=  n
    , "args" .= args
    ]

instance FromJSON SigCapability where
  parseJSON = withObject "SigCapability" $ \o -> SigCapability
    <$> o .: "name"
    <*> o .: "args"

-- | Literate boolean to signal whether cap was already in scope.
data CapAcquireResult
  = NewlyAcquired
  | AlreadyAcquired
  deriving (Eq,Show)

data CapScope
  = CapCallStack
    -- ^ Call stack scope a la 'with-capability'
  | CapManaged
    -- ^ Managed-scope capability
  | CapComposed
    -- ^ Composed into some other capability
  deriving (Eq,Show,Ord,Generic)
instance NFData CapScope

instance Pretty CapScope where
  pretty CapManaged {} = "CapManaged"
  pretty CapCallStack = "CapCallStack"
  pretty CapComposed = "CapComposed"

-- | Runtime storage of capability.
data CapSlot c = CapSlot
  { _csScope :: CapScope
  , _csCap :: c
  , _csComposed :: [c]
  } deriving (Eq,Show,Ord,Functor,Foldable,Traversable,Generic)
makeLenses ''CapSlot
instance NFData c => NFData (CapSlot c)

-- | Runtime datastructure.
data Capabilities = Capabilities
  { _capStack :: [CapSlot Capability]
    -- ^ Stack of "acquired" capabilities.
  , _capManaged :: Set (CapSlot Capability)
    -- ^ Set of managed capabilities.
  , _capManagedSeen :: (Set Capability)
    -- ^ Record of managed granted capabilities.
  }
  deriving (Eq,Show,Generic)
makeLenses ''Capabilities

instance NFData Capabilities
instance Default Capabilities where def = Capabilities [] mempty mempty
