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
  , CapEvalResult(..)
  , SigCapability(..)
  , UserCapability
  , ManagedCapability(..), mcInstalled, mcStatic, mcManaged
  , UserManagedCap(..), umcManagedValue, umcManageParamIndex, umcManageParamName, umcMgrFun
  , decomposeManaged, decomposeManaged', matchManaged
  , Capabilities(..), capStack, capManaged, capModuleAdmin, capAutonomous
  , CapScope(..)
  , CapSlot(..), csCap, csComposed, csScope
  ) where

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=),DefName)
import Data.Aeson
import Data.Default
import Data.Set (Set)
import Data.Text (Text)

import GHC.Generics

import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.PactValue
import Pact.Types.Pretty



data Capability
  = CapModuleAdmin ModuleName
  | CapUser UserCapability
  deriving (Eq,Show,Ord,Generic)
instance NFData Capability

instance Pretty Capability where
  pretty (CapModuleAdmin mn) = pretty mn
  pretty (CapUser s) = pretty s

-- | Both UX type (thus the name) and "UserCapability".
-- TODO rename when downstream deps are more stable.
type UserCapability = SigCapability
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

-- | Various results of evaluating a capability.
-- Note: dupe managed install is an error, thus no case here.
data CapEvalResult
  = NewlyAcquired
  | AlreadyAcquired
  | NewlyInstalled (ManagedCapability UserCapability)
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

instance Pretty CapScope where pretty = viaShow

-- | Runtime storage of acquired or managed capability.
data CapSlot c = CapSlot
  { _csScope :: CapScope
  , _csCap :: c
  , _csComposed :: [c]
  } deriving (Eq,Show,Ord,Functor,Foldable,Traversable,Generic)
instance NFData c => NFData (CapSlot c)

data UserManagedCap = UserManagedCap
  { _umcManagedValue :: PactValue
    -- ^ mutating value
  , _umcManageParamIndex :: Int
    -- ^ index of managed param value
  , _umcManageParamName :: Text
    -- ^ name of managed param value
  , _umcMgrFun :: Def Ref
    -- ^ manager function
  } deriving (Show,Generic)
instance NFData UserManagedCap

data ManagedCapability c = ManagedCapability
  { _mcInstalled :: CapSlot c
    -- ^ original installed capability
  , _mcStatic :: UserCapability
    -- ^ Cap without any mutating components (for auto, same as installed)
  , _mcManaged :: Either Bool UserManagedCap
    -- ^ either auto-managed or user-managed
  } deriving (Show,Generic,Foldable)

-- | Given arg index, split capability args into (before,at,after)
decomposeManaged :: Int -> UserCapability -> Maybe ([PactValue],PactValue,[PactValue])
decomposeManaged idx SigCapability{..}
  | idx < 0 || idx >= length _scArgs = Nothing
  | otherwise = Just (take idx _scArgs,_scArgs !! idx,drop (succ idx) _scArgs)
{-# INLINABLE decomposeManaged #-}

-- | Given arg index, get "static" capability and value
decomposeManaged' :: Int -> UserCapability -> Maybe (SigCapability,PactValue)
decomposeManaged' idx cap@SigCapability{..} = case decomposeManaged idx cap of
  Nothing -> Nothing
  Just (h,v,t) -> Just (SigCapability _scName (h ++ t),v)
{-# INLINABLE decomposeManaged' #-}

-- | Match static value to managed.
matchManaged :: ManagedCapability UserCapability -> UserCapability -> Bool
matchManaged ManagedCapability{..} cap@SigCapability{..} = case _mcManaged of
  Left {} -> _mcStatic == cap
  Right UserManagedCap{..} -> case decomposeManaged' _umcManageParamIndex cap of
    Nothing -> False
    Just (c,_) -> c == _mcStatic
{-# INLINABLE matchManaged #-}

instance Eq a => Eq (ManagedCapability a) where a == b = _mcStatic a == _mcStatic b
instance Ord a => Ord (ManagedCapability a) where a `compare` b = _mcStatic a `compare` _mcStatic b
instance Pretty a => Pretty (ManagedCapability a) where
  pretty ManagedCapability {..} = pretty _mcStatic <> "~" <> mgd _mcManaged
    where mgd (Left b) = pretty b
          mgd (Right UserManagedCap{..}) = pretty _umcManagedValue
instance NFData a => NFData (ManagedCapability a)


-- | Runtime datastructure.
data Capabilities = Capabilities
  { _capStack :: [CapSlot UserCapability]
    -- ^ Stack of "acquired" capabilities.
  , _capManaged :: Set (ManagedCapability UserCapability)
    -- ^ Set of installed managed capabilities. Maybe indicates whether it has been
    -- initialized from signature set.
  , _capModuleAdmin :: (Set ModuleName)
    -- ^ Set of module admin capabilities.
  , _capAutonomous :: (Set UserCapability)
  }
  deriving (Eq,Show,Generic)

instance Default Capabilities where def = Capabilities [] mempty mempty mempty
instance NFData Capabilities

makeLenses ''ManagedCapability
makeLenses ''Capabilities
makeLenses ''CapSlot
makeLenses ''UserManagedCap
