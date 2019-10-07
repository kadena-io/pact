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
  , parseSigCapability
  , Capabilities(..), capStack, capManaged, capSigMatched
  , CapScope(..)
  , CapSlot(..), csCap, csComposed, csScope
  ) where

import Control.DeepSeq (NFData)
import Control.Error (fmapL)
import Control.Lens hiding ((.=),DefName)
import Data.Aeson
import Data.Default
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, unpack)

import GHC.Generics

import Pact.Compile
import Pact.Parse (parsePact)
import Pact.Types.Lang
import Pact.Types.Orphans ()
import Pact.Types.PactValue
import Pact.Types.Pretty



data Capability
  = ModuleAdminCapability ModuleName
  | UserCapability QualifiedName [PactValue]
  deriving (Eq,Show,Ord)

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

instance ToJSON SigCapability
  where toJSON = toJSON . renderCompactText

instance FromJSON SigCapability where
  parseJSON = withText "SigCapability" $ \t -> case parseSigCapability t of
    Right c -> return c
    Left e -> fail e

parseSigCapability :: Text -> Either String SigCapability
parseSigCapability txt = parsed >>= compiled >>= parseApp
  where
    parseApp ts = case ts of
      [(TApp (App (TVar (QName q) _) as _) _)] -> SigCapability q <$> mapM toPV as
      _ -> Left $ "Sig capability parse failed: Expected single qualified capability in form (qual.DEFCAP arg arg ...)"
    compiled ParsedCode{..} = fmapL (("Sig capability parse failed: " ++) . show) $
      compileExps (mkTextInfo _pcCode) _pcExps
    parsed = parsePact txt
    toPV a = fmapL (("Sig capability argument parse failed, expected simple pact value: " ++) . unpack) $ toPactValue a

-- | Literate boolean to signal whether cap was already in scope.
data CapAcquireResult
  = NewlyAcquired
  | AlreadyAcquired
  deriving (Eq,Show)

data CapScope m
  = CapCallStack
    -- ^ Call stack scope a la 'with-capability'
  | CapManaged m
    -- ^ Managed-scope capability
  | CapComposed
    -- ^ Composed into some other capability
  deriving (Eq,Show,Ord,Functor,Foldable,Traversable)

data CapSlot c = CapSlot
  { _csScope :: CapScope (Maybe (Def Ref))
  , _csCap :: c
  , _csComposed :: [c]
  } deriving (Eq,Show,Ord,Functor,Foldable,Traversable)
makeLenses ''CapSlot

data Capabilities c = Capabilities
  { _capStack :: [CapSlot c]
  , _capManaged :: [CapSlot c]
  , _capSigMatched :: (Map PublicKey (Set Capability))
  }
  deriving (Eq,Show,Functor,Foldable,Traversable)
makeLenses ''Capabilities

instance Default (Capabilities a) where def = Capabilities [] [] mempty
