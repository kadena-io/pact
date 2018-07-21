{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pact.Analyze.Types.Model where

import           Control.Lens              (makeLenses)
import           Data.Map.Strict           (Map)
import           Data.SBV                  (Mergeable (symbolicMerge), SBV)
import           Data.Text                 (Text)
import           GHC.Natural               (Natural)
import           Prelude                   hiding (Float)

import qualified Pact.Types.Lang           as Pact
import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Types.Shared

-- | An argument to a function
data Arg = Arg
  { argName  :: Text
  , argVarId :: VarId
  , argNode  :: TC.Node
  , argType  :: EType
  }

data Located a
  = Located
    { _location :: Pact.Info
    , _located  :: a
    }
  deriving (Eq, Functor, Foldable, Traversable)

deriving instance Show a => Show (Located a)

instance Mergeable a => Mergeable (Located a) where
  symbolicMerge f t (Located i a) (Located i' a') =
    Located (symbolicMerge f t i i') (symbolicMerge f t a a')

newtype TagId
  = TagId Natural
  deriving (Num, Show, Ord, Eq)

data ModelTags
  = ModelTags
    { _mtVars   :: Map VarId (Located (Text, TVal))
    -- ^ each intermediate variable binding
    , _mtReads  :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each read, in traversal order
    , _mtWrites :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each write, in traversal order
    , _mtAuths  :: Map TagId (Located (SBV Bool))
    -- ^ one per each enforce/auth check, in traversal order. note that this
    -- includes all (enforce ks) and (enforce-keyset "ks") calls.
    , _mtResult :: Located TVal
    -- ^ return value of the function being checked
    }
  deriving (Eq, Show)

data Model
  = Model
    { _modelArgs    :: Map VarId (Located (Text, TVal))
    -- ^ one free value per input the function; allocatd post-translation.
    , _modelTags    :: ModelTags
    -- ^ free values to be constrained to equal values during analysis;
    -- allocated post-translation.
    , _modelKsProvs :: Map TagId Provenance
    -- ^ keyset 'Provenance's from analysis
    }
  deriving (Eq, Show)

data Goal
  = Satisfaction -- ^ Find satisfying model
  | Validation   -- ^ Prove no invalidating model exists

deriving instance Eq Goal

makeLenses ''Located
makeLenses ''ModelTags
makeLenses ''Model
