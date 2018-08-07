{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pact.Analyze.Types.Model where

import qualified Algebra.Graph             as Alga
import           Control.Lens              (makeLenses, makePrisms)
import           Data.Map.Strict           (Map)
import           Data.SBV                  (SBV)
import           Data.Text                 (Text)
import           GHC.Natural               (Natural)
import           Prelude                   hiding (Float)

import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Types.Shared

-- | An argument to a function
data Arg = Arg
  { argName  :: Text
  , argVarId :: VarId
  , argNode  :: TC.Node
  , argType  :: EType
  }

newtype TagId
  = TagId Natural
  deriving (Num, Enum, Show, Ord, Eq)

type Edge = (Vertex, Vertex)

newtype Vertex
  = Vertex Natural
  deriving (Num, Enum, Show, Ord, Eq)

data TraceEvent
  = TraceRead (Located (TagId, Schema))
  | TraceWrite (Located (TagId, Schema))
  | TraceEnforce (Located TagId)
  | TraceBind (Located (VarId, Text, EType))
  | TraceCheckpoint TagId
  deriving (Show)

data ExecutionGraph
  = ExecutionGraph
    { _egInitialVertex     :: Vertex
    , _egGraph             :: Alga.Graph Vertex
    , _egEdgeEvents        :: Map Edge [TraceEvent]
    , _egCheckpointEdges   :: Map TagId [Edge]
    }

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

makePrisms ''TraceEvent
makeLenses ''ExecutionGraph
makeLenses ''ModelTags
makeLenses ''Model
