{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pact.Analyze.Types.Model where

import qualified Algebra.Graph             as Alga
import           Control.Lens              (Traversal', _2, makeLenses,
                                            makePrisms)
import           Data.Map.Strict           (Map)
import           Data.SBV                  (SBV)
import           Data.Text                 (Text)
import           GHC.Natural               (Natural)
import           Prelude                   hiding (Float)

import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Util         (SnocList)

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
  | TraceEnforceTree (Located (TagId, SnocList (SnocList TraceEvent)))
  | TraceAssert (Located TagId)
  | TraceAuth (Located TagId)
  | TraceBind (Located (VarId, Text, EType))
  | TraceSubpathStart TagId
  deriving (Eq, Show)

data ExecutionGraph
  = ExecutionGraph
    { _egInitialVertex :: Vertex
    , _egRootPath      :: TagId
    , _egGraph         :: Alga.Graph Vertex
    , _egEdgeEvents    :: Map Edge [TraceEvent]
    , _egPathEdges     :: Map TagId [Edge]
    }
  deriving (Eq, Show)

data Concreteness
  = Concrete
  | Symbolic

data ModelTags (c :: Concreteness)
  = ModelTags
    { _mtVars         :: Map VarId (Located (Text, TVal))
    -- ^ each intermediate variable binding
    , _mtReads        :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each read
    , _mtWrites       :: Map TagId (Located (S RowKey, Object))
    -- ^ one per each write
    , _mtEnforceTrees :: Map TagId (Located (SBV Bool))
    -- ^ one per enforce-one
    , _mtAsserts      :: Map TagId (Located (SBV Bool))
    -- ^ one per non-keyset enforcement
    , _mtAuths        :: Map TagId (Located (S KeySet, SBV Bool))
    -- ^ one per each enforce/auth check. note that this includes all
    -- @(enforce ks)@ and @(enforce-keyset "ks")@ calls.
    , _mtResult       :: Located TVal
    -- ^ return value of the function being checked
    , _mtPaths        :: Map TagId (SBV Bool)
    -- ^ one at the start of the program, and on either side of the branches of
    -- each conditional. after a conditional, the path from before the
    -- conditional is resumed.
    }
  deriving (Eq, Show)

data Model (c :: Concreteness)
  = Model
    { _modelArgs           :: Map VarId (Located (Text, TVal))
    -- ^ one free value per input the function; allocatd post-translation.
    , _modelTags           :: ModelTags c
    -- ^ free values to be constrained to equal values during analysis;
    -- allocated post-translation.
    , _modelKsProvs        :: Map TagId Provenance
    -- ^ keyset 'Provenance's from analysis
    , _modelExecutionGraph :: ExecutionGraph
    -- ^ execution graph corresponding to the program for reporting linearized
    -- traces
    }
  deriving (Eq, Show)

data ExecutionTrace
  = ExecutionTrace
    { _etEvents :: [TraceEvent]
    , _etResult :: Maybe TVal   -- successful result or tx abort
    }

data Goal
  = Satisfaction -- ^ Find satisfying model
  | Validation   -- ^ Prove no invalidating model exists

deriving instance Eq Goal

makePrisms ''TraceEvent
makeLenses ''ExecutionGraph
makeLenses ''ModelTags
makeLenses ''Model

-- TODO: I think we actually want a Prism here: there are 0 or 1 of these at
-- the tree's toplevel.
treeCases :: Traversal' TraceEvent (SnocList (SnocList TraceEvent))
treeCases = _TraceEnforceTree.located._2

caseEvents :: Traversal' (SnocList (SnocList TraceEvent)) TraceEvent
caseEvents = traverse.traverse
