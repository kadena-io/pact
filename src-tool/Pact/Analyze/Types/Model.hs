{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Types specific to models, tags, and execution graphs and traces.
module Pact.Analyze.Types.Model where

import qualified Algebra.Graph             as Alga
import           Control.Lens              (makeLenses, makePrisms)
import           Data.Map.Strict           (Map)
import           Data.SBV                  (SBV)
import           Data.Text                 (Text)
import           GHC.Natural               (Natural)
import           Prelude                   hiding (Float)

import           Pact.Types.Persistence    (WriteType)
import qualified Pact.Types.Typecheck      as TC
import qualified Pact.Types.Lang           as Pact

import           Pact.Analyze.Types.Shared

-- | An argument to a function
data Arg = Arg
  { argName  :: Unmunged
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

newtype Path
  = Path { _pathTag :: TagId }
  deriving (Eq, Ord, Show)

data Recoverability
  -- | The path upon which to resume inclusion of events. An alternative here
  -- would be to have more edges in the subgraphs formed by @enforce-one@ --
  -- have vertices not just for each case, but additionally one after each
  -- recoverable assert/auth as well, to connect from right after the
  -- assert/auth to the next case. One would need to be careful here not to try
  -- to make two edges between a pair of vertices when an assert/auth in a
  -- preceeding case is right before the next case. We need to explicitly talk
  -- about the path upon which to resume execution because it's not necessarily
  -- the next vertex -- there could be more subpaths before where we should
  -- resume due to nested conditionals or @enforce-one@s.
  = Recoverable { _resumptionPath :: Path }
  | Unrecoverable
  deriving (Eq, Show)

-- For determining resumption paths for nested @enforce-one@s:
instance Semigroup Recoverability where
  --      outer <> inner
  Recoverable _ <> Recoverable y = Recoverable y
  Recoverable x <> Unrecoverable = Recoverable x
  Unrecoverable <> nested        = nested

instance Monoid Recoverability where
  mempty = Unrecoverable
  mappend = (<>)

data ScopeType
  = LetScope
  | ObjectScope
  | StepScope
  | RollbackScope
  | FunctionScope Pact.ModuleName Text
  | PactScope Pact.ModuleName Text
  | CapabilityScope Pact.ModuleName CapName
  deriving (Eq, Show)

data TraceEvent
  = TraceRead TableName ESchema (Located TagId)
  | TraceWrite WriteType TableName ESchema (Located TagId)
  | TraceAssert Recoverability (Located TagId)
  | TraceGuard Recoverability (Located TagId)
  | TraceSubpathStart Path
  | TracePushScope Natural ScopeType [Located Binding]
  | TracePopScope Natural ScopeType TagId EType
  | TraceRequireGrant Recoverability CapName [Located Binding] (Located TagId)
  | TraceYield EType TagId
  | TraceResume EType TagId
  | TraceReset
  | TraceCancel TagId
  deriving (Eq, Show)

-- | An @ExecutionGraph@ is produced by translation, and contains all
-- information about where 'TraceEvent's occur in the program. From a
-- @Model 'Concrete@ (which contains this type), we can produce a linear trace
-- of events for a concrete execution, in the form of 'ExecutionTrace'. For
-- more information about how this graph is constructed, see 'TranslateState'.
data ExecutionGraph
  = ExecutionGraph
    { _egInitialVertex :: Vertex
    , _egRootPath      :: Path
    , _egGraph         :: Alga.Graph Vertex
    , _egEdgeEvents    :: Map Edge [TraceEvent]
    , _egPathEdges     :: Map Path [Edge]
    }
  deriving (Eq, Show)

data Concreteness
  = Concrete
  | Symbolic

data Access
  = Access
    { _accRowKey  :: S RowKey
    , _accObject  :: UObject
    , _accSuccess :: SBV Bool
    }
  deriving (Eq, Show)

data GuardEnforcement
  = GuardEnforcement
    { _geGuard   :: S Guard
    , _geSuccess :: SBV Bool
    }
  deriving (Eq, Show)

data GrantRequest
  = GrantRequest
    { _grCapability :: CapName
    -- TODO: args, probably.
    , _grSuccess    :: SBV Bool
    }
  deriving (Eq, Show)

data ModelTags (c :: Concreteness)
  = ModelTags
    { _mtVars              :: Map VarId (Located (Unmunged, TVal))
    -- ^ each intermediate variable binding
    , _mtReads             :: Map TagId (Located Access)
    -- ^ one per each read
    , _mtWrites            :: Map TagId (Located Access)
    -- ^ one per each write
    , _mtYields            :: Map TagId TVal
    -- ^ one per each yield
    , _mtResumes           :: Map TagId TVal
    -- ^ one per each resume
    , _mtAsserts           :: Map TagId (Located (SBV Bool))
    -- ^ one per non-keyset enforcement
    , _mtGuardEnforcements :: Map TagId (Located GuardEnforcement)
    -- ^ one per each guard enforcement.
    , _mtGrantRequests     :: Map TagId (Located GrantRequest)
    -- ^ one per each require-capability call
    , _mtResult            :: (TagId, Located TVal)
    -- ^ return value of the function being checked
    , _mtPaths             :: Map Path (SBV Bool)
    -- ^ one at the start of the program, and on either side of the branches of
    -- each conditional. after a conditional, the path from before the
    -- conditional is resumed. we also split execution for each case of
    -- @enforce-one@.
    , _mtReturns           :: Map TagId TVal
    -- ^ return values from function calls
    , _mtCancels           :: Map TagId (SBV Bool)
    -- ^ whether each cancel fires
    }
  deriving (Eq, Show)

data Model (c :: Concreteness)
  = Model
    { _modelArgs           :: Map VarId (Located (Unmunged, TVal))
    -- ^ one free value per input the function; allocatd post-translation.
    , _modelTags           :: ModelTags c
    -- ^ free values to be constrained to equal values during analysis;
    -- allocated post-translation.
    , _modelGuardProvs     :: Map TagId Provenance
    -- ^ keyset 'Provenance's from analysis
    , _modelExecutionGraph :: ExecutionGraph
    -- ^ execution graph corresponding to the program for reporting linearized
    -- traces
    }
  deriving (Eq, Show)

-- | A linearized trace of 'TraceEvent's, derived from a @Model 'Concrete@.
-- This is used for presentation purposes.
data ExecutionTrace
  = ExecutionTrace
    { _etEvents :: [TraceEvent]
    , _etResult :: Maybe TVal   -- successful result or tx abort
    }

data Goal
  = Satisfaction -- ^ Find satisfying model
  | Validation   -- ^ Prove no invalidating model exists
  deriving (Eq, Show)

makePrisms ''TraceEvent
makeLenses ''ExecutionGraph
makeLenses ''Access
makeLenses ''GuardEnforcement
makeLenses ''GrantRequest
makeLenses ''ModelTags
makeLenses ''Model
