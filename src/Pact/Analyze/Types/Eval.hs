{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Type definitions and constructors related to symbolic evaluation.
module Pact.Analyze.Types.Eval where

import           Control.Applicative          (ZipList (..))
import           Control.Lens                 (Lens', at, ifoldl, iso, ix, lens,
                                               makeLenses, singular, view, (&),
                                               (.~), (<&>), (?~))
import           Control.Lens.Wrapped
import           Control.Monad.Except         (MonadError)
import           Control.Monad.Reader         (MonadReader)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (mapMaybe)
import           Data.SBV                     (HasKind,
                                               Mergeable (symbolicMerge), SBV,
                                               SBool,
                                               SymArray (readArray, writeArray),
                                               SymVal, uninterpret)
import qualified Data.SBV.Internals           as SBVI
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable             (for)
import           Data.Type.Equality           ((:~:)(Refl))
import           GHC.Generics                 hiding (S)
import           GHC.Stack                    (HasCallStack)

import           Pact.Types.Lang              (Info)
import           Pact.Types.Runtime           (Type (TyPrim))
import qualified Pact.Types.Runtime           as Pact
import qualified Pact.Types.Typecheck         as Pact

import           Pact.Analyze.Errors
import           Pact.Analyze.LegacySFunArray (SFunArray, mkSFunArray)
import           Pact.Analyze.Translate       (maybeTranslateType,
                                               maybeTranslateUserType')
import           Pact.Analyze.Types           hiding (tableName)
import qualified Pact.Analyze.Types           as Types
import           Pact.Analyze.Util

newtype SymbolicSuccess = SymbolicSuccess { successBool :: SBV Bool }
  deriving (Show, Generic, Mergeable)

instance Boolean SymbolicSuccess where
  sTrue = SymbolicSuccess sTrue
  sNot = SymbolicSuccess . sNot . successBool
  SymbolicSuccess x .&& SymbolicSuccess y = SymbolicSuccess (x .&& y)

instance Wrapped SymbolicSuccess where
  type Unwrapped SymbolicSuccess = SBV Bool
  _Wrapped' = iso successBool SymbolicSuccess

class (MonadError AnalyzeFailure m, S :*<: TermOf m) => Analyzer m where
  type TermOf m   :: Ty -> *
  eval            :: SingI a => TermOf m a -> m (S (Concrete a))
  throwErrorNoLoc :: AnalyzeFailureNoLoc   -> m a
  getVar          :: VarId                 -> m (Maybe AVal)
  withVar         :: VarId -> AVal -> m a  -> m a
  markFailure     :: SBV Bool              -> m ()
  withMergeableAnalyzer
    :: SingTy a
    -> (( Mergeable (m (S (Concrete a)))
        , Mergeable (m (SBV (Concrete a)))
        ) => b)
    -> b

-- | Expect a 'Just' in an 'Analyzer'.
(??)
  :: Analyzer m
  => Maybe a -> AnalyzeFailureNoLoc -> m a
Just a  ?? _   = pure a
Nothing ?? err = throwErrorNoLoc err
infix 1 ??

-- | Expect a computation to evaluate to a 'Just' in an 'Analyzer'.
(???) :: Analyzer m => m (Maybe a) -> AnalyzeFailureNoLoc -> m a
m ??? err = do
  m' <- m
  m' ?? err
infix 1 ???

data PactMetadata
  = PactMetadata
    { _pmInPact  :: S Bool
    , _pmPactId  :: S Str
    , _pmEntity  :: S Str
    }
  deriving Show

-- | An existentially wrapped symbolic value that might not always be set.
-- Consider a value that might only be set on one side of a conditional. We
-- can't "just use" 'EVal' for this case because we need 'a' to be of kind
-- 'Ty', and 'Ty' doesn't have a notion of Maybe-ness.
data EPossibleVal where
  PossibleVal :: SingTy a -> S (Maybe (Concrete a)) -> EPossibleVal

instance Mergeable EPossibleVal where
  symbolicMerge force test (PossibleVal lTy lVal) (PossibleVal rTy rVal)
    = case singEq lTy rTy of
      Nothing   -> error $ "failed symbolic merge with different types: " ++
        show lTy ++ " vs " ++ show rTy
      Just Refl -> PossibleVal lTy $ withSymVal lTy $
        symbolicMerge force test lVal rVal

instance Show EPossibleVal where
  showsPrec p (PossibleVal ty s)
    = showParen (p > 10)
    $ showString "PossibleVal "
    . showsPrec 11 ty
    . showChar ' '
    . showsPrec 11 s

-- | An existentially-wrapped symbolic value.
data EVal where
  SomeVal :: SingTy a -> S (Concrete a) -> EVal

instance Show EVal where
  showsPrec p (SomeVal ty s)
    = showParen (p > 10)
    $ showString "SomeVal "
    . showsPrec 11 ty
    . showChar ' '
    . showsPrec 11 s

-- We just use `uninterpret` for now until we need to extract these values from
-- the model.
mkPactMetadata :: PactMetadata
mkPactMetadata = PactMetadata
  (uninterpretS "in_pact")
  (uninterpretS "pact_id")
  (uninterpretS "entity")

-- | The registry of names to keysets that is shared across multiple modules in
-- a namespace.
newtype Registry
  = Registry
    { _registryMap :: SFunArray RegistryName Guard }
  deriving Show

mkRegistry :: Registry
mkRegistry = Registry $ mkFreeArray "registry"

resolveGuardFromReg :: Registry -> S RegistryName -> S Guard
resolveGuardFromReg reg sRn = withProv (fromRegistry sRn) $
  readArray (_registryMap reg) (_sSbv sRn)

data TxMetadata
  = TxMetadata
    { _tmKeySets  :: SFunArray Str Guard
    , _tmDecimals :: SFunArray Str Decimal
    , _tmIntegers :: SFunArray Str Integer
    , _tmStrings  :: SFunArray Str Str
    }
  deriving Show

data AnalyzeEnv
  = AnalyzeEnv
    { _aeModuleName    :: Pact.ModuleName
    , _aePactMetadata  :: PactMetadata
    , _aeRegistry      :: Registry
    , _aeTxMetadata    :: TxMetadata
    , _aeScope         :: Map VarId AVal -- used as a stack
    , _aeStepChoices   :: Map VarId (SBV Bool)
    , _invariants      :: TableMap [Located (Invariant 'TyBool)]
    , _aeTableSchemas  :: TableMap EType
    , _aeColumnIds     :: TableMap (Map Text VarId)
    , _aeModelTags     :: ModelTags 'Symbolic
    , _aeInfo          :: Info
    , _aeTrivialGuard  :: S Guard
    , _aeModuleGuard   :: S Guard
    , _aeEmptyGrants   :: TokenGrants
    -- ^ the default, blank slate of grants, where no token is granted.
    , _aeActiveGrants  :: TokenGrants
    -- ^ the current set of tokens that are granted, manipulated as a stack
    , _aeTables        :: [Table]
    , _aeDbRestriction :: Maybe DbRestriction
    } deriving Show

mkAnalyzeEnv
  :: Pact.ModuleName
  -> PactMetadata
  -> Governance
  -> Registry
  -> [Table]
  -> [Capability]
  -> Map VarId AVal
  -> Map VarId (SBV Bool)
  -> ModelTags 'Symbolic
  -> Info
  -> Maybe AnalyzeEnv
mkAnalyzeEnv modName pactMetadata gov registry tables caps args stepChoices tags info = do
  let txMetadata   = TxMetadata (mkFreeArray "txKeySets")
                                (mkFreeArray "txDecimals")
                                (mkFreeArray "txIntegers")
                                (mkFreeArray "txStrings")
      --
      -- NOTE: for now we create an always-passing singleton "trivial guard"
      -- that we hand out for pact and module creation. this will not suffice
      -- as soon as we need to share pact and module guards across pacts and
      -- modules. at this point, we'll need to replace our opaque (map to Bool)
      -- _aeGuardPasses with map to a symbolic sum capturing different guard
      -- types. this is because we we will need to "close over" the pact id or
      -- module name at the time of creation and compare that with pact id /
      -- module name during symbolic execution
      --
      trivialGuard = uninterpret "trivial_guard"

      invariants' = TableMap $ Map.fromList $ tables <&>
        \(Table tname _ut someInvariants) ->
          (TableName (T.unpack tname), someInvariants)

  tableETys <- for tables $ \(Table tname ut _) ->
    case maybeTranslateUserType' ut of
      Just ety ->
        Just (TableName (T.unpack tname), ety)
      _ ->
        Nothing

  let tableSchemas :: TableMap EType
      tableSchemas = TableMap $ Map.fromList tableETys

  columnIds' <- for tableSchemas $ \ety -> do
    EType (SObject ty) <- pure ety
    pure $ varIdColumns ty

  let emptyGrants  = mkTokenGrants caps
      activeGrants = emptyGrants
      modGuard     = case gov of
                       KsGovernance registryName ->
                         resolveGuardFromReg registry $ literalS registryName
                       CapGovernance _capName ->
                         --
                         -- NOTE: for now we do not try to interpret
                         -- capability-based governance because it is quite
                         -- difficult in the presence of capabilities that
                         -- perform database access.
                         --
                         withProv fromGovCap $ uninterpret "cap_gov_guard"

  pure $ AnalyzeEnv modName pactMetadata registry txMetadata args stepChoices
    invariants' tableSchemas columnIds' tags info (sansProv trivialGuard)
    modGuard emptyGrants activeGrants tables mempty

mkFreeArray :: (SymVal a, HasKind b) => Text -> SFunArray a b
mkFreeArray = mkSFunArray . uninterpret . T.unpack . sbvIdentifier

sbvIdentifier :: Text -> Text
sbvIdentifier = T.replace "-" "_"

data QueryEnv
  = QueryEnv
    { _qeAnalyzeEnv    :: AnalyzeEnv
    , _qeAnalyzeState  :: QueryAnalyzeState
    , _qeAnalyzeResult :: AVal
    , _qeTableScope    :: Map VarId TableName
    , _qeColumnScope   :: Map VarId ColumnName
    }

data SymbolicCells
  = SymbolicCells { _scValues :: ColumnMap (EValSFunArray RowKey) }
  deriving (Show)

instance Mergeable SymbolicCells where
  symbolicMerge force test (SymbolicCells left) (SymbolicCells right)
    = SymbolicCells $ symbolicMerge force test left right

-- In evaluation, we have one copy of @CellValues@, which we update as cells
-- are written. For querying we have two copies, representing the db state
-- before and after the transaction being analyzed.
data CellValues
  = CellValues
    { _cvTableCells :: TableMap SymbolicCells
    , _cvRowExists  :: TableMap (SFunArray RowKey Bool)
    }
  deriving (Generic, Show)

deriving instance Mergeable CellValues

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState a
  = LatticeAnalyzeState
    { _lasSucceeds            :: SymbolicSuccess
    , _lasPurelyReachable     :: SBV Bool
    --
    -- TODO: instead of having a single boolean here, we should probably use
    --       finer-grained tracking, so that we can test whether a single
    --       invariant is being maintained
    --
    , _lasMaintainsInvariants :: TableMap (ZipList (Located (SBV Bool)))
    , _lasGuardPasses         :: SFunArray Guard Bool
    , _lasTablesRead          :: SFunArray TableName Bool
    , _lasTablesWritten       :: SFunArray TableName Bool
    , _lasColumnsRead         :: TableMap (ColumnMap (SBV Bool))
    , _lasColumnsWritten      :: TableMap (ColumnMap (SBV Bool))
    , _lasIntCellDeltas       :: TableMap (ColumnMap (SFunArray RowKey Integer))
    , _lasDecCellDeltas       :: TableMap (ColumnMap (SFunArray RowKey Decimal))
    , _lasIntColumnDeltas     :: TableMap (ColumnMap (S Integer))
    , _lasDecColumnDeltas     :: TableMap (ColumnMap (S Decimal))
    , _lasRowsRead            :: TableMap (SFunArray RowKey Integer)
    , _lasRowsWritten         :: TableMap (SFunArray RowKey Integer)
    , _lasCellsEnforced       :: TableMap (ColumnMap (SFunArray RowKey Bool))
    -- We currently maintain cellsWritten only for deciding whether a cell has
    -- been "invalidated" for the purposes of keyset enforcement. If a keyset
    -- has been overwritten and *then* enforced, that does not constitute valid
    -- enforcement of the keyset.
    , _lasCellsWritten        :: TableMap (ColumnMap (SFunArray RowKey Bool))
    , _lasConstraints         :: S Bool
    , _lasPendingGrants       :: TokenGrants
    , _lasYieldedInPrevious   :: Maybe EPossibleVal
    , _lasYieldedInCurrent    :: Maybe EPossibleVal
    , _lasExtra               :: a
    }
  deriving (Generic, Show)

deriving instance Mergeable a => Mergeable (LatticeAnalyzeState a)

-- Checking state that is transferred through every computation, in-order.
data GlobalAnalyzeState
  = GlobalAnalyzeState
    { _gasGuardProvenances :: Map TagId Provenance -- added as we accum guard info
    , _gasRollbacks        :: [ETerm]
    -- ^ the stack of rollbacks to perform on failure
    , _gasCachedChainData  :: Maybe EVal
    }
  deriving (Show)

data AnalyzeState a
  = AnalyzeState
    { _latticeState :: LatticeAnalyzeState a
    , _globalState  :: GlobalAnalyzeState
    }
  deriving (Show)

data BeforeAndAfter = BeforeAndAfter
  { _before :: CellValues
  , _after  :: CellValues
  }

-- See note on @CellValues@
type QueryAnalyzeState = AnalyzeState BeforeAndAfter
type EvalAnalyzeState  = AnalyzeState CellValues

data AnalysisResult
  = AnalysisResult
    { _arEvalSuccess   :: SymbolicSuccess
    , _arTxSuccess     :: SBV Bool
    , _arProposition   :: SBV Bool
    , _arKsProvenances :: Map TagId Provenance
    }
  deriving (Show)

makeLenses ''TxMetadata
makeLenses ''PactMetadata
makeLenses ''Registry
makeLenses ''AnalyzeEnv
makeLenses ''AnalyzeState
makeLenses ''BeforeAndAfter
makeLenses ''CellValues
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells
makeLenses ''AnalysisResult
makeLenses ''QueryEnv

mkQueryEnv
  :: AnalyzeEnv
  -> AnalyzeState ()
  -> CellValues
  -> CellValues
  -> AVal
  -> QueryEnv
mkQueryEnv env state cv0 cv1 result =
  let state' = state & latticeState . lasExtra .~ BeforeAndAfter cv0 cv1
  in QueryEnv env state' result Map.empty Map.empty

mkInitialAnalyzeState :: SBV Guard -> [Table] -> [Capability] -> EvalAnalyzeState
mkInitialAnalyzeState trivialGuard tables caps = AnalyzeState
    { _latticeState = LatticeAnalyzeState
        { _lasSucceeds            = SymbolicSuccess sTrue
        , _lasPurelyReachable     = sTrue
        , _lasMaintainsInvariants = mkMaintainsInvariants
        , _lasGuardPasses         = writeArray (mkFreeArray "guardPasses") trivialGuard sTrue
        , _lasTablesRead          = mkSFunArray $ const sFalse
        , _lasTablesWritten       = mkSFunArray $ const sFalse
        , _lasColumnsRead         = mkTableColumnMap tables (const True) sFalse
        , _lasColumnsWritten      = mkTableColumnMap tables (const True) sFalse
        , _lasIntCellDeltas       = intCellDeltas
        , _lasDecCellDeltas       = decCellDeltas
        , _lasIntColumnDeltas     = intColumnDeltas
        , _lasDecColumnDeltas     = decColumnDeltas
        , _lasRowsRead            = mkPerTableSFunArray 0
        , _lasRowsWritten         = mkPerTableSFunArray 0
        , _lasCellsEnforced       = cellsEnforced
        , _lasCellsWritten        = cellsWritten
        , _lasConstraints         = sansProv sTrue
        , _lasPendingGrants       = mkTokenGrants caps
        , _lasYieldedInPrevious   = Nothing
        , _lasYieldedInCurrent    = Nothing
        , _lasExtra               = CellValues
          { _cvTableCells = mkSymbolicCells tables
          , _cvRowExists  = mkRowExists
          }
        }
    , _globalState = GlobalAnalyzeState
        { _gasGuardProvenances = mempty
        , _gasRollbacks        = []
        , _gasCachedChainData  = Nothing
        }
    }

  where
    tableNames :: [TableName]
    tableNames = map (TableName . T.unpack . view Types.tableName) tables

    intCellDeltas   = mkTableColumnMap tables (== TyPrim Pact.TyInteger) (mkSFunArray (const 0))
    decCellDeltas   = mkTableColumnMap tables (== TyPrim Pact.TyDecimal) (mkSFunArray (const (fromInteger 0)))
    intColumnDeltas = mkTableColumnMap tables (== TyPrim Pact.TyInteger) 0
    decColumnDeltas = mkTableColumnMap tables (== TyPrim Pact.TyDecimal) (fromInteger 0)
    cellsEnforced   = mkTableColumnMap tables isGuardTy (mkSFunArray (const sFalse))
    cellsWritten    = mkTableColumnMap tables (const True) (mkSFunArray (const sFalse))

    mkPerTableSFunArray :: SBV v -> TableMap (SFunArray k v)
    mkPerTableSFunArray defaultV = TableMap $ Map.fromList $ zip
      tableNames
      (repeat $ mkSFunArray $ const defaultV)

    mkMaintainsInvariants = TableMap $ Map.fromList $
      tables <&> \Table { _tableName, _tableInvariants } ->
        (TableName (T.unpack _tableName), ZipList $ fmap (const sTrue) <$> _tableInvariants)

    mkRowExists = TableMap $ Map.fromList $ tableNames <&> \tn@(TableName tn')
      -> (tn, mkFreeArray $ "row_exists__" <> T.pack tn')

mkTableColumnMap
  :: [Table]
  -> (Pact.Type Pact.UserType -> Bool) -- ^ Include this column in the mapping?
  -> a                                 -- ^ Default value
  -> TableMap (ColumnMap a)
mkTableColumnMap tables f defValue = TableMap $ Map.fromList $
  tables <&> \Table { _tableName, _tableType } ->
    let fields = Pact._utFields _tableType
        colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
          \(Pact.Arg argName ty _) ->
            if f ty
            then Just (ColumnName (T.unpack argName), defValue)
            else Nothing
    in (TableName (T.unpack _tableName), colMap)

mkSymbolicCells :: [Table] -> TableMap SymbolicCells
mkSymbolicCells tables = TableMap $ Map.fromList cellsList
  where
    cellsList = tables <&> \Table { _tableName, _tableType = Pact.Schema _ _ fields _ } ->
      let fields'  = Map.fromList $
            map (\(Pact.Arg argName ty _i) -> (argName, ty)) fields

      in (TableName (T.unpack _tableName), mkCells _tableName fields')

    mkCells :: Text -> Map Text (Pact.Type Pact.UserType) -> SymbolicCells
    mkCells tableName fields = ifoldl
      (\colName cells ty ->
        let col      = ColumnName $ T.unpack colName

            mkArray :: SingTy a -> EValSFunArray RowKey
            mkArray sTy = withHasKind sTy $ EValSFunArray sTy $ mkFreeArray $
              "cells__" <> tableName <> "__" <> colName

        in cells & case maybeTranslateType ty of
             Just (EType sTy) -> scValues . at col ?~ mkArray sTy
             --
             -- TODO: we should Left here. this means that mkSymbolicCells and
             --       mkInitialAnalyzeState should both return Either.
             --
             Nothing          -> id
      )
      (SymbolicCells mempty)
      fields

mkSVal :: SBV a -> SBVI.SVal
mkSVal (SBVI.SBV v) = v

class HasAnalyzeEnv a where
  {-# MINIMAL analyzeEnv #-}
  analyzeEnv :: Lens' a AnalyzeEnv

  scope :: Lens' a (Map VarId AVal)
  scope = analyzeEnv.aeScope

  registry :: Lens' a Registry
  registry = analyzeEnv.aeRegistry

  txKeySets :: Lens' a (SFunArray Str Guard)
  txKeySets = analyzeEnv.aeTxMetadata.tmKeySets

  txDecimals :: Lens' a (SFunArray Str Decimal)
  txDecimals = analyzeEnv.aeTxMetadata.tmDecimals

  txIntegers :: Lens' a (SFunArray Str Integer)
  txIntegers = analyzeEnv.aeTxMetadata.tmIntegers

  txStrings :: Lens' a (SFunArray Str Str)
  txStrings = analyzeEnv.aeTxMetadata.tmStrings

  inPact :: Lens' a (S Bool)
  inPact = analyzeEnv.aePactMetadata.pmInPact

  currentPactId :: Lens' a (S Str)
  currentPactId = analyzeEnv.aePactMetadata.pmPactId

  currentEntity :: Lens' a (S Str)
  currentEntity = analyzeEnv.aePactMetadata.pmEntity

  activeGrants :: Lens' a TokenGrants
  activeGrants = analyzeEnv.aeActiveGrants

  moduleGuard :: Lens' a (S Guard)
  moduleGuard = analyzeEnv.aeModuleGuard

instance HasAnalyzeEnv AnalyzeEnv where analyzeEnv = id
instance HasAnalyzeEnv QueryEnv   where analyzeEnv = qeAnalyzeEnv

-- | Whether the program will successfully run to completion without aborting.
succeeds :: Lens' (AnalyzeState a) (S Bool)
succeeds = latticeState.lasSucceeds._Wrapped'.sbv2S

-- | Whether execution will reach a given point in the program according to
-- conditionals, *without taking transaction failures into account*. This is
-- used to determine which linear execution path through an 'ExecutionGraph' is
-- taken by a concrete run of the program. By not taking transaction failures
-- into account, we ensure that our 'ExecutionGraph' has a single component
-- amongst the edges we call "reachable". If we were to naively use 'succeeds'
-- instead of this separate construct, the following program would yield an
-- incomplete graph:
--
--     (defun test (x:bool)
--       (if x (enforce false) (enforce false)))
--
-- This program yields a graph with six edges: one initial edge which splits at
-- the conditional, two on either side of the conditional (branching out and
-- rejoining back to one another), and one final edge after the conditional:
--
--        .
--       / x
--     ->   ->
--       \.x
--
-- The initial and final edges would both belong to the "root path" and
-- therefore be trivially reachable, the "branch-out" edges would be reachable,
-- and neither of the "rejoin" edges would reachable. If we only consider the
-- graph formed by reachable edges, we now have 2 components.
--
-- We prefer to give a single-component reachable graph to model reporting, and
-- let that code consider 'TraceAssert' and 'TraceGuard 'TraceEvent's on its
-- own to determine where linear execution aborts for a concrete program trace.
--
purelyReachable :: Lens' (AnalyzeState a) (S Bool)
purelyReachable = latticeState.lasPurelyReachable.sbv2S

maintainsInvariants :: Lens' (AnalyzeState a) (TableMap (ZipList (Located SBool)))
maintainsInvariants = latticeState.lasMaintainsInvariants

guardPasses :: S Guard -> Lens' (AnalyzeState a) (S Bool)
guardPasses sg = latticeState.lasGuardPasses.symArrayAt sg.sbv2S

tableRead :: TableName -> Lens' (AnalyzeState a) (S Bool)
tableRead tn = latticeState.lasTablesRead.symArrayAt (literalS tn).sbv2S

tableWritten :: TableName -> Lens' (AnalyzeState a) (S Bool)
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literalS tn).sbv2S

columnWritten
  :: HasCallStack
  => TableName -> ColumnName -> Lens' (AnalyzeState a) (S Bool)
columnWritten tn cn = latticeState.lasColumnsWritten.singular (ix tn).
  singular (ix cn).sbv2S

columnRead
  :: HasCallStack
  => TableName -> ColumnName -> Lens' (AnalyzeState a) (S Bool)
columnRead tn cn = latticeState.lasColumnsRead.singular (ix tn).
  singular (ix cn).sbv2S

intCellDelta
  :: HasCallStack
  => TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Integer)
intCellDelta tn cn sRk = latticeState.lasIntCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

decCellDelta
  :: HasCallStack
  => TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Decimal)
decCellDelta tn cn sRk = latticeState.lasDecCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

intColumnDelta
  :: HasCallStack
  => TableName -> ColumnName -> Lens' (AnalyzeState a) (S Integer)
intColumnDelta tn cn = latticeState.lasIntColumnDeltas.singular (ix tn).
  singular (ix cn)

decColumnDelta
  :: HasCallStack
  => TableName -> ColumnName -> Lens' (AnalyzeState a) (S Decimal)
decColumnDelta tn cn = latticeState.lasDecColumnDeltas.singular (ix tn).
  singular (ix cn)

rowReadCount
  :: HasCallStack
  => TableName -> S RowKey -> Lens' (AnalyzeState a) (S Integer)
rowReadCount tn sRk = latticeState.lasRowsRead.singular (ix tn).
  symArrayAt sRk.sbv2S

rowWriteCount
  :: HasCallStack
  => TableName -> S RowKey -> Lens' (AnalyzeState a) (S Integer)
rowWriteCount tn sRk = latticeState.lasRowsWritten.singular (ix tn).
  symArrayAt sRk.sbv2S

rowExists
  :: HasCallStack
  => Lens' a CellValues
  -> TableName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
rowExists cellValues tn sRk = latticeState.lasExtra.cellValues.
  cvRowExists.singular (ix tn).symArrayAt sRk.sbv2S

cellEnforced
  :: HasCallStack
  => TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
cellEnforced tn cn sRk = latticeState.lasCellsEnforced.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

cellWritten
  :: HasCallStack
  => TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
cellWritten tn cn sRk = latticeState.lasCellsWritten.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

tokenGranted
  :: HasCallStack
  => Token
  -> Lens' TokenGrants (S Bool)
tokenGranted (Token schema capName sObj)
  = capabilityGrants
  . singular (ix capName)
  . eKArrayAt (SObjectUnsafe schema) sObj
  . sbv2S

pendingTokenGranted
  :: HasCallStack
  => Token
  -> Lens' (AnalyzeState a) (S Bool)
pendingTokenGranted token
  = latticeState
  . lasPendingGrants
  . tokenGranted token

typedCell
  :: HasCallStack
  => SingTy b
  -> Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S (Concrete b))
typedCell ty cellValues tn cn sRk sDirty
  = latticeState
  . lasExtra
  . cellValues
  . cvTableCells
  . singular (ix tn)
  . scValues
  . singular (ix cn)
  . eVArrayAt ty sRk
  . sbv2SFrom (fromCell tn cn sRk sDirty)

symArrayAt
  :: forall array k v
   . (SymVal v, SymArray array)
  => S k -> Lens' (array k v) (SBV v)
symArrayAt (S _ symKey) = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

symRegistryName :: S Str -> S RegistryName
symRegistryName = unsafeCoerceS

-- | Resolve a named guard from the registry (not tx metadata)
resolveGuard
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S RegistryName
  -> m (S Guard)
resolveGuard sRn = do
  reg <- view registry
  pure $ resolveGuardFromReg reg sRn

-- | Reads a named keyset from tx metadata (not the keyset registry)
readKeySet
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S Str
  -> m (S Guard)
readKeySet sStr = fmap (withProv $ fromMetadata sStr) $
  readArray <$> view txKeySets <*> pure (_sSbv sStr)

-- | Reads a named decimal from tx metadata
readDecimal
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S Str
  -> m (S Decimal)
readDecimal sStr = fmap (withProv $ fromMetadata sStr) $
  readArray <$> view txDecimals <*> pure (_sSbv sStr)

-- | Reads a named string from tx metadata
readString
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S Str
  -> m (S Str)
readString sStr = fmap (withProv $ fromMetadata sStr) $
  readArray <$> view txStrings <*> pure (_sSbv sStr)

-- | Reads a named integer from tx metadata
readInteger
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S Str
  -> m (S Integer)
readInteger sStr = fmap (withProv $ fromMetadata sStr) $
  readArray <$> view txIntegers <*> pure (_sSbv sStr)
