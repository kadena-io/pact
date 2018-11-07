{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
import           Data.SBV                     (Boolean (bnot, true, (&&&)),
                                               HasKind, Mergeable, SBV, SBool,
                                               SymArray (readArray, writeArray),
                                               SymWord, Symbolic, false,
                                               uninterpret)
import qualified Data.SBV.Internals           as SBVI
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable             (for)
import           GHC.Generics                 (Generic)

import           Pact.Types.Lang              (Info)
import           Pact.Types.Runtime           (PrimType (TyBool, TyDecimal, TyInteger, TyKeySet, TyString, TyTime),
                                               Type (TyPrim))
import qualified Pact.Types.Runtime           as Pact
import qualified Pact.Types.Typecheck         as Pact

import           Pact.Analyze.Errors
import           Pact.Analyze.LegacySFunArray (SFunArray, mkSFunArray)
import           Pact.Analyze.Orphans         ()
import           Pact.Analyze.Translate       (maybeTranslateUserType')
import           Pact.Analyze.Types           hiding (tableName)
import qualified Pact.Analyze.Types           as Types
import           Pact.Analyze.Util


newtype SymbolicSuccess = SymbolicSuccess { successBool :: SBV Bool }
  deriving (Show, Generic, Mergeable)

instance Boolean SymbolicSuccess where
  true = SymbolicSuccess true
  bnot = SymbolicSuccess . bnot . successBool
  SymbolicSuccess x &&& SymbolicSuccess y = SymbolicSuccess (x &&& y)

instance Wrapped SymbolicSuccess where
  type Unwrapped SymbolicSuccess = SBV Bool
  _Wrapped' = iso successBool SymbolicSuccess

class (MonadError AnalyzeFailure m, S :<: TermOf m) => Analyzer m where
  type TermOf m   :: * -> *
  eval            :: (Show a, SymWord a) => TermOf m a -> m (S a)
  evalO           :: TermOf m Object -> m Object
  throwErrorNoLoc :: AnalyzeFailureNoLoc -> m a
  getVar          :: VarId -> m (Maybe AVal)
  markFailure     :: SBV Bool -> m ()

  -- unfortunately, because `Query` and `InvariantCheck` include `Symbolic` in
  -- their monad stack, they can't use `ite`, which we need to use to implement
  -- short-circuiting ops correctly for (effectful) terms. Though, luckily the
  -- invariant and prop languages are pure, so we're fine to implement them in
  -- terms of `|||` / `&&&`.
  evalLogicalOp   :: LogicalOp -> [TermOf m Bool] -> m (S Bool)

data AnalyzeEnv
  = AnalyzeEnv
    { _aeScope     :: !(Map VarId AVal)              -- used as a stack
    , _aeKeySets   :: !(SFunArray KeySetName KeySet) -- read-only
    , _aeKsAuths   :: !(SFunArray KeySet Bool)       -- read-only
    , _aeDecimals  :: !(SFunArray String Decimal)    -- read-only
    , _aeIntegers  :: !(SFunArray String Integer)    -- read-only
    , _invariants  :: !(TableMap [Located (Invariant Bool)])
    , _aeColumnIds :: !(TableMap (Map Text VarId))
    , _aeModelTags :: !(ModelTags 'Symbolic)
    , _aeInfo      :: !Info
    }
  deriving Show

mkAnalyzeEnv
  :: [Table]
  -> Map VarId AVal
  -> ModelTags 'Symbolic
  -> Info
  -> Maybe AnalyzeEnv
mkAnalyzeEnv tables args tags info = do
  let keySets'    = mkFreeArray "envKeySets"
      keySetAuths = mkFreeArray "keySetAuths"
      decimals    = mkFreeArray "envDecimals"
      integers    = mkFreeArray "envIntegers"

      invariants' = TableMap $ Map.fromList $ tables <&>
        \(Table tname _ut someInvariants) ->
          (TableName (T.unpack tname), someInvariants)

  columnIds <- for tables $ \(Table tname ut _) ->
    case maybeTranslateUserType' ut of
      Just (EObjectTy (Schema schema)) -> Just
        (TableName (T.unpack tname), varIdColumns schema)
      _ -> Nothing

  let columnIds' = TableMap (Map.fromList columnIds)

  pure $ AnalyzeEnv args keySets' keySetAuths decimals integers invariants'
    columnIds' tags info

mkFreeArray :: (SymWord a, HasKind b) => Text -> SFunArray a b
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

newtype Constraints
  = Constraints { runConstraints :: Symbolic () }

instance Show Constraints where
  show _ = "<symbolic>"

instance Semigroup Constraints where
  (Constraints act1) <> (Constraints act2) = Constraints $ act1 *> act2

instance Monoid Constraints where
  mempty = Constraints (pure ())

data SymbolicCells
  = SymbolicCells
    { _scIntValues     :: ColumnMap (SFunArray RowKey Integer)
    , _scBoolValues    :: ColumnMap (SFunArray RowKey Bool)
    , _scStringValues  :: ColumnMap (SFunArray RowKey String)
    , _scDecimalValues :: ColumnMap (SFunArray RowKey Decimal)
    , _scTimeValues    :: ColumnMap (SFunArray RowKey Time)
    , _scKsValues      :: ColumnMap (SFunArray RowKey KeySet)
    -- TODO: opaque blobs
    }
  deriving (Generic, Show)

deriving instance Mergeable SymbolicCells

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
    , _lasExtra               :: a
    }
  deriving (Generic, Show)

deriving instance Mergeable a => Mergeable (LatticeAnalyzeState a)

-- Checking state that is transferred through every computation, in-order.
data GlobalAnalyzeState
  = GlobalAnalyzeState
    { _gasConstraints   :: Constraints          -- we log these a la writer
    , _gasKsProvenances :: Map TagId Provenance -- added as we accum ks info
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
    , _arProposition   :: SBV Bool
    , _arKsProvenances :: Map TagId Provenance
    }
  deriving (Show)

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

mkInitialAnalyzeState :: [Table] -> EvalAnalyzeState
mkInitialAnalyzeState tables = AnalyzeState
    { _latticeState = LatticeAnalyzeState
        { _lasSucceeds            = true
        , _lasPurelyReachable     = true
        , _lasMaintainsInvariants = mkMaintainsInvariants
        , _lasTablesRead          = mkSFunArray $ const false
        , _lasTablesWritten       = mkSFunArray $ const false
        , _lasColumnsRead         = mkTableColumnMap (const True) false
        , _lasColumnsWritten      = mkTableColumnMap (const True) false
        , _lasIntCellDeltas       = intCellDeltas
        , _lasDecCellDeltas       = decCellDeltas
        , _lasIntColumnDeltas     = intColumnDeltas
        , _lasDecColumnDeltas     = decColumnDeltas
        , _lasRowsRead            = mkPerTableSFunArray 0
        , _lasRowsWritten         = mkPerTableSFunArray 0
        , _lasCellsEnforced       = cellsEnforced
        , _lasCellsWritten        = cellsWritten
        , _lasExtra               = CellValues
          { _cvTableCells          = mkSymbolicCells tables
          , _cvRowExists           = mkRowExists
          }
        }
    , _globalState = GlobalAnalyzeState
        { _gasConstraints   = mempty
        , _gasKsProvenances = mempty
        }
    }

  where
    tableNames :: [TableName]
    tableNames = map (TableName . T.unpack . view Types.tableName) tables

    intCellDeltas = mkTableColumnMap (== TyPrim TyInteger) (mkSFunArray (const 0))
    decCellDeltas = mkTableColumnMap (== TyPrim TyDecimal) (mkSFunArray (const (fromInteger 0)))
    intColumnDeltas = mkTableColumnMap (== TyPrim TyInteger) 0
    decColumnDeltas = mkTableColumnMap (== TyPrim TyDecimal) (fromInteger 0)
    cellsEnforced
      = mkTableColumnMap (== TyPrim TyKeySet) (mkSFunArray (const false))
    cellsWritten = mkTableColumnMap (const True) (mkSFunArray (const false))

    mkTableColumnMap
      :: (Pact.Type Pact.UserType -> Bool) -- ^ Include this column in the mapping?
      -> a                                 -- ^ Default value
      -> TableMap (ColumnMap a)
    mkTableColumnMap f defValue = TableMap $ Map.fromList $
      tables <&> \Table { _tableName, _tableType } ->
        let fields = Pact._utFields _tableType
            colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
              \(Pact.Arg argName ty _) ->
                if f ty
                then Just (ColumnName (T.unpack argName), defValue)
                else Nothing
        in (TableName (T.unpack _tableName), colMap)

    mkPerTableSFunArray :: SBV v -> TableMap (SFunArray k v)
    mkPerTableSFunArray defaultV = TableMap $ Map.fromList $ zip
      tableNames
      (repeat $ mkSFunArray $ const defaultV)

    mkMaintainsInvariants = TableMap $ Map.fromList $
      tables <&> \Table { _tableName, _tableInvariants } ->
        (TableName (T.unpack _tableName), ZipList $ const true <$$> _tableInvariants)

    mkRowExists = TableMap $ Map.fromList $ tableNames <&> \tn@(TableName tn')
      -> (tn, mkFreeArray $ "row_exists__" <> T.pack tn')

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

            mkArray :: forall a. HasKind a => SFunArray RowKey a
            mkArray  = mkFreeArray $ "cells__" <> tableName <> "__" <> colName

        in cells & case ty of
             TyPrim TyInteger -> scIntValues.at col     ?~ mkArray
             TyPrim TyBool    -> scBoolValues.at col    ?~ mkArray
             TyPrim TyDecimal -> scDecimalValues.at col ?~ mkArray
             TyPrim TyTime    -> scTimeValues.at col    ?~ mkArray
             TyPrim TyString  -> scStringValues.at col  ?~ mkArray
             TyPrim TyKeySet  -> scKsValues.at col      ?~ mkArray
             --
             -- TODO: we should Left here. this means that mkSymbolicCells and
             --       mkInitialAnalyzeState should both return Either.
             --
             _                -> id
      )
      (SymbolicCells mempty mempty mempty mempty mempty mempty)
      fields

mkSVal :: SBV a -> SBVI.SVal
mkSVal (SBVI.SBV v) = v

class HasAnalyzeEnv a where
  {-# MINIMAL analyzeEnv #-}
  analyzeEnv :: Lens' a AnalyzeEnv

  scope :: Lens' a (Map VarId AVal)
  scope = analyzeEnv.aeScope

  keySets :: Lens' a (SFunArray KeySetName KeySet)
  keySets = analyzeEnv.aeKeySets

  ksAuths :: Lens' a (SFunArray KeySet Bool)
  ksAuths = analyzeEnv.aeKsAuths

  envDecimals :: Lens' a (SFunArray String Decimal)
  envDecimals = analyzeEnv.aeDecimals

  envIntegers :: Lens' a (SFunArray String Integer)
  envIntegers = analyzeEnv.aeIntegers

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
-- let that code consider 'TraceAssert' and 'TraceAuth' 'TraceEvent's on its
-- own to determine where linear execution aborts for a concrete program trace.
--
purelyReachable :: Lens' (AnalyzeState a) (S Bool)
purelyReachable = latticeState.lasPurelyReachable.sbv2S

maintainsInvariants :: Lens' (AnalyzeState a) (TableMap (ZipList (Located SBool)))
maintainsInvariants = latticeState.lasMaintainsInvariants

tableRead :: TableName -> Lens' (AnalyzeState a) (S Bool)
tableRead tn = latticeState.lasTablesRead.symArrayAt (literalS tn).sbv2S

tableWritten :: TableName -> Lens' (AnalyzeState a) (S Bool)
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literalS tn).sbv2S

columnWritten :: TableName -> ColumnName -> Lens' (AnalyzeState a) (S Bool)
columnWritten tn cn = latticeState.lasColumnsWritten.singular (ix tn).
  singular (ix cn).sbv2S

columnRead :: TableName -> ColumnName -> Lens' (AnalyzeState a) (S Bool)
columnRead tn cn = latticeState.lasColumnsRead.singular (ix tn).
  singular (ix cn).sbv2S

intCellDelta
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Integer)
intCellDelta tn cn sRk = latticeState.lasIntCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

decCellDelta
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Decimal)
decCellDelta tn cn sRk = latticeState.lasDecCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

intColumnDelta :: TableName -> ColumnName -> Lens' (AnalyzeState a) (S Integer)
intColumnDelta tn cn = latticeState.lasIntColumnDeltas.singular (ix tn).
  singular (ix cn)

decColumnDelta :: TableName -> ColumnName -> Lens' (AnalyzeState a) (S Decimal)
decColumnDelta tn cn = latticeState.lasDecColumnDeltas.singular (ix tn).
  singular (ix cn)

rowReadCount :: TableName -> S RowKey -> Lens' (AnalyzeState a) (S Integer)
rowReadCount tn sRk = latticeState.lasRowsRead.singular (ix tn).
  symArrayAt sRk.sbv2S

rowWriteCount :: TableName -> S RowKey -> Lens' (AnalyzeState a) (S Integer)
rowWriteCount tn sRk = latticeState.lasRowsWritten.singular (ix tn).
  symArrayAt sRk.sbv2S

rowExists
  :: Lens' a CellValues
  -> TableName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
rowExists cellValues tn sRk = latticeState.lasExtra.cellValues.
  cvRowExists.singular (ix tn).symArrayAt sRk.sbv2S

cellEnforced
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
cellEnforced tn cn sRk = latticeState.lasCellsEnforced.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

cellWritten
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' (AnalyzeState a) (S Bool)
cellWritten tn cn sRk = latticeState.lasCellsWritten.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

intCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S Integer)
intCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scIntValues.singular (ix cn).
  symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

boolCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S Bool)
boolCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scBoolValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

stringCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S String)
stringCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scStringValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

decimalCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S Decimal)
decimalCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scDecimalValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

timeCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S Time)
timeCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scTimeValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

ksCell
  :: Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S KeySet)
ksCell cellValues tn cn sRk sDirty = latticeState.lasExtra.cellValues.
  cvTableCells.singular (ix tn).scKsValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

symArrayAt
  :: forall array k v
   . (SymWord v, SymArray array)
  => S k -> Lens' (array k v) (SBV v)
symArrayAt (S _ symKey) = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

nameAuthorized
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S KeySetName
  -> m (S Bool)
nameAuthorized sKsn = fmap sansProv $
  readArray <$> view ksAuths <*> (_sSbv <$> resolveKeySet sKsn)

resolveKeySet
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S KeySetName
  -> m (S KeySet)
resolveKeySet sKsn = fmap (withProv $ fromNamedKs sKsn) $
  readArray <$> view keySets <*> pure (_sSbv sKsn)

resolveDecimal
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S String
  -> m (S Decimal)
resolveDecimal sDn = fmap sansProv $
  readArray <$> view envDecimals <*> pure (_sSbv sDn)

resolveInteger
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S String
  -> m (S Integer)
resolveInteger sSn = fmap sansProv $
  readArray <$> view envIntegers <*> pure (_sSbv sSn)
