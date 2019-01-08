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
import           Data.SBV                     (HasKind, Mergeable(symbolicMerge), SBV, SBool,
                                               SymArray (readArray, writeArray),
                                               SymWord, uninterpret)
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
import           Pact.Analyze.Orphans         ()
import           Pact.Analyze.Translate       (maybeTranslateUserType', maybeTranslateType)
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
  analyzerIte     :: Mergeable a => SBV Bool -> m a -> m a -> m a

data AnalyzeEnv
  = AnalyzeEnv
    { _aeScope     :: !(Map VarId AVal)              -- used as a stack
    , _aeKeySets   :: !(SFunArray KeySetName KeySet) -- read-only
    , _aeKsAuths   :: !(SFunArray KeySet Bool)       -- read-only
    , _aeDecimals  :: !(SFunArray Str Decimal)    -- read-only
    , _aeIntegers  :: !(SFunArray Str Integer)    -- read-only
    , _invariants  :: !(TableMap [Located (Invariant 'TyBool)])
    , _aeColumnIds :: !(TableMap (Map Text VarId))
    , _aeModelTags :: !(ModelTags 'Symbolic)
    , _aeInfo      :: !Info
    }

instance Show AnalyzeEnv where
  showsPrec p AnalyzeEnv{..} = showParen (p > 10)
    $ showString "AnalyzeEnv "
    . showsPrec 11 _aeScope
    . showString " "
    . showsPrec 11 _aeKeySets
    . showString " "
    . showsPrec 11 _aeKsAuths
    . showString " "
    . showsPrec 11 _aeDecimals
    . showString " "
    . showsPrec 11 _aeIntegers
    . showString " "
    -- . showsPrec 11 _invariants TODO
    . showsPrec 11 _aeColumnIds
    . showString " "
    . showsPrec 11 _aeModelTags
    . showString " "
    . showsPrec 11 _aeInfo

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
      Just (EType (SObject ty)) -> Just
        (TableName (T.unpack tname), varIdColumns ty)
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

data ESFunArray where
  ESFunArray :: SingTy a -> SFunArray RowKey (Concrete a) -> ESFunArray

instance Show ESFunArray where
  showsPrec p (ESFunArray ty sfunarr) = showParen (p > 10) $
      showString "ESFunArray "
    . showsPrec 11 ty
    . showChar ' '
    . withHasKind ty (showsPrec 11 sfunarr)

data SymbolicCells = SymbolicCells { _scValues :: ColumnMap ESFunArray }
  deriving (Show)

eArrayAt :: forall a.
  SingTy a -> S RowKey -> Lens' ESFunArray (SBV (Concrete a))
eArrayAt ty (S _ symKey) = lens getter setter where

  getter :: ESFunArray -> SBV (Concrete a)
  getter (ESFunArray ty' arr) = case singEq ty ty' of
    Just Refl -> readArray arr symKey
    Nothing   -> error "TODO: eArrayAt: bad getter access"

  setter :: ESFunArray -> SBV (Concrete a) -> ESFunArray
  setter (ESFunArray ty' arr) val = case singEq ty ty' of
    Just Refl -> withSymWord ty $ ESFunArray ty $ writeArray arr symKey val
    Nothing   -> error "TODO: eArrayAt: bad setter access"

instance Mergeable ESFunArray where
  symbolicMerge force test (ESFunArray ty1 arr1) (ESFunArray ty2 arr2)
    = case singEq ty1 ty2 of
      Nothing   -> error "mismatched types when merging two ESFunArrays"
      Just Refl -> withSymWord ty1 $
        ESFunArray ty1 $ symbolicMerge force test arr1 arr2

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
    , _lasExtra               :: a
    }
  deriving (Generic, Show)

deriving instance Mergeable a => Mergeable (LatticeAnalyzeState a)

-- Checking state that is transferred through every computation, in-order.
data GlobalAnalyzeState
  = GlobalAnalyzeState
    { _gasKsProvenances :: Map TagId Provenance -- added as we accum ks info
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
        { _lasSucceeds            = SymbolicSuccess sTrue
        , _lasPurelyReachable     = sTrue
        , _lasMaintainsInvariants = mkMaintainsInvariants
        , _lasTablesRead          = mkSFunArray $ const sFalse
        , _lasTablesWritten       = mkSFunArray $ const sFalse
        , _lasColumnsRead         = mkTableColumnMap (const True) sFalse
        , _lasColumnsWritten      = mkTableColumnMap (const True) sFalse
        , _lasIntCellDeltas       = intCellDeltas
        , _lasDecCellDeltas       = decCellDeltas
        , _lasIntColumnDeltas     = intColumnDeltas
        , _lasDecColumnDeltas     = decColumnDeltas
        , _lasRowsRead            = mkPerTableSFunArray 0
        , _lasRowsWritten         = mkPerTableSFunArray 0
        , _lasCellsEnforced       = cellsEnforced
        , _lasCellsWritten        = cellsWritten
        , _lasConstraints         = sansProv sTrue
        , _lasExtra               = CellValues
          { _cvTableCells          = mkSymbolicCells tables
          , _cvRowExists           = mkRowExists
          }
        }
    , _globalState = GlobalAnalyzeState
        { _gasKsProvenances = mempty
        }
    }

  where
    tableNames :: [TableName]
    tableNames = map (TableName . T.unpack . view Types.tableName) tables

    intCellDeltas   = mkTableColumnMap (== TyPrim Pact.TyInteger) (mkSFunArray (const 0))
    decCellDeltas   = mkTableColumnMap (== TyPrim Pact.TyDecimal) (mkSFunArray (const (fromInteger 0)))
    intColumnDeltas = mkTableColumnMap (== TyPrim Pact.TyInteger) 0
    decColumnDeltas = mkTableColumnMap (== TyPrim Pact.TyDecimal) (fromInteger 0)
    cellsEnforced
      = mkTableColumnMap (== TyPrim Pact.TyKeySet) (mkSFunArray (const sFalse))
    cellsWritten = mkTableColumnMap (const True) (mkSFunArray (const sFalse))

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
        (TableName (T.unpack _tableName), ZipList $ fmap (const sTrue) <$> _tableInvariants)

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

            mkArray :: SingTy a -> ESFunArray
            mkArray sTy = withHasKind sTy $ ESFunArray sTy $ mkFreeArray $
              "cells__" <> tableName <> "__" <> colName

        in cells & case maybeTranslateType ty of
             Just (EType sTy) -> scValues . at col ?~ mkArray sTy
             --
             -- TODO: we should Left here. this means that mkSymbolicCells and
             --       mkInitialAnalyzeState should both return Either.
             --
             Nothing -> id
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

  keySets :: Lens' a (SFunArray KeySetName KeySet)
  keySets = analyzeEnv.aeKeySets

  ksAuths :: Lens' a (SFunArray KeySet Bool)
  ksAuths = analyzeEnv.aeKsAuths

  envDecimals :: Lens' a (SFunArray Str Decimal)
  envDecimals = analyzeEnv.aeDecimals

  envIntegers :: Lens' a (SFunArray Str Integer)
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

type CellLens a b
  =  Lens' a CellValues
  -> TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' (AnalyzeState a) (S b)

typedCell :: HasCallStack => SingTy b -> CellLens a (Concrete b)
typedCell ty cellValues tn cn sRk sDirty =
    latticeState
  . lasExtra
  . cellValues
  . cvTableCells
  . singular (ix tn)
  . scValues
  . singular (ix cn)
  . eArrayAt ty sRk
  . sbv2SFrom (fromCell tn cn sRk sDirty)

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
  => S Str
  -> m (S Decimal)
resolveDecimal sDn = fmap sansProv $
  readArray <$> view envDecimals <*> pure (_sSbv sDn)

resolveInteger
  :: (MonadReader r m, HasAnalyzeEnv r)
  => S Str
  -> m (S Integer)
resolveInteger sSn = fmap sansProv $
  readArray <$> view envIntegers <*> pure (_sSbv sSn)
