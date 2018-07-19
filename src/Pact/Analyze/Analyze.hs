{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}

module Pact.Analyze.Analyze where

import           Control.Applicative        (ZipList (..))
import           Control.Lens               (At (at), Ixed (ix), Lens', ifoldl,
                                             iforM, lens, makeLenses, over,
                                             preview, singular, use, view, (%=),
                                             (%~), (&), (+=), (.=), (.~), (<&>),
                                             (?~), (^.), (^?), _1, _2, _Just)
import           Control.Monad              (void)
import           Control.Monad.Except       (Except, ExceptT (ExceptT),
                                             MonadError (throwError), runExcept)
import           Control.Monad.Morph        (generalize, hoist)
import           Control.Monad.Reader       (MonadReader (local), ReaderT,
                                             runReaderT)
import           Control.Monad.RWS.Strict   (RWST (RWST, runRWST))
import           Control.Monad.State.Strict (MonadState, modify')
import           Control.Monad.Trans.Class  (lift)
import qualified Data.Aeson                 as Aeson
import           Data.ByteString.Lazy       (toStrict)
import           Data.Foldable              (foldl', foldrM)
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Monoid                ((<>))
import           Data.SBV                   (Boolean (bnot, true, (&&&), (==>), (|||)),
                                             EqSymbolic ((./=), (.==)), HasKind,
                                             Mergeable (symbolicMerge),
                                             OrdSymbolic ((.<), (.<=), (.>), (.>=)),
                                             SBV, SBool,
                                             SymArray (readArray, writeArray),
                                             SymWord (exists_, forall_),
                                             Symbolic, constrain, false, ite,
                                             uninterpret)
import qualified Data.SBV.Internals         as SBVI
import qualified Data.SBV.String            as SBV
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Thyme                 (formatTime, parseTime)
import           Data.Traversable           (for)
import           System.Locale

import qualified Pact.Types.Hash            as Pact
import           Pact.Types.Lang            (Info)
import           Pact.Types.Runtime         (PrimType (TyBool, TyDecimal, TyInteger, TyKeySet, TyString, TyTime),
                                             Type (TyPrim), tShow)
import qualified Pact.Types.Runtime         as Pact
import qualified Pact.Types.Typecheck       as Pact
import           Pact.Types.Version         (pactVersion)

import           Pact.Analyze.AnalyzeNumerical
import           Pact.Analyze.Errors
import           Pact.Analyze.LegacySFunArray (SFunArray, mkSFunArray)
import           Pact.Analyze.Orphans         ()
import           Pact.Analyze.Types           hiding (tableName)
import qualified Pact.Analyze.Types           as Types
import           Pact.Analyze.Util

data AnalyzeEnv
  = AnalyzeEnv
    { _aeScope     :: !(Map VarId AVal)              -- used as a stack
    , _aeKeySets   :: !(SFunArray KeySetName KeySet) -- read-only
    , _aeKsAuths   :: !(SFunArray KeySet Bool)       -- read-only
    , _invariants  :: !(TableMap [Located (Invariant Bool)])
    , _aeModelTags :: !ModelTags
    , _aeInfo      :: !Info
    }
  deriving Show

newtype Constraints
  = Constraints { runConstraints :: Symbolic () }

instance Show Constraints where
  show _ = "<symbolic>"

instance Monoid Constraints where
  mempty = Constraints (pure ())
  mappend (Constraints act1) (Constraints act2) = Constraints $ act1 *> act2

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
  deriving (Show)

-- Implemented by-hand until 8.2, when we have DerivingStrategies
instance Mergeable SymbolicCells where
  symbolicMerge force test
    (SymbolicCells a b c d e f)
    (SymbolicCells a' b' c' d' e' f')
    = SymbolicCells (m a a') (m b b') (m c c') (m d d') (m e e') (m f f')
    where
      m :: Mergeable a => ColumnMap a -> ColumnMap a -> ColumnMap a
      m = symbolicMerge force test

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds            :: SBV Bool
    --
    -- TODO: instead of having a single boolean here, we should probably use
    --       finer-grained tracking, so that we can test whether a single
    --       invariant is being maintained
    --
    , _lasMaintainsInvariants :: TableMap (ZipList (Located (SBV Bool)))
    , _lasTablesRead          :: SFunArray TableName Bool
    , _lasTablesWritten       :: SFunArray TableName Bool
    , _lasIntCellDeltas       :: TableMap (ColumnMap (SFunArray RowKey Integer))
    , _lasDecCellDeltas       :: TableMap (ColumnMap (SFunArray RowKey Decimal))
    , _lasIntColumnDeltas     :: TableMap (ColumnMap (S Integer))
    , _lasDecColumnDeltas     :: TableMap (ColumnMap (S Decimal))
    , _lasTableCells          :: TableMap SymbolicCells
    , _lasRowsRead            :: TableMap (SFunArray RowKey Integer)
    , _lasRowsWritten         :: TableMap (SFunArray RowKey Integer)
    , _lasCellsEnforced       :: TableMap (ColumnMap (SFunArray RowKey Bool))
    -- We currently maintain cellsWritten only for deciding whether a cell has
    -- been "invalidated" for the purposes of keyset enforcement. If a keyset
    -- has been overwritten and *then* enforced, that does not constitute valid
    -- enforcement of the keyset.
    , _lasCellsWritten        :: TableMap (ColumnMap (SFunArray RowKey Bool))
    }
  deriving (Show)

-- Implemented by-hand until 8.2, when we have DerivingStrategies
instance Mergeable LatticeAnalyzeState where
  symbolicMerge force test
    (LatticeAnalyzeState
      success  tsInvariants  tsRead  tsWritten  intCellDeltas  decCellDeltas
      intColDeltas  decColDeltas  cells  rsRead  rsWritten  csEnforced  csWritten)
    (LatticeAnalyzeState
      success' tsInvariants' tsRead' tsWritten' intCellDeltas' decCellDeltas'
      intColDeltas' decColDeltas' cells' rsRead' rsWritten' csEnforced' csWritten')
        = LatticeAnalyzeState
          (symbolicMerge force test success       success')
          (symbolicMerge force test tsInvariants  tsInvariants')
          (symbolicMerge force test tsRead        tsRead')
          (symbolicMerge force test tsWritten     tsWritten')
          (symbolicMerge force test intCellDeltas intCellDeltas')
          (symbolicMerge force test decCellDeltas decCellDeltas')
          (symbolicMerge force test intColDeltas  intColDeltas')
          (symbolicMerge force test decColDeltas  decColDeltas')
          (symbolicMerge force test cells         cells')
          (symbolicMerge force test rsRead        rsRead')
          (symbolicMerge force test rsWritten     rsWritten')
          (symbolicMerge force test csEnforced    csEnforced')
          (symbolicMerge force test csWritten     csWritten')

-- Checking state that is transferred through every computation, in-order.
data GlobalAnalyzeState
  = GlobalAnalyzeState
    { _gasConstraints   :: Constraints          -- we log these a la writer
    , _gasKsProvenances :: Map TagId Provenance -- added as we accum ks info
    }
  deriving (Show)

data AnalyzeState
  = AnalyzeState
    { _latticeState :: LatticeAnalyzeState
    , _globalState  :: GlobalAnalyzeState
    }
  deriving (Show)

data QueryEnv
  = QueryEnv
    { _qeAnalyzeEnv    :: AnalyzeEnv
    , _qeAnalyzeState  :: AnalyzeState
    , _qeAnalyzeResult :: AVal
    , _qeTableScope    :: Map VarId TableName
    --
    -- TODO: implement column quantification. update 'getLitColName', and
    -- 'analyzeProp' for @Forall@ and @Exists@ at the same time.
    --
    , _qeColumnScope   :: Map VarId ()
    }

data AnalysisResult
  = AnalysisResult
    { _arProposition   :: SBV Bool
    , _arKsProvenances :: Map TagId Provenance
    }
  deriving (Show)

makeLenses ''AnalyzeEnv
makeLenses ''AnalyzeState
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells
makeLenses ''QueryEnv
makeLenses ''AnalysisResult

mkInitialAnalyzeState :: [Table] -> AnalyzeState
mkInitialAnalyzeState tables = AnalyzeState
    { _latticeState = LatticeAnalyzeState
        { _lasSucceeds            = true
        , _lasMaintainsInvariants = mkMaintainsInvariants
        , _lasTablesRead          = mkSFunArray $ const false
        , _lasTablesWritten       = mkSFunArray $ const false
        , _lasIntCellDeltas       = intCellDeltas
        , _lasDecCellDeltas       = decCellDeltas
        , _lasIntColumnDeltas     = intColumnDeltas
        , _lasDecColumnDeltas     = decColumnDeltas
        , _lasTableCells          = mkSymbolicCells tables
        , _lasRowsRead            = mkPerTableSFunArray 0
        , _lasRowsWritten         = mkPerTableSFunArray 0
        , _lasCellsEnforced       = cellsEnforced
        , _lasCellsWritten        = cellsWritten
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
    decCellDeltas = mkTableColumnMap (== TyPrim TyDecimal) (mkSFunArray (const 0))
    intColumnDeltas = mkTableColumnMap (== TyPrim TyInteger) 0
    decColumnDeltas = mkTableColumnMap (== TyPrim TyDecimal) 0
    cellsEnforced
      = mkTableColumnMap (== TyPrim TyKeySet) (mkSFunArray (const false))
    cellsWritten = mkTableColumnMap (const True) (mkSFunArray (const false))

    mkTableColumnMap
      :: (Pact.Type Pact.UserType -> Bool) -> a -> TableMap (ColumnMap a)
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

addConstraint :: S Bool -> Analyze ()
addConstraint s = modify' $ globalState.gasConstraints %~ (<> c)
  where
    c = Constraints $ constrain $ _sSbv s

sbvIdentifier :: Text -> Text
sbvIdentifier = T.replace "-" "_"

mkFreeArray :: (SymWord a, HasKind b) => Text -> SFunArray a b
mkFreeArray = mkSFunArray . uninterpret . T.unpack . sbvIdentifier

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

newtype Analyze a
  = Analyze
    { runAnalyze :: RWST AnalyzeEnv () AnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState AnalyzeState, MonadError AnalyzeFailure)

mkQueryEnv :: AnalyzeEnv -> AnalyzeState -> AVal -> QueryEnv
mkQueryEnv env state result = QueryEnv env state result Map.empty Map.empty

--
-- TODO: rename this. @Query@ is already taken by sbv.
--
newtype Query a
  = Query
    { queryAction :: ReaderT QueryEnv (ExceptT AnalyzeFailure Symbolic) a }
  deriving (Functor, Applicative, Monad, MonadReader QueryEnv,
            MonadError AnalyzeFailure)

mkAnalyzeEnv :: [Table] -> ModelTags -> Info -> AnalyzeEnv
mkAnalyzeEnv tables tags info =
  let keySets'    = mkFreeArray "keySets"
      keySetAuths = mkFreeArray "keySetAuths"

      invariants' = TableMap $ Map.fromList $ tables <&>
        \(Table tname _ut someInvariants) ->
          (TableName (T.unpack tname), someInvariants)

      argMap :: Map VarId AVal
      argMap = view (located._2._2) <$> _mtArgs tags

  in AnalyzeEnv argMap keySets' keySetAuths invariants' tags info

instance (Mergeable a) => Mergeable (Analyze a) where
  symbolicMerge force test left right = Analyze $ RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged.
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST (runAnalyze act) r
    in do
      (lRes, AnalyzeState lls lgs, ()) <- run left s
      (rRes, AnalyzeState rls rgs, ()) <- run right $ s & globalState .~ lgs

      return ( symbolicMerge force test lRes rRes
             , AnalyzeState (symbolicMerge force test lls rls) rgs
             , ()
             )

class HasAnalyzeEnv a where
  {-# MINIMAL analyzeEnv #-}
  analyzeEnv :: Lens' a AnalyzeEnv

  scope :: Lens' a (Map VarId AVal)
  scope = analyzeEnv.aeScope

  keySets :: Lens' a (SFunArray KeySetName KeySet)
  keySets = analyzeEnv.aeKeySets

  ksAuths :: Lens' a (SFunArray KeySet Bool)
  ksAuths = analyzeEnv.aeKsAuths

instance HasAnalyzeEnv AnalyzeEnv where analyzeEnv = id
instance HasAnalyzeEnv QueryEnv   where analyzeEnv = qeAnalyzeEnv

-- TODO Change SVal to AVal (allowing objects), update analyzer
newtype InvariantCheck a = InvariantCheck
  { unInvariantCheck :: ReaderT
    (Located (Map VarId AVal))
    (Either AnalyzeFailure) a
  } deriving (Functor, Applicative, Monad, MonadError AnalyzeFailure,
    MonadReader (Located (Map VarId AVal)))

instance Analyzer Analyze where
  type TermOf Analyze = Term
  analyze             = analyzeTerm
  analyzeO            = analyzeTermO
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid = view (scope . at vid)

instance Analyzer Query where
  type TermOf Query = Prop
  analyze           = analyzeProp
  analyzeO          = analyzePropO
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid = view (scope . at vid)

instance Analyzer InvariantCheck where
  type TermOf InvariantCheck  = Invariant
  analyze  (PureInvariant tm) = analyzePure tm
  analyzeO (PureInvariant tm) = analyzePureO tm
  throwErrorNoLoc err = do
    info <- view location
    throwError $ AnalyzeFailure info err
  getVar vid = view (located . at vid)


symArrayAt
  :: forall array k v
   . (SymWord k, SymWord v, SymArray array)
  => S k -> Lens' (array k v) (SBV v)
symArrayAt (S _ symKey) = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

tagAccessKey
  :: Lens' ModelTags (Map TagId (Located (S RowKey, Object)))
  -> TagId
  -> S RowKey
  -> Analyze ()
tagAccessKey lens' tid srk = do
  mTup <- preview $ aeModelTags.lens'.at tid._Just.located._1
  case mTup of
    -- NOTE: ATM we allow a "partial" model. we could also decide to
    -- 'throwError' here; we simply don't tag.
    Nothing     -> pure ()
    Just tagSrk -> addConstraint $ sansProv $ srk .== tagSrk

-- | "Tag" an uninterpreted read value with value from our Model that was
-- allocated in Symbolic.
tagAccessCell
  :: Lens' ModelTags (Map TagId (Located (S RowKey, Object)))
  -> TagId
  -> Text
  -> AVal
  -> Analyze ()
tagAccessCell lens' tid fieldName av = do
  mTag <- preview $
    aeModelTags.lens'.at tid._Just.located._2.objFields.at fieldName._Just._2
  case mTag of
    -- NOTE: ATM we allow a "partial" model. we could also decide to
    -- 'throwError' here; we simply don't tag.
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

-- | "Tag" an uninterpreted auth value with value from our Model that was
-- allocated in Symbolic.
tagAuth :: TagId -> Maybe Provenance -> S Bool -> Analyze ()
tagAuth tid mKsProv sb = do
  mTag <- preview $ aeModelTags.mtAuths.at tid._Just.located
  case mTag of
    -- NOTE: ATM we allow a "partial" model. we could also decide to
    -- 'throwError' here; we simply don't tag.
    Nothing  -> pure ()
    Just sbv -> do
      addConstraint $ sansProv $ sbv .== _sSbv sb
      globalState.gasKsProvenances.at tid .= mKsProv

tagResult :: AVal -> Analyze ()
tagResult av = do
  tag <- view $ aeModelTags.mtResult.located._2
  addConstraint $ sansProv $ tag .== av

tagVarBinding :: VarId -> AVal -> Analyze ()
tagVarBinding vid av = do
  mTag <- preview $ aeModelTags.mtVars.at vid._Just.located._2._2
  case mTag of
    -- NOTE: ATM we allow a "partial" model. we could also decide to
    -- 'throwError' here; we simply don't tag.
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

succeeds :: Lens' AnalyzeState (S Bool)
succeeds = latticeState.lasSucceeds.sbv2S

maintainsInvariants :: Lens' AnalyzeState (TableMap (ZipList (Located SBool)))
maintainsInvariants = latticeState.lasMaintainsInvariants

tableRead :: TableName -> Lens' AnalyzeState (S Bool)
tableRead tn = latticeState.lasTablesRead.symArrayAt (literalS tn).sbv2S

tableWritten :: TableName -> Lens' AnalyzeState (S Bool)
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literalS tn).sbv2S

intCellDelta
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Integer)
intCellDelta tn cn sRk = latticeState.lasIntCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

decCellDelta
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Decimal)
decCellDelta tn cn sRk = latticeState.lasDecCellDeltas.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

intColumnDelta :: TableName -> ColumnName -> Lens' AnalyzeState (S Integer)
intColumnDelta tn cn = latticeState.lasIntColumnDeltas.singular (ix tn).
  singular (ix cn)

decColumnDelta :: TableName -> ColumnName -> Lens' AnalyzeState (S Decimal)
decColumnDelta tn cn = latticeState.lasDecColumnDeltas.singular (ix tn).
  singular (ix cn)

rowReadCount :: TableName -> S RowKey -> Lens' AnalyzeState (S Integer)
rowReadCount tn sRk = latticeState.lasRowsRead.singular (ix tn).
  symArrayAt sRk.sbv2S

rowWriteCount :: TableName -> S RowKey -> Lens' AnalyzeState (S Integer)
rowWriteCount tn sRk = latticeState.lasRowsWritten.singular (ix tn).
  symArrayAt sRk.sbv2S

cellEnforced
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Bool)
cellEnforced tn cn sRk = latticeState.lasCellsEnforced.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

cellWritten
  :: TableName
  -> ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Bool)
cellWritten tn cn sRk = latticeState.lasCellsWritten.singular (ix tn).
  singular (ix cn).symArrayAt sRk.sbv2S

intCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Integer)
intCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scIntValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

boolCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Bool)
boolCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scBoolValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

stringCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S String)
stringCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scStringValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

decimalCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Decimal)
decimalCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scDecimalValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

timeCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Time)
timeCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scTimeValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

ksCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S KeySet)
ksCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scKsValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (fromCell tn cn sRk sDirty)

symKsName :: S String -> S KeySetName
symKsName = coerceS

resolveKeySet
  :: (MonadReader r m, HasAnalyzeEnv r, MonadError AnalyzeFailure m)
  => S KeySetName
  -> m (S KeySet)
resolveKeySet sKsn = fmap (withProv $ fromNamedKs sKsn) $
  readArray <$> view keySets <*> pure (_sSbv sKsn)

nameAuthorized
  :: (MonadReader r m, HasAnalyzeEnv r, MonadError AnalyzeFailure m)
  => S KeySetName
  -> m (S Bool)
nameAuthorized sKsn = fmap sansProv $
  readArray <$> view ksAuths <*> (_sSbv <$> resolveKeySet sKsn)

ksAuthorized :: S KeySet -> Analyze (S Bool)
ksAuthorized sKs = do
  -- NOTE: we know that KsAuthorized constructions are only emitted within
  -- Enforced constructions, so we know that this keyset is being enforced
  -- here.
  case sKs ^? sProv._Just._FromCell of
    Just (OriginatingCell tn sCn sRk sDirty) ->
      cellEnforced tn sCn sRk %= (||| bnot sDirty)
    Nothing ->
      pure ()
  fmap sansProv $ readArray <$> view ksAuths <*> pure (_sSbv sKs)

aval
  :: Analyzer m
  => (Maybe Provenance -> SBVI.SVal -> m a)
  -> (Object -> m a)
  -> AVal
  -> m a
aval elimVal elimObj = \case
  AVal mProv sval -> elimVal mProv sval
  AnObj obj       -> elimObj obj
  OpaqueVal       -> throwErrorNoLoc OpaqueValEncountered

expectVal :: Analyzer m => AVal -> m (S a)
expectVal = aval (pure ... mkS) (throwErrorNoLoc . AValUnexpectedlyObj)

expectObj :: Analyzer m => AVal -> m Object
expectObj = aval ((throwErrorNoLoc . AValUnexpectedlySVal) ... getSVal) pure
  where
    getSVal :: Maybe Provenance -> SBVI.SVal -> SBVI.SVal
    getSVal = flip const

applyInvariants
  :: TableName
  -> Map VarId AVal
  -- ^ Mapping from the fields in this table to the @SVal@ holding that field
  --   in this context.
  -> ([S Bool] -> Analyze ())
  -- ^ The function used to apply an invariant in this context. The @SBV Bool@
  --   is an assertion of what it would take for the invariant to be true in
  --   this context.
  -> Analyze ()
applyInvariants tn sValFields addInvariants = do
  mInvariants <- view (invariants . at tn)
  case mInvariants of
    Nothing -> pure ()
    Just invariants' -> do
      invariants'' <- for invariants' $ \(Located info invariant) ->
        case runReaderT (unInvariantCheck (analyze invariant))
                        (Located info sValFields) of
          -- Use the location of the invariant
          Left  (AnalyzeFailure _ err) -> throwError $ AnalyzeFailure info err
          Right inv -> pure inv
      addInvariants invariants''

analyzeAtO
  :: forall m
   . Analyzer m
  => TermOf m String
  -> TermOf m Object
  -> m Object
analyzeAtO colNameT objT = do
    obj@(Object fields) <- analyzeO objT
    sCn <- analyze colNameT

    let getObjVal :: Text -> m Object
        getObjVal fieldName = case Map.lookup fieldName fields of
          Nothing -> throwErrorNoLoc $ KeyNotPresent fieldName obj
          Just (fieldType, AVal _ _) -> throwErrorNoLoc $
            ObjFieldOfWrongType fieldName fieldType
          Just (_fieldType, AnObj subObj) -> pure subObj
          Just (_fieldType, OpaqueVal) -> throwErrorNoLoc OpaqueValEncountered

    case unliteralS sCn of
      Nothing -> throwErrorNoLoc "Unable to determine statically the key used in an object access evaluating to an object (this is an object in an object)"
      Just concreteColName -> getObjVal (T.pack concreteColName)

analyzeAt
  :: (Analyzer m, SymWord a)
  => Schema
  -> TermOf m String
  -> TermOf m Object
  -> EType
  -> m (S a)
analyzeAt schema@(Schema schemaFields) colNameT objT retType = do
  obj@(Object fields) <- analyzeO objT

  -- Filter down to only fields which contain the type we're looking for
  let relevantFields
        = map fst
        $ filter (\(_name, ty) -> ty == retType)
        $ Map.toList schemaFields

  colName :: S String <- analyze colNameT

  firstName:relevantFields' <- case relevantFields of
    [] -> throwErrorNoLoc $ AtHasNoRelevantFields retType schema
    _  -> pure relevantFields

  let getObjVal fieldName = case Map.lookup fieldName fields of
        Nothing -> throwErrorNoLoc $ KeyNotPresent fieldName obj

        Just (_fieldType, AVal mProv sval) -> pure $ mkS mProv sval

        Just (fieldType, AnObj _subObj) -> throwErrorNoLoc $
          ObjFieldOfWrongType fieldName fieldType

        Just (_fieldType, OpaqueVal) -> throwErrorNoLoc OpaqueValEncountered

  firstVal <- getObjVal firstName

  -- Fold over each relevant field, building a sequence of `ite`s. We require
  -- at least one matching field, ie firstVal. At first glance, this should
  -- just be a `foldr1M`, but we want the type of accumulator and element to
  -- differ, because elements are `String` `fieldName`s, while the accumulator
  -- is an `SBV a`.
  foldrM
    (\fieldName rest -> do
      val <- getObjVal fieldName
      pure $ ite (colName .== literalS (T.unpack fieldName)) val rest
    )
    firstVal
    relevantFields'

analyzeETerm :: ETerm -> Analyze AVal
analyzeETerm (ETerm _ tm)   = mkAVal <$> analyzeTerm tm
analyzeETerm (EObject _ tm) = AnObj <$> analyzeTermO tm

analyzeETerm' :: ETerm -> Analyze TVal
analyzeETerm' et = (existentialType et,) <$> analyzeETerm et

analyzeTermO :: Term Object -> Analyze Object
analyzeTermO = \case
  PureTerm a -> analyzePureO a

  Read tid tn (Schema fields) rowKey -> do
    sRk <- symRowKey <$> analyzeTerm rowKey
    tableRead tn .= true
    rowReadCount tn sRk += 1
    tagAccessKey mtReads tid sRk


    aValFields <- iforM fields $ \fieldName fieldType -> do
      let cn = ColumnName $ T.unpack fieldName
      sDirty <- use $ cellWritten tn cn sRk

      av <- case fieldType of
        EType TInt     -> mkAVal <$> use (intCell     tn cn sRk sDirty)
        EType TBool    -> mkAVal <$> use (boolCell    tn cn sRk sDirty)
        EType TStr     -> mkAVal <$> use (stringCell  tn cn sRk sDirty)
        EType TDecimal -> mkAVal <$> use (decimalCell tn cn sRk sDirty)
        EType TTime    -> mkAVal <$> use (timeCell    tn cn sRk sDirty)
        EType TKeySet  -> mkAVal <$> use (ksCell      tn cn sRk sDirty)
        EType TAny     -> pure OpaqueVal
        --
        -- TODO: if we add nested object support here, we need to install
        --       the correct provenance into AVals all the way down into
        --       sub-objects.
        --
        EObjectTy _    -> throwErrorNoLoc UnsupportedObjectInDbCell

      tagAccessCell mtReads tid fieldName av

      pure (fieldType, av)

    applyInvariants tn (snd <$> varIdColumns' aValFields) (mapM_ addConstraint)

    pure $ Object aValFields

  Let _name vid eterm body -> do
    av <- analyzeETerm eterm
    tagVarBinding vid av
    local (scope.at vid ?~ av) $
      analyzeTermO body

  Sequence eterm objT -> analyzeETerm eterm *> analyzeTermO objT

  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    case unliteralS testPasses of
      Just True  -> analyzeTermO then'
      Just False -> analyzeTermO else'
      Nothing    -> throwErrorNoLoc "Unable to determine statically the branch taken in an if-then-else evaluating to an object"

-- Note [Time Representation]
--
-- Pact uses the Thyme library (UTCTime) to represent times. Thyme internally
-- uses a 64-bit count of microseconds since the MJD epoch. So, our symbolic
-- representation is naturally a 64-bit integer.
--
-- The effect from a Pact-user's point of view is that we stores 6 digits to
-- the right of the decimal point in times (even though we don't print
-- sub-second precision by default...).
--
-- pact> (add-time (time "2016-07-23T13:30:45Z") 0.001002)
-- "2016-07-23T13:30:45Z"
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.0010021))
-- true
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.001003))
-- false

analyzeIntAddTime
  :: Analyzer m
  => TermOf m Time
  -> TermOf m Integer
  -> m (S Time)
analyzeIntAddTime timeT secsT = do
  time <- analyze timeT
  secs <- analyze secsT
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  pure $ time + fromIntegralS (secs * 1000000)

analyzeDecAddTime
  :: Analyzer m
  => TermOf m Time
  -> TermOf m Decimal
  -> m (S Time)
analyzeDecAddTime timeT secsT = do
  time <- analyze timeT
  secs <- analyze secsT
  if isConcreteS secs
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  then pure $ time + fromIntegralS (banker'sMethod (secs * 1000000))
  else throwErrorNoLoc $ PossibleRoundoff
    "A time being added is not concrete, so we can't guarantee that roundoff won't happen when it's converted to an integer."

analyzeEqNeq
  :: (Analyzer m, SymWord a, Show a)
  => EqNeq
  -> TermOf m a
  -> TermOf m a
  -> m (S Bool)
analyzeEqNeq op xT yT = do
  x <- analyze xT
  y <- analyze yT
  pure $ sansProv $ case op of
    Eq'  -> x .== y
    Neq' -> x ./= y

analyzeObjectEqNeq
  :: Analyzer m
  => EqNeq
  -> TermOf m Object
  -> TermOf m Object
  -> m (S Bool)
analyzeObjectEqNeq op xT yT = do
  x <- analyzeO xT
  y <- analyzeO yT
  pure $ sansProv $ case op of
    Eq'  -> x .== y
    Neq' -> x ./= y

analyzeComparisonOp
  :: (Analyzer m, SymWord a, Show a)
  => ComparisonOp
  -> TermOf m a
  -> TermOf m a
  -> m (S Bool)
analyzeComparisonOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  pure $ sansProv $ case op of
    Gt  -> x .> y
    Lt  -> x .< y
    Gte -> x .>= y
    Lte -> x .<= y
    Eq  -> x .== y
    Neq -> x ./= y

analyzeLogicalOp
  :: (Analyzer m, Boolean (S a), Show a, SymWord a)
  => LogicalOp
  -> [TermOf m a]
  -> m (S a)
analyzeLogicalOp op terms = do
  symBools <- traverse analyze terms
  case (op, symBools) of
    (AndOp, [a, b]) -> pure $ a &&& b
    (OrOp,  [a, b]) -> pure $ a ||| b
    (NotOp, [a])    -> pure $ bnot a
    _               -> throwErrorNoLoc $ MalformedLogicalOpExec op $ length terms

analyzeTerm :: (Show a, SymWord a) => Term a -> Analyze (S a)
analyzeTerm = \case
  PureTerm a -> analyzePure a

  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    iteS testPasses (analyzeTerm then') (analyzeTerm else')

  Enforce cond -> do
    cond' <- analyzeTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence eterm valT -> analyzeETerm eterm *> analyzeTerm valT

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tid tn rowKey obj -> do
    Object obj' <- analyzeTermO obj
    sRk <- symRowKey <$> analyzeTerm rowKey
    tableWritten tn .= true
    rowWriteCount tn sRk += 1
    tagAccessKey mtWrites tid sRk

    mValFields <- iforM obj' $ \colName (fieldType, aval') -> do
      let cn = ColumnName (T.unpack colName)
      cellWritten tn cn sRk .= true
      tagAccessCell mtWrites tid colName aval'

      case aval' of
        AVal mProv sVal -> do
          let writeDelta :: forall t
                          . (Num t, SymWord t)
                         => (TableName -> ColumnName -> S RowKey -> S Bool -> Lens' AnalyzeState (S t))
                         -> (TableName -> ColumnName -> S RowKey ->           Lens' AnalyzeState (S t))
                         -> (TableName -> ColumnName ->                       Lens' AnalyzeState (S t))
                         -> Analyze ()
              writeDelta mkCellL mkCellDeltaL mkColDeltaL = do
                let cell :: Lens' AnalyzeState (S t)
                    cell = mkCellL tn cn sRk true
                let next = mkS mProv sVal
                prev <- use cell
                cell .= next
                let diff = next - prev
                mkCellDeltaL tn cn sRk += diff
                mkColDeltaL  tn cn     += diff

          case fieldType of
            EType TInt     -> writeDelta intCell intCellDelta intColumnDelta
            EType TBool    -> boolCell    tn cn sRk true .= mkS mProv sVal
            EType TDecimal -> writeDelta decimalCell decCellDelta decColumnDelta
            EType TTime    -> timeCell    tn cn sRk true .= mkS mProv sVal
            EType TStr     -> stringCell  tn cn sRk true .= mkS mProv sVal
            EType TKeySet  -> ksCell      tn cn sRk true .= mkS mProv sVal
            EType TAny     -> void $ throwErrorNoLoc OpaqueValEncountered
            EObjectTy _    -> void $ throwErrorNoLoc UnsupportedObjectInDbCell

          pure (Just aval')

            -- TODO: handle EObjectTy here

        -- TODO(joel): I'm not sure this is the right error to throw
        AnObj obj'' -> throwErrorNoLoc $ AValUnexpectedlyObj obj''
        OpaqueVal   -> throwErrorNoLoc OpaqueValEncountered


    let aValFields :: Map VarId AVal
        aValFields = Map.mapMaybe id (varIdColumns' mValFields)

    applyInvariants tn aValFields $ \invariants' ->
      let fs :: ZipList (Located (SBV Bool) -> Located (SBV Bool))
          fs = ZipList $ (\s -> fmap (_sSbv s &&&)) <$> invariants'
      in maintainsInvariants . at tn . _Just %= (fs <*>)

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literalS "Write succeeded"

  Let _name vid eterm body -> do
    av <- analyzeETerm eterm
    tagVarBinding vid av
    local (scope.at vid ?~ av) $
      analyzeTerm body

  ReadKeySet str -> resolveKeySet =<< symKsName <$> analyzeTerm str

  KsAuthorized tid ksT -> do
    ks <- analyzeTerm ksT
    authorized <- ksAuthorized ks
    tagAuth tid (ks ^. sProv) authorized
    pure authorized

  NameAuthorized tid str -> do
    ksn <- symKsName <$> analyzeTerm str
    authorized <- nameAuthorized ksn
    tagAuth tid (Just $ fromNamedKs ksn) authorized
    pure authorized

  PactVersion -> pure $ literalS $ T.unpack pactVersion

  Format formatStr args -> do
    formatStr' <- analyze formatStr
    args' <- for args $ \case
      ETerm TStr  str  -> Left          <$> analyze str
      ETerm TInt  int  -> Right . Left  <$> analyze int
      ETerm TBool bool -> Right . Right <$> analyze bool
      _                -> throwErrorNoLoc "We can only analyze calls to `format` formatting {string,integer,bool}"
    case unliteralS formatStr' of
      Nothing -> throwErrorNoLoc "We can only analyze calls to `format` with statically determined contents (both arguments)"
      Just concreteStr -> case format concreteStr args' of
        Left err -> throwError err
        Right tm -> pure tm

  FormatTime formatStr time -> do
    formatStr' <- analyze formatStr
    time'      <- analyze time
    case (unliteralS formatStr', unliteralS time') of
      (Just formatStr'', Just time'') -> pure $ literalS $
        formatTime defaultTimeLocale formatStr'' (unMkTime time'')
      _ -> throwErrorNoLoc "We can only analyze calls to `format-time` with statically determined contents (both arguments)"

  ParseTime mFormatStr timeStr -> do
    formatStr' <- case mFormatStr of
      Just formatStr -> analyze formatStr
      Nothing        -> pure $ literalS Pact.simpleISO8601
    timeStr'   <- analyze timeStr
    case (unliteralS formatStr', unliteralS timeStr') of
      (Just formatStr'', Just timeStr'') ->
        case parseTime defaultTimeLocale formatStr'' timeStr'' of
          Nothing   -> succeeds .= false >> pure 0
          Just time -> pure $ literalS $ mkTime time
      _ -> throwErrorNoLoc "We can only analyze calls to `parse-time` with statically determined contents (both arguments)"

  Hash value -> do
    let sHash = literalS . T.unpack . Pact.asString . Pact.hash
        notStaticErr :: AnalyzeFailure
        notStaticErr = AnalyzeFailure dummyInfo "We can only analyze calls to `hash` with statically determined contents"
    case value of
      -- Note that strings are hashed in a different way from the other types
      ETerm TStr tm -> analyze tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just str -> pure $ sHash $ encodeUtf8 $ T.pack str

      -- Everything else is hashed by first converting it to JSON:
      ETerm TInt tm -> analyze tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just int -> pure $ sHash $ toStrict $ Aeson.encode int
      ETerm TBool tm -> analyze tm <&> unliteralS >>= \case
        Nothing   -> throwError notStaticErr
        Just bool -> pure $ sHash $ toStrict $ Aeson.encode bool

      -- In theory we should be able to analyze decimals -- we just need to be
      -- able to convert them back into Decimal.Decimal decimals (from SBV's
      -- Real representation). This is probably possible if we think about it
      -- hard enough.
      ETerm TDecimal _ -> throwErrorNoLoc "We can't yet analyze calls to `hash` on decimals"

      ETerm _ _        -> throwErrorNoLoc "We can't yet analyze calls to `hash` on non-{string,integer,bool}"
      EObject _ _      -> throwErrorNoLoc "We can't yet analyze calls to `hash on objects"

  n -> throwErrorNoLoc $ UnhandledTerm $ tShow n


liftSymbolic :: Symbolic a -> Query a
liftSymbolic = Query . lift . lift


-- For now we only allow these three types to be formatted.
--
-- Formatting behavior is not well specified. Its behavior on these types is
-- easy to infer from examples in the docs. We would also like to be able to
-- format decimals, but that's a little harder (we could still make it work).
-- Behavior on structured data is not specified.
type Formattable = Either (S String) (Either (S Integer) (S Bool))

-- This definition was taken from Pact.Native, then modified to be symbolic
format :: String -> [Formattable] -> Either AnalyzeFailure (S String)
format s tms = do
  -- TODO: don't convert to Text and back. splitOn is provided by both the
  -- split and MissingH packages.
  let parts = literalS . T.unpack <$> T.splitOn "{}" (pack s)
      plen = length parts
      rep = \case
        Left  str          -> str
        Right (Right bool) -> ite (_sSbv bool) "true" "false"
        Right (Left int)   -> sansProv (SBV.natToStr (_sSbv int))
  if plen == 1
  then Right (literalS s)
  else if plen - length tms > 1
       then Left (AnalyzeFailure dummyInfo "format: not enough arguments for template")
       else Right $ foldl'
              (\r (e, t) -> r .++ rep e .++ t)
              (head parts)
              (zip tms (tail parts))

getLitTableName :: Prop TableName -> Query TableName
getLitTableName (PLit tn) = pure tn
getLitTableName (PureProp (Var vid name)) = do
  mTn <- view $ qeTableScope . at vid
  case mTn of
    Nothing -> throwErrorNoLoc $ fromString $
      "could not find table in scope: " <> T.unpack name
    Just tn -> pure tn
getLitTableName (PropSpecific Result)
  = throwErrorNoLoc "Function results can't be table names"
getLitTableName PureProp{} = throwErrorNoLoc "Pure values can't be table names"


getLitColName :: Prop ColumnName -> Query ColumnName
getLitColName (PLit cn) = pure cn
getLitColName _ = throwErrorNoLoc "TODO: column quantification"


analyzeProp :: SymWord a => Prop a -> Query (S a)
analyzeProp (PureProp tm)    = analyzePure tm
analyzeProp (PropSpecific a) = analyzePropSpecific a


analyzePropO :: Prop Object -> Query Object
analyzePropO (PureProp a)          = analyzePureO a
analyzePropO (PropSpecific Result) = expectObj =<< view qeAnalyzeResult


analyzePropSpecific :: SymWord a => PropSpecific a -> Query (S a)
analyzePropSpecific Success = view $ qeAnalyzeState.succeeds
analyzePropSpecific Abort   = bnot <$> analyzePropSpecific Success
analyzePropSpecific Result  = expectVal =<< view qeAnalyzeResult
analyzePropSpecific (Forall vid _name (EType (_ :: Types.Type ty)) p) = do
  sbv <- liftSymbolic (forall_ :: Symbolic (SBV ty))
  local (scope.at vid ?~ mkAVal' sbv) $ analyzeProp p
analyzePropSpecific (Forall _vid _name (EObjectTy _) _p) =
  throwErrorNoLoc "objects can't currently be quantified in properties (issue 139)"
analyzePropSpecific (Forall vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (& qeTableScope . at vid ?~ tableName) (analyzeProp prop)
  pure $ foldr (&&&) true bools
analyzePropSpecific (Forall _vid _name (QColumnOf _tab) _p) =
  throwErrorNoLoc "TODO: column quantification"
analyzePropSpecific (Exists vid _name (EType (_ :: Types.Type ty)) p) = do
  sbv <- liftSymbolic (exists_ :: Symbolic (SBV ty))
  local (scope.at vid ?~ mkAVal' sbv) $ analyzeProp p
analyzePropSpecific (Exists _vid _name (EObjectTy _) _p) =
  throwErrorNoLoc "objects can't currently be quantified in properties (issue 139)"
analyzePropSpecific (Exists vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (& qeTableScope . at vid ?~ tableName) (analyzeProp prop)
  pure $ foldr (|||) true bools
analyzePropSpecific (Exists _vid _name (QColumnOf _tab) _p) =
  throwErrorNoLoc "TODO: column quantification"

-- DB properties
analyzePropSpecific (TableRead tn) = do
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.tableRead tn'
analyzePropSpecific (TableWrite tn) = do
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.tableWritten tn'
analyzePropSpecific (ColumnWrite _ _)
  = throwErrorNoLoc "column write analysis not yet implemented"
analyzePropSpecific (ColumnRead _ _)
  = throwErrorNoLoc "column read analysis not yet implemented"
--
-- TODO: should we introduce and use CellWrite to subsume other cases?
--
analyzePropSpecific (IntCellDelta tn cn pRk) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  sRk <- analyzeProp pRk
  view $ qeAnalyzeState.intCellDelta tn' cn' sRk
analyzePropSpecific (DecCellDelta tn cn pRk) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  sRk <- analyzeProp pRk
  view $ qeAnalyzeState.decCellDelta tn' cn' sRk
analyzePropSpecific (IntColumnDelta tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.intColumnDelta tn' cn'
analyzePropSpecific (DecColumnDelta tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.decColumnDelta tn' cn'
analyzePropSpecific (RowRead tn pRk)  = do
  sRk <- analyzeProp pRk
  tn' <- getLitTableName tn
  numReads <- view $ qeAnalyzeState.rowReadCount tn' sRk
  pure $ sansProv $ numReads .== 1
analyzePropSpecific (RowReadCount tn pRk)  = do
  sRk <- analyzeProp pRk
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.rowReadCount tn' sRk
analyzePropSpecific (RowWrite tn pRk) = do
  sRk <- analyzeProp pRk
  tn' <- getLitTableName tn
  writes <- view $ qeAnalyzeState.rowWriteCount tn' sRk
  pure $ sansProv $ writes .== 1
analyzePropSpecific (RowWriteCount tn pRk) = do
  sRk <- analyzeProp pRk
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.rowWriteCount tn' sRk

-- Authorization
analyzePropSpecific (KsNameAuthorized ksn) = nameAuthorized $ literalS ksn
analyzePropSpecific (RowEnforced tn cn pRk) = do
  sRk <- analyzeProp pRk
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.cellEnforced tn' cn' sRk


analyzeE :: Analyzer m => Existential (TermOf m) -> m (EType, AVal)
analyzeE = \case
  ETerm ty prop   -> do
    prop' <- analyze prop
    pure (EType ty, mkAVal prop')
  EObject ty prop -> do
    prop' <- analyzeO prop
    pure (EObjectTy ty, AnObj prop')


-- TODO: move to Pact.Analyze.AnalyzePure
analyzePure
  :: (Analyzer m, SymWord a)
  => PureTerm (TermOf m) a -> m (S a)
analyzePure (Lit a)                    = pure (literalS a)
analyzePure (Sym s)                    = pure s
analyzePure (StrConcat p1 p2)          = (.++) <$> analyze p1 <*> analyze p2
analyzePure (StrLength p)              = over s2Sbv SBV.length <$> analyze p
analyzePure (Numerical a)              = analyzeNumerical a
analyzePure (IntAddTime time secs)     = analyzeIntAddTime time secs
analyzePure (DecAddTime time secs)     = analyzeDecAddTime time secs
analyzePure (IntegerComparison op x y) = analyzeComparisonOp op x y
analyzePure (DecimalComparison op x y) = analyzeComparisonOp op x y
analyzePure (TimeComparison op x y)    = analyzeComparisonOp op x y
analyzePure (StringComparison op x y)  = analyzeComparisonOp op x y
analyzePure (BoolComparison op x y)    = analyzeComparisonOp op x y
analyzePure (ObjectEqNeq op x y)       = analyzeObjectEqNeq  op x y
analyzePure (KeySetEqNeq      op x y)  = analyzeEqNeq        op x y
analyzePure (Logical op props)         = analyzeLogicalOp op props
analyzePure (At schema colNameT objT retType)
  = analyzeAt schema colNameT objT retType
analyzePure LiteralObject {}
  = error "literal object can't be an argument to analyzePure"
analyzePure (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing                -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just (AnObj obj)       -> throwErrorNoLoc $ AValUnexpectedlyObj obj
    Just (OpaqueVal)       -> throwErrorNoLoc OpaqueValEncountered


analyzePureO
  :: Analyzer m
  => PureTerm (TermOf m) Object -> m Object
analyzePureO (LiteralObject obj) = Object <$> traverse analyzeE obj
analyzePureO (At _schema colNameT objT _retType)
  = analyzeAtO colNameT objT
analyzePureO (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing            -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal _ val') -> throwErrorNoLoc $ AValUnexpectedlySVal val'
    Just (AnObj obj)   -> pure obj
    Just (OpaqueVal)   -> throwErrorNoLoc OpaqueValEncountered

-- TODO(joel): I don't think an object can appear hear. Get more clarity on
-- this.
analyzePureO (Lit obj)     = pure obj
analyzePureO (Sym _)       = vacuousMatch "an object cannot be a symbolic value"
analyzePureO (Numerical _) = vacuousMatch "an object cannot be a numerical value"


analyzeCheck :: Check -> Query (S Bool)
analyzeCheck = \case
    PropertyHolds p -> assumingSuccess =<< analyzeProp p
    Valid p         -> analyzeProp p
    Satisfiable p   -> analyzeProp p

  where
    assumingSuccess :: S Bool -> Query (S Bool)
    assumingSuccess p = do
      success <- view (qeAnalyzeState.succeeds)
      pure $ success ==> p

-- | A convenience to treat a nested 'TableMap', '[]', and tuple as a single
-- functor instead of three.
newtype InvariantsF a = InvariantsF { unInvariantsF :: TableMap [Located a] }

instance Functor InvariantsF where
  fmap f (InvariantsF a) = InvariantsF ((fmap . fmap . fmap) f a)

analyzeInvariants :: Query (InvariantsF (S Bool))
analyzeInvariants = assumingSuccess =<< invariantsHold''
  where
    assumingSuccess :: InvariantsF (S Bool) -> Query (InvariantsF (S Bool))
    assumingSuccess ps = do
      success <- view (qeAnalyzeState.succeeds)
      pure $ (success ==>) <$> ps

    invariantsHold :: Query (TableMap (ZipList (Located (SBV Bool))))
    invariantsHold = view (qeAnalyzeState.maintainsInvariants)

    invariantsHold' :: Query (InvariantsF (SBV Bool))
    invariantsHold' = InvariantsF <$> (getZipList <$$> invariantsHold)

    invariantsHold'' :: Query (InvariantsF (S Bool))
    invariantsHold'' = sansProv <$$> invariantsHold'

-- | Helper to run either property or invariant analysis
runAnalysis'
  :: Functor f
  => Query (f (S Bool))
  -> [Table]
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (f AnalysisResult)
runAnalysis' query tables tm tags info = do
  let act    = analyzeETerm tm >>= \res -> tagResult res >> pure res
      aEnv   = mkAnalyzeEnv tables tags info
      state0 = mkInitialAnalyzeState tables

  (funResult, state1, ()) <- hoist generalize $
    runRWST (runAnalyze act) aEnv state0

  lift $ runConstraints $ state1 ^. globalState.gasConstraints

  let qEnv  = mkQueryEnv aEnv state1 funResult
      ksProvs = state1 ^. globalState.gasKsProvenances

  results <- runReaderT (queryAction query) qEnv
  pure $ results <&> \prop -> AnalysisResult (_sSbv prop) ksProvs

runPropertyAnalysis
  :: Check
  -> [Table]
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic AnalysisResult
runPropertyAnalysis check tables tm tags info =
  runIdentity <$> runAnalysis' (Identity <$> analyzeCheck check) tables tm tags info

runInvariantAnalysis
  :: [Table]
  -> ETerm
  -> ModelTags
  -> Info
  -> ExceptT AnalyzeFailure Symbolic (TableMap [Located AnalysisResult])
runInvariantAnalysis tables tm tags info =
  unInvariantsF <$> runAnalysis' analyzeInvariants tables tm tags info
