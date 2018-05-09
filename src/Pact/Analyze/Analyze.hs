{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Pact.Analyze.Analyze where

import           Control.Lens              (At (at), Index, IxValue, Ixed (ix),
                                            Lens', ifoldlM, iforM, lens,
                                            makeLenses, over, singular, use,
                                            view, (%=), (&), (+=), (.=), (.~),
                                            (?~), (^.), _2)
import           Control.Monad             (void)
import           Control.Monad.Except      (Except, ExceptT (ExceptT),
                                            MonadError (throwError), runExcept)
import           Control.Monad.Reader      (MonadReader (local), Reader,
                                            ReaderT, asks, runReader)
import           Control.Monad.RWS.Strict  (RWST (RWST, runRWST))
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer      (MonadWriter (tell))
import           Data.Foldable             (foldrM)
import           Data.Functor.Identity     (Identity (Identity))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Map.Strict.Merge     (mapMissing, merge, zipWithMatched)
import           Data.Maybe                (catMaybes)
import           Data.Monoid               ((<>))
import           Data.SBV                  hiding (ProofError, Satisfiable,
                                            Unknown, Unsatisfiable, name, (.++))
import qualified Data.SBV.Internals        as SBVI
import qualified Data.SBV.String           as SBV
import qualified Data.Set                  as Set
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable          (for)

import           Pact.Types.Runtime        (Arg (Arg), PrimType (TyBool, TyDecimal, TyInteger, TyKeySet, TyString, TyTime, TyValue),
                                            Type (TyAny, TyFun, TyList, TyPrim, TySchema, TyUser, TyVar),
                                            tShow)
import qualified Pact.Types.Runtime        as Pact
import qualified Pact.Types.Typecheck      as Pact
import           Pact.Types.Version        (pactVersion)

import           Pact.Analyze.Prop
import           Pact.Analyze.Types

data AnalyzeEnv = AnalyzeEnv
  { _aeScope    :: Map Text AVal            -- used with 'local' as a stack
  , _aeKeySets  :: SArray KeySetName KeySet -- read-only
  , _aeKsAuths  :: SArray KeySet Bool       -- read-only
  , _invariants :: Map (TableName, ColumnName) (SchemaInvariant Bool)
  } deriving Show

newtype Constraints
  = Constraints { runConstraints :: Symbolic () }

instance Monoid Constraints where
  mempty = Constraints (pure ())
  mappend (Constraints act1) (Constraints act2) = Constraints $ act1 *> act2

instance Mergeable Constraints where
  symbolicMerge _f _t = mappend

data SymbolicCells
  = SymbolicCells
    { _scIntValues     :: ColumnMap (SArray RowKey Integer)
    , _scBoolValues    :: ColumnMap (SArray RowKey Bool)
    , _scStringValues  :: ColumnMap (SArray RowKey String)
    , _scDecimalValues :: ColumnMap (SArray RowKey Decimal)
    , _scTimeValues    :: ColumnMap (SArray RowKey Time)
    , _scKsValues      :: ColumnMap (SArray RowKey KeySet)
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

newtype ColumnMap a
  = ColumnMap { _columnMap :: Map ColumnName a }
  deriving (Show, Functor, Foldable, Traversable, Monoid)

instance Mergeable a => Mergeable (ColumnMap a) where
  symbolicMerge force test (ColumnMap left) (ColumnMap right) = ColumnMap $
    merge
      (mapMissing $ \_ _ -> error "bad column map merge")
      (mapMissing $ \_ _ -> error "bad column map merge")
      (zipWithMatched $ \_k l r -> symbolicMerge force test l r)
      left
      right

newtype TableMap a
  = TableMap { _tableMap :: Map TableName a }
  deriving (Show, Functor, Foldable, Traversable)

instance Mergeable a => Mergeable (TableMap a) where
  symbolicMerge force test (TableMap left) (TableMap right) = TableMap $
    -- intersection is fine here; we know each map has all tables:
    Map.intersectionWith (symbolicMerge force test) left right

-- Checking state that is split before, and merged after, conditionals.
data LatticeAnalyzeState
  = LatticeAnalyzeState
    { _lasSucceeds            :: SBV Bool
    , _lasMaintainsInvariants :: SBV Bool
    , _lasTablesRead          :: SFunArray TableName Bool
    , _lasTablesWritten       :: SFunArray TableName Bool
    , _lasIntColumnDeltas     :: TableMap (ColumnMap (S Integer))
    , _lasDecColumnDeltas     :: TableMap (ColumnMap (S Decimal))
    , _lasTableCells          :: TableMap SymbolicCells
    , _lasRowsRead            :: TableMap (SFunArray RowKey Bool)
    , _lasRowsWritten         :: TableMap (SFunArray RowKey Bool)
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
      success  tsInvariants  tsRead  tsWritten  intDeltas  decDeltas  cells
      rsRead  rsWritten  csEnforced  csWritten)
    (LatticeAnalyzeState
      success' tsInvariants' tsRead' tsWritten' intDeltas' decDeltas' cells'
      rsRead' rsWritten' csEnforced' csWritten')
        = LatticeAnalyzeState
          (symbolicMerge force test success      success')
          (symbolicMerge force test tsInvariants tsInvariants')
          (symbolicMerge force test tsRead       tsRead')
          (symbolicMerge force test tsWritten    tsWritten')
          (symbolicMerge force test intDeltas    intDeltas')
          (symbolicMerge force test decDeltas    decDeltas')
          (symbolicMerge force test cells        cells')
          (symbolicMerge force test rsRead       rsRead')
          (symbolicMerge force test rsWritten    rsWritten')
          (symbolicMerge force test csEnforced   csEnforced')
          (symbolicMerge force test csWritten    csWritten')

-- Checking state that is transferred through every computation, in-order.
newtype GlobalAnalyzeState
  = GlobalAnalyzeState ()
  deriving (Show, Eq)

data AnalyzeState
  = AnalyzeState
    { _latticeState :: LatticeAnalyzeState
    , _globalState  :: GlobalAnalyzeState
    }
  deriving (Show)

data QueryEnv
  = QueryEnv
    { _qeAnalyzeEnv    :: AnalyzeEnv
    , _model           :: AnalyzeState
    , _qeAnalyzeResult :: AVal
    }

makeLenses ''ColumnMap
makeLenses ''AnalyzeEnv
makeLenses ''TableMap
makeLenses ''AnalyzeState
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells
makeLenses ''QueryEnv

type instance Index (ColumnMap a) = ColumnName
type instance IxValue (ColumnMap a) = a
instance Ixed (ColumnMap a) where ix k = columnMap.ix k
instance At (ColumnMap a) where at k = columnMap.at k

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k

instance Mergeable AnalyzeState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (AnalyzeState lls _) (AnalyzeState rls rgs) =
    AnalyzeState (symbolicMerge force test lls rls) rgs

mkInitialAnalyzeState
  :: [(Text, Pact.UserType)]
  -> TableMap SymbolicCells
  -> AnalyzeState
mkInitialAnalyzeState tables tableCells = AnalyzeState
    { _latticeState = LatticeAnalyzeState
        { _lasSucceeds            = true
        , _lasMaintainsInvariants = true
        , _lasTablesRead          = mkSFunArray $ const false
        , _lasTablesWritten       = mkSFunArray $ const false
        , _lasIntColumnDeltas     = intColumnDeltas
        , _lasDecColumnDeltas     = decColumnDeltas
        , _lasTableCells          = tableCells
        , _lasRowsRead            = mkPerTableSFunArray false
        , _lasRowsWritten         = mkPerTableSFunArray false
        , _lasCellsEnforced       = cellsEnforced
        , _lasCellsWritten        = cellsWritten
        }
    , _globalState = GlobalAnalyzeState ()
    }

  where
    tableNames :: [TableName]
    tableNames = map (TableName . T.unpack . fst) tables

    intColumnDeltas = mkTableColumnMap (== TyPrim TyInteger) 0
    decColumnDeltas = mkTableColumnMap (== TyPrim TyDecimal) 0
    cellsEnforced
      = mkTableColumnMap (== TyPrim TyKeySet) (mkSFunArray (const false))
    cellsWritten = mkTableColumnMap (const True) (mkSFunArray (const false))

    mkTableColumnMap
      :: (Pact.Type Pact.UserType -> Bool) -> a -> TableMap (ColumnMap a)
    mkTableColumnMap f defValue = TableMap $ Map.fromList $
      flip fmap tables $ \(tabName, userTy) ->
        let fields = Pact._utFields userTy
            colMap = ColumnMap $ Map.fromList $ catMaybes $
              flip fmap fields $ \(Arg argName ty _) ->
                if f ty
                then Just (ColumnName (T.unpack argName), defValue)
                else Nothing
        in (TableName (T.unpack tabName), colMap)

    mkPerTableSFunArray :: SBV v -> TableMap (SFunArray k v)
    mkPerTableSFunArray defaultV = TableMap $ Map.fromList $ zip
      tableNames
      (repeat $ mkSFunArray $ const defaultV)

addConstraint :: SBool -> Analyze ()
addConstraint = tell . Constraints . constrain

allocateSymbolicCells
  :: [(Text, Pact.UserType)]
  -> Symbolic (TableMap SymbolicCells)
allocateSymbolicCells tables = do
  cellsList <- for tables $ \(tabName, Pact.Schema _ _ fields _) -> do
    let fields' = Map.fromList $
          map (\(Arg argName ty _i) -> (argName, ty)) fields

    (TableName (T.unpack tabName),) <$> mkCells fields'

  pure $ TableMap $ Map.fromList cellsList

  where
    mkCells
      :: Map Text (Pact.Type Pact.UserType)
      -> Symbolic SymbolicCells
    mkCells fields =

      let cells0 = SymbolicCells mempty mempty mempty mempty mempty mempty

      -- fold over the fields, creating an array with constrained values for
      -- each column
      in ifoldlM
        (\colName cells ty ->
          let colName' = T.unpack colName
          in case ty of
              TyPrim TyInteger -> do
                arr <- newArray $ "int cells (" ++ colName' ++ ")"
                pure $ cells & scIntValues . at (ColumnName colName') ?~ arr
              TyPrim TyBool    -> do
                arr <- newArray $ "bool cells (" ++ colName' ++ ")"
                pure $ cells & scBoolValues . at (ColumnName colName') ?~ arr
              TyPrim TyDecimal -> do
                arr <- newArray $ "decimal cells (" ++ colName' ++ ")"
                pure $ cells & scDecimalValues . at (ColumnName colName') ?~ arr
              TyPrim TyTime    -> do
                arr <- newArray $ "time cells (" ++ colName' ++ ")"
                pure $ cells & scTimeValues . at (ColumnName colName') ?~ arr
              TyPrim TyString  -> do
                arr <- newArray $ "string cells (" ++ colName' ++ ")"
                pure $ cells & scStringValues . at (ColumnName colName') ?~ arr
              TyPrim TyKeySet  -> do
                arr <- newArray $ "keyset cells (" ++ colName' ++ ")"
                pure $ cells & scKsValues . at (ColumnName colName') ?~ arr
              _ -> pure cells -- error (show ty)
        )
        cells0
        fields

mkSVal :: SBV a -> SBVI.SVal
mkSVal (SBVI.SBV v) = v

data AnalyzeFailure
  = AtHasNoRelevantFields EType Schema
  | AValUnexpectedlySVal SBVI.SVal
  | AValUnexpectedlyObj Object
  | KeyNotPresent String Object
  | MalformedLogicalOpExec LogicalOp Int
  | ObjFieldOfWrongType String EType
  | PossibleRoundoff Text
  | UnsupportedDecArithOp ArithOp
  | UnsupportedIntArithOp ArithOp
  | UnsupportedUnaryOp UnaryArithOp
  | UnsupportedRoundingLikeOp1 RoundingLikeOp
  | UnsupportedRoundingLikeOp2 RoundingLikeOp
  | FailureMessage Text
  | OpaqueValEncountered
  | VarNotInScope Text
  | UnsupportedObjectInDbCell
  -- For cases we don't handle yet:
  | UnhandledObject (Term Object)
  | UnhandledTerm Text
  deriving Show

describeAnalyzeFailure :: AnalyzeFailure -> Text
describeAnalyzeFailure = \case
    -- these are internal errors. not quite as much care is taken on the messaging
    AtHasNoRelevantFields etype schema -> "When analyzing an `at` access, we expected to return a " <> tShow etype <> " but there were no fields of that type in the object with schema " <> tShow schema
    AValUnexpectedlySVal sval -> "in analyzeTermO, found AVal where we expected AnObj" <> tShow sval
    AValUnexpectedlyObj obj -> "in analyzeTerm, found AnObj where we expected AVal" <> tShow obj
    KeyNotPresent key obj -> "key " <> T.pack key <> " unexpectedly not found in object " <> tShow obj
    MalformedLogicalOpExec op count -> "malformed logical op " <> tShow op <> " with " <> tShow count <> " args"
    ObjFieldOfWrongType fName fType -> "object field " <> T.pack fName <> " of type " <> tShow fType <> " unexpectedly either an object or a ground type when we expected the other"
    PossibleRoundoff msg -> msg
    UnsupportedDecArithOp op -> "unsupported decimal arithmetic op: " <> tShow op
    UnsupportedIntArithOp op -> "unsupported integer arithmetic op: " <> tShow op
    UnsupportedUnaryOp op -> "unsupported unary arithmetic op: " <> tShow op
    UnsupportedRoundingLikeOp1 op -> "unsupported rounding (1) op: " <> tShow op
    UnsupportedRoundingLikeOp2 op -> "unsupported rounding (2) op: " <> tShow op

    -- these are likely user-facing errors
    FailureMessage msg -> msg
    UnhandledObject obj -> foundUnsupported $ tShow obj
    UnhandledTerm termText -> foundUnsupported termText
    VarNotInScope name -> "variable not in scope: " <> name
    --
    -- TODO: maybe we should differentiate between opaque values and type
    -- variables, because the latter would probably mean a problem from type
    -- inference or the need for a type annotation?
    --
    OpaqueValEncountered -> "We encountered an opaque value in analysis. This would be either a JSON value or a type variable. We can't prove properties of these values."
    UnsupportedObjectInDbCell -> "We encountered the use of an object in a DB cell, which we don't yet support. " <> pleaseReportThis

  where
    foundUnsupported :: Text -> Text
    foundUnsupported termText = "You found a term we don't have analysis support for yet. " <> pleaseReportThis <> "\n\n" <> termText

    pleaseReportThis :: Text
    pleaseReportThis = "Please report this as a bug at https://github.com/kadena-io/pact/issues"

instance IsString AnalyzeFailure where
  fromString = FailureMessage . T.pack

newtype Analyze a
  = Analyze
    { runAnalyze :: RWST AnalyzeEnv Constraints AnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState AnalyzeState, MonadError AnalyzeFailure,
            MonadWriter Constraints)

mkQueryEnv :: AnalyzeEnv -> AnalyzeState -> AVal -> QueryEnv
mkQueryEnv = QueryEnv

newtype Query a
  = Query
    { queryAction :: ReaderT QueryEnv (ExceptT AnalyzeFailure Symbolic) a }
  deriving (Functor, Applicative, Monad, MonadReader QueryEnv,
            MonadError AnalyzeFailure)

allocateArgs :: [(Text, Pact.Type Pact.UserType)] -> Symbolic (Map Text AVal)
allocateArgs argTys = fmap Map.fromList $ for argTys $ \(name, ty) -> do
    let name' = T.unpack name
    var <- case ty of
      TyPrim TyInteger -> mkAVal . sansProv <$> sInteger name'
      TyPrim TyBool    -> mkAVal . sansProv <$> sBool name'
      TyPrim TyDecimal -> mkAVal . sansProv <$> sDecimal name'
      TyPrim TyTime    -> mkAVal . sansProv <$> sInt64 name'
      TyPrim TyString  -> mkAVal . sansProv <$> sString name'
      TyUser _         -> mkAVal . sansProv <$> (free_ :: Symbolic (SBV UserType))
      TyPrim TyKeySet  -> mkAVal . sansProv <$> (free_ :: Symbolic (SBV KeySet))

      -- TODO
      TyPrim TyValue   -> error "unimplemented type analysis"
      TyAny            -> error "unimplemented type analysis"
      TyVar _v         -> error "unimplemented type analysis"
      TyList _         -> error "unimplemented type analysis"
      TySchema _ _     -> error "unimplemented type analysis"
      TyFun _          -> error "unimplemented type analysis"
    pure (name, var)

  where
    sDecimal :: String -> Symbolic (SBV Decimal)
    sDecimal = symbolic

mkAnalyzeEnv
  :: [(Text, Pact.Type Pact.UserType)]
  -> [(Text, Pact.UserType, [(Text, SchemaInvariant Bool)])]
  -> Symbolic AnalyzeEnv
mkAnalyzeEnv argTys tables = do
  args        <- allocateArgs argTys
  keySets'    <- newArray "keySets'"
  keySetAuths <- newArray "keySetAuths"

  pure $ foldr
    (\(tableName, _ut, someInvariants) env -> foldr
      (\(colName, invariant) env' ->
        let tableName' = TableName (T.unpack tableName)
            colName'   = ColumnName (T.unpack colName)
        in env' & invariants . at (tableName', colName') ?~ invariant
      )
      env
      someInvariants
    )
    (AnalyzeEnv args keySets' keySetAuths Map.empty)
    tables

instance (Mergeable a) => Mergeable (Analyze a) where
  symbolicMerge force test left right = Analyze $ RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per AnalyzeState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST (runAnalyze act) r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

class HasAnalyzeEnv a where
  {-# MINIMAL analyzeEnv #-}
  analyzeEnv :: Lens' a AnalyzeEnv

  scope :: Lens' a (Map Text AVal)
  scope = analyzeEnv.aeScope

  keySets :: Lens' a (SArray KeySetName KeySet)
  keySets = analyzeEnv.aeKeySets

  ksAuths :: Lens' a (SArray KeySet Bool)
  ksAuths = analyzeEnv.aeKsAuths

instance HasAnalyzeEnv AnalyzeEnv where analyzeEnv = id
instance HasAnalyzeEnv QueryEnv   where analyzeEnv = qeAnalyzeEnv

class (MonadError AnalyzeFailure m) => Analyzer m term where
  analyze  :: (Show a, SymWord a) => term a -> m (S a)
  analyzeO :: term Object -> m Object

instance Analyzer Analyze Term where
  analyze  = analyzeTerm
  analyzeO = analyzeTermO

instance Analyzer Query Prop where
  analyze  = analyzeProp
  analyzeO = analyzePropO

class SymbolicTerm term where
  injectS :: S a -> term a

instance SymbolicTerm Term where injectS = Literal
instance SymbolicTerm Prop where injectS = PSym

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

succeeds :: Lens' AnalyzeState (S Bool)
succeeds = latticeState.lasSucceeds.sbv2S

maintainsInvariants :: Lens' AnalyzeState SBool
maintainsInvariants = latticeState.lasMaintainsInvariants

tableRead :: TableName -> Lens' AnalyzeState (S Bool)
tableRead tn = latticeState.lasTablesRead.symArrayAt (literalS tn).sbv2S

tableWritten :: TableName -> Lens' AnalyzeState (S Bool)
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literalS tn).sbv2S

intColumnDelta :: TableName -> ColumnName -> Lens' AnalyzeState (S Integer)
intColumnDelta tn cn = latticeState.lasIntColumnDeltas.singular (ix tn).
  singular (ix cn)

decColumnDelta :: TableName -> ColumnName -> Lens' AnalyzeState (S Decimal)
decColumnDelta tn cn = latticeState.lasDecColumnDeltas.singular (ix tn).
  singular (ix cn)

rowRead :: TableName -> S RowKey -> Lens' AnalyzeState (S Bool)
rowRead tn sRk = latticeState.lasRowsRead.singular (ix tn).
  symArrayAt sRk.sbv2S

rowWritten :: TableName -> S RowKey -> Lens' AnalyzeState (S Bool)
rowWritten tn sRk = latticeState.lasRowsWritten.singular (ix tn).
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
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

boolCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Bool)
boolCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scBoolValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

stringCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S String)
stringCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scStringValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

decimalCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Decimal)
decimalCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scDecimalValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

timeCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S Time)
timeCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scTimeValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

ksCell
  :: TableName
  -> ColumnName
  -> S RowKey
  -> S Bool
  -> Lens' AnalyzeState (S KeySet)
ksCell tn cn sRk sDirty = latticeState.lasTableCells.singular (ix tn).scKsValues.
  singular (ix cn).symArrayAt sRk.sbv2SFrom (mkProv tn cn sRk sDirty)

symKsName :: S String -> S KeySetName
symKsName = coerceS

-- TODO: switch to lens
resolveKeySet
  :: (MonadReader r m, HasAnalyzeEnv r, MonadError AnalyzeFailure m)
  => S KeySetName
  -> m (S KeySet)
resolveKeySet sKsn = fmap sansProv $
  readArray <$> view keySets <*> pure (_sSbv sKsn)

--keySetNamed :: SBV KeySetName -> Lens' AnalyzeEnv (SBV KeySet)
--keySetNamed sKsn = aeKeySets.symArrayAt sKsn

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
  case sKs ^. sProv of
    Just (Provenance tn sCn sRk sDirty) ->
      cellEnforced tn sCn sRk %= (||| bnot sDirty)
    Nothing ->
      pure ()
  fmap sansProv $ readArray <$> view ksAuths <*> pure (_sSbv sKs)

aval
  :: MonadError AnalyzeFailure m
  => (Maybe Provenance -> SBVI.SVal -> m a)
  -> (Object -> m a)
  -> AVal
  -> m a
aval elimVal elimObj = \case
  AVal mProv sval -> elimVal mProv sval
  AnObj obj       -> elimObj obj
  OpaqueVal       -> throwError OpaqueValEncountered

-- | Function composition that consumes two args instead of one
(...) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(...) = (.) . (.)

expectVal :: MonadError AnalyzeFailure m => AVal -> m (S a)
expectVal = aval (pure ... mkS) (throwError . AValUnexpectedlyObj)

expectObj :: MonadError AnalyzeFailure m => AVal -> m Object
expectObj = aval ((throwError . AValUnexpectedlySVal) ... getSVal) pure
  where
    getSVal :: Maybe Provenance -> SBVI.SVal -> SBVI.SVal
    getSVal = flip const

lookupObj
  :: (MonadReader r m, HasAnalyzeEnv r, MonadError AnalyzeFailure m)
  => Text
  -> m Object
lookupObj name = do
  mVal <- view (scope . at name)
  case mVal of
    Nothing            -> throwError $ VarNotInScope name
    Just (AVal _ val') -> throwError $ AValUnexpectedlySVal val'
    Just (AnObj obj)   -> pure obj
    Just (OpaqueVal)   -> throwError OpaqueValEncountered

lookupVal
  :: (MonadReader r m, HasAnalyzeEnv r, MonadError AnalyzeFailure m)
  => Text
  -> m (S a)
lookupVal name = do
  mVal <- view $ scope . at name
  case mVal of
    Nothing                -> throwError $ VarNotInScope name
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just (AnObj obj)       -> throwError $ AValUnexpectedlyObj obj
    Just (OpaqueVal)       -> throwError OpaqueValEncountered

analyzeRead :: TableName -> Map String EType -> Term String -> Analyze Object
analyzeRead tn fields rowKey = do
  sRk <- symRowKey <$> analyzeTerm rowKey
  tableRead tn .= true
  rowRead tn sRk .= true
  obj <- iforM fields $ \fieldName fieldType -> do
    let cn = ColumnName fieldName
    mInvariant <- view (invariants . at (tn, cn))
    sDirty <- use $ cellWritten tn cn sRk

    let constrained :: forall a. S a -> Analyze AVal
        constrained s@(S _prov (SBVI.SBV sval)) = do
          case mInvariant of
            Nothing -> pure ()
            Just invariant -> addConstraint $
              runReader (checkSchemaInvariant invariant) sval
          pure $ mkAVal s

    x <- case fieldType of
      EType TInt     -> constrained =<< use (intCell     tn cn sRk sDirty)
      EType TBool    -> constrained =<< use (boolCell    tn cn sRk sDirty)
      EType TStr     -> constrained =<< use (stringCell  tn cn sRk sDirty)
      EType TDecimal -> constrained =<< use (decimalCell tn cn sRk sDirty)
      EType TTime    -> constrained =<< use (timeCell    tn cn sRk sDirty)
      EType TKeySet  -> constrained =<< use (ksCell      tn cn sRk sDirty)
      EType TAny     -> pure OpaqueVal
      --
      -- TODO: if we add nested object support here, we need to install
      --       the correct provenance into AVals all the way down into
      --       sub-objects.
      --
      EObjectTy _    -> throwError UnsupportedObjectInDbCell

    pure (fieldType, x)
  pure $ Object obj

analyzeAtO
  :: forall m term
   . Analyzer m term
  => term String
  -> term Object
  -> m Object
analyzeAtO colNameT objT = do
    obj@(Object fields) <- analyzeO objT
    sCn <- analyze colNameT

    let getObjVal :: String -> m Object
        getObjVal fieldName = case Map.lookup fieldName fields of
          Nothing -> throwError $ KeyNotPresent fieldName obj
          Just (fieldType, AVal _ _) -> throwError $
            ObjFieldOfWrongType fieldName fieldType
          Just (_fieldType, AnObj subObj) -> pure subObj
          Just (_fieldType, OpaqueVal) -> throwError OpaqueValEncountered

    case unliteralS sCn of
      Nothing -> throwError "Unable to determine statically the key used in an object access evaluating to an object (this is an object in an object)"
      Just concreteColName -> getObjVal concreteColName

analyzeAt
  :: (Analyzer m term, SymWord a)
  => Schema
  -> term String
  -> term Object
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
    [] -> throwError $ AtHasNoRelevantFields retType schema
    _  -> pure relevantFields

  let getObjVal fieldName = case Map.lookup fieldName fields of
        Nothing -> throwError $ KeyNotPresent fieldName obj

        Just (_fieldType, AVal mProv sval) -> pure $ mkS mProv sval

        Just (fieldType, AnObj _subObj) -> throwError $
          ObjFieldOfWrongType fieldName fieldType

        Just (_fieldType, OpaqueVal) -> throwError OpaqueValEncountered

  firstVal <- getObjVal firstName

  -- Fold over each relevant field, building a sequence of `ite`s. We require
  -- at least one matching field, ie firstVal. At first glance, this should
  -- just be a `foldr1M`, but we want the type of accumulator and element to
  -- differ, because elements are `String` `fieldName`s, while the accumulator
  -- is an `SBV a`.
  foldrM
    (\fieldName rest -> do
      val <- getObjVal fieldName
      pure $ iteS (sansProv (colName .== literalS fieldName)) val rest
    )
    firstVal
    relevantFields'

analyzeETerm :: ETerm -> Analyze AVal
analyzeETerm (ETerm tm _)   = mkAVal <$> analyzeTerm tm
analyzeETerm (EObject tm _) = AnObj <$> analyzeTermO tm

analyzeTermO :: Term Object -> Analyze Object
analyzeTermO = \case
  LiteralObject obj -> Object <$> (traverse . traverse) analyzeETerm obj

  Read tn (Schema fields) rowKey -> analyzeRead tn fields rowKey

  ReadCols tn (Schema fields) rowKey cols -> do
    -- Intersect both the returned object and its type with the requested
    -- columns
    let colSet = Set.fromList cols
        relevantFields
          = Map.filterWithKey (\k _ -> T.pack k `Set.member` colSet) fields

    analyzeRead tn relevantFields rowKey

  Var name -> lookupObj name

  Let name eterm body -> do
    av <- analyzeETerm eterm
    local (scope.at name ?~ av) $
      analyzeTermO body

  Sequence eterm objT -> analyzeETerm eterm *> analyzeTermO objT

  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    case unliteralS testPasses of
      Just True  -> analyzeTermO then'
      Just False -> analyzeTermO else'
      Nothing    -> throwError "Unable to determine statically the branch taken in an if-then-else evaluating to an object"

  At _schema colNameT objT _retType -> analyzeAtO colNameT objT

  objT -> throwError $ UnhandledObject objT

analyzeDecArithOp
  :: Analyzer m term
  => ArithOp
  -> term Decimal
  -> term Decimal
  -> m (S Decimal)
analyzeDecArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> pure $ x / y
    Pow -> throwError $ UnsupportedDecArithOp op
    Log -> throwError $ UnsupportedDecArithOp op

analyzeIntArithOp
  :: Analyzer m term
  => ArithOp
  -> term Integer
  -> term Integer
  -> m (S Integer)
analyzeIntArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + y
    Sub -> pure $ x - y
    Mul -> pure $ x * y
    Div -> pure $ x `sDiv` y
    Pow -> throwError $ UnsupportedDecArithOp op
    Log -> throwError $ UnsupportedDecArithOp op

analyzeIntDecArithOp
  :: Analyzer m term
  => ArithOp
  -> term Integer
  -> term Decimal
  -> m (S Decimal)
analyzeIntDecArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ fromIntegralS x + y
    Sub -> pure $ fromIntegralS x - y
    Mul -> pure $ fromIntegralS x * y
    Div -> pure $ fromIntegralS x / y
    Pow -> throwError $ UnsupportedDecArithOp op
    Log -> throwError $ UnsupportedDecArithOp op

analyzeDecIntArithOp
  :: Analyzer m term
  => ArithOp
  -> term Decimal
  -> term Integer
  -> m (S Decimal)
analyzeDecIntArithOp op xT yT = do
  x <- analyze xT
  y <- analyze yT
  case op of
    Add -> pure $ x + fromIntegralS y
    Sub -> pure $ x - fromIntegralS y
    Mul -> pure $ x * fromIntegralS y
    Div -> pure $ x / fromIntegralS y
    Pow -> throwError $ UnsupportedDecArithOp op
    Log -> throwError $ UnsupportedDecArithOp op

analyzeUnaryArithOp
  :: (Analyzer m term, Num a, Show a, SymWord a)
  => UnaryArithOp
  -> term a
  -> m (S a)
analyzeUnaryArithOp op term = do
  x <- analyze term
  case op of
    Negate -> pure $ negate x
    Sqrt   -> throwError $ UnsupportedUnaryOp op
    Ln     -> throwError $ UnsupportedUnaryOp op
    Exp    -> throwError $ UnsupportedUnaryOp op -- TODO: use svExp
    Abs    -> pure $ abs x
    Signum -> pure $ signum x

analyzeModOp
  :: Analyzer m term
  => term Integer
  -> term Integer
  -> m (S Integer)
analyzeModOp xT yT = sMod <$> analyze xT <*> analyze yT

analyzeRoundingLikeOp1
  :: Analyzer m term
  => RoundingLikeOp
  -> term Decimal
  -> m (S Integer)
analyzeRoundingLikeOp1 op x = do
  x' <- analyze x
  pure $ case op of
    -- The only SReal -> SInteger conversion function that sbv provides is
    -- sRealToSInteger, which computes the floor.
    Floor   -> realToIntegerS x'

    -- For ceiling we use the identity:
    -- ceil(x) = -floor(-x)
    Ceiling -> negate (realToIntegerS (negate x'))

    -- Round is much more complicated because pact uses the banker's method,
    -- where a real exactly between two integers (_.5) is rounded to the
    -- nearest even.
    Round   ->
      let wholePart      = realToIntegerS x'
          wholePartIsOdd = sansProv $ wholePart `sMod` 2 .== 1
          isExactlyHalf  = sansProv $ fromIntegralS wholePart + 1 / 2 .== x'

      in iteS isExactlyHalf
        -- nearest even number!
        (wholePart + oneIfS wholePartIsOdd)
        -- otherwise we take the floor of `x + 0.5`
        (realToIntegerS (x' + 0.5))

-- In the decimal rounding operations we shift the number left by `precision`
-- digits, round using the integer method, and shift back right.
--
-- x': SReal            := -100.15234
-- precision': SInteger := 2
-- x'': SReal           := -10015.234
-- x''': SInteger       := -10015
-- return: SReal        := -100.15
analyzeRoundingLikeOp2
  :: forall m term
   . (Analyzer m term, SymbolicTerm term)
  => RoundingLikeOp
  -> term Decimal
  -> term Integer
  -> m (S Decimal)
analyzeRoundingLikeOp2 op x precision = do
  x'         <- analyze x
  precision' <- analyze precision
  let digitShift = over s2Sbv (10 .^) precision' :: S Integer
      x''        = x' * fromIntegralS digitShift
  x''' <- analyzeRoundingLikeOp1 op (injectS x'' :: term Decimal)
  pure $ fromIntegralS x''' / fromIntegralS digitShift

analyzeIntAddTime
  :: Analyzer m term
  => term Time
  -> term Integer
  -> m (S Time)
analyzeIntAddTime timeT secsT = do
  time <- analyze timeT
  secs <- analyze secsT
  pure $ time + fromIntegralS secs

analyzeDecAddTime
  :: Analyzer m term
  => term Time
  -> term Decimal
  -> m (S Time)
analyzeDecAddTime timeT secsT = do
  time <- analyze timeT
  secs <- analyze secsT
  if isConcreteS secs
  then pure $ time + fromIntegralS (realToIntegerS secs)
  else throwError $ PossibleRoundoff
    "A time being added is not concrete, so we can't guarantee that roundoff won't happen when it's converted to an integer."

analyzeComparisonOp
  :: (Analyzer m term, SymWord a, Show a)
  => ComparisonOp
  -> term a
  -> term a
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
  :: (Analyzer m term, Boolean (S a), Show a, SymWord a)
  => LogicalOp
  -> [term a]
  -> m (S a)
analyzeLogicalOp op terms = do
  symBools <- traverse analyze terms
  case (op, symBools) of
    (AndOp, [a, b]) -> pure $ a &&& b
    (OrOp,  [a, b]) -> pure $ a ||| b
    (NotOp, [a])    -> pure $ bnot a
    _               -> throwError $ MalformedLogicalOpExec op $ length terms

analyzeTerm :: (Show a, SymWord a) => Term a -> Analyze (S a)
analyzeTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    iteS testPasses (analyzeTerm then') (analyzeTerm else')

  Enforce cond -> do
    cond' <- analyzeTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence eterm valT -> analyzeETerm eterm *> analyzeTerm valT

  Literal a -> pure a

  At schema colNameT objT retType -> analyzeAt schema colNameT objT retType

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tn rowKey obj -> do
    Object obj' <- analyzeTermO obj
    sRk <- symRowKey <$> analyzeTerm rowKey
    tableWritten tn .= true
    rowWritten tn sRk .= true
    void $ iforM obj' $ \colName (fieldType, aval') -> do
      let cn = ColumnName colName
      cellWritten tn cn sRk .= true

      let checkInvariants :: SBVI.SVal -> Analyze ()
          checkInvariants val = do
            mInvariant <- view (invariants . at (tn, cn))
            case mInvariant of
              Nothing -> pure ()
              Just invariant -> do
                let inv = runReader (checkSchemaInvariant invariant) val
                maintainsInvariants %= (&&& inv)

      case aval' of
        AVal mProv val' -> do
          checkInvariants val'

          let writeDelta :: forall t
                          . (Num t, SymWord t)
                         => (TableName -> ColumnName -> S RowKey -> S Bool -> Lens' AnalyzeState (S t))
                         -> (TableName -> ColumnName ->                       Lens' AnalyzeState (S t))
                         -> Analyze ()
              writeDelta mkCellL mkDeltaL = do
                let cell :: Lens' AnalyzeState (S t)
                    cell = mkCellL tn cn sRk true
                let next = mkS mProv val'
                prev <- use cell
                cell .= next
                mkDeltaL tn cn += next - prev

          case fieldType of
            EType TInt     -> writeDelta intCell intColumnDelta
            EType TBool    -> boolCell    tn cn sRk true .= mkS mProv val'
            EType TDecimal -> writeDelta decimalCell decColumnDelta
            EType TTime    -> timeCell    tn cn sRk true .= mkS mProv val'
            EType TStr     -> stringCell  tn cn sRk true .= mkS mProv val'
            EType TKeySet  -> ksCell      tn cn sRk true .= mkS mProv val'
            EType TAny     -> throwError OpaqueValEncountered
            EObjectTy _    -> throwError UnsupportedObjectInDbCell

            -- TODO: handle EObjectTy here

        -- TODO(joel): I'm not sure this is the right error to throw
        AnObj obj'' -> void $ throwError $ AValUnexpectedlyObj obj''
        OpaqueVal   -> throwError OpaqueValEncountered

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literalS "Write succeeded"

  Let name eterm body -> do
    av <- analyzeETerm eterm
    local (scope.at name ?~ av) $
      analyzeTerm body

  Var name -> lookupVal name

  DecArithOp op x y         -> analyzeDecArithOp op x y
  IntArithOp op x y         -> analyzeIntArithOp op x y
  IntDecArithOp op x y      -> analyzeIntDecArithOp op x y
  DecIntArithOp op x y      -> analyzeDecIntArithOp op x y
  IntUnaryArithOp op x      -> analyzeUnaryArithOp op x
  DecUnaryArithOp op x      -> analyzeUnaryArithOp op x
  ModOp x y                 -> analyzeModOp x y
  RoundingLikeOp1 op x      -> analyzeRoundingLikeOp1 op x
  RoundingLikeOp2 op x prec -> analyzeRoundingLikeOp2 op x prec

  AddTime time (ETerm secs TInt)     -> analyzeIntAddTime time secs
  AddTime time (ETerm secs TDecimal) -> analyzeDecAddTime time secs

  Comparison op x y -> analyzeComparisonOp op x y

  Logical op args -> analyzeLogicalOp op args

  ReadKeySet str -> resolveKeySet =<< symKsName <$> analyzeTerm str

  KsAuthorized ks -> ksAuthorized =<< analyzeTerm ks
  NameAuthorized str -> nameAuthorized =<< symKsName <$> analyzeTerm str

  Concat str1 str2 -> (.++) <$> analyzeTerm str1 <*> analyzeTerm str2

  PactVersion -> pure $ literalS $ T.unpack pactVersion

  n -> throwError $ UnhandledTerm $ tShow n

liftSymbolic :: Symbolic a -> Query a
liftSymbolic = Query . lift . lift


checkInvariantsHeld :: Query (S Bool)
checkInvariantsHeld = do
  success   <- view (model.succeeds)
  maintains <- sansProv <$> view (model.maintainsInvariants)
  pure $ success ==> maintains

--
-- TODO: convert this to use `S a`
--
checkSchemaInvariant :: SchemaInvariant a -> Reader SBVI.SVal (SBV a)
checkSchemaInvariant = \case

  -- comparison
  SchemaDecimalComparison op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Gt  -> a' .>  b'
      Lt  -> a' .<  b'
      Gte -> a' .>= b'
      Lte -> a' .<= b'
      Eq  -> a' .== b'
      Neq -> a' ./= b'

  SchemaIntComparison op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Gt  -> a' .>  b'
      Lt  -> a' .<  b'
      Gte -> a' .>= b'
      Lte -> a' .<= b'
      Eq  -> a' .== b'
      Neq -> a' ./= b'

  SchemaTimeComparison op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Gt  -> a' .>  b'
      Lt  -> a' .<  b'
      Gte -> a' .>= b'
      Lte -> a' .<= b'
      Eq  -> a' .== b'
      Neq -> a' ./= b'

  SchemaStringComparison op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Gt  -> a' .>  b'
      Lt  -> a' .<  b'
      Gte -> a' .>= b'
      Lte -> a' .<= b'
      Eq  -> a' .== b'
      Neq -> a' ./= b'

  SchemaBoolEqNeq op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Eq'  -> a' .== b'
      Neq' -> a' ./= b'

  SchemaKeySetEqNeq op a b -> do
    a' <- checkSchemaInvariant a
    b' <- checkSchemaInvariant b
    pure $ case op of
      Eq'  -> a' .== b'
      Neq' -> a' ./= b'

  -- literals
  SchemaDecimalLiteral d -> pure $ literal d
  SchemaIntLiteral i     -> pure $ literal i
  SchemaStringLiteral s  -> pure $ literal (T.unpack s)
  SchemaTimeLiteral t    -> pure $ literal t
  SchemaBoolLiteral b    -> pure $ literal b

  SchemaVar _            -> asks SBVI.SBV

  SchemaLogicalOp op args -> do
    args' <- for args checkSchemaInvariant
    case (op, args') of
      (AndOp, [a, b]) -> pure $ a &&& b
      (OrOp,  [a, b]) -> pure $ a ||| b
      (NotOp, [a])    -> pure $ bnot a
      _               -> error "impossible schema logical op"

analyzePropO :: Prop Object -> Query Object
analyzePropO Result = expectObj =<< view qeAnalyzeResult
analyzePropO (PVar name) = lookupObj name
analyzePropO (PAt _schema colNameP objP _ety) = analyzeAtO colNameP objP
analyzePropO (PLit _) = throwError "We don't support property object literals"
analyzePropO (PSym _) = throwError "Symbols can't be objects"
analyzePropO (Forall name (Ty (Rep :: Rep ty)) p) = do
  sbv <- liftSymbolic (forall_ :: Symbolic (SBV ty))
  local (scope.at name ?~ mkAVal' sbv) $ analyzePropO p
analyzePropO (Exists name (Ty (Rep :: Rep ty)) p) = do
  sbv <- liftSymbolic (exists_ :: Symbolic (SBV ty))
  local (scope.at name ?~ mkAVal' sbv) $ analyzePropO p

analyzeProp :: SymWord a => Prop a -> Query (S a)
analyzeProp (PLit a) = pure $ literalS a
analyzeProp (PSym a) = pure a

analyzeProp Success = view $ model.succeeds
analyzeProp Abort   = bnot <$> analyzeProp Success
analyzeProp Result  = expectVal =<< view qeAnalyzeResult
analyzeProp (PAt schema colNameP objP ety) = analyzeAt schema colNameP objP ety

-- Abstraction
analyzeProp (Forall name (Ty (Rep :: Rep ty)) p) = do
  sbv <- liftSymbolic (forall_ :: Symbolic (SBV ty))
  local (scope.at name ?~ mkAVal' sbv) $ analyzeProp p
analyzeProp (Exists name (Ty (Rep :: Rep ty)) p) = do
  sbv <- liftSymbolic (exists_ :: Symbolic (SBV ty))
  local (scope.at name ?~ mkAVal' sbv) $ analyzeProp p
analyzeProp (PVar name) = lookupVal name

-- String ops
analyzeProp (PStrConcat p1 p2) = (.++) <$> analyzeProp p1 <*> analyzeProp p2
analyzeProp (PStrLength p)     = over s2Sbv SBV.length <$> analyzeProp p

-- Numeric ops
analyzeProp (PDecArithOp op x y)      = analyzeDecArithOp op x y
analyzeProp (PIntArithOp op x y)      = analyzeIntArithOp op x y
analyzeProp (PIntDecArithOp op x y)   = analyzeIntDecArithOp op x y
analyzeProp (PDecIntArithOp op x y)   = analyzeDecIntArithOp op x y
analyzeProp (PIntUnaryArithOp op x)   = analyzeUnaryArithOp op x
analyzeProp (PDecUnaryArithOp op x)   = analyzeUnaryArithOp op x
analyzeProp (PModOp x y)              = analyzeModOp x y
analyzeProp (PRoundingLikeOp1 op x)   = analyzeRoundingLikeOp1 op x
analyzeProp (PRoundingLikeOp2 op x p) = analyzeRoundingLikeOp2 op x p

analyzeProp (PIntAddTime time secs) = analyzeIntAddTime time secs
analyzeProp (PDecAddTime time secs) = analyzeDecAddTime time secs

analyzeProp (PComparison op x y) = analyzeComparisonOp op x y

-- Boolean ops
analyzeProp (PLogical op props) = analyzeLogicalOp op props

-- DB properties
analyzeProp (TableRead tn)  = view $ model.tableRead tn
analyzeProp (TableWrite tn) = view $ model.tableWritten tn
analyzeProp (ColumnWrite _tableName _colName)
  = throwError "column write analysis not yet implemented"
analyzeProp (CellIncrease _tableName _colName)
  = throwError "cell increase analysis not yet implemented"
analyzeProp (IntColumnDelta tableName colName) = view $
  model.intColumnDelta tableName colName
analyzeProp (DecColumnDelta tableName colName) = view $
  model.decColumnDelta tableName colName
analyzeProp (RowRead tn pRk)  = do
  sRk <- analyzeProp pRk
  view $ model.rowRead tn sRk
analyzeProp (RowWrite tn pRk) = do
  sRk <- analyzeProp pRk
  view $ model.rowWritten tn sRk

-- Authorization
analyzeProp (KsNameAuthorized ksn) = nameAuthorized $ literalS ksn
analyzeProp (RowEnforced tn cn pRk) = do
  sRk <- analyzeProp pRk
  view $ model.cellEnforced tn cn sRk
