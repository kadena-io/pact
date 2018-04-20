{-# language DeriveFunctor              #-}
{-# language DeriveDataTypeable         #-}
{-# language DeriveTraversable          #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language OverloadedStrings          #-}
{-# language Rank2Types                 #-}
{-# language ScopedTypeVariables        #-}
{-# language TemplateHaskell            #-}
{-# language TupleSections              #-}
{-# language TypeFamilies               #-}

module Pact.Analyze.Analyze where

import Control.Monad
import Control.Monad.Except (MonadError, ExceptT(..), runExcept, throwError)
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.SBV hiding ((.++), Satisfiable, Unsatisfiable, Unknown, ProofError,
                        name)
import qualified Data.SBV.Internals as SBVI
import qualified Data.Text as T
import Data.Traversable (for)
import Pact.Types.Runtime hiding (TableName, Term, Type, EObject, RowKey(..),
                                  WriteType(..), KeySet, TKeySet)
import qualified Pact.Types.Runtime as Pact
import qualified Pact.Types.Typecheck as TC
import Pact.Types.Version (pactVersion)

import Pact.Analyze.Prop
import Pact.Analyze.Types

-- a unique cell, from a column name and a row key
-- e.g. balance__25
newtype CellId
  = CellId String
  deriving (Eq, Ord)

instance SymWord CellId where
  mkSymWord = SBVI.genMkSymVar KString
  literal (CellId cid) = mkConcreteString cid
  fromCW = wrappedStringFromCW CellId

instance HasKind CellId where
  kindOf _ = KString

instance IsString CellId where
  fromString = CellId

data AnalyzeEnv = AnalyzeEnv
  { _scope     :: Map Text AVal            -- used with 'local' as a stack
  , _keySets   :: SArray KeySetName KeySet -- read-only
  , _ksAuths   :: SArray KeySet Bool       -- read-only
  } deriving Show

allocateArgs :: [(Text, Pact.Type TC.UserType)] -> Symbolic (Map Text AVal)
allocateArgs argTys = fmap Map.fromList $ for argTys $ \(name, ty) -> do
    let name' = T.unpack name
    var <- case ty of
      TyPrim TyInteger -> mkAVal . sansProv <$> sInteger name'
      TyPrim TyBool    -> mkAVal . sansProv <$> sBool name'
      TyPrim TyDecimal -> mkAVal . sansProv <$> sDecimal name'
      TyPrim TyTime    -> mkAVal . sansProv <$> sInt64 name'
      TyPrim TyString  -> mkAVal . sansProv <$> sString name'
      TyUser _         -> mkAVal . sansProv <$> (free_ :: Symbolic (SBV UserType))

      -- TODO
      TyPrim TyValue   -> error "unimplemented type analysis"
      TyPrim TyKeySet  -> error "unimplemented type analysis"
      TyAny            -> error "unimplemented type analysis"
      TyVar _v         -> error "unimplemented type analysis"
      TyList _         -> error "unimplemented type analysis"
      TySchema _ _     -> error "unimplemented type analysis"
      TyFun _          -> error "unimplemented type analysis"
    pure (name, var)

  where
    sDecimal :: String -> Symbolic (SBV Decimal)
    sDecimal = symbolic

mkAnalyzeEnv :: [(Text, Pact.Type TC.UserType)] -> Symbolic AnalyzeEnv
mkAnalyzeEnv argTys = AnalyzeEnv
  <$> allocateArgs argTys
  <*> newArray "keySets"
  <*> newArray "keySetAuths"

newtype AnalyzeLog
  = AnalyzeLog ()

instance Monoid AnalyzeLog where
  mempty = AnalyzeLog ()
  mappend _ _ = AnalyzeLog ()

instance Mergeable AnalyzeLog where
  --
  -- NOTE: If we change the underlying representation of AnalyzeLog to a list,
  -- the default Mergeable instance for this will have the wrong semantics, as
  -- it requires that lists have the same length. We more likely want to use
  -- monoidal semantics for anything we log:
  --
  symbolicMerge _f _t = mappend

data SymbolicCells
  = SymbolicCells
    { _scIntValues     :: SArray CellId Integer
    , _scBoolValues    :: SArray CellId Bool
    , _scStringValues  :: SArray CellId String
    , _scDecimalValues :: SArray CellId Decimal
    , _scTimeValues    :: SArray CellId Time
    , _scKsValues      :: SArray CellId KeySet
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
      m :: SymWord a => SArray CellId a -> SArray CellId a -> SArray CellId a
      m = symbolicMerge force test

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
    { _lasSucceeds      :: SBV Bool
    , _lasTablesRead    :: SFunArray TableName Bool
    , _lasTablesWritten :: SFunArray TableName Bool
    , _lasColumnDeltas  :: TableMap (SFunArray ColumnName Integer)
    , _lasTableCells    :: TableMap SymbolicCells
    , _lasRowsRead      :: TableMap (SFunArray RowKey Bool)
    , _lasRowsEnforced  :: TableMap (SFunArray RowKey Bool)
    }
  deriving (Show)

-- Implemented by-hand until 8.2, when we have DerivingStrategies
instance Mergeable LatticeAnalyzeState where
  symbolicMerge force test
    (LatticeAnalyzeState
      success  tsRead  tsWritten  deltas  cells  rsRead  rsEnforced)
    (LatticeAnalyzeState
      success' tsRead' tsWritten' deltas' cells' rsRead' rsEnforced') =
      LatticeAnalyzeState
        (symbolicMerge force test success    success')
        (symbolicMerge force test tsRead     tsRead')
        (symbolicMerge force test tsWritten  tsWritten')
        (symbolicMerge force test deltas     deltas')
        (symbolicMerge force test cells      cells')
        (symbolicMerge force test rsRead     rsRead')
        (symbolicMerge force test rsEnforced rsEnforced')

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

instance Mergeable AnalyzeState where
  -- NOTE: We discard the left global state because this is out-of-date and was
  -- already fed to the right computation -- we use the updated right global
  -- state.
  symbolicMerge force test (AnalyzeState lls _) (AnalyzeState rls rgs) =
    AnalyzeState (symbolicMerge force test lls rls) rgs

mkInitialAnalyzeState :: TableMap SymbolicCells -> AnalyzeState
mkInitialAnalyzeState tableCells = AnalyzeState
    { _latticeState = LatticeAnalyzeState
        { _lasSucceeds      = true
        , _lasTablesRead    = mkSFunArray $ const false
        , _lasTablesWritten = mkSFunArray $ const false
        , _lasColumnDeltas  = mkPerTableSFunArray 0
        , _lasTableCells    = tableCells
        , _lasRowsRead      = mkPerTableSFunArray false
        , _lasRowsEnforced  = mkPerTableSFunArray false
        }
    , _globalState = GlobalAnalyzeState ()
    }

  where
    tableNames :: [TableName]
    tableNames = Map.keys $ _tableMap tableCells

    mkPerTableSFunArray :: SBV v -> TableMap (SFunArray k v)
    mkPerTableSFunArray defaultV = TableMap $ Map.fromList $ zip
      tableNames
      (repeat $ mkSFunArray $ const defaultV)

allocateSymbolicCells :: [TableName] -> Symbolic (TableMap SymbolicCells)
allocateSymbolicCells tableNames = sequence $ TableMap $ Map.fromList $
    (, mkCells) <$> tableNames
  where
    mkCells :: Symbolic SymbolicCells
    mkCells = SymbolicCells
      <$> newArray "intCells"
      <*> newArray "boolCells"
      <*> newArray "stringCells"
      <*> newArray "decimalCells"
      <*> newArray "timeCells"
      <*> newArray "keySetCells"

data AnalyzeFailure
  = AtHasNoRelevantFields EType Schema
  | AValUnexpectedlySVal SBVI.SVal
  | AValUnexpectedlyObj Object
  | KeyNotPresent String Object
  | MalformedLogicalOpExec LogicalOp [Term Bool]
  | ObjFieldOfWrongType String EType
  | PossibleRoundoff Text
  | UnsupportedDecArithOp ArithOp
  | UnsupportedIntArithOp ArithOp
  | UnsupportedUnaryOp UnaryArithOp
  | UnsupportedRoundingLikeOp1 RoundingLikeOp
  | UnsupportedRoundingLikeOp2 RoundingLikeOp
  | FailureMessage Text
  | OpaqueValEncountered
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
  MalformedLogicalOpExec op args -> "malformed logical op " <> tShow op <> " with args " <> tShow args
  ObjFieldOfWrongType fName fType -> "object field " <> T.pack fName <> " of type " <> tShow fType <> " unexpectedly either an object or a ground type when we expected the other"
  PossibleRoundoff msg -> msg
  UnsupportedDecArithOp op -> "unsupported decimal arithmetic op: " <> tShow op
  UnsupportedIntArithOp op -> "unsupported integer arithmetic op: " <> tShow op
  UnsupportedUnaryOp op -> "unsupported unary arithmetic op: " <> tShow op
  UnsupportedRoundingLikeOp1 op -> "unsupported rounding (1) op: " <> tShow op
  UnsupportedRoundingLikeOp2 op -> "unsupported rounding (2) op: " <> tShow op

  -- these are likely user-facing errors
  FailureMessage msg -> msg
  UnhandledObject obj -> "You found a term we don't have analysis support for yet. Please report this as a bug at https://github.com/kadena-io/pact/issues\n\n" <> tShow obj
  UnhandledTerm termText -> "You found a term we don't have analysis support for yet. Please report this as a bug at https://github.com/kadena-io/pact/issues\n\n" <> termText
  --
  -- TODO: maybe we should differentiate between opaque values and type
  -- variables, because the latter would probably mean a problem from type
  -- inference or the need for a type annotation?
  --
  OpaqueValEncountered -> "We encountered an opaque value in analysis. This would be either a JSON value or a type variable. We can't prove properties of these values."

tShow :: Show a => a -> Text
tShow = T.pack . show

instance IsString AnalyzeFailure where
  fromString = FailureMessage . T.pack

newtype AnalyzeT m a
  = AnalyzeT
    { runAnalyzeT :: RWST AnalyzeEnv AnalyzeLog AnalyzeState (ExceptT AnalyzeFailure m) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState AnalyzeState, MonadError AnalyzeFailure)

instance MonadTrans AnalyzeT where
  lift = AnalyzeT . lift . lift

--
-- TODO: remove M from name
--
type AnalyzeM a = AnalyzeT Identity a

makeLenses ''AnalyzeEnv
makeLenses ''TableMap
makeLenses ''AnalyzeState
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells

instance (Mergeable a) => Mergeable (AnalyzeM a) where
  symbolicMerge force test left right = AnalyzeT $ RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per AnalyzeState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST (runAnalyzeT act) r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

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

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k

succeeds :: Lens' AnalyzeState (S Bool)
succeeds = latticeState.lasSucceeds.sbv2S

tableRead :: TableName -> Lens' AnalyzeState (S Bool)
tableRead tn = latticeState.lasTablesRead.symArrayAt (literalS tn).sbv2S

tableWritten :: TableName -> Lens' AnalyzeState (S Bool)
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literalS tn).sbv2S

--
-- NOTE: at the moment our `SBV ColumnName`s are actually always concrete. If
-- in the future we want to start using free symbolic column names (and
-- similarly, symbolic table names), we should accumulate constraints that
-- column names must be one of the valid column names for that table (and if we
-- know the type, this helps us constrain even further. also symbolic table
-- names must be one of the statically-known tables.
--

columnDelta :: TableName -> S ColumnName -> Lens' AnalyzeState (S Integer)
columnDelta tn sCn = latticeState.lasColumnDeltas.singular (ix tn).symArrayAt sCn.sbv2S

rowRead :: TableName -> S RowKey -> Lens' AnalyzeState (S Bool)
rowRead tn sRk = latticeState.lasRowsRead.singular (ix tn).symArrayAt sRk.sbv2S

rowEnforced :: TableName -> S RowKey -> Lens' AnalyzeState (S Bool)
rowEnforced tn sRk = latticeState.lasRowsEnforced.singular (ix tn).symArrayAt sRk.sbv2S

sCellId :: S ColumnName -> S RowKey -> S CellId
sCellId sCn sRk = coerceS $ coerceS sCn .++ "__" .++ coerceS sRk

intCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Integer)
intCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scIntValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

boolCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Bool)
boolCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scBoolValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

stringCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S String)
stringCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scStringValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

decimalCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Decimal)
decimalCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scDecimalValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

timeCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S Time)
timeCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scTimeValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

ksCell
  :: TableName
  -> S ColumnName
  -> S RowKey
  -> Lens' AnalyzeState (S KeySet)
ksCell tn sCn sRk = latticeState.lasTableCells.singular (ix tn).scKsValues.
  symArrayAt (sCellId sCn sRk).sbv2SFrom (mkProv tn sRk)

symKsName :: S String -> S KeySetName
symKsName = coerceS

-- TODO: potentially switch to lenses here for the following 3 functions:

resolveKeySet :: S KeySetName -> AnalyzeM (S KeySet)
resolveKeySet sKsn = fmap sansProv $
  readArray <$> view keySets <*> pure (_sSbv sKsn)

nameAuthorized :: S KeySetName -> AnalyzeM (S Bool)
nameAuthorized sKsn = fmap sansProv $
  readArray <$> view ksAuths <*> (_sSbv <$> resolveKeySet sKsn)

ksAuthorized :: S KeySet -> AnalyzeM (S Bool)
ksAuthorized sKs = do
  -- NOTE: we know that KsAuthorized constructions are only emitted within
  -- Enforced constructions, so we know that this keyset is being enforced
  -- here.
  case sKs ^. sProv of
    Just (Provenance tn sRk) -> rowEnforced tn (sansProv sRk) .= true
    Nothing -> pure ()
  fmap sansProv $ readArray <$> view ksAuths <*> pure (_sSbv sKs)

--keySetNamed :: SBV KeySetName -> Lens' AnalyzeEnv (SBV KeySet)
--keySetNamed sKsn = keySets.symArrayAt sKsn
--
--ksnAuthorization :: SBV KeySetName -> Lens' AnalyzeEnv (SBV Bool)
--ksnAuthorization sKsn = _todoKsnAuthorization

analyzeTermO :: Term Object -> AnalyzeM Object
analyzeTermO = \case
  LiteralObject obj -> Object <$>
    for obj (\(fieldType, ETerm tm _) -> do
      val <- analyzeTerm tm
      pure (fieldType, mkAVal val))

  Read tn (Schema fields) rowKey -> do
    sRk <- symRowKey <$> analyzeTerm rowKey
    tableRead tn .= true
    rowRead tn sRk .= true
    obj <- iforM fields $ \fieldName fieldType -> do
      let sCn  = literalS $ ColumnName fieldName
      x <- case fieldType of
        EType TInt     -> mkAVal <$> use (intCell     tn sCn sRk)
        EType TBool    -> mkAVal <$> use (boolCell    tn sCn sRk)
        EType TStr     -> mkAVal <$> use (stringCell  tn sCn sRk)
        EType TDecimal -> mkAVal <$> use (decimalCell tn sCn sRk)
        EType TTime    -> mkAVal <$> use (timeCell    tn sCn sRk)
        EType TKeySet  -> mkAVal <$> use (ksCell      tn sCn sRk)
        EType TAny     -> pure OpaqueVal
        --
        -- TODO: if we add nested object support here, we need to install
        --       the correct provenance into AVals all the way down into
        --       sub-objects.
        --

      pure (fieldType, x)
    pure $ Object obj

  ReadCols tn (Schema fields) rowKey cols -> do
    -- Intersect both the returned object and its type with the requested
    -- columns
    let colSet = Set.fromList cols
        relevantFields
          = Map.filterWithKey (\k _ -> T.pack k `Set.member` colSet) fields

    sRk <- symRowKey <$> analyzeTerm rowKey
    tableRead tn .= true
    rowRead tn sRk .= true
    obj <- iforM relevantFields $ \fieldName fieldType -> do
      let sCn = literalS $ ColumnName fieldName
      x <- case fieldType of
        EType TInt     -> mkAVal <$> use (intCell     tn sCn sRk)
        EType TBool    -> mkAVal <$> use (boolCell    tn sCn sRk)
        EType TStr     -> mkAVal <$> use (stringCell  tn sCn sRk)
        EType TDecimal -> mkAVal <$> use (decimalCell tn sCn sRk)
        EType TTime    -> mkAVal <$> use (timeCell    tn sCn sRk)
        EType TKeySet  -> mkAVal <$> use (ksCell      tn sCn sRk)
        EType TAny     -> pure OpaqueVal
        --
        -- TODO: if we add nested object support here, we need to install
        --       the correct provenance into AVals all the way down into
        --       sub-objects.
        --

      pure (fieldType, x)
    pure $ Object obj

  Var name -> do
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    case val of
      AVal _ val' -> throwError $ AValUnexpectedlySVal val'
      AnObj obj   -> pure obj
      OpaqueVal   -> throwError OpaqueValEncountered

  Let name (ETerm rhs _) body -> do
    val <- analyzeTerm rhs
    local (scope.at name ?~ mkAVal val) $
      analyzeTermO body

  Let name (EObject rhs _) body -> do
    rhs' <- analyzeTermO rhs
    local (scope.at name ?~ AnObj rhs') $
      analyzeTermO body

  Sequence (ETerm   a _) b -> analyzeTerm  a *> analyzeTermO b
  Sequence (EObject a _) b -> analyzeTermO a *> analyzeTermO b

  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    case unliteralS testPasses of
      Just True  -> analyzeTermO then'
      Just False -> analyzeTermO else'
      Nothing    -> throwError "Unable to determine statically the branch taken in an if-then-else evaluating to an object"

  At _schema colName objT _retType -> do
    obj@(Object fields) <- analyzeTermO objT

    colName' <- analyzeTerm colName

    let getObjVal :: String -> AnalyzeM Object
        getObjVal fieldName = case Map.lookup fieldName fields of
          Nothing -> throwError $ KeyNotPresent fieldName obj
          Just (fieldType, AVal _ _) -> throwError $
            ObjFieldOfWrongType fieldName fieldType
          Just (_fieldType, AnObj subObj) -> pure subObj
          Just (_fieldType, OpaqueVal) -> throwError OpaqueValEncountered

    case unliteralS colName' of
      Nothing -> throwError "Unable to determine statically the key used in an object access evaluating to an object (this is an object in an object)"
      Just concreteColName -> getObjVal concreteColName

  objT -> throwError $ UnhandledObject objT

analyzeTerm
  :: forall a. (Show a, SymWord a) => Term a -> AnalyzeM (S a)
analyzeTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    iteS testPasses (analyzeTerm then') (analyzeTerm else')

  Enforce cond -> do
    cond' <- analyzeTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence (ETerm   a _) b -> analyzeTerm  a *> analyzeTerm b
  Sequence (EObject a _) b -> analyzeTermO a *> analyzeTerm b

  Literal a -> pure a

  At schema@(Schema schemaFields) colNameT objT retType -> do
    obj@(Object fields) <- analyzeTermO objT

    -- Filter down to only fields which contain the type we're looking for
    let relevantFields
          = map fst
          $ filter (\(_name, ty) -> ty == retType)
          $ Map.toList schemaFields

    colName :: S String <- analyzeTerm colNameT

    firstName:relevantFields' <- case relevantFields of
      [] -> throwError $ AtHasNoRelevantFields retType schema
      _ -> pure relevantFields

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

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tn rowKey obj -> do
    Object obj' <- analyzeTermO obj
    tableWritten tn .= true
    sRk <- symRowKey <$> analyzeTerm rowKey
    void $ iforM obj' $ \colName (fieldType, aval) -> do
      let sCn = literalS $ ColumnName colName
      case aval of
        AVal mProv val' -> case fieldType of
          EType TInt  -> do
            let cell :: Lens' AnalyzeState (S Integer)
                cell = intCell tn sCn sRk
                next = mkS mProv val'
            prev <- use cell
            cell .= next
            columnDelta tn sCn += next - prev

          EType TBool    -> boolCell    tn sCn sRk .= mkS mProv val'
          EType TStr     -> stringCell  tn sCn sRk .= mkS mProv val'

          --
          -- TODO: we should support column delta for decimals
          --
          EType TDecimal -> decimalCell tn sCn sRk .= mkS mProv val'

          EType TTime    -> timeCell    tn sCn sRk .= mkS mProv val'
          EType TKeySet  -> ksCell      tn sCn sRk .= mkS mProv val'

          -- TODO: what to do with EType TAny here?

          -- TODO: handle EObjectTy here

        -- TODO(joel): I'm not sure this is the right error to throw
        AnObj obj'' -> void $ throwError $ AValUnexpectedlyObj obj''
        OpaqueVal   -> throwError OpaqueValEncountered

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literalS "Write succeeded"

  Let name (ETerm rhs _) body -> do
    val <- analyzeTerm rhs
    local (scope.at name ?~ mkAVal val) $
      analyzeTerm body

  Let name (EObject rhs _) body -> do
    rhs' <- analyzeTermO rhs
    local (scope.at name ?~ AnObj rhs') $
      analyzeTerm body

  Var name -> do
    -- theScope <- view scope
    -- traceShowM ("Var name", name, theScope)
    --
    -- TODO: probably throw AnalyzeFailures where we currently assume, because
    --       it's possible we've messed up translation for future non-trivial
    --       forms.
    --
    -- Assume the term is well-scoped after typechecking
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    case val of
      AVal mProv sval -> pure $ mkS mProv sval
      AnObj obj -> throwError $ AValUnexpectedlyObj obj
      OpaqueVal -> throwError OpaqueValEncountered

  IntArithOp op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    case op of
      Add -> pure $ x' + y'
      Sub -> pure $ x' - y'
      Mul -> pure $ x' * y'
      Div -> pure $ x' `sDiv` y'
      Pow -> throwError $ UnsupportedIntArithOp op
      Log -> throwError $ UnsupportedIntArithOp op

  DecArithOp op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    case op of
      Add -> pure $ x' + y'
      Sub -> pure $ x' - y'
      Mul -> pure $ x' * y'
      Div -> pure $ x' / y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  IntDecArithOp op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    case op of
      Add -> pure $ fromIntegralS x' + y'
      Sub -> pure $ fromIntegralS x' - y'
      Mul -> pure $ fromIntegralS x' * y'
      Div -> pure $ fromIntegralS x' / y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  DecIntArithOp op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    case op of
      Add -> pure $ x' + fromIntegralS y'
      Sub -> pure $ x' - fromIntegralS y'
      Mul -> pure $ x' * fromIntegralS y'
      Div -> pure $ x' / fromIntegralS y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  ModOp x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    pure $ x' `sMod` y'

  IntUnaryArithOp op x -> do
    x' <- analyzeTerm x
    case op of
      Negate -> pure $ negate x'
      Sqrt   -> throwError $ UnsupportedUnaryOp op
      Ln     -> throwError $ UnsupportedUnaryOp op
      Exp    -> throwError $ UnsupportedUnaryOp op -- TODO: use svExp
      Abs    -> pure $ abs x'
      Signum -> pure $ signum x'

  DecUnaryArithOp op x -> do
    x' <- analyzeTerm x
    case op of
      Negate -> pure $ negate x'
      Sqrt   -> throwError $ UnsupportedUnaryOp op
      Ln     -> throwError $ UnsupportedUnaryOp op
      Exp    -> throwError $ UnsupportedUnaryOp op -- TODO: use svExp
      Abs    -> pure $ abs x'
      Signum -> pure $ signum x'

  RoundingLikeOp1 op x -> do
    x' <- analyzeTerm x
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
  RoundingLikeOp2 op x precision -> do
    x'         <- analyzeTerm x
    precision' <- analyzeTerm precision
    let digitShift :: S Integer
        digitShift = sansProv $ 10 .^ _sSbv precision'
        x'' = x' * fromIntegralS digitShift

    x''' <- analyzeTerm (RoundingLikeOp1 op (Literal x''))

    pure $ fromIntegralS x''' / fromIntegralS digitShift

  AddTime time (ETerm secs TInt) -> do
    time' <- analyzeTerm time
    secs' <- analyzeTerm secs
    pure $ time' + fromIntegralS secs'

  AddTime time (ETerm secs TDecimal) -> do
    time' <- analyzeTerm time
    secs' <- analyzeTerm secs
    if isConcreteS secs'
    then pure $ time' + fromIntegralS (realToIntegerS secs')
    else throwError $ PossibleRoundoff
      "A time being added is not concrete, so we can't guarantee that roundoff won't happen when it's converted to an integer."

  Comparison op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    pure $ sansProv $ case op of
      Gt  -> x' .> y'
      Lt  -> x' .< y'
      Gte -> x' .>= y'
      Lte -> x' .<= y'
      Eq  -> x' .== y'
      Neq -> x' ./= y'

  Logical op args -> do
    args' <- forM args analyzeTerm
    case (op, args') of
      (AndOp, [a, b]) -> pure $ a &&& b
      (OrOp, [a, b])  -> pure $ a ||| b
      (NotOp, [a])    -> pure $ bnot a
      _               -> throwError $ MalformedLogicalOpExec op args

  ReadKeySet str -> resolveKeySet =<< symKsName <$> analyzeTerm str

  KsAuthorized ks -> ksAuthorized =<< analyzeTerm ks

  NameAuthorized str -> nameAuthorized =<< symKsName <$> analyzeTerm str

  Concat str1 str2 -> (.++) <$> analyzeTerm str1 <*> analyzeTerm str2

  PactVersion -> pure $ literalS $ T.unpack pactVersion

  n -> throwError $ UnhandledTerm $ tShow n

analyzeProperty :: Prop a -> AnalyzeM (S a)
-- Logical connectives
analyzeProperty (p1 `Implies` p2) = do
  b1 <- analyzeProperty p1
  b2 <- analyzeProperty p2
  pure $ b1 ==> b2
analyzeProperty (p1 `And` p2) = do
  b1 <- analyzeProperty p1
  b2 <- analyzeProperty p2
  pure $ b1 &&& b2
analyzeProperty (p1 `Or` p2) = do
  b1 <- analyzeProperty p1
  b2 <- analyzeProperty p2
  pure $ b1 ||| b2
analyzeProperty (Not p) = bnot <$> analyzeProperty p

-- Domain properties
analyzeProperty Success = use succeeds
analyzeProperty Abort = bnot <$> analyzeProperty Success
analyzeProperty (KsNameAuthorized ksn) = nameAuthorized $ literalS ksn
analyzeProperty (TableRead tn) = use $ tableRead tn
analyzeProperty (TableWrite tn) = use $ tableWritten tn
-- analyzeProperty (CellIncrease tableName colName)
analyzeProperty (ColumnConserve tableName colName) =
  sansProv . (0 .==) <$> use (columnDelta tableName (literalS colName))
analyzeProperty (ColumnIncrease tableName colName) =
  sansProv . (0 .<) <$> use (columnDelta tableName (literalS colName))
