{-# language DeriveFunctor              #-}
{-# language DeriveTraversable          #-}
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
import Control.Monad.Except (MonadError, ExceptT(..), Except, runExcept,
                             throwError)
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV.Internals as SBVI
import qualified Data.Set as Set
import qualified Data.Text as T
import Pact.Types.Runtime hiding (TableName, Term, Type, EObject, RowKey(..), WriteType(..))
import Pact.Types.Version (pactVersion)

import Pact.Analyze.Types

data AnalyzeEnv = AnalyzeEnv
  { _scope     :: Map Text AVal      -- used with 'local' in a stack fashion
  , _nameAuths :: SArray String Bool -- read-only
  } deriving Show

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
    { _scIntValues    :: SArray CellId Integer
    , _scBoolValues   :: SArray CellId Bool
    , _scStringValues :: SArray CellId String
    -- TODO: decimal
    -- TODO: time
    -- TODO: opaque blobs
    }
    deriving (Show)

-- Implemented by-hand until 8.4, when we have DerivingStrategies
instance Mergeable SymbolicCells where
  symbolicMerge force test (SymbolicCells a b c) (SymbolicCells a' b' c') =
      SymbolicCells (f a a') (f b b') (f c c')
    where
      f :: SymWord a => SArray CellId a -> SArray CellId a -> SArray CellId a
      f = symbolicMerge force test

mkSymbolicCells :: Symbolic SymbolicCells
mkSymbolicCells = SymbolicCells
  <$> newArray "intCells"
  <*> newArray "boolCells"
  <*> newArray "stringCells"

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
    { _lasSucceeds      :: SBool
    , _lasTablesRead    :: SFunArray TableName Bool
    , _lasTablesWritten :: SFunArray TableName Bool
    , _lasColumnDeltas  :: TableMap (SFunArray ColumnName Integer)
    , _lasTableCells    :: TableMap SymbolicCells
    }
  deriving (Show)

-- Implemented by-hand until 8.4, when we have DerivingStrategies
instance Mergeable LatticeAnalyzeState where
  symbolicMerge force test
    (LatticeAnalyzeState success  tsRead  tsWritten  deltas  cells)
    (LatticeAnalyzeState success' tsRead' tsWritten' deltas' cells') =
      LatticeAnalyzeState
        (symbolicMerge force test success   success')
        (symbolicMerge force test tsRead    tsRead')
        (symbolicMerge force test tsWritten tsWritten')
        (symbolicMerge force test deltas    deltas')
        (symbolicMerge force test cells     cells')

-- Checking state that is transferred through every computation, in-order.
newtype GlobalAnalyzeState
  --
  -- TODO: it seems that we'll need to accumulate constraints on
  --       `SBV ColumnName`s as we project from objects and write tables.
  --
  --  In addition to column names coming from a whitelist determined by type,
  --  also accum row key constraints -- that strings can't be empty.
  --
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
        , _lasColumnDeltas  = columnDeltas
        , _lasTableCells    = tableCells
        }
    , _globalState = GlobalAnalyzeState ()
    }

  where
    columnDeltas :: TableMap (SFunArray ColumnName Integer)
    columnDeltas = TableMap $ Map.fromList $ zip
      (Map.keys $ _tableMap tableCells)
      (repeat $ mkSFunArray (const 0))

allocateSymbolicCells :: [TableName] -> Symbolic (TableMap SymbolicCells)
allocateSymbolicCells tableNames = sequence $ TableMap $ Map.fromList $
  (, mkSymbolicCells) <$> tableNames

data AnalyzeFailure
  = AtHasNoRelevantFields EType Schema
  | AValUnexpectedlySVal SBVI.SVal
  | AValUnexpectedlyObj Object
  | KeyNotPresent String Object
  | MalformedLogicalOpExec LogicalOp [Term Bool]
  | ObjFieldOfWrongType String EType
  | UnsupportedDecArithOp ArithOp
  | UnsupportedIntArithOp ArithOp
  | UnsupportedUnaryOp UnaryArithOp
  | UnsupportedRoundingLikeOp1 RoundingLikeOp
  | UnsupportedRoundingLikeOp2 RoundingLikeOp
  -- For cases we don't handle yet:
  | UnhandledObject (Term Object)
  | UnhandledTerm String ETerm
  deriving Show

newtype AnalyzeM a
  = AnalyzeM
    { runAnalyzeM :: RWST AnalyzeEnv AnalyzeLog AnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState AnalyzeState, MonadError AnalyzeFailure)

makeLenses ''AnalyzeEnv
makeLenses ''TableMap
makeLenses ''AnalyzeState
makeLenses ''GlobalAnalyzeState
makeLenses ''LatticeAnalyzeState
makeLenses ''SymbolicCells

instance (Mergeable a) => Mergeable (AnalyzeM a) where
  symbolicMerge force test left right = AnalyzeM $ RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged (per AnalyzeState's Mergeable instance.)
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST (runAnalyzeM act) r
    in do
      lTup <- run left s
      let gs = lTup ^. _2.globalState
      rTup <- run right $ s & globalState .~ gs
      return $ symbolicMerge force test lTup rTup

symArrayAt
  :: forall array k v
   . (SymWord k, SymWord v, SymArray array)
  => SBV k -> Lens' (array k v) (SBV v)
symArrayAt symKey = lens getter setter
  where
    getter :: array k v -> SBV v
    getter arr = readArray arr symKey

    setter :: array k v -> SBV v -> array k v
    setter arr = writeArray arr symKey

type instance Index (TableMap a) = TableName
type instance IxValue (TableMap a) = a
instance Ixed (TableMap a) where ix k = tableMap.ix k
instance At (TableMap a) where at k = tableMap.at k

succeeds :: Lens' AnalyzeState SBool
succeeds = latticeState.lasSucceeds

tableRead :: TableName -> Lens' AnalyzeState SBool
tableRead tn = latticeState.lasTablesRead.symArrayAt (literal tn)

tableWritten :: TableName -> Lens' AnalyzeState SBool
tableWritten tn = latticeState.lasTablesWritten.symArrayAt (literal tn)

columnDelta :: TableName -> SBV ColumnName -> Lens' AnalyzeState SInteger
columnDelta tn sCn = latticeState.lasColumnDeltas.singular (ix tn).symArrayAt sCn

--
-- TODO: accumulate symbolic column name and row key variables (in a Set, in
-- GlobalState) and then assert contraints that these variables must take names
-- from a whitelist, or never contain "__".
--

sCellId :: SBV ColumnName -> SBV RowKey -> SBV CellId
sCellId sCn sRk = coerceSBV $ coerceSBV sCn .++ "__" .++ coerceSBV sRk

intCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SInteger
intCell tn sCn sRk =
  latticeState.lasTableCells.singular (ix tn).scIntValues.symArrayAt (sCellId sCn sRk)

boolCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SBool
boolCell tn sCn sRk =
  latticeState.lasTableCells.singular (ix tn).scBoolValues.symArrayAt (sCellId sCn sRk)

stringCell
  :: TableName
  -> SBV ColumnName
  -> SBV RowKey
  -> Lens' AnalyzeState SString
stringCell tn sCn sRk =
  latticeState.lasTableCells.singular (ix tn).scStringValues.symArrayAt (sCellId sCn sRk)

namedAuth :: SString -> AnalyzeM SBool
namedAuth str = do
  arr <- view nameAuths
  pure $ readArray arr str

analyzeTermO :: Term Object -> AnalyzeM Object
analyzeTermO = \case
  LiteralObject obj -> do
    obj' <- iforM obj $ \colName (fieldType, ETerm tm _) -> do
      val <- analyzeTerm tm
      pure (fieldType, mkAVal val)
    pure (Object obj')

  Read tn (Schema fields) rowId -> do
    rId <- analyzeTerm rowId
    tableRead tn .= true
    obj <- iforM fields $ \fieldName fieldType -> do
      let sCn = literal $ ColumnName fieldName
      x <- case fieldType of
        EType TInt  -> mkAVal <$> use (intCell    tn sCn (coerceSBV rId))
        EType TBool -> mkAVal <$> use (boolCell   tn sCn (coerceSBV rId))
        EType TStr  -> mkAVal <$> use (stringCell tn sCn (coerceSBV rId))
        -- TODO: more field types
      pure (fieldType, x)
    pure (Object obj)

  Var name -> do
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    case val of
      AVal  val -> throwError $ AValUnexpectedlySVal val
      AnObj obj -> pure obj

  obj -> throwError $ UnhandledObject obj

analyzeTerm
  :: forall a. (Show a, SymWord a) => Term a -> AnalyzeM (SBV a)
analyzeTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- analyzeTerm cond
    ite testPasses (analyzeTerm then') (analyzeTerm else')

  Enforce cond -> do
    cond' <- analyzeTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence a b -> analyzeTerm a *> analyzeTerm b

  Literal a -> pure a

  At schema@(Schema schemaFields) colName obj retType -> do
    Object obj' <- analyzeTermO obj

    -- Filter down to only fields which contain the type we're looking for
    let relevantFields
          = map fst
          $ filter (\(_name, ty) -> ty == retType)
          $ Map.toList schemaFields

    colName' <- analyzeTerm colName

    firstName:relevantFields' <- case relevantFields of
      [] -> throwError $ AtHasNoRelevantFields retType schema
      _ -> pure relevantFields

    let getObjVal fieldName = case Map.lookup fieldName obj' of
          Nothing -> throwError $ KeyNotPresent fieldName (Object obj')
          Just (_fieldType, AVal val) -> pure (mkSBV val)
          Just (fieldType, AnObj _x) -> throwError $
            ObjFieldOfWrongType fieldName fieldType

    firstVal <- getObjVal firstName

    -- Fold over each relevant field, building a sequence of `ite`s. We require
    -- at least one matching field, ie firstVal. At first glance, this should
    -- just be a `foldr1M`, but we want the type of accumulator and element to
    -- differ, because elements are `String` `fieldName`s, while the accumulator
    -- is an `SBV a`.
    foldrM
      (\fieldName rest -> do
        val <- getObjVal fieldName
        pure $ ite (colName' .== literal fieldName) val rest
      )
      firstVal
      relevantFields'

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tn rowKey obj -> do
    Object obj' <- analyzeTermO obj
    --
    -- TODO: handle write of non-literal object
    --
    tableWritten tn .= true
    sRk <- analyzeTerm rowKey
    void $ iforM obj' $ \colName (fieldType, aval) -> do
      let sCn = literal $ ColumnName colName
      case aval of
        AVal val' -> case fieldType of
          EType TInt  -> do
            let cell  :: Lens' AnalyzeState SInteger
                cell  = intCell tn sCn (coerceSBV sRk)
                next  = mkSBV val'
            prev <- use cell
            cell .= next
            columnDelta tn sCn += next - prev

          EType TBool -> boolCell   tn sCn (coerceSBV sRk) .= mkSBV val'

          EType TStr  -> stringCell tn sCn (coerceSBV sRk) .= mkSBV val'
          -- TODO: rest of cell types
        AnObj obj'' -> void $ throwError $ AValUnexpectedlyObj obj''

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literal "Write succeeded"

  Let name (ETerm rhs _) body -> do
    val <- analyzeTerm rhs
    local (scope.at name ?~ mkAVal val) $
      analyzeTerm body

  Let name (EObject rhs _) body -> do
    rhs' <- analyzeTermO rhs
    local (scope.at name ?~ AnObj rhs') $
      analyzeTerm body

  Var name -> do
    theScope <- view scope
    -- traceShowM ("Var name", name, theScope)
    -- Assume the term is well-scoped after typechecking
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    case val of
      AVal x -> pure (mkSBV x)
      AnObj _ -> undefined

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
      Add -> pure $ sFromIntegral x' + y'
      Sub -> pure $ sFromIntegral x' - y'
      Mul -> pure $ sFromIntegral x' * y'
      Div -> pure $ sFromIntegral x' / y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  DecIntArithOp op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    case op of
      Add -> pure $ x' + sFromIntegral y'
      Sub -> pure $ x' - sFromIntegral y'
      Mul -> pure $ x' * sFromIntegral y'
      Div -> pure $ x' / sFromIntegral y'
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
      -- if the number is exactly between two whole numbers, round to the
      -- nearest even.
      Round   ->
        let wholePart      = sRealToSInteger x'
            wholePartIsOdd = wholePart `sMod` 2 .== 1
            isExactlyHalf  = sFromIntegral wholePart + 1 / 2 .== x'

        in ite isExactlyHalf
          -- nearest even number!
          (wholePart + oneIf wholePartIsOdd)
          -- otherwise we take the floor of `x + 0.5`
          (sRealToSInteger (x' + 0.5))
      Ceiling -> negate (sRealToSInteger (negate x'))
      Floor   -> sRealToSInteger x'

  RoundingLikeOp2 op x precision -> do
    x'         <- analyzeTerm x
    precision' <- analyzeTerm precision
    let digitShift :: SInteger
        digitShift = 10 .^ precision'
        x'' = x' * sFromIntegral digitShift

    x''' <- analyzeTerm (RoundingLikeOp1 op (Literal x''))

    pure $ sFromIntegral x''' / sFromIntegral digitShift

  AddTime time (ETerm secs TInt) -> do
    time' <- analyzeTerm time
    secs' <- analyzeTerm secs
    pure $ time' + sFromIntegral secs'

  -- n@(AddTimeDec _ _) -> throwError $ UnhandledTerm
  --   "We don't support adding decimals to time yet"
  --   (ETerm n undefined)

  Comparison op x y -> do
    x' <- analyzeTerm x
    y' <- analyzeTerm y
    pure $ case op of
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

  NameAuthorized str -> namedAuth =<< analyzeTerm str

  Concat str1 str2 -> (.++) <$> analyzeTerm str1 <*> analyzeTerm str2

  PactVersion -> pure $ literal $ T.unpack pactVersion

  --
  -- NOTE: instead of this we will probably translate with-read to Read+Let+At
  --
  WithRead tn rowId bindings body -> do
    rId <- analyzeTerm rowId -- TODO: use this
    tableRead tn .= true
    -- TODO: this should actually read to bind variables here
    newVars <- forM bindings $ \varName ->
      pure (varName, mkAVal (literal True))
    local (scope <>~ Map.fromList newVars) $
      analyzeTerm body

  n -> do
    --traceShowM n
    throwError $ UnhandledTerm "unhandled term" (ETerm n undefined)

analyzeDomainProperty :: DomainProperty -> AnalyzeM SBool
analyzeDomainProperty Success = use succeeds
analyzeDomainProperty Abort = bnot <$> analyzeDomainProperty Success
analyzeDomainProperty (KsNameAuthorized (KeySetName n)) =
  namedAuth $ literal $ T.unpack n
analyzeDomainProperty (TableRead tn) = use $ tableRead tn
analyzeDomainProperty (TableWrite tn) = use $ tableWritten tn
-- analyzeDomainProperty (CellIncrease tableName colName)
analyzeDomainProperty (ColumnConserve tableName colName) =
  (0 .==) <$> use (columnDelta tableName (literal colName))
analyzeDomainProperty (ColumnIncrease tableName colName) =
  (0 .<) <$> use (columnDelta tableName (literal colName))

analyzeProperty :: Property -> AnalyzeM SBool
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
analyzeProperty (Occurs dp) = analyzeDomainProperty dp
