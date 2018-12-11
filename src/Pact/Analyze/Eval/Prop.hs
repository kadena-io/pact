{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Pact.Analyze.Eval.Prop where

import           Control.Lens               (Lens', at, iforM, ix, view, (%=),
                                             (?~))
import           Control.Monad.Except       (ExceptT, MonadError (throwError))
import           Control.Monad.Reader       (MonadReader (local), ReaderT)
import           Control.Monad.State.Strict (MonadState, StateT (..))
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (Boolean (bnot, false, true, (&&&), (|||)),
                                             EqSymbolic ((.==)),
                                             Mergeable (symbolicMerge), SBV,
                                             SymWord)
import qualified Data.SBV.Internals         as SBVI
import           Data.String                (IsString (fromString))
import qualified Data.Text                  as T
import           Data.Traversable           (for)
import           Data.Type.Equality         ((:~:) (Refl))

import           Pact.Analyze.Alloc         (Alloc, MonadAlloc, exists, forAll)
import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Orphans       ()
import           Pact.Analyze.Types         hiding (tableName)
import qualified Pact.Analyze.Types         as Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util


--
-- TODO: rename this. @Query@ is already taken by sbv.
--
newtype Query a
  = Query
    { queryAction :: StateT SymbolicSuccess (ReaderT QueryEnv (ExceptT AnalyzeFailure Alloc)) a }
  deriving (Functor, Applicative, Monad, MonadReader QueryEnv,
            MonadError AnalyzeFailure, MonadState SymbolicSuccess, MonadAlloc)

instance (Mergeable a) => Mergeable (Query a) where
  -- We merge the result and state, performing any 'Alloc' actions that occur
  -- in-order.
  symbolicMerge force test left right = Query $ StateT $ \s0 -> do
    (resL, sL) <- runStateT (queryAction left) s0
    (resR, sR) <- runStateT (queryAction right) s0
    pure ( symbolicMerge force test resL resR
         , symbolicMerge force test sL   sR
         )

instance Analyzer Query where
  type TermOf Query = Prop
  eval           = evalProp
  evalO          = evalPropO
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid = view (scope . at vid)
  withVar vid val m = local (scope . at vid ?~ val) m
  markFailure b = id %= (&&& SymbolicSuccess (bnot b))

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

expectList :: Analyzer m => AVal -> m (SBV [a])
expectList = aval
  (\_prov sval -> pure (SBVI.SBV sval))
  (throwErrorNoLoc . AValUnexpectedlyObj)

getLitTableName :: Prop TyTableName -> Query TableName
getLitTableName (StrLit tn) = pure $ TableName tn
getLitTableName (CoreProp (Var vid name)) = do
  mTn <- view $ qeTableScope . at vid
  case mTn of
    Nothing -> throwErrorNoLoc $ fromString $
      "could not find table in scope: " <> T.unpack name
    Just tn -> pure tn
getLitTableName (PropSpecific Result)
  = throwErrorNoLoc "Function results can't be table names"
getLitTableName CoreProp{} = throwErrorNoLoc "Core values can't be table names"


getLitColName :: Prop TyColumnName -> Query ColumnName
getLitColName (StrLit cn) = pure $ ColumnName cn
getLitColName (CoreProp (Var vid name)) = do
  mCn <- view $ qeColumnScope . at vid
  case mCn of
    Nothing -> throwErrorNoLoc $ fromString $
      "could not find column in scope: " <> T.unpack name
    Just cn -> pure cn
getLitColName (PropSpecific Result)
  = throwErrorNoLoc "Function results can't be column names"
getLitColName CoreProp{} = throwErrorNoLoc "Core values can't be column names"


evalProp :: (a' ~ Concrete a, SymWord a') => Prop a -> Query (S a')
evalProp (CoreProp tm)    = evalCore tm
evalProp (PropSpecific a) = evalPropSpecific a

beforeAfterLens :: BeforeOrAfter -> Lens' BeforeAndAfter CellValues
beforeAfterLens = \case
  Before -> before
  After  -> after

evalPropO :: Prop 'TyObject -> Query Object
evalPropO (CoreProp a)          = evalCoreO a
evalPropO (PropSpecific Result) = expectObj =<< view qeAnalyzeResult
evalPropO (PropSpecific (PropRead ba (Schema fields) tn pRk)) = do
  tn' <- getLitTableName tn
  sRk <- evalProp pRk

  -- TODO: there is a lot of duplication between this and the corresponding
  -- term evaluation code. It would be nice to consolidate these.
  aValFields <- iforM fields $ \fieldName fieldType -> do
    let cn = ColumnName $ T.unpack fieldName

    av <- case fieldType of
      EType SInteger -> mkAVal <$> view
        (qeAnalyzeState.intCell     (beforeAfterLens ba) tn' cn sRk false)
      EType SBool    -> mkAVal <$> view
        (qeAnalyzeState.boolCell    (beforeAfterLens ba) tn' cn sRk false)
      EType SStr     -> mkAVal <$> view
        (qeAnalyzeState.stringCell  (beforeAfterLens ba) tn' cn sRk false)
      EType SDecimal -> mkAVal <$> view
        (qeAnalyzeState.decimalCell (beforeAfterLens ba) tn' cn sRk false)
      EType STime    -> mkAVal <$> view
        (qeAnalyzeState.timeCell    (beforeAfterLens ba) tn' cn sRk false)
      EType SKeySet  -> mkAVal <$> view
        (qeAnalyzeState.ksCell      (beforeAfterLens ba) tn' cn sRk false)

      EType (SList SInteger) -> mkAVal <$> view
        (qeAnalyzeState.intListCell     (beforeAfterLens ba) tn' cn sRk false)
      EType (SList SBool   ) -> mkAVal <$> view
        (qeAnalyzeState.boolListCell    (beforeAfterLens ba) tn' cn sRk false)
      EType (SList SStr    ) -> mkAVal <$> view
        (qeAnalyzeState.stringListCell  (beforeAfterLens ba) tn' cn sRk false)
      EType (SList SDecimal) -> mkAVal <$> view
        (qeAnalyzeState.decimalListCell (beforeAfterLens ba) tn' cn sRk false)
      EType (SList STime   ) -> mkAVal <$> view
        (qeAnalyzeState.timeListCell    (beforeAfterLens ba) tn' cn sRk false)
      EType (SList SKeySet ) -> mkAVal <$> view
        (qeAnalyzeState.ksListCell      (beforeAfterLens ba) tn' cn sRk false)

      EType SAny         -> pure OpaqueVal
      EType (SList SAny) -> pure OpaqueVal
      --
      -- TODO: if we add nested object support here, we need to install
      --       the correct provenance into AVals all the way down into
      --       sub-objects.
      --
      EObjectTy _    -> throwErrorNoLoc UnsupportedObjectInDbCell
      EType SObject  -> throwErrorNoLoc UnsupportedObjectInDbCell

    pure (fieldType, av)

  pure $ Object aValFields

evalPropSpecific :: PropSpecific a -> Query (S (Concrete a))
evalPropSpecific Success = view $ qeAnalyzeState.succeeds
evalPropSpecific Abort   = bnot <$> evalPropSpecific Success
evalPropSpecific Result  = expectVal =<< view qeAnalyzeResult
evalPropSpecific (Forall vid _name (EType (ty :: Types.SingTy k ty)) p)
  = singCase ty
  (\Refl -> withSymWord ty $ do
    var <- forAll :: Query (S (Concrete ty))
    local (scope.at vid ?~ mkAVal var) $ evalProp p)
  (\Refl -> throwErrorNoLoc "Only simple types can currently be quantified")
  (\Refl -> throwErrorNoLoc "Only simple types can currently be quantified")
evalPropSpecific (Forall _vid _name (EObjectTy _) _p) =
  throwErrorNoLoc "objects can't currently be quantified in properties (issue 139)"
evalPropSpecific (Forall vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (qeTableScope . at vid ?~ tableName) (evalProp prop)
  pure $ foldr (&&&) true bools
evalPropSpecific (Forall vid _name (QColumnOf tabName) prop) = do
  columns <- view (analyzeEnv . aeColumnIds . ix tabName)
  bools <- for (Map.keys columns) $ \colName ->
    let colName' = ColumnName $ T.unpack colName
    in local (qeColumnScope . at vid ?~ colName') (evalProp prop)
  pure $ foldr (&&&) true bools
evalPropSpecific (Exists vid _name (EType (ty :: Types.SingTy k ty)) p)
  = singCase ty
  (\Refl -> withSymWord ty $ do
    var <- exists :: Query (S (Concrete ty))
    local (scope.at vid ?~ mkAVal var) $ evalProp p)
  (\Refl -> throwErrorNoLoc "Only simple types can currently be quantified")
  (\Refl -> throwErrorNoLoc "Only simple types can currently be quantified")
evalPropSpecific (Exists _vid _name (EObjectTy _) _p) =
  throwErrorNoLoc "objects can't currently be quantified in properties (issue 139)"
evalPropSpecific (Exists vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (qeTableScope . at vid ?~ tableName) (evalProp prop)
  pure $ foldr (|||) false bools
evalPropSpecific (Exists vid _name (QColumnOf tabName) prop) = do
  columns <- view (analyzeEnv . aeColumnIds . ix tabName)
  bools <- for (Map.keys columns) $ \colName ->
    let colName' = ColumnName $ T.unpack colName
    in local (qeColumnScope . at vid ?~ colName') (evalProp prop)
  pure $ foldr (|||) false bools

-- DB properties
evalPropSpecific (TableRead tn) = do
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.tableRead tn'
evalPropSpecific (TableWrite tn) = do
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.tableWritten tn'
evalPropSpecific (ColumnWritten tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.columnWritten tn' cn'
evalPropSpecific (ColumnRead tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.columnRead tn' cn'
--
-- TODO: should we introduce and use CellWrite to subsume other cases?
--
evalPropSpecific (IntCellDelta tn cn pRk) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  sRk <- evalProp pRk
  view $ qeAnalyzeState.intCellDelta tn' cn' sRk
evalPropSpecific (DecCellDelta tn cn pRk) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  sRk <- evalProp pRk
  view $ qeAnalyzeState.decCellDelta tn' cn' sRk
evalPropSpecific (IntColumnDelta tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.intColumnDelta tn' cn'
evalPropSpecific (DecColumnDelta tn cn) = do
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.decColumnDelta tn' cn'
evalPropSpecific (RowRead tn pRk)  = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  numReads <- view $ qeAnalyzeState.rowReadCount tn' sRk
  pure $ sansProv $ numReads .== 1
evalPropSpecific (RowReadCount tn pRk)  = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.rowReadCount tn' sRk
evalPropSpecific (RowWrite tn pRk) = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  writes <- view $ qeAnalyzeState.rowWriteCount tn' sRk
  pure $ sansProv $ writes .== 1
evalPropSpecific (RowWriteCount tn pRk) = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.rowWriteCount tn' sRk
evalPropSpecific (RowExists tn pRk beforeAfter) = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  view $ qeAnalyzeState.
    rowExists (case beforeAfter of {Before -> before; After -> after}) tn' sRk
evalPropSpecific PropRead{}
  = vacuousMatch "an object cannot be a symbolic value"

-- Authorization
evalPropSpecific (KsNameAuthorized ksn) = nameAuthorized $ literalS ksn
evalPropSpecific (RowEnforced tn cn pRk) = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.cellEnforced tn' cn' sRk
