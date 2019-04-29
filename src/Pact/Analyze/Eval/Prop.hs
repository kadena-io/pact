{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Symbolic evaluation of the property language, 'Prop' (as opposed to the
-- 'Term' or 'Invariant' languages).
module Pact.Analyze.Eval.Prop where

import Control.Monad.Fail
import           Control.Lens               (Lens', at, ix, view, (%=), (?~))
import           Control.Monad.Except       (ExceptT, MonadError (throwError))
import           Control.Monad.Reader       (MonadReader (local), ReaderT)
import           Control.Monad.State.Strict (MonadState, StateT (..))
import           Data.Default               (def)
import qualified Data.Map.Strict            as Map
import           Data.SBV                   (EqSymbolic ((.==)),
                                             Mergeable (symbolicMerge), literal)
import qualified Data.SBV.Internals         as SBVI
import           Data.SBV.Tuple             (tuple)
import           Data.String                (IsString (fromString))
import qualified Data.Text                  as T
import           Data.Traversable           (for)
import           Data.Type.Equality         ((:~:) (Refl))
import           GHC.TypeLits               (symbolVal)

import           Pact.Analyze.Alloc         (Alloc, MonadAlloc, singExists,
                                             singForAll)
import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Types         hiding (objFields, tableName)
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

instance MonadFail Query where
    fail = throwError . AnalyzeFailure def . fromString

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
  eval = evalProp
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid        = view (scope . at vid)
  withVar vid val m = local (scope . at vid ?~ val) m
  markFailure b     = id %= (.&& SymbolicSuccess (sNot b))
  withMergeableAnalyzer ty f = withSymVal ty f

aval
  :: Analyzer m
  => (Maybe Provenance -> SBVI.SVal -> m a)
  -> AVal
  -> m a
aval elimVal = \case
  AVal mProv sval -> elimVal mProv sval
  OpaqueVal       -> throwErrorNoLoc OpaqueValEncountered

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


evalProp :: SingI a => Prop a -> Query (S (Concrete a))
evalProp (CoreProp tm)    = evalCore tm
evalProp (PropSpecific a) = evalPropSpecific a

beforeAfterLens :: BeforeOrAfter -> Lens' BeforeAndAfter CellValues
beforeAfterLens = \case
  Before -> before
  After  -> after

evalPropSpecific :: PropSpecific a -> Query (S (Concrete a))
evalPropSpecific Success = view $ qeAnalyzeState.succeeds
evalPropSpecific Abort   = sNot <$> evalPropSpecific Success
evalPropSpecific Result  = aval (pure ... mkS) =<< view qeAnalyzeResult
evalPropSpecific (Forall vid name (EType (ty :: Types.SingTy ty)) p) = do
  var <- singForAll ("forall_" ++ T.unpack name) ty
  local (scope.at vid ?~ mkAVal var) $ evalProp p
evalPropSpecific (Forall vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (qeTableScope . at vid ?~ tableName) (evalProp prop)
  pure $ foldr (.&&) sTrue bools
evalPropSpecific (Forall vid _name (QColumnOf tabName) prop) = do
  columns <- view (analyzeEnv . aeColumnIds . ix tabName)
  bools <- for (Map.keys columns) $ \colName ->
    let colName' = ColumnName $ T.unpack colName
    in local (qeColumnScope . at vid ?~ colName') (evalProp prop)
  pure $ foldr (.&&) sTrue bools
evalPropSpecific (Exists vid name (EType (ty :: Types.SingTy ty)) p) = do
  var <- singExists ("exists_" ++ T.unpack name) ty
  local (scope.at vid ?~ mkAVal var) $ evalProp p
evalPropSpecific (Exists vid _name QTable prop) = do
  TableMap tables <- view (analyzeEnv . invariants)
  bools <- for (Map.keys tables) $ \tableName ->
    local (qeTableScope . at vid ?~ tableName) (evalProp prop)
  pure $ foldr (.||) sFalse bools
evalPropSpecific (Exists vid _name (QColumnOf tabName) prop) = do
  columns <- view (analyzeEnv . aeColumnIds . ix tabName)
  bools <- for (Map.keys columns) $ \colName ->
    let colName' = ColumnName $ T.unpack colName
    in local (qeColumnScope . at vid ?~ colName') (evalProp prop)
  pure $ foldr (.||) sFalse bools

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

-- Authorization
evalPropSpecific (GuardPassed n) = namedGuardPasses $ literalS n
evalPropSpecific (RowEnforced tn cn pRk) = do
  sRk <- evalProp pRk
  tn' <- getLitTableName tn
  cn' <- getLitColName cn
  view $ qeAnalyzeState.cellEnforced tn' cn' sRk

evalPropSpecific (PropRead objTy@(SObjectUnsafe fields) ba tn pRk) = do
  tn' <- getLitTableName tn
  sRk <- evalProp pRk

  aValFields <- foldrSingList
    (pure $ Some SObjectNil $ AnSBV $ literal ())
    (\k ty accum -> do
      Some objTy'@(SObject schema) (AnSBV obj) <- accum
      let fieldName = symbolVal k
          cn = ColumnName fieldName

      AVal _prov sval <- mkAVal <$> view
        (qeAnalyzeState.typedCell ty (beforeAfterLens ba) tn' cn sRk sFalse)

      withSymVal ty $ withSymVal objTy' $ pure $ Some
        (SObjectUnsafe (SCons' k ty schema))
        (AnSBV (tuple (SBVI.SBV sval, obj))))
    fields

  case aValFields of
    Some ty (AnSBV obj) -> case singEq ty objTy of
      Nothing   -> throwErrorNoLoc "expected a different object type"
      Just Refl -> pure $ sansProv obj
