{-# language DeriveFunctor              #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language OverloadedStrings          #-}
{-# language ScopedTypeVariables        #-}

module Pact.Analyze.Analyze where

import Control.Monad
import Control.Monad.Except (MonadError, ExceptT(..), Except, runExcept,
                             throwError)
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.Set as Set
import qualified Data.Text as T
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName, Type, EObject)
import Pact.Types.Version (pactVersion)

import Pact.Analyze.Types

newtype AnalyzeM a
  = AnalyzeM
    { runAnalyzeM :: RWST AnalyzeEnv AnalyzeLog AnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState AnalyzeState, MonadError AnalyzeFailure)

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

namedAuth :: SString -> AnalyzeM SBool
namedAuth str = do
  arr <- view nameAuths
  pure $ readArray arr str

evalTermO :: Term Object -> AnalyzeM Object
evalTermO = \case
  LiteralObject obj -> do
    obj' <- iforM obj $ \colName (fieldType, ETerm tm _) -> do
      val <- evalTerm tm
      pure (fieldType, mkAVal val)
    pure (Object obj')

  Read tn (Schema fields) rowId -> do
    rId <- evalTerm rowId
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

evalTerm
  :: forall a. (Show a, SymWord a) => Term a -> AnalyzeM (SBV a)
evalTerm = \case
  IfThenElse cond then' else' -> do
    testPasses <- evalTerm cond
    ite testPasses (evalTerm then') (evalTerm else')

  Enforce cond -> do
    cond' <- evalTerm cond
    succeeds %= (&&& cond')
    pure true

  Sequence a b -> evalTerm a *> evalTerm b

  Literal a -> pure a

  At schema@(Schema schemaFields) colName obj retType -> do
    Object obj' <- evalTermO obj

    -- Filter down to only fields which contain the type we're looking for
    let relevantFields
          = map fst
          $ filter (\(_name, ty) -> ty == retType)
          $ Map.toList schemaFields

    colName' <- evalTerm colName

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
    Object obj' <- evalTermO obj
    --
    -- TODO: handle write of non-literal object
    --
    tableWritten tn .= true
    sRk <- evalTerm rowKey
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
    val <- evalTerm rhs
    local (scope.at name ?~ mkAVal val) $
      evalTerm body

  Let name (EObject rhs _) body -> do
    rhs' <- evalTermO rhs
    local (scope.at name ?~ AnObj rhs') $
      evalTerm body

  Var name -> do
    theScope <- view scope
    -- traceShowM ("Var name", name, theScope)
    -- Assume the term is well-scoped after typechecking
    Just val <- view (scope . at name)
    -- Assume the variable is well-typed after typechecking
    case val of
      AVal x -> pure (mkSBV x)
      AnObj _ -> undefined

  Arith op args ->
    if op `Set.member` unsupportedArithOps
    then throwError $ UnsupportedArithOp op
    else do

            args' <- forM args evalTerm
            case (op, args') of
              (Sub, [x, y]) -> pure $ x - y
              (Add, [x, y]) -> pure $ x + y
              (Mul, [x, y]) -> pure $ x * y
              (Div, [x, y]) -> pure $ x `sDiv` y
              (Mod, [x, y]) -> pure $ x `sMod` y
              (Abs, [x])    -> pure $ abs x
              (Signum, [x]) -> pure $ signum x
              (Negate, [x]) -> pure $ negate x
              _             -> throwError $ MalformedArithOpExec op args

  Comparison op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    pure $ case op of
      Gt  -> x' .> y'
      Lt  -> x' .< y'
      Gte -> x' .>= y'
      Lte -> x' .<= y'
      Eq  -> x' .== y'
      Neq -> x' ./= y'

  Logical op args -> do
    args' <- forM args evalTerm
    case (op, args') of
      (AndOp, [a, b]) -> pure $ a &&& b
      (OrOp, [a, b])  -> pure $ a ||| b
      (NotOp, [a])    -> pure $ bnot a
      _               -> throwError $ MalformedLogicalOpExec op args

  AddTimeInt time secs -> do
    time' <- evalTerm time
    secs' <- evalTerm secs
    pure $ time' + sFromIntegral secs'

  n@(AddTimeDec _ _) -> throwError $ UnhandledTerm
    "We don't support adding decimals to time yet"
    (ETerm n undefined)

  NameAuthorized str -> namedAuth =<< evalTerm str

  Concat str1 str2 -> (.++) <$> evalTerm str1 <*> evalTerm str2

  PactVersion -> pure $ literal $ T.unpack pactVersion

  --
  -- NOTE: instead of this we will probably translate with-read to Read+Let+At
  --
  WithRead tn rowId bindings body -> do
    rId <- evalTerm rowId -- TODO: use this
    tableRead tn .= true
    -- TODO: this should actually read to bind variables here
    newVars <- forM bindings $ \varName ->
      pure (varName, mkAVal (literal True))
    local (scope <>~ Map.fromList newVars) $
      evalTerm body

  n -> do
    --traceShowM n
    throwError $ UnhandledTerm "unhandled term" (ETerm n undefined)

evalDomainProperty :: DomainProperty -> AnalyzeM SBool
evalDomainProperty Success = use succeeds
evalDomainProperty Abort = bnot <$> evalDomainProperty Success
evalDomainProperty (KsNameAuthorized (KeySetName n)) =
  namedAuth $ literal $ T.unpack n
evalDomainProperty (TableRead tn) = use $ tableRead tn
evalDomainProperty (TableWrite tn) = use $ tableWritten tn
-- evalDomainProperty (CellIncrease tableName colName)
evalDomainProperty (ColumnConserve tableName colName) =
  (0 .==) <$> use (columnDelta tableName (literal colName))
evalDomainProperty (ColumnIncrease tableName colName) =
  (0 .<) <$> use (columnDelta tableName (literal colName))

evalProperty :: Property -> AnalyzeM SBool
evalProperty (p1 `Implies` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 ==> b2
evalProperty (p1 `And` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 &&& b2
evalProperty (p1 `Or` p2) = do
  b1 <- evalProperty p1
  b2 <- evalProperty p2
  pure $ b1 ||| b2
evalProperty (Not p) = bnot <$> evalProperty p
evalProperty (Occurs dp) = evalDomainProperty dp
