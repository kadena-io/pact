{-# language GADTs             #-}
{-# language LambdaCase        #-}
{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types        #-}
{-# language TupleSections     #-}

module Pact.Analyze.Check
  ( checkTopFunction
  , verifyModule
  , failedTcOrAnalyze
  , describeCheckResult
  , CheckFailure(..)
  , CheckSuccess(..)
  , CheckResult
  ) where

import Control.Concurrent.MVar
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Traversable (for)
import Data.Set (Set)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import qualified Data.Text as T
import Pact.Typechecker hiding (debug)
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName, Type, EObject)
import qualified Pact.Types.Runtime as Pact
import Pact.Types.Typecheck hiding (Var, UserType, Object, Schema)
import qualified Pact.Types.Typecheck as TC

import Pact.Analyze.Analyze (Analyze, AnalyzeFailure, allocateSymbolicCells,
                             analyzeTerm, analyzeTermO, analyzeProp,
                             describeAnalyzeFailure, mkAnalyzeEnv,
                             mkInitialAnalyzeState, mkQueryEnv, runAnalyze,
                             queryAction, mkInitialAnalyzeState,
                             checkInvariantsHeld)
import Pact.Analyze.Prop
import Pact.Analyze.Translate
import Pact.Analyze.Types
import Pact.Compile (expToCheck, expToInvariant)

data CheckFailure
  = Invalid SBVI.SMTModel
  | Unsatisfiable
  | Unknown String -- reason
  | SatExtensionField SBVI.SMTModel
  | ProofError [String]
  | TypecheckFailure (Set TC.Failure)
  | AnalyzeFailure AnalyzeFailure
  | TranslateFailure TranslateFailure
  | PropertyParseError Exp
  --
  -- TODO: maybe remove this constructor from from CheckFailure.
  --
  | CodeCompilationFailed String
  deriving (Show)

describeCheckFailure :: CheckFailure -> Text
describeCheckFailure = \case
  Invalid model ->
    "Invalidating model found:\n" <>
    T.pack (show model)
  Unsatisfiable  -> "This property is unsatisfiable"
  Unknown reason ->
    "The solver returned unknown with reason:\n" <>
    T.pack (show reason)
  SatExtensionField model ->
    "The solver return a model, but in an extension field containing infinite / epsilon:\n" <>
    T.pack (show model)
  ProofError lines' ->
    "The prover errored:\n" <>
    T.unlines (T.pack <$> lines')
  TypecheckFailure fails ->
    "The module failed to typecheck:\n" <>
    (T.unlines $ map
      (\(Failure ti s) -> T.pack (renderInfo (_tiInfo ti) ++ " error: " ++ s))
      (Set.toList fails))
  AnalyzeFailure err        -> describeAnalyzeFailure err
  TranslateFailure err      -> describeTranslateFailure err
  PropertyParseError expr   -> "Couldn't parse property: " <> T.pack (show expr)
  CodeCompilationFailed msg -> T.pack msg

data CheckSuccess
  = SatisfiedProperty SBVI.SMTModel
  | ProvedTheorem
  deriving (Show)

describeCheckSuccess :: CheckSuccess -> Text
describeCheckSuccess = \case
  SatisfiedProperty model ->
    "Property satisfied with model:\n" <>
    T.pack (show model)
  ProvedTheorem           -> "Property proven valid"

type CheckResult
  = Either CheckFailure CheckSuccess

describeCheckResult :: CheckResult -> Text
describeCheckResult = either describeCheckFailure describeCheckSuccess

checkFunctionBody
  :: [(Text, TC.UserType, [(Text, SchemaInvariant Bool)])]
  -> Maybe Check
  -> [AST Node]
  -> [(Text, Pact.Type TC.UserType)]
  -> Map Node Text
  -> IO CheckResult
checkFunctionBody tables (Just check) body argTys nodeNames =
  case runExcept (evalStateT (runReaderT (unTranslateM (translateBody body)) nodeNames) 0) of
    Left reason -> pure $ Left $ TranslateFailure reason

    Right tm -> do
      compileFailureVar <- newEmptyMVar

      let prop   = check ^. ckProp
          action :: AnalyzeT Symbolic (S Bool)
          action = case tm of
            ETerm   body'' _ -> do
              _ <- hoist generalize (analyzeTerm  body'')
              (&&&) <$> analyzeProperty prop <*> checkInvariantsHeld
            EObject body'' _ -> do
              _ <- hoist generalize (analyzeTermO body'')
              (&&&) <$> analyzeProperty prop <*> checkInvariantsHeld

      checkResult <- runCheck check $ do
        env0      <- mkAnalyzeEnv argTys tables
        state0    <- mkInitialAnalyzeState
          (tables & traverse %~ (\(a, b, _c) -> (a, b)))
          <$> allocateSymbolicCells tables
        eAnalysis <- runExceptT $ runRWST (runAnalyze action) env0 state0

        case eAnalysis of
          Left cf -> do
            liftIO $ putMVar compileFailureVar cf
            pure false
          Right (propResult, _env, _log) ->
            pure propResult

--       checkResult <- runCheck check $ do
--         aEnv <- mkAnalyzeEnv argTys tables
--         state0 <- mkInitialAnalyzeState <$> allocateSymbolicCells tableNames

--         let prop = check ^. ckProp

--             go :: Analyze AVal -> Symbolic (S Bool)
--             go act = do
--               let eAnalysis = runIdentity $ runExceptT $ runRWST (runAnalyze act) aEnv state0
--               case eAnalysis of
--                 Left cf -> do
--                   liftIO $ putMVar compileFailureVar cf
--                   pure false
--                 Right (propResult, state1, _log) -> do
--                   let qEnv = mkQueryEnv aEnv state1 propResult
--                   eQuery <- runExceptT $ runReaderT (queryAction $ analyzeProp prop) qEnv
--                   case eQuery of
--                     Left cf' -> do
--                       liftIO $ putMVar compileFailureVar cf'
--                       pure false
--                     Right symAction -> pure $ symAction

--         case tm of
--           ETerm   body'' _ -> go . (fmap mkAVal) . analyzeTerm $ body''
--           EObject body'' _ -> go . (fmap AnObj) . analyzeTermO $ body''

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

checkFunctionBody tables Nothing body argTys nodeNames =
  case runExcept (evalStateT (runReaderT (unTranslateM (translateBody body)) nodeNames) 0) of
    Left reason -> pure $ Left $ TranslateFailure reason

    Right tm -> do
      compileFailureVar <- newEmptyMVar

      let action = case tm of
            ETerm   body'' _ ->
              hoist generalize (analyzeTerm  body'')
                *> checkInvariantsHeld
            EObject body'' _ ->
              hoist generalize (analyzeTermO body'')
                *> checkInvariantsHeld

      checkResult <- runProvable $ do
        env0 <- mkAnalyzeEnv argTys tables
        state0 <- mkInitialAnalyzeState
          (tables & traverse %~ (\(a, b, _c) -> (a, b)))
          <$> allocateSymbolicCells tables

        case runExcept $ runRWST (runAnalyzeT action) env0 state0 of
          Left cf -> do
            liftIO $ putMVar compileFailureVar cf
            pure false
          Right (propResult, _env, _log) ->
            pure propResult

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

checkTopFunction
  :: [(Text, TC.UserType, [(Text, SchemaInvariant Bool)])]
  -> TopLevel Node
  -> Maybe Check
  -> IO CheckResult
checkTopFunction tables (TopFun (FDefun _ _ _ args body' _)) check =
  let nodes :: [Node]
      nodes = _nnNamed <$> args

      -- Extract the plain/unmunged names from the source code. We use the
      -- munged names for let/bind/with-read/etc -bound variables, but plain
      -- names for the args for usability. Because let/bind/etc can't shadow
      -- these unmunged names, we retain our SSA property.
      names :: [Text]
      names = _nnName <$> args

      argTys :: [(Text, Pact.Type TC.UserType)]
      argTys = zip names (_aTy <$> nodes)

      nodeNames :: Map Node Text
      nodeNames = Map.fromList $ zip nodes names

  in checkFunctionBody tables check body' argTys nodeNames

checkTopFunction _ _ _ = pure $ Left $ CodeCompilationFailed "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

runProvable :: Provable a => a -> IO CheckResult
runProvable provable = do
  (ThmResult smtRes) <- prove provable
  pure $ case smtRes of
    SBV.Unsatisfiable{}           -> Right ProvedTheorem
    SBV.Satisfiable _config model -> Left $ Invalid model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason    -> Left $ Unknown reason
    SBV.ProofError _config strs   -> Left $ ProofError strs

-- This does not use the underlying property -- this merely dispatches to
-- sat/prove appropriately, and accordingly translates sat/unsat to
-- semantically-meaningful results.
runCheck :: Provable a => Check -> a -> IO CheckResult
runCheck (Satisfiable _prop) provable = do
  (SatResult smtRes) <- sat provable
  pure $ case smtRes of
    SBV.Unsatisfiable{} -> Left Unsatisfiable
    SBV.Satisfiable _config model -> Right $ SatisfiedProperty model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs
runCheck (Valid _prop) provable = do
  (ThmResult smtRes) <- proveWith (z3 {verbose=True}) provable
  pure $ case smtRes of
    SBV.Unsatisfiable{} -> Right ProvedTheorem
    SBV.Satisfiable _config model -> Left $ Invalid model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs

failedTcOrAnalyze
  :: [(Text, TC.UserType, [(Text, SchemaInvariant Bool)])]
  -> TcState
  -> TopLevel Node
  -> Maybe Check
  -> IO CheckResult
failedTcOrAnalyze tables tcState fun check =
    if Set.null failures
    then checkTopFunction tables fun check
    else pure $ Left $ TypecheckFailure failures
  where
    failures = tcState ^. tcFailures

verifyModule :: ModuleData -> IO (HM.HashMap Text [CheckResult])
verifyModule (_mod, modRefs) = do

  let tables = flip HM.filter modRefs $ \(ref, _checks) -> case ref of
        Ref (TTable _ _ _ _ _) -> True
        Direct (TTable {})     -> False
        _                      -> False

  tables' <- for (HM.toList tables) $ \(tabName, (ref, metas)) -> do
    let invariants = catMaybes $ flip fmap metas $ \(colName, meta) -> do
          "invariant" <- pure colName
          SomeSchemaInvariant expr TBool <- expToInvariant meta
          [v] <- pure $ Set.toList (invariantVars expr)
          pure (v, expr)
    (TopTable _info _name (TyUser schema), _tcState)
      <- runTC 0 False $ typecheckTopLevel ref
    pure (tabName, schema, invariants)

  -- convert metas to checks
  let modRefs' :: HM.HashMap Text (Ref, [Check])
      modRefs' = flip fmap modRefs $ \(ref, metas) ->
        (ref, catMaybes $ flip fmap metas $ \(name, meta) -> case name of
          "property" -> expToCheck meta
          _          -> Nothing)

  for modRefs' $ \(ref, props) -> do
    (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
    case fun of
      TopFun (FDefun {}) -> do
        result  <- failedTcOrAnalyze tables' tcState fun Nothing
        results <- forM props $
          failedTcOrAnalyze tables' tcState fun . Just
        pure $ result : results
      _ -> pure []
