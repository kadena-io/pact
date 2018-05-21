{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Pact.Analyze.Check
  ( checkTopFunction
  , verifyModule
  , failedTcOrAnalyze
  , describeCheckResult
  , CheckFailure(..)
  , CheckSuccess(..)
  , CheckResult
  ) where

import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, tryTakeMVar)
import           Control.Lens               (at, cons, itraversed, ix,
                                             runIdentity, traversed, (%~), (&),
                                             (^.), (^?), (^@..), _2, _Just)
import           Control.Monad.Except       (runExcept, runExceptT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.RWS.Strict   (RWST (..))
import           Control.Monad.State.Strict (evalStateT)
import qualified Data.HashMap.Strict        as HM
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Monoid                ((<>))
import           Data.SBV                   (Provable, SatResult (SatResult),
                                             Symbolic, ThmResult (ThmResult),
                                             false, proveWith, sat, verbose, z3,
                                             (&&&))
import qualified Data.SBV                   as SBV
import qualified Data.SBV.Internals         as SBVI
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Traversable           (for)

import           Pact.Typechecker           (typecheckTopLevel)
import           Pact.Types.Lang            (mMetas, tMeta)
import           Pact.Types.Runtime         (Exp, ModuleData, ModuleName,
                                             Ref (Ref),
                                             Term (TDef, TSchema, TTable),
                                             Type (TyUser), asString,
                                             renderInfo)
import qualified Pact.Types.Runtime         as Pact
import           Pact.Types.Typecheck       (AST, Fun (FDefun),
                                             Named (_nnName, _nnNamed),
                                             Node (_aTy), TcId (_tiInfo),
                                             TcState,
                                             TopLevel (TopFun, TopTable),
                                             UserType (_utFields, _utName),
                                             runTC, tcFailures)
import qualified Pact.Types.Typecheck       as TC

import           Pact.Analyze.Analyze       hiding (invariants, model)
import           Pact.Analyze.Parse         (expToCheck, expToInvariant)
import           Pact.Analyze.Term
import           Pact.Analyze.Translate
import           Pact.Analyze.Types

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
    T.unlines (map
      (\(TC.Failure ti s) -> T.pack (renderInfo (_tiInfo ti) ++ " error: " ++ s))
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

      checkResult <- runCheck check $ do
        let tables' = tables & traverse %~ (\(a, b, _c) -> (a, b))
            aEnv    = mkAnalyzeEnv argTys tables
            state0  = mkInitialAnalyzeState tables'
            prop    = check ^. ckProp

            go :: Analyze AVal -> Symbolic (S Bool)
            go act = do
              let eAnalysis = runIdentity $ runExceptT $ runRWST (runAnalyze act) aEnv state0
              case eAnalysis of
                Left cf -> do
                  liftIO $ putMVar compileFailureVar cf
                  pure false
                Right (propResult, state1, constraints) -> do
                  let qEnv = mkQueryEnv aEnv state1 propResult
                      qAction = (&&&)
                        <$> analyzeProp prop
                        <*> checkInvariantsHeld
                  runConstraints constraints
                  eQuery <- runExceptT $ runReaderT (queryAction qAction) qEnv
                  case eQuery of
                    Left cf' -> do
                      liftIO $ putMVar compileFailureVar cf'
                      pure false
                    Right symAction -> pure symAction

        case tm of
          ETerm   body'' _ -> go . (fmap mkAVal) . analyzeTerm $ body''
          EObject body'' _ -> go . (fmap AnObj) . analyzeTermO $ body''

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

checkFunctionBody tables Nothing body argTys nodeNames =
  case runExcept (evalStateT (runReaderT (unTranslateM (translateBody body)) nodeNames) 0) of
    Left reason -> pure $ Left $ TranslateFailure reason

    Right tm -> do
      compileFailureVar <- newEmptyMVar

      checkResult <- runProvable $ do
        let tables' = tables & traverse %~ (\(a, b, _c) -> (a, b))
            aEnv    = mkAnalyzeEnv argTys tables
            state0  = mkInitialAnalyzeState tables'

            go :: Analyze AVal -> Symbolic (S Bool)
            go act = do
              let eAnalysis = runIdentity $ runExceptT $ runRWST (runAnalyze act) aEnv state0
              case eAnalysis of
                Left cf -> do
                  liftIO $ putMVar compileFailureVar cf
                  pure false
                Right (propResult, state1, constraints) -> do
                  let qEnv = mkQueryEnv aEnv state1 propResult
                  runConstraints constraints
                  eQuery <- runExceptT $ runReaderT (queryAction checkInvariantsHeld) qEnv
                  case eQuery of
                    Left cf' -> do
                      liftIO $ putMVar compileFailureVar cf'
                      pure false
                    Right symAction -> pure symAction

        case tm of
          ETerm   body'' _ -> go . (fmap mkAVal) . analyzeTerm $ body''
          EObject body'' _ -> go . (fmap AnObj) . analyzeTermO $ body''

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

checkTopFunction
  :: [(Text, TC.UserType, [(Text, SchemaInvariant Bool)])]
  -> TopLevel Node
  -> Maybe Check
  -> IO CheckResult
checkTopFunction tables (TopFun (FDefun _ _ _ args body') _) check =
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
  ThmResult smtRes <- proveWith (z3 {verbose=False}) provable
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
    SBV.Unsatisfiable{}           -> Left Unsatisfiable
    SBV.Satisfiable _config model -> Right $ SatisfiedProperty model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason    -> Left $ Unknown reason
    SBV.ProofError _config strs   -> Left $ ProofError strs
runCheck (Valid _prop) provable = do
  ThmResult smtRes <- proveWith (z3 {verbose=False}) provable
  pure $ case smtRes of
    SBV.Unsatisfiable{}           -> Right ProvedTheorem
    SBV.Satisfiable _config model -> Left $ Invalid model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason    -> Left $ Unknown reason
    SBV.ProofError _config strs   -> Left $ ProofError strs

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

verifyModule
  :: Maybe Check
  -- ^ all loaded modules
  -> HM.HashMap ModuleName ModuleData
  -- ^ the module we're verifying
  -> ModuleData
  -> IO (HM.HashMap Text [CheckResult])
verifyModule testCheck modules (_mod, modRefs) = do

  -- All tables defined in this module, and imported by it. We're going to look
  -- through these for their schemas, which we'll look through for invariants.
  let tables = flip mapMaybe (modules ^@.. traversed . _2 . itraversed) $ \case
        (name, Ref (table@TTable {})) -> Just (name, table)
        _                             -> Nothing

  -- TODO: need mapMaybe for HashMap
  let schemas = HM.fromList $ flip mapMaybe (HM.toList modRefs) $ \case
        (name, Ref (schema@TSchema {})) -> Just (name, schema)
        _                               -> Nothing

  -- All function definitions in this module. We're going to look through these
  -- for properties.
  let defns = flip HM.filter modRefs $ \ref -> case ref of
        Ref (TDef {}) -> True
        _             -> False

  tablesWithInvariants <- for tables $ \(tabName, tab) -> do
    (TopTable _info _name (TyUser schema) _meta, _tcState)
      <- runTC 0 False $ typecheckTopLevel (Ref tab)

    let schemaName = asString (_utName schema)

    -- look through every meta-property in the schema for invariants
    let mExp :: Maybe Exp
        mExp = schemas ^? ix schemaName . tMeta . _Just . mMetas . ix "invariants"

    let invariants :: [(Text, SchemaInvariant Bool)]
        invariants = case mExp of
          Just (Pact.EList' exps) -> catMaybes $ flip fmap exps $ \meta -> do
            SomeSchemaInvariant expr TBool
              <- expToInvariant (_utFields schema) meta
            [v] <- pure $ Set.toList (invariantVars expr)
            pure (v, expr)
          _                  -> []

    pure (tabName, schema, invariants)

  -- convert metas to checks
  defnsWithChecks <- for defns $ \ref -> do
    -- look through every meta-property in the definition for invariants
    let Ref defn = ref
        mExp :: Maybe Exp
        mExp = defn ^? tMeta . _Just . mMetas . ix "properties"
    let checks :: [Check]
        checks = case mExp of
          Just (Pact.ELitList exps) -> catMaybes $ expToCheck <$> exps
          _                         -> []
    pure (ref, checks)

  let defnsWithChecks' = case testCheck of
        Nothing    -> defnsWithChecks
        Just check -> defnsWithChecks & at "test" . _Just . _2 %~ cons check

  -- Now the meat of verification! For each definition in the module we check
  -- 1. that is maintains all invariants
  -- 2. that is passes any properties declared for it
  for defnsWithChecks' $ \(ref, props) -> do
    (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
    case fun of
      TopFun (FDefun {}) _ -> do
        result  <- failedTcOrAnalyze tablesWithInvariants tcState fun Nothing
        results <- for props $
          failedTcOrAnalyze tablesWithInvariants tcState fun . Just
        pure $ result : results
      _ -> pure []
