{-# language GADTs               #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language Rank2Types          #-}

module Pact.Analyze.Analyze
  ( runCompiler
  , runCompilerDebug
  , runCompilerTest
  , runTest
  , analyzeFunction
  , inferFun
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Control.Lens hiding (op, (.>), (...))
import Data.Text (Text)
import Pact.Typechecker hiding (debug)
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName, Type)
import qualified Pact.Types.Runtime as Pact
import Pact.Types.Typecheck hiding (Var, UserType, Object, Schema)
import qualified Pact.Types.Typecheck as TC
import Pact.Types.Version (pactVersion)
import Pact.Repl
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Default (def)
import Data.Traversable (for)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.SBV as SBV
import qualified Data.Text as T

import Pact.Analyze.Translate
import Pact.Analyze.Types

import Debug.Trace

analyzeFunction'
  :: Check
  -> [AST Node]
  -> [(Text, Pact.Type TC.UserType)]
  -> Map Node Text
  -> IO CheckResult
analyzeFunction' check body argTys nodeNames =
  case runExcept (runReaderT (translateBody body) nodeNames) of
    Left reason -> pure $ Left $ AnalyzeFailure reason

    Right (ETerm body'' _) -> do
      compileFailureVar <- newEmptyMVar
      checkResult <- runCheck check $ do
        scope0 <- allocateArgs argTys
        nameAuths' <- newArray "nameAuthorizations"
        symCells <- mkSymbolicCells

        let prop   = checkProperty check
            env0   = AnalyzeEnv scope0 nameAuths'
            state0 = initialAnalyzeState symCells
            action = evalTerm body''
                  *> evalProperty prop

        case runExcept $ runRWST action env0 state0 of
          Left cf -> do
            liftIO $ putMVar compileFailureVar cf
            pure false
          Right (propResult, _env, _log) ->
            pure propResult

      mVarVal <- tryTakeMVar compileFailureVar
      pure $ case mVarVal of
        Nothing -> checkResult
        Just cf -> Left (AnalyzeFailure cf)

namedAuth :: SString -> AnalyzeM SBool
namedAuth str = do
  arr <- view nameAuths
  pure $ readArray arr str

evalTermO :: Term Object -> AnalyzeM Object
evalTermO = \case
  LiteralObject o -> pure o
  Read tn (Schema fields) rowId -> do
    rId <- evalTerm rowId
    tableRead tn .= true
    obj <- iforM fields $ \fieldName fieldType -> do
      x <- case fieldType of
        EType TInt -> mkAVal <$> use (intCell tn (literal (ColumnName fieldName)) (coerceSBV rId))
        EType TBool -> mkAVal <$> use (boolCell tn (literal (ColumnName fieldName)) (coerceSBV rId))
        EType TStr -> mkAVal <$> use (stringCell tn (literal (ColumnName fieldName)) (coerceSBV rId))
      pure (fieldType, x)
    pure (Object obj)

evalTerm :: (Show a, SymWord a) => Term a -> AnalyzeM (SBV a)
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

  -- In principle we could do this lookup with dynamic keys, but what do we
  -- return when the key is not present?
  At colName obj -> do
    Object obj' <- evalTermO obj
    case Map.lookup colName obj' of
      Nothing -> throwError $ KeyNotPresent colName (Object obj')
      Just (_fieldType, AVal val) -> pure $ mkSBV val
      Just (_fieldType, AnObj _x) -> undefined

  --
  -- TODO: we might want to eventually support checking each of the semantics
  -- of Pact.Types.Runtime's WriteType.
  --
  Write tn (Schema fields) rowId (LiteralObject (Object obj)) -> do
    tableWritten tn .= true
    rId <- evalTerm rowId
    iforM obj $ \colName (fieldType, val) ->
      case val of
        AVal val -> case fieldType of
          EType TInt -> intCell tn (literal (ColumnName colName)) (coerceSBV rId) .= mkSBV val
          EType TBool -> boolCell tn (literal (ColumnName colName)) (coerceSBV rId) .= mkSBV val
          EType TStr -> stringCell tn (literal (ColumnName colName)) (coerceSBV rId) .= mkSBV val
        AnObj _ -> pure () -- XXX throw error
    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literal "Write succeeded"

  Let name rhs body -> do
    val <- evalTerm rhs
    local (scope.at name ?~ mkAVal val) $
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
    traceShowM n
    throwError $ UnhandledTerm "unhandled term" (ETerm n undefined)

evalDomainProperty :: DomainProperty -> AnalyzeM SBool
evalDomainProperty Success = use succeeds
evalDomainProperty Abort = bnot <$> evalDomainProperty Success
evalDomainProperty (KsNameAuthorized (KeySetName n)) =
  namedAuth $ literal $ T.unpack n
evalDomainProperty (TableRead tn) = use $ tableRead tn
evalDomainProperty (TableWrite tn) = use $ tableWritten tn
-- evalDomainProperty (ColumnConserves tableName colName)
-- evalDomainProperty (CellIncrease tableName colName)

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

analyzeFunction
  :: TopLevel Node
  -> Check
  -> IO CheckResult
analyzeFunction (TopFun (FDefun _ _ (FunType _ retTy) args body' _)) check =
  let argNodes :: [Node]
      argNodes = _nnNamed <$> args

      -- extract the typechecker's name for a node, eg "analyze-tests.layup_x".
      nodeNames :: [Text]
      nodeNames = tcName <$> argNodes

      nodeNames' :: Map Node Text
      nodeNames' = Map.fromList $ zip argNodes nodeNames

      argTys :: [(Text, Pact.Type TC.UserType)]
      argTys = zip nodeNames (_aTy <$> argNodes)

  in analyzeFunction' check body' argTys nodeNames'

analyzeFunction _ _ = pure $ Left $ CodeCompilationFailed "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

checkProperty :: Check -> Property
checkProperty (Satisfiable p) = p
checkProperty (Valid p) = p

tcName :: Node -> Text
tcName = _tiName . _aId

sDecimal :: String -> Symbolic (SBV Decimal)
sDecimal = symbolic

allocateArgs :: [(Text, Pact.Type TC.UserType)] -> Symbolic (Map Text AVal)
allocateArgs argTys = fmap Map.fromList $ for argTys $ \(name, ty) -> do
  let name' = T.unpack name
  var <- case ty of
    TyPrim TyInteger -> mkAVal <$> sInteger name'
    TyPrim TyBool    -> mkAVal <$> sBool name'
    TyPrim TyDecimal -> mkAVal <$> sDecimal name'
    TyPrim TyTime    -> mkAVal <$> sInt64 name'
    TyPrim TyString  -> mkAVal <$> sString name'
    TyUser _         -> mkAVal <$> (free_ :: Symbolic (SBV UserType))

    -- TODO
    TyPrim TyValue   -> error "unimplemented type analysis"
    TyPrim TyKeySet  -> error "unimplemented type analysis"
    TyAny            -> error "unimplemented type analysis"
    TyVar _v         -> error "unimplemented type analysis"
    TyList _         -> error "unimplemented type analysis"
    TySchema _ _     -> error "unimplemented type analysis"
    TyFun _          -> error "unimplemented type analysis"
  pure (name, var)

-- This does not use the underlying property -- this merely dispatches to
-- sat/prove appropriately, and accordingly translates sat/unsat to
-- semantically-meaningful results.
runCheck :: Provable a => Check -> a -> IO CheckResult
runCheck (Satisfiable _prop) provable = do
  (SatResult smtRes) <- sat provable
  pure $ case smtRes of
    SBV.Unsatisfiable _config -> Left Unsatisfiable
    SBV.Satisfiable _config model -> Right $ SatisfiedProperty model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs
runCheck (Valid _prop) provable = do
  (ThmResult smtRes) <- prove provable
  pure $ case smtRes of
    SBV.Unsatisfiable _config -> Right ProvedTheorem
    SBV.Satisfiable _config model -> Left $ Invalid model
    SBV.SatExtField _config model -> Left $ SatExtensionField model
    SBV.Unknown _config reason -> Left $ Unknown reason
    SBV.ProofError _config strs -> Left $ ProofError strs

rsModuleData :: ModuleName -> Lens' ReplState (Maybe ModuleData)
rsModuleData mn = rEnv . eeRefStore . rsModules . at mn

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  -- XXX(joel): I don't think we should execScript' here
  (r,s) <- execScript' (Script False fp) fp
  either (die def) (const (return ())) r
  case view (rsModuleData mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f

inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO (TopLevel Node, TcState)
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)

runCompiler :: String -> Text -> Text -> Check -> IO ()
runCompiler = runCompilerDebug False

runCompilerDebug :: Bool -> String -> Text -> Text -> Check -> IO ()
runCompilerDebug dbg replPath' modName' funcName' check = do
  (fun, tcState) <- inferFun dbg replPath' (ModuleName modName') funcName'
  let failures = tcState ^. tcFailures
  if Set.null failures
  then do
    r <- analyzeFunction fun check
    putStrLn $ case r of
      Left err  -> show err
      Right res -> show res
  else putStrLn $ "typechecking failed: " ++ show failures

failedTcOrAnalyze :: TcState -> TopLevel Node -> Check -> IO CheckResult
failedTcOrAnalyze tcState fun check =
    if Set.null failures
    then analyzeFunction fun check
    else pure $ Left $ TypecheckFailure failures
  where
    failures = tcState ^. tcFailures

runCompilerTest :: String -> Text -> Text -> Check -> IO CheckResult
runCompilerTest replPath modName funcName check = do
  (fun, tcState) <- inferFun False replPath (ModuleName modName) funcName
  failedTcOrAnalyze tcState fun check

runTest :: Text -> Check -> IO CheckResult
runTest code check = do
  replState0 <- initReplState StringEval
  (eTerm, replState) <- runStateT (evalRepl' $ T.unpack code) replState0
  case eTerm of
    Left err ->
      pure $ Left $ CodeCompilationFailed err
    Right _t ->
      case view (rsModuleData "test") replState of
        Nothing ->
          pure $ Left $ CodeCompilationFailed "expected module 'test'"
        Just (_mod, modRefs) ->
          case HM.lookup "test" modRefs of
            Nothing ->
              pure $ Left $ CodeCompilationFailed "expected function 'test'"
            Just ref -> do
              (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
              failedTcOrAnalyze tcState fun check

