{-# language GADTs               #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language Rank2Types          #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections       #-}

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
import Data.Foldable (foldrM)
import Data.Text (Text)
import Pact.Typechecker hiding (debug)
import Pact.Types.Runtime hiding (Term, WriteType(..), TableName, Type, EObject)
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
  -> [TableName]
  -> IO CheckResult
analyzeFunction' check body argTys nodeNames tableNames =
  case runExcept (runReaderT (unTranslateM (translateBody body)) nodeNames) of
    Left reason -> pure $ Left $ AnalyzeFailure reason

    Right (ETerm body'' _) -> do
      compileFailureVar <- newEmptyMVar
      checkResult <- runCheck check $ do
        scope0 <- allocateArgs argTys
        nameAuths' <- newArray "nameAuthorizations"
        allTableCells <- sequence $ TableMap $ Map.fromList $
          (, mkSymbolicCells) <$> tableNames

        let prop   = checkProperty check
            env0   = AnalyzeEnv scope0 nameAuths'
            state0 = initialAnalyzeState allTableCells
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

  IntArithOp op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    case op of
      Add -> pure $ x' + y'
      Sub -> pure $ x' - y'
      Mul -> pure $ x' * y'
      Div -> pure $ x' `sDiv` y'
      Pow -> throwError $ UnsupportedIntArithOp op
      Log -> throwError $ UnsupportedIntArithOp op

  DecArithOp op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    case op of
      Add -> pure $ x' + y'
      Sub -> pure $ x' - y'
      Mul -> pure $ x' * y'
      Div -> pure $ x' / y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  IntDecArithOp op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    case op of
      Add -> pure $ sFromIntegral x' + y'
      Sub -> pure $ sFromIntegral x' - y'
      Mul -> pure $ sFromIntegral x' * y'
      Div -> pure $ sFromIntegral x' / y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  DecIntArithOp op x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    case op of
      Add -> pure $ x' + sFromIntegral y'
      Sub -> pure $ x' - sFromIntegral y'
      Mul -> pure $ x' * sFromIntegral y'
      Div -> pure $ x' / sFromIntegral y'
      Pow -> throwError $ UnsupportedDecArithOp op
      Log -> throwError $ UnsupportedDecArithOp op

  ModOp x y -> do
    x' <- evalTerm x
    y' <- evalTerm y
    pure $ x' `sMod` y'

  IntUnaryArithOp op x -> do
    x' <- evalTerm x
    case op of
      Negate -> pure $ negate x'
      Sqrt   -> throwError $ UnsupportedUnaryOp op
      Ln     -> throwError $ UnsupportedUnaryOp op
      Exp    -> throwError $ UnsupportedUnaryOp op -- TODO: use svExp
      Abs    -> pure $ abs x'
      Signum -> pure $ signum x'

  DecUnaryArithOp op x -> do
    x' <- evalTerm x
    case op of
      Negate -> pure $ negate x'
      Sqrt   -> throwError $ UnsupportedUnaryOp op
      Ln     -> throwError $ UnsupportedUnaryOp op
      Exp    -> throwError $ UnsupportedUnaryOp op -- TODO: use svExp
      Abs    -> pure $ abs x'
      Signum -> pure $ signum x'

  RoundingLikeOp1 op x -> do
    x' <- evalTerm x
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
    x'         <- evalTerm x
    precision' <- evalTerm precision
    let digitShift :: SInteger
        digitShift = 10 .^ precision'
        x'' = x' * sFromIntegral digitShift

    x''' <- evalTerm (RoundingLikeOp1 op (Literal x''))

    pure $ sFromIntegral x''' / sFromIntegral digitShift

  AddTime time (ETerm secs TInt) -> do
    time' <- evalTerm time
    secs' <- evalTerm secs
    pure $ time' + sFromIntegral secs'

  -- n@(AddTimeDec _ _) -> throwError $ UnhandledTerm
  --   "We don't support adding decimals to time yet"
  --   (ETerm n undefined)

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

      --
      -- TODO: FIXME: this is broken for anything but our current tests. we
      --              need to extract these from the program.
      --
      tableNames :: [TableName]
      tableNames = ["accounts", "tokens"]

  in analyzeFunction' check body' argTys nodeNames' tableNames

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
