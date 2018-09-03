{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Pact.Analyze.Check
  ( verifyModule
  , verifyCheck
  , describeCheckFailure
  , describeCheckResult
  , describeParseFailure
  , describeVerificationWarnings
  , falsifyingModel
  , showModel
  , CheckFailure(..)
  , CheckFailureNoLoc(..)
  , CheckSuccess(..)
  , CheckResult
  , ModuleChecks(..)
  , SmtFailure(..)
  , ParseFailure
  , VerificationFailure(..)
  ) where

import           Control.Exception         as E
import           Control.Lens              (at, ifoldrM, itraversed, ix,
                                            traversed, view, (%~), (&), (<&>),
                                            (^.), (^?), (^@..), _1, _2, _Just,
                                            _Left)
import           Control.Monad             (void)
import           Control.Monad.Except      (Except, ExceptT (ExceptT),
                                            MonadError, catchError, runExceptT,
                                            throwError, withExcept, withExceptT)
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.HashMap.Strict       as HM
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (mapMaybe)
import           Data.Monoid               ((<>))
import           Data.SBV                  (Symbolic)
import qualified Data.SBV                  as SBV
import qualified Data.SBV.Control          as SBV
import qualified Data.SBV.Internals        as SBVI
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable          (for)
import           Prelude                   hiding (exp)

import           Pact.Analyze.Parse        hiding (tableEnv)
import           Pact.Typechecker          (typecheckTopLevel)
import           Pact.Types.Lang           (Info, mModel, mName, renderInfo,
                                            renderParsed, tMeta)
import           Pact.Types.Runtime        (Exp, ModuleData, ModuleName,
                                            Ref (Ref),
                                            Term (TConst, TDef, TSchema, TTable),
                                            asString, getInfo, tShow)
import qualified Pact.Types.Runtime        as Pact
import           Pact.Types.Typecheck      (AST,
                                            Fun (FDefun, _fArgs, _fBody, _fInfo),
                                            Named, Node, TcId (_tiInfo),
                                            TopLevel (TopFun, TopTable),
                                            UserType (_utFields, _utName),
                                            runTC, tcFailures)
import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval         hiding (invariants)
import           Pact.Analyze.Model        (allocArgs, allocModelTags,
                                            saturateModel, showModel)
import           Pact.Analyze.Parse        (expToCheck, expToInvariant,
                                            parseBindings)
import           Pact.Analyze.Translate
import           Pact.Analyze.Types
import           Pact.Analyze.Util


newtype VerificationWarnings = VerificationWarnings [Text]
  deriving (Eq, Show)

describeVerificationWarnings :: VerificationWarnings -> Text
describeVerificationWarnings (VerificationWarnings dups) = case dups of
  [] -> ""
  _  -> "Warning: duplicated property definitions for " <> T.intercalate ", " dups

data CheckSuccess
  = SatisfiedProperty (Model 'Concrete)
  | ProvedTheorem
  deriving (Eq, Show)

type ParseFailure = (Exp Info, String)

data SmtFailure
  = Invalid (Model 'Concrete)
  | Unsatisfiable
  | Unknown SBV.SMTReasonUnknown
  | UnexpectedFailure SBV.SMTException
  deriving Show

instance Eq SmtFailure where
  Invalid m1    == Invalid m2    = m1 == m2
  Unsatisfiable == Unsatisfiable = True

  -- SMTReasonUnknown and SMTException don't provide instances of Eq, so we
  -- always return 'False' in these cases.
  _             ==             _ = False

data CheckFailureNoLoc
  = NotAFunction Text
  | TypecheckFailure (Set TC.Failure)
  | TranslateFailure' TranslateFailureNoLoc
  | AnalyzeFailure' AnalyzeFailureNoLoc
  | SmtFailure SmtFailure
  deriving (Eq, Show)

data CheckFailure = CheckFailure
  { _checkFailureParsed :: !Info
  , _checkFailure       :: !CheckFailureNoLoc
  } deriving (Eq, Show)

type CheckResult = Either CheckFailure CheckSuccess

data ModuleChecks = ModuleChecks
  { propertyChecks  :: HM.HashMap Text [CheckResult]
  , invariantChecks :: HM.HashMap Text (TableMap [CheckResult])
  , moduleWarnings  :: VerificationWarnings
  } deriving (Eq, Show)

data VerificationFailure
  = ModuleParseFailure ParseFailure
  | ModuleCheckFailure CheckFailure
  | TypeTranslationFailure Text (Pact.Type TC.UserType)
  | InvalidRefType
  deriving Show

describeCheckSuccess :: CheckSuccess -> Text
describeCheckSuccess = \case
  SatisfiedProperty model -> "Property satisfied with model:\n"
                          <> showModel model
  ProvedTheorem           -> "Property proven valid"

describeParseFailure :: ParseFailure -> Text
describeParseFailure (exp, info)
  = T.pack (renderInfo (getInfo exp))
  <> ": could not parse " <> tShow exp <> ": " <> T.pack info

describeSmtFailure :: SmtFailure -> Text
describeSmtFailure = \case
  Invalid model  -> "Invalidating model found:\n" <> showModel model
  Unsatisfiable  -> "This property is unsatisfiable"
  Unknown reason -> "The solver returned 'unknown':\n" <> tShow reason
  UnexpectedFailure smtE -> T.pack $ show smtE

describeCheckFailure :: CheckFailure -> Text
describeCheckFailure (CheckFailure info failure) =
  case failure of
    TypecheckFailure fails -> T.unlines $ map
      (\(TC.Failure ti s) -> T.pack (renderInfo (_tiInfo ti) ++ ":Warning: " ++ s))
      (Set.toList fails)

    _ ->
      let str = case failure of
            NotAFunction name     -> "No function named " <> name
            TypecheckFailure _    -> error "impossible (handled above)"
            TranslateFailure' err -> describeTranslateFailureNoLoc err
            AnalyzeFailure' err   -> describeAnalyzeFailureNoLoc err
            SmtFailure err        -> describeSmtFailure err
      in T.pack (renderParsed (infoToParsed info)) <> ":Warning: " <> str

describeCheckResult :: CheckResult -> Text
describeCheckResult = either describeCheckFailure describeCheckSuccess

falsifyingModel :: CheckFailure -> Maybe (Model 'Concrete)
falsifyingModel (CheckFailure _ (SmtFailure (Invalid m))) = Just m
falsifyingModel _                                         = Nothing

-- TODO: don't throw out these Infos
translateToCheckFailure :: TranslateFailure -> CheckFailure
translateToCheckFailure (TranslateFailure info err)
  = CheckFailure info (TranslateFailure' err)

analyzeToCheckFailure :: AnalyzeFailure -> CheckFailure
analyzeToCheckFailure (AnalyzeFailure info err)
  = CheckFailure info (AnalyzeFailure' err)

smtToCheckFailure :: Info -> SmtFailure -> CheckFailure
smtToCheckFailure info = CheckFailure info . SmtFailure

--
-- TODO: implement machine-friendly JSON output for CheckResult
--

resultQuery
  :: Goal
  -> Model 'Symbolic
  -> ExceptT SmtFailure SBV.Query CheckSuccess
resultQuery goal model0 = do
  satResult <- lift SBV.checkSat
  case goal of
    Validation ->
      case satResult of
        SBV.Sat   -> throwError . Invalid =<< lift (saturateModel model0)
        SBV.Unsat -> pure ProvedTheorem
        SBV.Unk   -> throwError . Unknown =<< lift SBV.getUnknownReason

    Satisfaction ->
      case satResult of
        SBV.Sat   -> SatisfiedProperty <$> lift (saturateModel model0)
        SBV.Unsat -> throwError Unsatisfiable
        SBV.Unk   -> throwError . Unknown =<< lift SBV.getUnknownReason

-- -- Assumes sat mode. It might be a decent idea for us to introduce an indexed
-- -- type to denote which things assume certain modes.
-- checkConstraintVacuity :: ExceptT SmtFailure SBV.Query ()
-- checkConstraintVacuity = do
--   prePropRes <- lift $ SBV.checkSat
--   case prePropRes of
--     SBV.Sat   -> pure ()
--     SBV.Unsat -> throwError VacuousConstraints
--     SBV.Unk   -> throwError . Unknown =<< lift SBV.getUnknownReason

-- SBV also provides 'inNewAssertionStack', but in 'Query'
inNewAssertionStack
  :: ExceptT a SBV.Query CheckSuccess
  -> ExceptT a SBV.Query CheckSuccess
inNewAssertionStack act = do
    push
    result <- act `catchError` \e -> pop *> throwError e
    pop
    pure result

  where
    push = lift $ SBV.push 1
    pop  = lift $ SBV.pop 1

-- Produces args for analysis from model args
analysisArgs :: Map VarId (Located (Text, TVal)) -> Map VarId AVal
analysisArgs = fmap (view (located._2._2))

verifyFunctionInvariants'
  :: Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants' funInfo tables pactArgs body = runExceptT $ do
    (args, tm, graph) <- hoist generalize $
      withExcept translateToCheckFailure $ runTranslation funInfo pactArgs body

    ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
      modelArgs' <- lift $ allocArgs args
      tags <- lift $ allocModelTags (Located funInfo tm) graph
      resultsTable <- withExceptT analyzeToCheckFailure $
        runInvariantAnalysis tables (analysisArgs modelArgs') tm tags funInfo

      -- Iterate through each invariant in a single query so we can reuse our
      -- assertion stack.
      ExceptT $ fmap Right $
        SBV.query $
          for2 resultsTable $ \(Located info (AnalysisResult prop ksProvs)) -> do
            queryResult <- runExceptT $
              inNewAssertionStack $ do
                void $ lift $ SBV.constrain $ SBV.bnot prop
                resultQuery goal $ Model modelArgs' tags ksProvs graph

            -- Either SmtFailure CheckSuccess -> CheckResult
            pure $ case queryResult of
               Left smtFailure -> Left $
                 CheckFailure info (SmtFailure smtFailure)
               Right pass      -> Right pass

  where
    goal :: Goal
    goal = Validation

    config :: SBV.SMTConfig
    config = SBV.z3

    -- Discharges impure 'SMTException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SMTException) ->
      pure $ Left $ CheckFailure funInfo $ SmtFailure $ UnexpectedFailure e

    runSymbolic :: Symbolic a -> IO a
    runSymbolic = SBV.runSMTWith config

verifyFunctionProperty
  :: Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> Located Check
  -> IO (Either CheckFailure CheckSuccess)
verifyFunctionProperty funInfo tables pactArgs body (Located propInfo check) =
    runExceptT $ do
      (args, tm, graph) <- hoist generalize $
        withExcept translateToCheckFailure $
          runTranslation funInfo pactArgs body
      ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
        modelArgs' <- lift $ allocArgs args
        tags <- lift $ allocModelTags (Located funInfo tm) graph
        AnalysisResult prop ksProvs <- withExceptT analyzeToCheckFailure $
          runPropertyAnalysis check tables (analysisArgs modelArgs') tm tags funInfo
        void $ lift $ SBV.output prop
        hoist SBV.query $ withExceptT (smtToCheckFailure propInfo) $
          resultQuery goal $ Model modelArgs' tags ksProvs graph

  where
    goal :: Goal
    goal = checkGoal check

    config :: SBV.SMTConfig
    config = SBV.z3

    -- Discharges impure 'SMTException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SMTException) ->
      pure $ Left $ smtToCheckFailure propInfo $ UnexpectedFailure e

    runSymbolic :: Symbolic a -> IO a
    runSymbolic = fmap fst .
      SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup (goal == Satisfaction) config)

moduleTables
  :: HM.HashMap ModuleName ModuleData -- ^ all loaded modules
  -> ModuleData                       -- ^ the module we're verifying
  -> ExceptT ParseFailure IO [Table]
moduleTables modules (_mod, modRefs) = do
  -- All tables defined in this module, and imported by it. We're going to look
  -- through these for their schemas, which we'll look through for invariants.
  let tables = flip mapMaybe (modules ^@.. traversed . _2 . itraversed) $ \case
        (name, Ref (table@TTable {})) -> Just (name, table)
        _                             -> Nothing

  -- TODO: need mapMaybe for HashMap
  let schemas = HM.fromList $ flip mapMaybe (HM.toList modRefs) $ \case
        (name, Ref (schema@TSchema {})) -> Just (name, schema)
        _                               -> Nothing

  for tables $ \(tabName, tab) -> do
    (TopTable _info _name (Pact.TyUser schema) _meta, _tcState)
      <- lift $ runTC 0 False $ typecheckTopLevel (Ref tab)

    let TC.Schema{_utName,_utFields} = schema
        schemaName = asString _utName

    invariants <- case schemas ^? ix schemaName.tMeta.mModel._Just of
      -- no model = no invariants
      Nothing    -> pure []
      Just model -> liftEither $ do
        let model'        = expToMapping model
            expInvariants = model' ^? _Just . ix "invariants"
            expInvariant  = model' ^? _Just . ix "invariant"
        exps <- collectExps "invariants" expInvariants expInvariant
        runExpParserOver exps $
          flip runReaderT (varIdArgs _utFields) . expToInvariant TBool

    pure $ Table tabName schema invariants

-- | Parse a property definition like
--
-- * '(defproperty foo (> 1 0))'
-- * '(defproperty foo (a:integer b:integer) (> a b))'
parseDefprops :: Exp Info -> Either ParseFailure [(Text, DefinedProperty (Exp Info))]
parseDefprops (SquareList exps) = traverse parseDefprops' exps where
  parseDefprops' exp@(ParenList (EAtom' "defproperty" : rest)) = case rest of
    [ EAtom' propname, ParenList args, body ] -> do
      args' <- parseBindings (curry Right) args & _Left %~ (exp,)
      pure (propname, DefinedProperty args' body)
    [ EAtom' propname,              body ] ->
      pure (propname, DefinedProperty [] body)
    _ -> Left (exp, "Invalid property definition")
  parseDefprops' exp = Left (exp, "expected set of defproperty")
parseDefprops exp = Left (exp, "expected set of defproperty")

-- Get the set (HashMap) of refs to functions in this module.
moduleTypecheckableRefs :: ModuleData -> HM.HashMap Text Ref
moduleTypecheckableRefs (_mod, modRefs) = flip HM.filter modRefs $ \case
  Ref (TDef {})   -> True
  Ref (TConst {}) -> True
  _               -> False

-- Get the set of properties defined in this module
modulePropDefs
  :: ModuleData -> Either ParseFailure (HM.HashMap Text (DefinedProperty (Exp Info)))
modulePropDefs (Pact.Module{Pact._mMeta=Pact.Meta _ model}, _modRefs)
  = case model of
      Just model' -> HM.fromList <$> parseDefprops model'
      Nothing     -> Right HM.empty

moduleFunChecks
  :: [Table]
  -> HM.HashMap Text (Ref, Pact.FunType TC.UserType)
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -> Except VerificationFailure
       (HM.HashMap Text (Ref, Either ParseFailure [Located Check]))
moduleFunChecks tables modTys propDefs = for modTys $ \case
  -- TODO How better to handle the type mismatch?
  (Pact.Direct _, _) -> throwError InvalidRefType
  (ref@(Ref defn), Pact.FunType argTys resultTy) -> do

    let -- TODO: Ideally we wouldn't have any ad-hoc VID generation, but we're
        --       not there yet:
        vids = VarId <$> [1..]

    -- TODO(joel): this relies on generating the same unique ids as
    -- @checkFunction@. We need to more carefully enforce this is true!
    argTys' <- for argTys $ \(Pact.Arg name ty _info) ->
      case maybeTranslateType ty of
        Just ety -> pure (name, ety)
        Nothing  -> throwError $
          TypeTranslationFailure "couldn't translate argument type" ty

    resultTy' <- case maybeTranslateType resultTy of
      Just ety -> pure (0, "result", ety)
      Nothing  -> throwError $
        TypeTranslationFailure "couldn't translate result type" resultTy

    let env :: [(VarId, Text, EType)]
        env = resultTy' :
          ((\(vid, (nm, ty)) -> (vid, nm, ty)) <$> zip vids argTys')

        nameEnv :: Map Text VarId
        nameEnv = Map.fromList $ fmap (\(vid, nm, _) -> (nm, vid)) env

        idEnv :: Map VarId EType
        idEnv = Map.fromList $ fmap (\(vid, _, ty) -> (vid, ty)) env

        vidStart = VarId (length env)

        tableEnv = TableMap $ Map.fromList $
          tables <&> \Table { _tableName, _tableType } ->
            let fields = _utFields _tableType
                colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
                  \(Pact.Arg argName ty _) ->
                    (ColumnName (T.unpack argName),) <$> maybeTranslateType ty
            in (TableName (T.unpack _tableName), colMap)

    checks <- case defn ^? tMeta . mModel . _Just of
      -- no model = no properties
      Nothing    -> pure []
      Just model -> withExcept ModuleParseFailure $ liftEither $ do
        let model'        = expToMapping model
            expProperties = model' ^? _Just . ix "properties"
            expProperty   = model' ^? _Just . ix "property"
        exps <- collectExps "properties" expProperties expProperty
        runExpParserOver exps $
          expToCheck tableEnv vidStart nameEnv idEnv propDefs

    pure (ref, Right checks)

-- | Given an exp like '(k v)', convert it to a singleton map
expToMapping :: Exp Info -> Maybe (Map Text (Exp Info))
expToMapping (ParenList [EAtom' k, v]) = Just $ Map.singleton k v
expToMapping _                         = Nothing

-- | For both properties and invariants you're allowed to use either the
-- singular ("property") or plural ("properties") name. This helper just
-- collects the properties / invariants in a list.
collectExps
  :: String
  -> Maybe (Exp Info)
  -> Maybe (Exp Info)
  -> Either ParseFailure [Exp Info]
collectExps name multiExp singularExp = case multiExp of
  Just (SquareList exps') -> Right exps'
  Just exp -> Left (exp, name ++ " must be a list")
  Nothing -> case singularExp of
    Just exp -> Right [exp]
    Nothing  -> Right []

-- | This runs a parser over a collection of 'Exp's, collecting the failures
-- or successes.
runExpParserOver
  :: forall t.
     [Exp Info]
  -> (Exp Info -> Either String t)
  -> Either ParseFailure [Located t]
runExpParserOver exps parser = sequence $ exps <&> \meta -> case parser meta of
  Left err   -> Left (meta, err)
  Right good -> Right (Located (getInfo meta) good)

verifyFunctionProps :: [Table] -> Ref -> [Located Check] -> IO [CheckResult]
verifyFunctionProps tables ref props = do
  (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures

  case fun of
    TopFun (FDefun {_fInfo, _fArgs, _fBody}) _ ->
      if Set.null failures
      then for props $ verifyFunctionProperty _fInfo tables _fArgs _fBody
      else pure [Left (CheckFailure _fInfo (TypecheckFailure failures))]
    _ -> pure []

verifyFunctionInvariants
  :: [Table]
  -> Ref
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants tables ref = do
  (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures

  case fun of
    TopFun (FDefun {_fInfo, _fArgs, _fBody}) _ ->
      if Set.null failures
      then verifyFunctionInvariants' _fInfo tables _fArgs _fBody
      else pure $ Left $ CheckFailure _fInfo (TypecheckFailure failures)
    _ -> pure $ Right $ TableMap Map.empty

-- TODO: use from Control.Monad.Except when on mtl 2.2.2
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

-- | Verifies properties on all functions, and that each function maintains all
-- invariants.
verifyModule
  :: HM.HashMap ModuleName ModuleData   -- ^ all loaded modules
  -> ModuleData                         -- ^ the module we're verifying
  -> IO (Either VerificationFailure ModuleChecks)
verifyModule modules moduleData = runExceptT $ do
  tables <- withExceptT ModuleParseFailure $ moduleTables modules moduleData

  let -- HM.unions is biased towards the start of the list. This module should
      -- shadow the others. Note that load / shadow order of imported modules
      -- is undefined and in particular not the same as their import order.
      allModules = moduleData : HM.elems modules

  allModulePropDefs <-
    withExceptT ModuleParseFailure $ liftEither $
      traverse modulePropDefs allModules

  let -- how many times have these names been defined across all in-scope
      -- modules
      allModulePropNameDuplicates =
          HM.keys
        $ HM.filter (> (1 :: Int))
        $ foldl (\acc k -> acc & at k %~ (Just . maybe 0 succ)) HM.empty
        $ concatMap HM.keys
        $ allModulePropDefs

      propDefs :: HM.HashMap Text (DefinedProperty (Exp Info))
      propDefs = HM.unions allModulePropDefs

      typecheckedRefs :: HM.HashMap Text Ref
      typecheckedRefs = moduleTypecheckableRefs moduleData

  -- For each ref, if it typechecks as a function (it'll be either a function
  -- or a constant), keep its signature.
  --
  (funTypes :: HM.HashMap Text (Ref, Pact.FunType TC.UserType)) <- ifoldrM
    (\name ref accum -> do
      maybeFun <- lift $ runTC 0 False $ typecheckTopLevel ref
      pure $ case maybeFun of
        (TopFun (FDefun _info _name funType _args _body) _meta, _tcState)
          -> HM.insert name (ref, funType) accum
        _ -> accum
    )
    HM.empty
    typecheckedRefs

  (funChecks :: HM.HashMap Text (Ref, Either ParseFailure [Located Check]))
    <- hoist generalize $ moduleFunChecks tables funTypes propDefs

  let funChecks' :: Either ParseFailure (HM.HashMap Text (Ref, [Located Check]))
      funChecks' = traverse sequence funChecks

      verifyFunProps :: (Ref, [Located Check]) -> IO [CheckResult]
      verifyFunProps = uncurry (verifyFunctionProps tables)

  funChecks'' <- case funChecks' of
    Left errs         -> throwError $ ModuleParseFailure errs
    Right funChecks'' -> pure funChecks''

  funChecks''' <- lift $ traverse verifyFunProps funChecks''
  invariantChecks <- for typecheckedRefs $ \ref -> do
    withExceptT ModuleCheckFailure $ ExceptT $
      verifyFunctionInvariants tables ref

  let warnings = VerificationWarnings allModulePropNameDuplicates

  pure $ ModuleChecks funChecks''' invariantChecks warnings

-- | Verifies a one-off 'Check' for a function.
verifyCheck
  :: ModuleData     -- ^ the module we're verifying
  -> Text           -- ^ the name of the function
  -> Check          -- ^ the check we're running
  -> ExceptT VerificationFailure IO CheckResult
verifyCheck moduleData funName check = do
  let info       = dummyInfo
      moduleName = moduleData ^. _1.mName
      modules    = HM.fromList [(moduleName, moduleData)]
      moduleFun :: ModuleData -> Text -> Maybe Ref
      moduleFun (_mod, modRefs) name = name `HM.lookup` modRefs

  tables <- withExceptT ModuleParseFailure $ moduleTables modules moduleData
  case moduleFun moduleData funName of
    Just funRef -> ExceptT $
      Right . head <$> verifyFunctionProps tables funRef [Located info check]
    Nothing -> pure $ Left $ CheckFailure info $ NotAFunction funName
