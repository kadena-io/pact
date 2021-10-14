{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

-- | Entrypoint for symbolic analysis of Pact programs -- for checking
-- that functions obey properties and maintain invariants.
module Pact.Analyze.Check
  ( verifyModule
  , renderVerifiedModule
  , verifyCheck
  , describeCheckFailure
  , describeCheckResult
  , describeParseFailure
  , falsifyingModel
  , showModel
  , hasVerificationError
  , CheckFailure(..)
  , CheckFailureNoLoc(..)
  , CheckSuccess(..)
  , CheckResult
  , ScopeError(..)
  , ModuleChecks(..)
  , SmtFailure(..)
  , ParseFailure
  , VerificationFailure(..)
  , RenderedOutput(..)

  -- Exported just for inclusion in haddocks:
  , verifyFunctionProperty
  , verifyFunctionInvariants
  ) where

import           Control.Exception          as E
import           Control.Lens hiding (DefName)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Morph        (generalize, hoist)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.State.Strict (evalStateT)
import           Data.Bifunctor             (first)
import           Data.Either                (partitionEithers)
import           Data.Foldable              (foldl')
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (isPrefixOf,nub)
import qualified Data.List                  as List
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Data.SBV                   (Symbolic)
import qualified Data.SBV                   as SBV
import qualified Data.SBV.Control           as SBV
import qualified Data.SBV.Internals         as SBVI
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Traversable           (for)
import           Prelude                    hiding (exp)

import           Pact.Typechecker           (typecheckTopLevel)
import           Pact.Types.Lang            (pattern ColonExp, pattern CommaExp,
                                             Def (..), DefType (..), Info,
                                             dFunType, dMeta, mModel,
                                             renderInfo, tDef, tInfo, tMeta,
                                             _aName, _dMeta, _mModel, _tDef)
import Pact.Types.PactError
import           Pact.Types.Pretty          (renderCompactText)
import           Pact.Types.Runtime         (Exp, ModuleData (..), ModuleName,
                                             Ref, Ref' (Ref),
                                             Term (TConst, TDef, TSchema, TTable),
                                             asString, getInfo, mdModule,
                                             mdRefMap, tShow)
import qualified Pact.Types.Runtime         as Pact
import           Pact.Types.Term            (DefName (..),
                                             dDefType, moduleDefMeta,
                                             moduleDefName, _Ref, _gGovernance)
import           Pact.Types.Type            (ftArgs, _ftArgs)
import           Pact.Types.Typecheck       (AST, Fun (FDefun, _fArgs, _fBody, _fInfo),
                                             Named, Node,
                                             TopLevel (TopConst, TopFun, TopTable),
                                             UserType (..), Schema (_schFields, _schName),
                                             runTC, tcFailures, toplevelInfo, DynEnv, mkTcState, renderTcFailure)
import qualified Pact.Types.Typecheck       as TC

import           Pact.Analyze.Alloc         (runAlloc)
import           Pact.Analyze.Errors
import           Pact.Analyze.Eval          hiding (invariants)
import           Pact.Analyze.Model         (allocArgs, allocModelTags,
                                             allocStepChoices, saturateModel,
                                             showModel)
import           Pact.Analyze.Parse         hiding (tableEnv)
import           Pact.Analyze.Translate
import           Pact.Analyze.Types
import           Pact.Analyze.Util

newtype VerificationWarnings = VerificationWarnings [Text]
  deriving (Eq, Show)

describeVerificationWarnings :: VerificationWarnings -> [Text]
describeVerificationWarnings (VerificationWarnings dups) = case dups of
  [] -> []
  _  -> ["Duplicated property definitions for " <>
    T.intercalate ", " dups]

data CheckSuccess
  = SatisfiedProperty (Model 'Concrete)
  | ProvedTheorem
  deriving (Eq, Show)

type ParseFailure = (Exp Info, String)

data SmtFailure
  = Invalid (Model 'Concrete)
  | Unsatisfiable
  | Unknown SBV.SMTReasonUnknown
  | SortMismatch String
  | UnexpectedFailure SBV.SBVException
  deriving Show

instance Eq SmtFailure where
  Invalid m1    == Invalid m2    = m1 == m2
  Unsatisfiable == Unsatisfiable = True

  -- SMTReasonUnknown and SBVException don't provide instances of Eq, so we
  -- always return 'False' in these cases.
  _             ==             _ = False

data CheckFailureNoLoc
  = NotAFunction Text
  | TypecheckFailure (Set TC.Failure)
  | TranslateFailure' TranslateFailureNoLoc
  | AnalyzeFailure' AnalyzeFailureNoLoc
  | SmtFailure SmtFailure
  | QueryFailure SmtFailure
  | VacuousProperty SmtFailure
  deriving (Eq, Show)

data CheckFailure = CheckFailure
  { _checkFailureParsed :: Info
  , _checkFailure       :: CheckFailureNoLoc
  } deriving (Eq, Show)

type CheckResult = Either CheckFailure CheckSuccess

data ScopeError
  = ScopeParseFailure ParseFailure
  | NotInScope Text
  | ScopeInvalidDirectRef
  deriving (Eq, Show)

describeScopeError :: ScopeError -> Text
describeScopeError = \case
  ScopeParseFailure pf ->
    describeParseFailure pf
  NotInScope name ->
    "Variable not in scope: " <> name
  ScopeInvalidDirectRef ->
    "Invalid Direct reference given to scope checker instead of Ref."

data ModuleChecks = ModuleChecks
  { propertyChecks  :: HM.HashMap Text [CheckResult]
  , stepChecks      :: HM.HashMap (Text, Int) [CheckResult]
  , invariantChecks :: HM.HashMap Text (TableMap [CheckResult])
  , moduleWarnings  :: VerificationWarnings
  } deriving (Eq, Show)

-- | Does this 'ModuleChecks' have either a property or invariant failure?
-- Warnings don't count.
hasVerificationError :: ModuleChecks -> Bool
hasVerificationError (ModuleChecks propChecks stepChecks invChecks _)
  = let errs = toListOf (traverse . traverse .            _Left) propChecks ++
               toListOf (traverse . traverse .            _Left) stepChecks ++
               toListOf (traverse . traverse . traverse . _Left) invChecks
    in not (null errs)

data CheckEnv = CheckEnv
  { _tables     :: [Table]
  , _consts     :: HM.HashMap Text EProp
  , _propDefs   :: HM.HashMap Text (DefinedProperty (Exp Info))
  , _moduleData :: ModuleData Ref
  , _caps       :: [Capability]
  , _moduleGov  :: Governance
  , _dynEnv     :: DynEnv
  }

-- | Essential data used to check a function (where function could actually be
-- a defun, defpact, or step).
--
-- Note: We don't include props, the function name, or it's check type. Why? We
-- use this structure for invariant checks, which don't check any props. We
-- also use it for checking pact steps which are a different check type and
-- borrow the name of their enclosing pact.
data FunData = FunData
  Info         -- Location info (for error messages)
  [Named Node] -- Arguments
  [AST Node]   -- Body

mkFunInfo :: Fun Node -> FunData
mkFunInfo = \case
  FDefun{_fInfo, _fArgs, _fBody} -> FunData _fInfo _fArgs _fBody
  _ -> error "invariant violation: mkFunInfo called on non-function"

data VerificationFailure
  = ModuleParseFailure ParseFailure
  | ModuleCheckFailure CheckFailure
  | TypeTranslationFailure Text (Pact.Type TC.UserType)
  | InvalidDirectReference
  | ModuleSpecInSchemaPosition Pact.ModuleName
  | ExpectedConcreteModule
  | FailedConstTranslation String
  | SchemalessTable Info
  | ScopeErrors [ScopeError]
  | NonDefTerm (Pact.Term (Ref' (Pact.Term Pact.Name)))
  deriving Show

_describeCheckSuccess :: CheckSuccess -> Text
_describeCheckSuccess = \case
  SatisfiedProperty model -> "Property satisfied with model:\n"
                          <> fst (showModel model)
  ProvedTheorem           -> "Property proven valid"

describeParseFailure :: ParseFailure -> Text
describeParseFailure (exp, info)
  = T.pack (renderInfo (getInfo exp))
  <> ": could not parse " <> renderCompactText exp <> ": " <> T.pack info

describeSmtFailure :: SmtFailure -> Text
describeSmtFailure = \case
  Invalid model    ->
    let (desc,header) = showModel model
    in case header of
      Just h -> "Invalidating model found in " <> h <> "\n" <> desc
      Nothing -> "Invalidating model found\n" <> desc
  Unsatisfiable    -> "This property is unsatisfiable"
  Unknown reason   -> "The solver returned 'unknown':\n" <> tShow reason
  SortMismatch msg -> T.unlines
    [ "The solver returned a sort mismatch:"
    , T.pack msg
    , "This may be the result of a bug in z3 versions 4.8.0 and earlier."
    , "Specifically, before commit a37d05d54b9ca10d4c613a4bb3a980f1bb0c1c4a."
    ]
  UnexpectedFailure smtE -> T.pack $ show smtE

describeUnknownFailure :: SBV.SMTReasonUnknown -> Text
describeUnknownFailure = \case
  SBV.UnknownMemOut -> "SMT solver out of memory"
  SBV.UnknownTimeOut -> "SMT solver timeout"
  r -> "SMT solver error: " <> tShow r

describeQueryFailure :: SmtFailure -> Text
describeQueryFailure = \case
  Invalid model  -> "Invalid model failure:\n" <> fst (showModel model)
  Unknown reason -> describeUnknownFailure reason
  err@SortMismatch{} -> "(QueryFailure): " <> describeSmtFailure err
  Unsatisfiable  -> "Unsatisfiable query failure: please report this as a bug"
  UnexpectedFailure smtE -> T.pack $ show smtE

describeVacuousProperty :: SmtFailure -> Text
describeVacuousProperty = \case
  Invalid _ -> "Unexpected solver response during vacuous property check: please report this as a bug"
  Unknown reason -> describeUnknownFailure reason
  err@SortMismatch{} -> "Vacuous property check: " <> describeSmtFailure err
  Unsatisfiable -> "Vacuous property encountered! There is no way for a transaction to succeed if this function is called from the top-level. Because all `property` expressions in Pact assume transaction success, in this case it would be possible to validate *any* `property`, even e.g. `false`."
  UnexpectedFailure smtE -> T.pack $ show smtE

describeCheckFailure :: CheckFailure -> [RenderedOutput]
describeCheckFailure (CheckFailure info failure) = case failure of
  TypecheckFailure fails -> map renderTcFailure $ Set.toList fails
  NotAFunction name     -> fatal $ "No function named " <> name
  TranslateFailure' err -> pure $ setInf $ describeTranslateFailureNoLoc err
  AnalyzeFailure' err   -> fatal $ describeAnalyzeFailureNoLoc err
  SmtFailure err        -> fatal $ describeSmtFailure err
  QueryFailure err      -> fatal $ describeQueryFailure err
  VacuousProperty err   -> fatal $ describeVacuousProperty err

  where
    fatal = pure . setInf . renderFatal
    setInf = set roInfo info

describeCheckResult :: CheckResult -> [RenderedOutput]
describeCheckResult = either describeCheckFailure (const [])

falsifyingModel :: CheckFailure -> Maybe (Model 'Concrete)
falsifyingModel (CheckFailure _ (SmtFailure (Invalid m))) = Just m
falsifyingModel _                                         = Nothing

translateToCheckFailure :: TranslateFailure -> CheckFailure
translateToCheckFailure (TranslateFailure info err)
  = CheckFailure info (TranslateFailure' err)

translateToVerificationFailure :: TranslateFailure -> VerificationFailure
translateToVerificationFailure = ModuleCheckFailure . translateToCheckFailure

analyzeToCheckFailure :: AnalyzeFailure -> CheckFailure
analyzeToCheckFailure (AnalyzeFailure info err)
  = CheckFailure info (AnalyzeFailure' err)

smtToCheckFailure :: Info -> SmtFailure -> CheckFailure
smtToCheckFailure info = CheckFailure info . SmtFailure

smtToQueryFailure :: Info -> SmtFailure -> CheckFailure
smtToQueryFailure info = CheckFailure info . QueryFailure

smtToVacuousProperty :: Info -> SmtFailure -> CheckFailure
smtToVacuousProperty info = CheckFailure info . VacuousProperty

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
        SBV.DSat _ -> throwError . Invalid =<< lift (saturateModel model0)
        SBV.Unsat -> pure ProvedTheorem
        SBV.Unk   -> throwError . mkUnknown =<< lift SBV.getUnknownReason

    Satisfaction ->
      case satResult of
        SBV.Sat   -> SatisfiedProperty <$> lift (saturateModel model0)
        SBV.DSat _ -> SatisfiedProperty <$> lift (saturateModel model0)
        SBV.Unsat -> throwError Unsatisfiable
        SBV.Unk   -> throwError . mkUnknown =<< lift SBV.getUnknownReason

  where mkUnknown = \case
          SBV.UnknownOther explanation
            | "Sort mismatch" `isPrefixOf` explanation
            -> SortMismatch explanation
          other -> Unknown other

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
  :: ExceptT a SBV.Query b
  -> ExceptT a SBV.Query b
inNewAssertionStack act = do
    push
    result <- act `catchError` \e -> pop *> throwError e
    pop
    pure result

  where
    push = lift $ SBV.push 1
    pop  = lift $ SBV.pop 1

-- Produces args for analysis from model args
analysisArgs :: Map VarId (Located (Unmunged, TVal)) -> Map VarId AVal
analysisArgs = fmap (view (located._2._2))

-- | Solver timeout in millis. Usually timeout is a bad sign
-- but can always try larger values here.
timeout :: Integer
timeout = 1000 -- one second

-- | Check that all invariants hold for a function (this is actually used for
-- defun, defpact, and step)
verifyFunctionInvariants
  :: CheckEnv
  -> FunData
  -> Text
  -> CheckableType
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants (CheckEnv tables _consts _pDefs moduleData caps gov _de)
  (FunData funInfo pactArgs body) funName checkType = runExceptT $ do
    let modName = moduleDefName $ _mdModule moduleData

    -- ignoring warnings here as they will be collected in 'verifyFunctionProperty'
    (args, stepChoices, tm, graph, _) <- hoist generalize $
      withExcept translateToCheckFailure $ runTranslation modName funName
        funInfo caps pactArgs body checkType

    let invsMap = TableMap $ Map.fromList $
          tables <&> \Table { _tableName, _tableInvariants } ->
            ( TableName (T.unpack _tableName)
            , fmap (const ()) <$> _tableInvariants
            )

    -- Check to see if there are any invariants in this module. If there aren't
    -- we can skip these checks.
    case invsMap ^.. traverse . traverse of
      [] -> pure $ invsMap & traverse .~ []

      _ -> ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
        lift $ SBV.setTimeOut timeout
        modelArgs'   <- lift $ runAlloc $ allocArgs args
        stepChoices' <- lift $ runAlloc $ allocStepChoices stepChoices
        tags         <- lift $ runAlloc $ allocModelTags modelArgs'
          (Located funInfo tm) graph
        let rootPath = _egRootPath graph
        resultsTable <- withExceptT analyzeToCheckFailure $
          runInvariantAnalysis modName gov tables caps
            (analysisArgs modelArgs') stepChoices' tm rootPath tags funInfo

        -- Iterate through each invariant in a single query so we can reuse our
        -- assertion stack.
        ExceptT $ fmap Right $
          SBV.query $
            for2 resultsTable $ \(Located info
              (AnalysisResult querySucceeds _ prop ksProvs)) -> do
              let model = Model modelArgs' tags ksProvs graph

              _ <- runExceptT $ inNewAssertionStack $ do
                void $ lift $ SBV.constrain $ sNot $ successBool querySucceeds
                withExceptT (smtToQueryFailure info) $
                  resultQuery Validation model

              queryResult <- runExceptT $ inNewAssertionStack $ do
                void $ lift $ SBV.constrain $ sNot prop
                resultQuery Validation model

              -- Either SmtFailure CheckSuccess -> CheckResult
              pure $ case queryResult of
                 Left smtFailure -> Left $
                   CheckFailure info (SmtFailure smtFailure)
                 Right pass      -> Right pass

  where
    config :: SBV.SMTConfig
    config = SBV.z3 { SBVI.allowQuantifiedQueries = True }

    -- Discharges impure 'SBVException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SBVException) ->
      pure $ Left $ CheckFailure funInfo $ SmtFailure $ UnexpectedFailure e

    runSymbolic :: Symbolic a -> IO a
    runSymbolic = SBV.runSMTWith config

-- | Check that a specific property holds for a function (this is actually used
-- for defun, defpact, and step)
verifyFunctionProperty
  :: CheckEnv
  -> FunData
  -> Text
  -> CheckableType
  -> Located Check
  -> IO [Either CheckFailure CheckSuccess]
verifyFunctionProperty (CheckEnv tables _consts _propDefs moduleData caps gov _de)
  (FunData funInfo pactArgs body) funName checkType
  (Located propInfo check) = fmap sequence' $ runExceptT $ do
    let modName = moduleDefName (_mdModule moduleData)
    (args, stepChoices, tm, graph, warnings) <- hoist generalize $
      withExcept translateToCheckFailure $
        runTranslation modName funName funInfo caps pactArgs body checkType

    -- Set up the model and our query
    let setupSmtProblem = do
          lift $ SBV.setTimeOut timeout
          modelArgs'   <- lift $ runAlloc $ allocArgs args
          stepChoices' <- lift $ runAlloc $ allocStepChoices stepChoices
          tags         <- lift $ runAlloc $ allocModelTags modelArgs'
            (Located funInfo tm) graph
          let rootPath = _egRootPath graph
          ar@(AnalysisResult _querySucceeds _txSuccess _prop ksProvs)
            <- withExceptT analyzeToCheckFailure $
              runPropertyAnalysis modName gov check tables caps
                (analysisArgs modelArgs') stepChoices' tm rootPath tags funInfo

          let model = Model modelArgs' tags ksProvs graph

          pure (ar, model)

    -- First we check whether the query definitely succeeds. Queries don't
    -- succeed if the (pure) property throws an error (eg division by 0 or
    -- indexing to an invalid array position). If the query fails we bail.
    _ <- ExceptT $ catchingExceptions $ runSymbolicSat $ runExceptT $ do
      (AnalysisResult querySucceeds _txSucc _ _, model) <- setupSmtProblem

      void $ lift $ SBV.output $ SBV.sNot $ successBool querySucceeds
      hoist SBV.query $ do
        withExceptT (smtToQueryFailure propInfo) $
          resultQuery Validation model

    -- If the-end user is checking for the validity of a proposition while
    -- assuming transaction success, throw an error if it is not possible for
    -- the transaction to succeed.
    --
    -- Unfortunately we need a completely separate `query` here. We cannot
    -- combine this with the query-success check by using `inNewAssertionStack`
    -- because this breaks support for quantifiers in properties. e.g.:
    --
    --    (property
    --      (forall (i:integer)
    --        (when (and (>= i 0) (<  i 5))
    --          (= (at i result) a))))
    --
    case check of
      PropertyHolds _ ->
        void $ ExceptT $ catchingExceptions $ runSymbolicSat $ runExceptT $ do
          (AnalysisResult _ txSuccess _ _, model) <- setupSmtProblem
          void $ lift $ SBV.output txSuccess
          hoist SBV.query $ do
            withExceptT (smtToVacuousProperty propInfo) $
              resultQuery Satisfaction model
      _ ->
        pure ()

    ExceptT $ catchingExceptions $ runSymbolicGoal $ runExceptT $ do
      (AnalysisResult _ _txSucc prop _, model) <- setupSmtProblem

      void $ lift $ SBV.output prop
      r <- hoist SBV.query $ do
        withExceptT (smtToCheckFailure propInfo) $
          resultQuery goal model
      -- note here that it is important that the main result is first
      -- for 'verifyCheck'/unit tests
      return $ (Right r : map (Left . translateToCheckFailure) warnings)

  where
    goal :: Goal
    goal = checkGoal check

    sequence' (Left a) = [Left a]
    sequence' (Right rs) = rs

    config :: SBV.SMTConfig
    config = SBV.z3 { SBVI.allowQuantifiedQueries = True }

    -- Discharges impure 'SBVException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SBVException) ->
      pure $ Left $ smtToCheckFailure propInfo $ UnexpectedFailure e

    -- Run a 'Symbolic' in sat mode
    runSymbolicSat :: Symbolic a -> IO a
    runSymbolicSat = SBV.runSMTWith config

    -- Run a 'Symbolic' in the mode corresponding to our goal
    runSymbolicGoal :: Symbolic a -> IO a
    runSymbolicGoal = fmap fst
      . SBVI.runSymbolic (SBVI.SMTMode SBVI.QueryExternal SBVI.ISetup
        (goal == Satisfaction) config)

-- | Get the set of tables in the specified modules.
moduleTables
  :: HM.HashMap ModuleName (ModuleData Ref)
  -- ^ all loaded modules
  -> ModuleRefs
  -- ^ the refs of the module we're verifying
  -> HM.HashMap Text EProp
  -- ^ constants in the module
  -> ExceptT VerificationFailure IO [Table]
moduleTables modules modRefs consts = do
  -- All tables defined in this module, and imported by it. We're going to look
  -- through these for their schemas, which we'll look through for invariants.
  let tables = flip mapMaybe (modules ^@.. traversed . mdRefMap . itraversed) $
        \case
          (name, Ref (table@TTable {})) -> Just (name, table)
          _                             -> Nothing

      schemas = modRefs ^. defschemas

  for tables $ \(tabName, tab) -> do
    (TopTable _info _name tableTy _meta, _tcState)
      <- lift $ runTC 0 False $ typecheckTopLevel (Ref tab)
    case tableTy of
      Pact.TyUser (TC.UTModSpec (TC.ModSpec mn)) -> throwError $ ModuleSpecInSchemaPosition mn
      Pact.TyUser schema@(TC.UTSchema (TC.Schema{_schName,_schFields})) -> do
        VarEnv vidStart invEnv vidTys <- hoist generalize $
          mkInvariantEnv schema

        let schemaName = asString _schName

            mkInvariant :: Exp Info -> Either String (Invariant 'TyBool)
            mkInvariant = expToInvariant vidStart invEnv vidTys consts SBool

        invariants <- case schemas ^? ix schemaName._Ref.tMeta.mModel of
          -- no model = no invariants
          Nothing    -> pure []
          Just model -> case normalizeListLit model of
            Nothing -> throwError $ ModuleParseFailure
              -- reconstruct an `Exp Info` for this list
              ( Pact.EList $ Pact.ListExp model Pact.Brackets $
                  schemas ^?! ix schemaName._Ref.tInfo
              , "malformed list (inconsistent use of comma separators?)"
              )
            Just model' -> withExceptT ModuleParseFailure $ liftEither $ do
              exps <- collectInvariants model'

              for exps $ \meta ->
                case mkInvariant meta of
                  Left err   -> Left (meta, err)
                  Right good -> Right (Located (getInfo meta) good)

        pure $ Table tabName schema invariants

      -- If we don't have a user type, the type should be `TyAny` (`*`),
      -- meaning the table has no schema. Refuse to verify the module.
      _ -> throwError $ SchemalessTable $
        HM.fromList tables ^?! ix tabName.tInfo

-- | Get the set of capabilities in this module. This is done by typechecking
-- every capability ref and converting to a 'Capability'.
moduleCapabilities
  :: DynEnv -> [ModuleData Ref] -> ExceptT VerificationFailure IO [Capability]
moduleCapabilities de mds = fmap concat $ forM mds $ \md -> do
    toplevels <- withExceptT ModuleCheckFailure $
                   traverse (ExceptT . typecheck de) (defcapRefs md)
    hoist generalize $ traverse mkCap toplevels

  where
    defcapRefs md = toListOf
      (mdRefMap.traverse.filtered
        (\ref -> ref ^? _Ref.tDef.dDefType == Just Defcap))
      md

    mkCap :: TopLevel Node -> Except VerificationFailure Capability
    mkCap toplevel = do
        eSchema <- mkESchema <$> traverse (translateArgTy "argument") pactArgs
        pure $ case eSchema of
          ESchema schema -> Capability schema capName

      where
        (capName, pactArgs) = case toplevel of
          TopFun FDefun{_fName,_fType,_fModule} _ ->
            (mkCapName _fModule _fName, _ftArgs _fType)
          _ ->
            error "invariant violation: defcap toplevel must be a defun"


translateArgTy
  :: Text
  -> Pact.Arg UserType
  -> Except VerificationFailure (Text, EType)
translateArgTy errNoun (Pact.Arg name ty _info) =
  case maybeTranslateType ty of
    Just ety -> pure (name, ety)
    Nothing  -> throwError $
      TypeTranslationFailure ("couldn't translate " <> errNoun <> " type") ty

data PropertyScope
  = Everywhere
  -- ^ This property applies to the whole module
  | Excluding (Set Text)
  -- ^ This property applies to all but the named functions (and pacts)
  | Including (Set Text)
  -- ^ This property applies to only the named functions (and pacts)
  deriving Show

data ModuleCheck = ModuleCheck
  { _moduleCheckType   :: Prop 'TyBool -> Check
  -- ^ The style of check to use ('PropertyHolds', 'SucceedsWhen', or
  -- 'FailsWhen')
  , _checkPropertyBody :: Exp Info
  -- ^ The body of the property to check
  , _moduleCheckScope  :: PropertyScope
  -- ^ Where does this property apply?
  }

-- Does this (module-scoped) property apply to this function?
applicableCheck :: DefName -> ModuleCheck -> Bool
applicableCheck (DefName funName) (ModuleCheck _ _ propScope) =
  case propScope of
    Everywhere      -> True
    Excluding names -> funName `Set.notMember` names
    Including names -> funName `Set.member`    names

-- List literals are valid either with no commas or with commas interspersed
-- between each element. Remove all commas.
normalizeListLit :: [Exp i] -> Maybe [Exp i]
normalizeListLit lits = case lits of
  _ : CommaExp : _ -> removeCommas lits
  _                ->
    let isComma = \case { CommaExp -> True; _ -> False }
    in case List.find isComma lits of
         Nothing     -> Just lits
         Just _comma -> Nothing

  where

    -- invariant: the input is the previous input *after a comma*, meaning the
    -- empty list is not valid
    removeCommas :: [Exp i] -> Maybe [Exp i]
    removeCommas = \case
      x : CommaExp : xs -> (x:) <$> removeCommas xs
      [CommaExp]        -> Nothing
      [x]               -> Just [x]
      _                 -> Nothing

-- | Parse a property definition or property like
--
-- * '(defproperty foo (> 1 0))'
-- * '(defproperty foo (a:integer b:integer) (> a b))'
-- * '(property foo)'
parseModuleModelDecl
  :: [Exp Info]
  -> Either ParseFailure
       [Either (Text, DefinedProperty (Exp Info)) ModuleCheck]
parseModuleModelDecl exps = traverse parseDecl exps where

  parseDecl exp@(ParenList (EAtom' "defproperty" : rest)) = case rest of
    [ EAtom' propname, ParenList args, body ] -> do
      args' <- parseBindings (curry Right) args & _Left %~ (exp,)
      pure $ Left (propname, DefinedProperty args' body)
    [ EAtom' propname,              body ] ->
      pure $ Left (propname, DefinedProperty [] body)
    _ -> Left (exp, "Invalid property definition")
  parseDecl exp = do
    (propTy, body, propScope) <- parsePropertyExp exp
    pure $ Right $ ModuleCheck propTy body propScope

  parseNames :: Exp Info -> Either ParseFailure (Set Text)
  parseNames exp@(SquareList names) = case normalizeListLit names of
    Just names' -> fmap Set.fromList $ traverse parseName names'
    Nothing     -> Left (exp, "expected a list of names")
  parseNames exp                = Left (exp, "expected a list of names")

  parseName :: Exp Info -> Either ParseFailure Text
  parseName (EAtom' name) = Right name
  parseName exp           = Left (exp, "expected a bare word name")

  getPropTy = \case
    "property"      -> Just PropertyHolds
    "succeeds-when" -> Just SucceedsWhen
    "fails-when"    -> Just FailsWhen
    _               -> Nothing

  parsePropertyExp
    :: Exp Info
    -> Either ParseFailure (Prop 'TyBool -> Check, Exp Info, PropertyScope)
  parsePropertyExp exp = case exp of
    ParenList (EAtom' propTyName : rest)
      | Just propTy <- getPropTy propTyName
      -> case rest of
        [ exp' ]
          -> pure (propTy, exp', Everywhere)
        [ exp', BraceList [ EStrLiteral' "except", ColonExp, names ] ]
          -> (propTy, exp',) . Excluding <$> parseNames names
        [ exp', BraceList [ EStrLiteral' "only",   ColonExp, names ] ]
          -> (propTy, exp',) . Including <$> parseNames names
        _ -> Left (exp, "malformed property definition")
    _ -> Left (exp, "expected a set of property / defproperty")

-- | Organize the module's refs by type
moduleRefs :: ModuleData Ref -> ModuleRefs
moduleRefs (ModuleData _ refMap) = foldl' f noRefs (HM.toList refMap)
  where
    f accum (name, ref) = case ref of
      Ref (TDef (Def{_dDefType, _dDefBody}) _) ->
        case _dDefType of
          Defun   -> accum & defuns . at name ?~ ref
          Defpact -> accum & defpacts . at name ?~ ref
          Defcap  -> accum
      Ref TConst{} ->
        accum & defconsts . at name ?~ ref
      Ref TSchema{} ->
        accum & defschemas . at name ?~ ref
      _ ->
        accum

    noRefs = ModuleRefs HM.empty HM.empty HM.empty HM.empty

-- | Module-level propery definitions and declarations
data ModelDecl = ModelDecl
  { _moduleDefProperties :: HM.HashMap Text (DefinedProperty (Exp Info))
  , _moduleProperties    :: [ModuleCheck]
  }

-- | Get the model defined in this module
moduleModelDecl :: ModuleData Ref -> Either ParseFailure ModelDecl
moduleModelDecl ModuleData{..} = do
  lst <- parseModuleModelDecl $ Pact._mModel $ moduleDefMeta _mdModule
  let (propList, checkList) = partitionEithers lst
  pure $ ModelDecl (HM.fromList propList) checkList

-- | Then environment for variables at the beginning of execution
data VarEnv = VarEnv
  { _vidStart :: VarId
  -- ^ The first 'VarId' the function can issue.
  , _nameVids :: Map Text VarId
  -- ^ A 'VarId' for each argument to the function. For steps these are the
  -- variables bound by the enclosing @defpact@.
  , _vidTys   :: Map VarId EType
  -- ^ The type of each variable in scope.
  }

-- | Given a schema, returns an environment of canonical assignment of var ids
-- to each column, and an environment of types. The canonical ordering is
-- determined by the lexicographic order of variable names. Also see
-- 'varIdColumns'.
mkInvariantEnv :: UserType -> Except VerificationFailure VarEnv
mkInvariantEnv (TC.UTModSpec (TC.ModSpec mn)) = throwError $ ModuleSpecInSchemaPosition mn
mkInvariantEnv (TC.UTSchema TC.Schema{_schFields}) = do
  tys <- Map.fromList . map (first (env Map.!)) <$>
    traverse (translateArgTy "schema field's") _schFields
  pure $ VarEnv vidStart env tys

  where
    -- Order variables lexicographically over their names when assigning
    -- variable IDs.
    env :: Map Text VarId
    env = Map.fromList $ flip zip [0..] $ List.sort $ map Pact._aName _schFields

    vidStart :: VarId
    vidStart = VarId $ Map.size env

-- | Make an environment (binding result and args) from a function type.
makeFunctionEnv
  :: Pact.FunType TC.UserType -> Except VerificationFailure VarEnv
makeFunctionEnv (Pact.FunType argTys resultTy) = do
  let -- We use VID 0 for the result, the one for each argument variable.
      -- Finally, we start the VID generator in the translation environment at
      -- the next VID. envVidStart is the first VID that will be issued.
      --
      -- TODO: Ideally we wouldn't have any ad-hoc VID generation, but we're
      --       not there yet.
      envVidStart = VarId (length argTys + 1)
      vids        = [1..(envVidStart - 1)]

  -- TODO(joel): this relies on generating the same unique ids as
  -- @checkFunction@. We need to more carefully enforce this is true!
  argTys' <- for argTys $ \(Pact.Arg name ty _info) ->
    case maybeTranslateType ty of
      Just ety -> pure (Unmunged name, ety)
      Nothing  -> throwError $
        TypeTranslationFailure "couldn't translate argument type" ty

  resultBinding <- case maybeTranslateType resultTy of
    Just ety -> pure $ Binding 0 (Unmunged "result") (Munged "result") ety
    Nothing  -> throwError $
      TypeTranslationFailure "couldn't translate result type" resultTy

  -- NOTE: At the moment, we leave all variables except for the toplevel args
  -- under analysis as the original munged/SSA'd variable names. And result,
  -- which we introduce. We also rely on this assumption in Translate's
  -- mkTranslateEnv.

  let env :: [Binding]
      env = resultBinding :
        (zip vids argTys' <&> \(vid, (Unmunged nm, ty))
          -> Binding vid (Unmunged nm) (Munged nm) ty)

      nameVids :: Map Text VarId
      nameVids = Map.fromList $ env <&> \(Binding vid (Unmunged nm) _ _)
        -> (nm, vid)

      vidTys :: Map VarId EType
      vidTys = Map.fromList $ fmap (\(Binding vid _ _ ty) -> (vid, ty)) env

  pure $ VarEnv envVidStart nameVids vidTys

mkTableEnv :: [Table] -> TableMap (ColumnMap EType)
mkTableEnv tables = TableMap $ Map.fromList $ foldr go [] tables
  where
    go Table { _tableName, _tableType } acc = case _tableType of
      TC.UTModSpec{} -> acc
      TC.UTSchema schema ->
        let fields = _schFields schema
            colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
              \(Pact.Arg argName ty _) ->
                (ColumnName (T.unpack argName),) <$> maybeTranslateType ty
        in (TableName (T.unpack _tableName), colMap):acc

-- | Get the set of checks for a step.
stepCheck
  :: [Table]
  -- ^ All tables defined in this module and imported by it
  -> HM.HashMap Text EProp
  -- ^ Constants defined in this module
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -- ^ Properties defined in this module
  -> Pact.FunType TC.UserType
  -- ^ The type of the pact this step is part of (we extract argument types
  -- from this)
  -> [Exp Info]
  -- ^ The model
  -> Except VerificationFailure (Either ParseFailure [Located Check])
stepCheck tables consts propDefs funTy model = do
  VarEnv envVidStart nameVids vidTys <- makeFunctionEnv funTy
  let getCheck = expToCheck (mkTableEnv tables) envVidStart nameVids vidTys
        consts propDefs
  checks <- withExcept ModuleParseFailure $ liftEither $ do
    exps <- collectProperties model
    for exps $ \(propTy, meta) -> case getCheck propTy meta of
      Left err   -> Left (meta, err)
      Right good -> Right (Located (getInfo meta) good)

  pure $ Right checks

-- | Get the set of checks for a function.
moduleFunCheck
  :: [Table]
  -- ^ All tables defined in this module and imported by it
  -> [ModuleCheck]
  -- ^ The set of properties that apply to all functions in the module
  -> HM.HashMap Text EProp
  -- ^ Constants defined in this module
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -- ^ Properties defined in this module
  -> Pact.Term (Ref' (Pact.Term Pact.Name))
  -- ^ The term under analysis
  -> Pact.FunType TC.UserType
  -- ^ The type of the term under analysis
  -> Except VerificationFailure (Either ParseFailure [Located Check])
moduleFunCheck tables modCheckExps consts propDefs defTerm funTy = do
  VarEnv envVidStart nameVids vidTys <- makeFunctionEnv funTy

  checks <- case defTerm of
    TDef def info ->
      let model = _mModel (_dMeta def)
      in case normalizeListLit model of
        Nothing -> throwError $ ModuleParseFailure
          -- reconstruct an `Exp Info` for this list
          ( Pact.EList (Pact.ListExp model Pact.Brackets info)
          , "malformed list (inconsistent use of comma separators?)"
          )
        Just model' -> withExcept ModuleParseFailure $ liftEither $ do
          exps <- collectProperties model'
          let funName = _dDefName (_tDef defTerm)
              applicableModuleChecks =
                filter (applicableCheck funName) modCheckExps
                  <&> \(ModuleCheck ty prop _scope) -> (ty, prop)

          for (applicableModuleChecks <> exps) $ \(propTy, meta) ->
            case expToCheck (mkTableEnv tables) envVidStart nameVids vidTys
              consts propDefs propTy meta of
              Left err   -> Left (meta, err)
              Right good -> Right (Located (getInfo meta) good)
    _ ->
      throwError $ NonDefTerm defTerm

  pure $ Right checks

-- | Remove the "property", "succeeds-when", or "fails-when" application from
-- every exp.
collectProperties
  :: [Exp Info] -> Either ParseFailure [(Prop 'TyBool -> Check, Exp Info)]
collectProperties multiExp = for multiExp $ \case
  ParenList [EAtom' "property",      v] -> Right (PropertyHolds, v)
  ParenList [EAtom' "succeeds-when", v] -> Right (SucceedsWhen,  v)
  ParenList [EAtom' "fails-when",    v] -> Right (FailsWhen,     v)
  exp -> Left (exp, "expected an application of \"property\", \"succeeds-when\", or \"fails-when\"")

-- | Remove the "invariant" application from every exp
collectInvariants :: [Exp Info] -> Either ParseFailure [Exp Info]
collectInvariants multiExp = for multiExp $ \case
  ParenList [EAtom' "invariant", v] -> Right v
  exp -> Left (exp, "expected an application of \"invariant\"")

-- | Typecheck a 'Ref'. This is used to extract an @'AST' 'Node'@, which is
-- translated to either a term or property.
typecheck :: DynEnv -> Ref -> IO (Either CheckFailure (TopLevel Node))
typecheck de ref = do
  (toplevel, tcState) <- TC.runTCState (mkTcState 0 False de) $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures
      info = toplevelInfo toplevel
  pure $ if Set.null failures
            then Right toplevel
            else Left $ CheckFailure info $ TypecheckFailure failures

-- | Extract constants by typechecking and translating to properties.
getConsts
  :: DynEnv
  -> HM.HashMap Text Ref
  -> ExceptT VerificationFailure IO (HM.HashMap Text EProp)
getConsts de defconstRefs = do

  (consts :: HM.HashMap Text (AST Node)) <- ifoldrM
    (\name ref accum -> do
      maybeConst <- lift $ typecheck de ref
      case maybeConst of
        Left checkFailure -> throwError $ ModuleCheckFailure checkFailure
        Right (TopConst _info _qualifiedName _type val _doc)
          -> pure $ accum & at name ?~ val
        Right _
          -> error "invariant failure: anything but a const is unexpected here"
    )
    HM.empty
    defconstRefs

  let constToProp :: ETerm -> Except VerificationFailure EProp
      constToProp tm = case constantToProp tm of
        Right prop -> pure prop
        Left msg   -> throwError $ FailedConstTranslation msg

      translateNodeNoGraph'
        = withExceptT translateToVerificationFailure . translateNodeNoGraph

  hoist generalize $
    traverseOf each (constToProp <=< translateNodeNoGraph') consts

-- | Get the set of property check results for steps. Note that we just check
-- properties of individual steps here. Invariants are checked in at the
-- defpact level.
getStepChecks
  :: CheckEnv
  -> HM.HashMap Text Ref
  -> ExceptT VerificationFailure IO (HM.HashMap (Text, Int) [CheckResult])
getStepChecks env@(CheckEnv tables consts propDefs _ _ _ de) defpactRefs = do

  (steps :: HM.HashMap (Text, Int)
    ((AST Node, [Named Node], Info), Pact.FunType TC.UserType))
    <- ifoldrM
    (\name ref accum -> do
      maybeDef <- lift $ typecheck de ref
      case maybeDef of
        Left checkFailure -> throwError $ ModuleCheckFailure checkFailure
        Right (TopFun (FDefun info _ _ Defpact funType args steps _) _meta)
          -> pure $ ifoldl
            (\i stepAccum step ->
              stepAccum & at (name, i) ?~ ((step, args, info), funType))
            accum
            steps
        Right _ -> error
          "invariant failure: anything but a function is unexpected here"
    )
    HM.empty
    defpactRefs

  (stepChecks :: HM.HashMap (Text, Int)
    ((AST Node, [Named Node], Info), Either ParseFailure [Located Check]))
    <- hoist generalize $ for steps $ \((step, args, info), pactType) ->
      case step of
        TC.Step _ _ exec _ _ model -> ((exec,args,info),) <$>
          stepCheck tables consts propDefs pactType model
        _ -> error
          "invariant violation: anything but a step is unexpected in stepChecks"

  stepChecks' <- case traverse sequence stepChecks of
    Left errs         -> throwError $ ModuleParseFailure errs
    Right stepChecks' -> pure stepChecks'

  lift $ fmap (fmap (nub . concat)) $ ifor stepChecks' $
    \(name, _stepNum) ((node, args, info), checks) -> for checks $
     verifyFunctionProperty env (FunData info args [node]) name CheckPactStep


-- | Get the set of property and invariant check results for functions (defun
-- and defpact)
getFunChecks
  :: CheckEnv
  -> HM.HashMap Text Ref
  -> ExceptT VerificationFailure IO
    ( HM.HashMap Text [CheckResult]
    , HM.HashMap Text (TableMap [CheckResult])
    )
getFunChecks env@(CheckEnv tables consts propDefs moduleData _cs _g de) refs = do
  ModelDecl _ checkExps <-
    withExceptT ModuleParseFailure $ liftEither $
      moduleModelDecl moduleData

  (funTypes :: HM.HashMap Text
    (Ref, TopLevel Node, Pact.FunType TC.UserType, CheckableType))
    <- ifoldrM
      (\name ref accum -> do
        maybeFun <- lift $ typecheck de ref
        case maybeFun of
          Left checkFailure -> throwError $ ModuleCheckFailure checkFailure
          Right topfun@(TopFun (FDefun _ _ _ defType funType _ _ _) _)
            -> let checkType = case defType of
                     Defpact -> CheckDefpact
                     Defun   -> CheckDefun
                     _       -> error
                       "invariant violation: only defpact / defun are allowed"

               in pure $ accum & at name ?~ (ref, topfun, funType, checkType)
          Right _ -> error
            "invariant failure: anything but a function is unexpected here"
      )
      HM.empty
      refs

  (funChecks
    :: HM.HashMap Text
      ((TopLevel Node, CheckableType), Either ParseFailure [Located Check]))
    <- hoist generalize $ for funTypes $ \case
      (Pact.Direct _, _, _, _) -> throwError InvalidDirectReference
      (Pact.Ref defn, toplevel, userTy, checkType) -> ((toplevel,checkType),)
        <$> moduleFunCheck tables checkExps consts propDefs defn userTy

  -- check for parse failures in any of the checks
  funChecks' <- case traverse sequence funChecks of
    Left errs        -> throwError $ ModuleParseFailure errs
    Right funChecks' -> pure funChecks'

  let invariantCheckable :: HM.HashMap Text (TopLevel Node, CheckableType)
      invariantCheckable = fst <$> funChecks'

  invariantChecks <- ifor invariantCheckable $ \name (toplevel, checkType) ->
    case toplevel of
      TopFun fun _ -> withExceptT ModuleCheckFailure $ ExceptT $
        verifyFunctionInvariants env (mkFunInfo fun) name checkType
      _ -> error "invariant violation: anything but a TopFun is unexpected in \
        \invariantCheckable"

  funChecks'' <- lift $ ifor funChecks' $ \name ((toplevel, checkType), checks)
    -> case toplevel of
      TopFun fun _ -> for checks $
        verifyFunctionProperty env (mkFunInfo fun) name checkType
      _            -> error
        "invariant violation: anything but a TopFun is unexpected in funChecks"

  pure (nub . concat <$> funChecks'', invariantChecks)

-- | Check that every property variable is in scope.
scopeCheckInterface
  :: Set Text
  -- ^ A set of table, definition and property names in scope
  -> HM.HashMap Text Ref
  -- ^ The set of refs to check
  -> [ScopeError]
scopeCheckInterface globalNames refs = refs <&&> \case
  Pact.Direct _ -> [ScopeInvalidDirectRef]
  Pact.Ref defn -> case defn ^? tDef . dMeta . mModel of
    Nothing -> []
    Just model -> case normalizeListLit model of
      Nothing ->
        [ ScopeParseFailure
          -- reconstruct an `Exp Info` for this list
          ( Pact.EList (Pact.ListExp model Pact.Brackets (defn ^. tInfo))
          , "malformed list (inconsistent use of comma separators?)"
          )
        ]
      Just model' -> case collectProperties model' of
        Left err   -> [ScopeParseFailure err]
        Right exps -> exps <&&> \(_propTy, meta) -> do
          let args     = fmap _aName $ defn ^. tDef . dFunType . ftArgs
              nameEnv  = Map.fromList $ ("result", 0) : zip args [1..]
              genStart = fromIntegral $ length nameEnv
          case evalStateT (runReaderT (expToPreProp meta) nameEnv) genStart of
            Left err           -> [ScopeParseFailure (meta, err)]
            Right preTypedBody -> prePropGlobals preTypedBody <&&>
              \globalName ->
                if globalName `Set.notMember` globalNames
                then [NotInScope globalName]
                else []
  where

    (<&&>) :: Foldable t => t a -> (a -> [ScopeError]) -> [ScopeError]
    (<&&>) = flip foldMap

moduleGovernance :: ModuleData Ref -> ExceptT VerificationFailure IO Governance
moduleGovernance moduleData = case _mdModule moduleData of
  Pact.MDModule (Pact.Module {_mGovernance}) ->
    case _gGovernance _mGovernance of
      Left (Pact.KeySetName rn) ->
        pure $ KsGovernance $ RegistryName rn
      Right (Def {_dDefName=Pact.DefName dn,_dModule}) ->
        pure $ CapGovernance $ mkCapName _dModule dn
  Pact.MDInterface _ ->
    throwError ExpectedConcreteModule

-- | Verifies properties on all functions, and that each function maintains all
-- invariants.
verifyModule
  :: DynEnv
  -> HM.HashMap ModuleName (ModuleData Ref) -- ^ all loaded modules
  -> ModuleData Ref                         -- ^ the module we're verifying
  -> IO (Either VerificationFailure ModuleChecks)
verifyModule de modules moduleData@(ModuleData modDef allRefs) = runExceptT $ do
  let modRefs = moduleRefs moduleData

  consts <- getConsts de $ modRefs ^. defconsts
  tables <- moduleTables modules modRefs consts

  let -- HM.unions is biased towards the start of the list. This module should
      -- shadow the others. Note that load / shadow order of imported modules
      -- is undefined and in particular not the same as their import order.
      allModules = moduleData : HM.elems modules

  allModuleModelDecls <- for allModules $ \modul ->
    case moduleModelDecl modul of
      Left err        -> throwError $ ModuleParseFailure err
      Right modelDecl -> pure modelDecl

  let allModulePropDefs = fmap _moduleDefProperties allModuleModelDecls

      -- how many times have these names been defined across all in-scope
      -- modules
      allModulePropNameDuplicates =
          HM.keys
        $ HM.filter (> (1 :: Int))
        $ foldl (\acc k -> acc & at k %~ (Just . maybe 0 succ)) HM.empty
        $ concatMap HM.keys allModulePropDefs

      warnings = VerificationWarnings allModulePropNameDuplicates

      propDefs :: HM.HashMap Text (DefinedProperty (Exp Info))
      propDefs = HM.unions allModulePropDefs

  case modDef of
    -- If we're passed an interface there is no implementation to check so we
    -- just check that every property variable referenced is in scope
    Pact.MDInterface{} ->
      let success = ModuleChecks HM.empty HM.empty HM.empty warnings
          globalNames = Set.unions $ fmap Set.fromList
            [ fmap _tableName tables
            , HM.keys propDefs
            , HM.keys allRefs
            ]
          scopeErrors = scopeCheckInterface globalNames allRefs

      in if length scopeErrors > 0
         then throwError $ ScopeErrors scopeErrors
         else pure success

    -- If we're passed a module we actually check properties
    Pact.MDModule{} -> do
      let defunRefs, defpactRefs :: HM.HashMap Text Ref
          ModuleRefs defunRefs defpactRefs _ _ = modRefs

      caps   <- moduleCapabilities de allModules
      gov    <- moduleGovernance moduleData

      let checkEnv = CheckEnv tables consts propDefs moduleData caps gov de

      -- Note that invariants are only checked at the defpact level, not in
      -- individual steps.
      (funChecks, invariantChecks)
        <- getFunChecks checkEnv $ defunRefs <> defpactRefs
      stepChecks <- getStepChecks checkEnv defpactRefs

      pure $ ModuleChecks funChecks stepChecks invariantChecks warnings

-- | Produce errors/warnings from result.
renderVerifiedModule :: Either VerificationFailure ModuleChecks -> [RenderedOutput]
renderVerifiedModule = \case
  Left (ModuleParseFailure failure)  ->
    [renderFatal $ describeParseFailure failure]
  Left (ModuleCheckFailure checkFailure) -> describeCheckFailure checkFailure
  Left (TypeTranslationFailure msg ty) ->
    [renderFatal $ msg <> ": " <> tShow ty]
  Left InvalidDirectReference ->
    [renderFatal $ "Invalid Direct reference given to typechecker instead of Ref"]
  Left ExpectedConcreteModule ->
    [renderFatal $ "Expected concrete module but encountered an interface"]
  Left (NonDefTerm term) ->
    [renderFatal $ "Expected TDef Term but encountered: " <> tShow term]
  Left (FailedConstTranslation msg) ->
    [renderFatal $ T.pack msg]
  Left (SchemalessTable info) ->
    [renderFatal $ T.pack (renderInfo info) <>
      "Verification requires all tables to have schemas"
    ]
  Left (ModuleSpecInSchemaPosition mn) ->
    [renderFatal $ "Found modref spec in schema position: " <> renderCompactText mn
    ]
  Left (ScopeErrors errs) ->
    renderFatal <$> ("Scope checking errors" : fmap describeScopeError errs)
  Right (ModuleChecks propResults stepResults invariantResults warnings) ->
    let propResults'      = toListOf (traverse.each)          propResults
        stepResults'      = toListOf (traverse.each)          stepResults
        invariantResults' = toListOf (traverse.traverse.each) invariantResults
        allResults         = propResults' <> stepResults' <> invariantResults'
    in (concatMap describeCheckResult allResults) <>
         (renderWarn <$> describeVerificationWarnings warnings)


-- | Verifies a one-off 'Check' for a function.
verifyCheck
  :: DynEnv
  -> ModuleData Ref -- ^ the module we're verifying
  -> Text           -- ^ the name of the function
  -> Check          -- ^ the check we're running
  -> CheckableType
  -> ExceptT VerificationFailure IO [CheckResult]
verifyCheck de moduleData funName check checkType = do
  let info       = dummyInfo
      moduleName = moduleDefName $ moduleData ^. mdModule
      modules    = HM.fromList [(moduleName, moduleData)]
      moduleFun :: ModuleData Ref -> Text -> Maybe Ref
      moduleFun ModuleData{..} name = name `HM.lookup` _mdRefMap
      modRefs    = moduleRefs moduleData

  caps   <- moduleCapabilities de [moduleData]
  consts <- getConsts de $ modRefs ^. defconsts
  tables <- moduleTables modules modRefs consts
  gov    <- moduleGovernance moduleData

  let checkEnv = CheckEnv tables HM.empty HM.empty moduleData caps gov de

  case moduleFun moduleData funName of
    Just funRef -> do
      toplevel <- lift $ typecheck de funRef
      case toplevel of
        Left checkFailure -> throwError $ ModuleCheckFailure checkFailure
        Right (TopFun fun _) -> ExceptT $ fmap Right $
          verifyFunctionProperty checkEnv (mkFunInfo fun) funName checkType $
            Located info check
        Right _
          -> error "invariant violation: verifyCheck called on non-function"
    Nothing -> pure [Left $ CheckFailure info $ NotAFunction funName]
