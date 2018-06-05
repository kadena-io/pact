{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pact.Analyze.Check
  ( verifyModule
  , verifyCheck
  , describeCheckResult
  , CheckFailure(..)
  , CheckSuccess(..)
  , CheckResult
  ) where

import           Control.Lens              (ifoldrM, itraversed, ix, traversed,
                                            (<&>), (^.), (^?), (^@..), _2,
                                            _Just)
import           Control.Monad             (void)
import           Control.Monad.Except      (ExceptT, runExcept, runExceptT)
import           Control.Monad.Gen         (runGenTFrom)
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.RWS.Strict  (RWST (..))
import           Control.Monad.Trans.Class (lift)
import qualified Data.Default              as Default
import           Data.Either               (lefts, rights)
import qualified Data.HashMap.Strict       as HM
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (mapMaybe)
import           Data.Monoid               ((<>))
import           Data.SBV                  (SBV, Symbolic)
import qualified Data.SBV                  as SBV
import qualified Data.SBV.Control          as SBV
import qualified Data.SBV.Internals        as SBVI
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable          (for)
import           Prelude                   hiding (exp)

import           Pact.Typechecker          (typecheckTopLevel)
import           Pact.Types.Lang           (Parsed, eParsed, mMetas, renderInfo,
                                            renderParsed, tMeta, _iInfo)
import           Pact.Types.Runtime        (Exp, ModuleData, ModuleName,
                                            Ref (Ref),
                                            Term (TDef, TSchema, TTable),
                                            asString, tInfo, tShow)
import qualified Pact.Types.Runtime        as Pact
import           Pact.Types.Typecheck      (AST, Fun (FDefun, _fInfo),
                                            Named (_nnName, _nnNamed), Node,
                                            TcId (_tiInfo),
                                            TopLevel (TopFun, TopTable),
                                            UserType (_utFields, _utName),
                                            runTC, tcFailures)
import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Analyze      hiding (invariants, model)
import           Pact.Analyze.Parse        (expToCheck, expToInvariant)
import           Pact.Analyze.Term
import           Pact.Analyze.Translate
import           Pact.Analyze.Types

data CheckSuccess
  = SatisfiedProperty SBVI.SMTModel
  | ProvedTheorem
  deriving Show

data CheckFailure
  = Invalid SBVI.SMTModel
  | Unsatisfiable
  | Unknown String

  | TypecheckFailure (Set TC.Failure)
  | AnalyzeFailure AnalyzeFailure
  | TranslateFailure TranslateFailure
  | PropertyParseError Exp

  | CodeCompilationFailed String
  deriving Show

describeCheckFailure :: Parsed -> CheckFailure -> Text
describeCheckFailure parsed failure =
  case failure of
    TypecheckFailure fails -> T.unlines $ map
      (\(TC.Failure ti s) -> T.pack (renderInfo (_tiInfo ti) ++ ":Warning: " ++ s))
      (Set.toList fails)

    _ ->
      let str = case failure of
            Invalid model -> "Invalidating model found: " <> showModel model
            Unsatisfiable  -> "This property is unsatisfiable"
            Unknown reason ->
              "The solver returned unknown with reason:\n" <>
              tShow reason
            AnalyzeFailure err        -> describeAnalyzeFailure err
            TranslateFailure err      -> describeTranslateFailure err
            PropertyParseError expr   -> "Couldn't parse property: " <> tShow expr
            CodeCompilationFailed msg -> T.pack msg
            TypecheckFailure _        -> error "impossible (handled above)"
      in T.pack (renderParsed parsed) <> ":Warning: " <> str

showModel :: SBVI.SMTModel -> Text
showModel model = T.intercalate ", " $ map
  (\(name, cw) -> T.pack name <> " = " <> tShow cw)
  (SBVI.modelAssocs model)

describeCheckSuccess :: CheckSuccess -> Text
describeCheckSuccess = \case
  SatisfiedProperty model -> "Property satisfied with model: " <> showModel model
  ProvedTheorem           -> "Property proven valid"

type CheckResult = Either (Parsed, CheckFailure) CheckSuccess

describeCheckResult :: CheckResult -> Text
describeCheckResult = either (uncurry describeCheckFailure) describeCheckSuccess

--
-- TODO: implement machine-friendly JSON output for CheckResult
--

dummyParsed :: Parsed
dummyParsed = Default.def

resultQuery
  :: Parsed                -- ^ parse information for error context
  -> Goal                  -- ^ are we in sat or valid mode?
  -> Map UniqueId AVal     -- ^ args map
  -> SBV.Query CheckResult
resultQuery parsed goal _argsMap = do
  satResult <- SBV.checkSat
  case goal of
    Validation ->
      case satResult of
        SBV.Sat   -> Left . (parsed,) . Invalid <$> SBV.getModel
        SBV.Unsat -> pure $ Right ProvedTheorem
        SBV.Unk   -> Left . (parsed,) . Unknown <$> SBV.getUnknownReason

    Satisfaction ->
      case satResult of
        SBV.Sat   -> Right . SatisfiedProperty <$> SBV.getModel
        SBV.Unsat -> pure $ Left (parsed, Unsatisfiable)
        SBV.Unk   -> Left . (parsed,) . Unknown <$> SBV.getUnknownReason

-- NOTE: this was adapted from SBV's internal @runWithQuery@ to be more
--       flexible for our needs.
runAndQuery
  :: forall env r
   . Goal                                                -- ^ are we using sat or valid?
  -> ExceptT AnalyzeFailure Symbolic env                 -- ^ initial setup in Symbolic
  -> (env -> ExceptT AnalyzeFailure Symbolic (SBV Bool)) -- ^ produces the Provable to be run
  -> (env -> SBV.Query r)                                -- ^ produces the Query to be run
  -> IO (Either AnalyzeFailure r)
runAndQuery goal mkEnv mkProvable mkQuery = fst <$>
    SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup (goal == Satisfaction) cfg) comp

  where
    cfg :: SBV.SMTConfig
    cfg = SBV.z3

    comp :: Symbolic (Either AnalyzeFailure r)
    comp = runExceptT $ do
      env <- mkEnv
      void $ lift . SBVI.output =<< mkProvable env
      lift $ SBV.query $ mkQuery env

-- Note: @length args@ unique names are already generated by
-- @checkTopFunction@, so we start the name generator at the next index.
checkFunctionBody
  :: [Table]
  -> [Arg]
  -> [AST Node]
  -> (Parsed, Check)
  -> IO CheckResult
checkFunctionBody tables args body (parsed, check) =
  case runExcept
    (runGenTFrom
      (UniqueId (length args))
      (runReaderT
        (unTranslateM (translateBody body))
        (mkTranslateEnv args))) of
    Left reason ->
      pure $ Left (parsed, TranslateFailure reason)

    Right tm -> do
      let runAnalysis
            :: Analyze AVal
            -> Map UniqueId AVal
            -> ExceptT AnalyzeFailure Symbolic (SBV Bool)
          runAnalysis act argMap = do
            let aEnv   = mkAnalyzeEnv argMap tables
                state0 = mkInitialAnalyzeState tables

            (propResult, state1, constraints) <- hoist generalize $
              runRWST (runAnalyze act) aEnv state0

            let qEnv  = mkQueryEnv aEnv state1 propResult
                query = analyzeCheck check

            lift $ runConstraints constraints
            _sSbv <$> runReaderT (queryAction query) qEnv

          goal :: Goal
          goal = checkGoal check

          analysis :: Analyze AVal
          analysis = case tm of
            ETerm   body' _ -> mkAVal <$> analyzeTerm  body'
            EObject body' _ -> AnObj  <$> analyzeTermO body'

      checkResult <- runAndQuery
        goal
        (allocateArgMap args)
        (runAnalysis analysis)
        (resultQuery parsed goal)

      --
      -- TODO: at this point, if our result is Invalid, we should feed our
      --       resulting Model back into the symbolic evaluator to produce an
      --       execution trace (everything should be concrete).
      --
      --       during this second run, while producing the trace, we should
      --       log effects (DB writes) in the order that they occur. for
      --       writes, we actually don't have to use the tagging technique
      --       because everything is concrete! Output will be composed of
      --       Trace and the result AVal; unclear at the moment whether the
      --       trace should contain intermediate values and effects
      --       interleaved, or whether these two should be separate.
      --

      pure $ case checkResult of
        Left af -> Left (parsed, AnalyzeFailure af)
        Right r -> r

--
-- TODO: probably inline this function into 'verifyFunction'
--
checkTopFunction :: [Table] -> TopLevel Node -> (Parsed, Check) -> IO CheckResult
checkTopFunction tables (TopFun (FDefun _ _ _ args body) _) parsedCheck =
  let nodes :: [Node]
      nodes = _nnNamed <$> args

      -- Extract the plain/unmunged names from the source code. We use the
      -- munged names for let/bind/with-read/etc -bound variables, but plain
      -- names for the args for usability. Because let/bind/etc can't shadow
      -- these unmunged names, we retain our SSA property.
      names :: [Text]
      names = _nnName <$> args

      uids = UniqueId <$> [0..]

      args' :: [Arg]
      args' = zip3 names uids nodes

  in checkFunctionBody tables args' body parsedCheck

checkTopFunction _ _ (parsed, _) = pure $ Left (parsed, CodeCompilationFailed "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)")

moduleTables
  :: HM.HashMap ModuleName ModuleData -- ^ all loaded modules
  -> ModuleData                       -- ^ the module we're verifying
  -> IO [Table]
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
      <- runTC 0 False $ typecheckTopLevel (Ref tab)

    let schemaName = asString (_utName schema)

        invariants = schemas ^? ix schemaName.tMeta._Just.mMetas.ix "invariants"
        invariant  = schemas ^? ix schemaName.tMeta._Just.mMetas.ix "invariant"

        exps :: [Exp]
        exps = case invariants of
          Just (Pact.ELitList x) -> x
          -- TODO(joel): don't just throw an error
          Just _ -> error "invariants must be a list"
          Nothing -> case invariant of
            Just exp -> [exp]
            Nothing  -> []

        parsedList = exps <&> \meta ->
          case runReaderT (expToInvariant TBool meta) (_utFields schema) of
            Left err   -> Left (meta, err)
            Right good -> Right good
        failures = lefts parsedList

        invariants' :: [Invariant Bool]
        invariants' = if null failures
          then rights parsedList
          -- TODO(joel): don't just throw an error
          else error ("failed parse of " ++ show failures)

    pure $ Table tabName schema invariants'

getInfoParsed :: Pact.Info -> Parsed
getInfoParsed info = case _iInfo info of
  Nothing               -> dummyParsed
  Just (_code, parsed') -> parsed'

-- Get the set (HashMap) of refs to functions in this module.
moduleFunRefs :: ModuleData -> HM.HashMap Text Ref
moduleFunRefs (_mod, modRefs) = flip HM.filter modRefs $ \case
  Ref (TDef {}) -> True
  _             -> False

moduleFunChecks
  :: HM.HashMap Text (Ref, Pact.FunType TC.UserType)
  -> HM.HashMap Text (Ref, [(Parsed, Check)])
moduleFunChecks modTys = modTys <&> \(ref@(Ref defn), Pact.FunType argTys _) ->
  -- TODO(joel): right now we can get away with ignoring the result type but we
  -- should use it for type checking

  let properties = defn ^? tMeta . _Just . mMetas . ix "properties"
      property   = defn ^? tMeta . _Just . mMetas . ix "property"
      parsed     = getInfoParsed (defn ^. tInfo)

      uids = UniqueId <$> [0..]

      -- TODO(joel): we probably don't want mapMaybe here and instead should
      -- fail harder if we can't make sense of a type
      --
      -- TODO(joel): this relies on generating the same unique ids as
      -- @checkTopFunction@. We need to more carefully enforce this is true!
      env :: [(Text, UniqueId, EType)]
      env = fmap (\(uid, (text, ty)) -> (text, uid, ty))
        $ zip uids
        $ flip mapMaybe argTys $ \(Pact.Arg name ty _info) ->
            case translateType' ty of
              Just ety -> Just (name, ety)
              Nothing  -> Nothing

      nameEnv :: Map Text UniqueId
      nameEnv = Map.fromList $ fmap (\(name, uid, _) -> (name, uid)) env

      idEnv :: Map UniqueId EType
      idEnv = Map.fromList $ fmap (\(_, uid, ty) -> (uid, ty)) env

      uidStart = UniqueId (length env)

      exps :: [Exp]
      exps = case properties of
        Just (Pact.ELitList exps') -> exps'
        -- TODO(joel): don't just throw an error
        Just _ -> error "properties must be a list"
        Nothing -> case property of
          Just exp -> [exp]
          Nothing  -> []

      parsedList :: [Either Exp (Parsed, Check)]
      parsedList = exps <&> \meta ->
        case expToCheck uidStart nameEnv idEnv meta of
          Nothing   -> Left meta
          Just good -> Right (meta ^. eParsed, good)
      failures = lefts parsedList

      checks :: [(Parsed, Check)]
      checks = if null failures
        then (parsed, InvariantsHold) : rights parsedList
        -- TODO(joel): don't just throw an error
        else error ("failed parse of " ++ show failures)

  in (ref, checks)

verifyFunction :: [Table] -> Ref -> [(Parsed, Check)] -> IO [CheckResult]
verifyFunction tables ref props = do
  (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures

  case fun of
    TopFun (FDefun {_fInfo}) _ ->
      if Set.null failures
      then traverse (checkTopFunction tables fun) props
      else pure [Left (getInfoParsed _fInfo, TypecheckFailure failures)]
    _ -> pure []

-- | Verifies properties on all functions, and that each function maintains all
-- invariants.
verifyModule
  :: HM.HashMap ModuleName ModuleData   -- ^ all loaded modules
  -> ModuleData                         -- ^ the module we're verifying
  -> IO (HM.HashMap Text [CheckResult])
verifyModule modules moduleData = do
  tables <- moduleTables modules moduleData

  let funRefs :: HM.HashMap Text Ref
      funRefs = moduleFunRefs moduleData

  -- For each ref, if it typechecks as a function (which it should), keep its
  -- signature.
  --
  (funTypes :: HM.HashMap Text (Ref, Pact.FunType TC.UserType)) <- ifoldrM
    (\name ref accum -> do
      maybeFun <- runTC 0 False $ typecheckTopLevel ref
      pure $ case maybeFun of
        (TopFun (FDefun _info _name funType _args _body) _meta, _tcState)
          -> HM.insert name (ref, funType) accum
        _ -> accum
    )
    HM.empty
    funRefs

  let funChecks :: HM.HashMap Text (Ref, [(Parsed, Check)])
      funChecks = moduleFunChecks funTypes
      verifyFun = uncurry (verifyFunction tables)

  traverse verifyFun funChecks

moduleFun :: ModuleData -> Text -> Maybe Ref
moduleFun (_mod, modRefs) name = name `HM.lookup` modRefs

-- | Verifies a one-off 'Check' for a function.
verifyCheck
  :: ModuleData                       -- ^ the module we're verifying
  -> Text                             -- ^ the name of the function
  -> Check                            -- ^ the check we're running
  -> IO CheckResult
verifyCheck moduleData funName check = do
  let parsed = dummyParsed
      modules = HM.fromList [("test", moduleData)]
  tables <- moduleTables modules moduleData
  case moduleFun moduleData funName of
    Just funRef -> head <$> verifyFunction tables funRef [(parsed, check)]
    Nothing     -> pure $ Left
      ( parsed
      , CodeCompilationFailed ("function not found: " ++ T.unpack funName)
      )
