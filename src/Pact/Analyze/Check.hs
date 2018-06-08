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

import           Control.Lens              (ifoldr, ifoldrM, itraversed, ix, traversed,
                                            (<&>), (^.), (^?), (^@..), _1, _2,
                                            _Just)
import           Control.Monad             (void)
import           Control.Monad.Except      (ExceptT, runExceptT, throwError,
                                            withExcept, withExceptT)
import           Control.Monad.Gen         (runGenTFrom)
import           Control.Monad.Morph       (MFunctor, generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.RWS.Strict  (RWST (..))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Bifunctor            (first)
import qualified Data.Default              as Default
import           Data.Either               (lefts, rights)
import qualified Data.HashMap.Strict       as HM
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, mapMaybe)
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
                                            renderParsed, tMeta, _iInfo, mName)
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

import           Pact.Analyze.Analyze      hiding (invariants)
import           Pact.Analyze.Parse        (expToCheck, expToInvariant)
import           Pact.Analyze.Term
import           Pact.Analyze.Translate
import           Pact.Analyze.Types

data CheckSuccess
  = SatisfiedProperty Model
  | ProvedTheorem
  deriving Show

data CheckFailure
  --
  -- TODO: possibly nest these 3 under a new "SmtFailure"
  --
  = Invalid Model
  | Unsatisfiable
  | Unknown String

  | TypecheckFailure (Set TC.Failure)
  | TranslateFailure TranslateFailure
  | AnalyzeFailure AnalyzeFailure
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
            Invalid model -> "Invalidating model found:\n" <> showModel model
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

showModel :: Model -> Text
showModel (Model args readObjs auths) = T.intercalate "\n"
    [ "Arguments:"
    , foldMap (\val -> "  " <> showVal val <> "\n") args
    , ""
    , "Reads:"
    , foldMap (\obj -> "  " <> showObject obj <> "\n") readObjs
    , ""
    , "Authorizations:"
    , foldMap (\auth -> "  " <> showAuth auth <> "\n") auths
    ]

  where
    showVal :: (EType, AVal) -> Text
    showVal (ety, av) = case av of
      OpaqueVal   -> "[opaque]"
      AnObj obj   -> showObject obj
      AVal _ sval -> case ety of
        EObjectTy _ -> error "showModel: impossible object type for AVal"
        EType (_ :: Type t) -> fromMaybe "[symbolic]" $
          tShow <$> SBV.unliteral (SBVI.SBV sval :: SBV t)

    showObject :: Object -> Text
    showObject (Object m) = "{ "
      <> T.intercalate ", "
           (ifoldr (\key val acc -> showObjMapping key val : acc) [] m)
      <> " }"

    showObjMapping :: Text -> (EType, AVal) -> Text
    showObjMapping key val = key <> ": " <> showVal val

    showAuth :: SBV Bool -> Text
    showAuth sbv = fromMaybe "[symbolic]" $ tShow <$> SBV.unliteral sbv

describeCheckSuccess :: CheckSuccess -> Text
describeCheckSuccess = \case
  SatisfiedProperty model -> "Property satisfied with model:\n"
                          <> showModel model
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
  :: Goal                                        -- ^ are we in sat or valid mode?
  -> Model                                       -- ^ initial model
  -> ExceptT CheckFailure SBV.Query CheckSuccess
resultQuery goal model0 = do
  satResult <- lift $ SBV.checkSat
  case goal of
    Validation ->
      case satResult of
        SBV.Sat   -> throwError . Invalid =<< lift buildEnv
        SBV.Unsat -> pure ProvedTheorem
        SBV.Unk   -> throwError . Unknown =<< lift SBV.getUnknownReason

    Satisfaction ->
      case satResult of
        SBV.Sat   -> SatisfiedProperty <$> lift buildEnv
        SBV.Unsat -> throwError Unsatisfiable
        SBV.Unk   -> throwError . Unknown =<< lift SBV.getUnknownReason

  where
    -- | Builds a new 'Model' by querying the SMT model to concretize the
    -- initial 'Model'.
    buildEnv :: SBV.Query Model
    buildEnv = Model
      <$> traverse fetch       (_modelArgs model0)
      <*> traverse fetchObject (_modelReads model0)
      <*> traverse fetchSBool  (_modelAuths model0)

    fetch :: (EType, AVal) -> SBVI.Query (EType, AVal)
    fetch (ety, av) = (ety,) <$> go ety av
      where
        go :: EType -> AVal -> SBVI.Query AVal
        go (EType (_ :: Type t)) (AVal _mProv sval) = mkAVal' . SBV.literal <$>
          SBV.getValue (SBVI.SBV sval :: SBV t)
        go (EObjectTy _) (AnObj obj) = AnObj <$> fetchObject obj
        go _ _ = error "fetch: impossible"

    fetchObject :: Object -> SBVI.Query Object
    fetchObject (Object fields) = Object <$> traverse fetch fields

    -- NOTE: This currently rebuilds an SBV. Not sure if necessary.
    fetchSBool :: SBV Bool -> SBVI.Query (SBV Bool)
    fetchSBool = fmap SBV.literal . SBV.getValue

-- NOTE: this was adapted from SBV's internal @runWithQuery@ to be more
--       flexible for our needs.
--
-- We need to do 'MonadTrans'/'MFunctor'/'hoist' footwork here because sbv does
-- not have something like @MonadSymbolic@ -- 'Symbolic' is a concrete
-- datatype, and methods on the 'SymWord' typeclass produce these concrete
-- 'Symbolic' computations.
--
runAndQuery
  :: forall env r t.
   ( MonadTrans t
   , MFunctor t
   , Monad (t Symbolic)
   )
  => Goal                           -- ^ are we using sat or valid?
  -> t Symbolic env                 -- ^ initial setup in Symbolic
  -> (env -> t Symbolic (SBV Bool)) -- ^ produces the Provable to be run
  -> (env -> t SBV.Query r)         -- ^ produces the Query to be run
  -> t IO r
runAndQuery goal mkEnv mkProvable mkQuery = hoist runSymbolic comp
  where
    cfg :: SBV.SMTConfig
    cfg = SBV.z3

    runSymbolic :: Symbolic a -> IO a
    runSymbolic s = fst <$>
      SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup (goal == Satisfaction) cfg) s

    comp :: t Symbolic r
    comp = do
      env <- mkEnv
      void $ lift . SBVI.output =<< mkProvable env
      hoist SBV.query $ mkQuery env

-- | Creates an initial, free, 'Model' for use when the client does not have a
-- provided 'Model' from a previous run.
mkEmptyModel :: [Arg] -> ETerm -> ExceptT CheckFailure Symbolic Model
mkEmptyModel args _tm = Model
    <$> allocateArgs
    --
    -- TODO: instead of using ETerm here, use Writer output from translation.
    --
    -- TODO: implement these:
    --
    <*> pure Map.empty
    <*> pure Map.empty

  where
    alloc :: EType -> Symbolic AVal
    alloc = \case
      EObjectTy (Schema fieldTys) -> AnObj . Object <$>
        for fieldTys (\ety -> (ety,) <$> alloc ety)
      EType (_ :: Type t) -> mkAVal . sansProv <$>
        (SBV.free_ :: Symbolic (SBV t))

    --
    -- TODO: consider whether we should possibly get the translated args from
    --       Writer output or the translation step generally, too. this would
    --       allow us to change this function to simply return @Symbolic Model@
    --
    allocateArgs :: ExceptT CheckFailure Symbolic [(EType, AVal)]
    allocateArgs = for args $ \(_nm, _uid, node) -> do
      (ety :: EType) <- withExceptT TranslateFailure $ translateType node
      lift $ (ety,) <$> alloc ety

-- Note: @length args@ unique names are already generated by
-- @checkTopFunction@, so we start the name generator at the next index.
checkFunctionBody
  :: [Table]
  -> [Arg]
  -> [AST Node]
  -> Check
  -> IO (Either CheckFailure CheckSuccess)
checkFunctionBody tables args body check = runExceptT $ do
    tm <- hoist generalize $ withExcept TranslateFailure $
      runGenTFrom
        (UniqueId (length args))
        (runReaderT (unTranslateM (translateBody body)) (mkTranslateEnv args))

    --
    -- TODO: use 'catchError' around the following and, if our result is
    --       Invalid, feed our resulting Model back into the symbolic evaluator
    --       to produce an execution trace (everything should be concrete).
    --
    --       during this second run, while producing the trace, we should
    --       log effects (DB writes) in the order that they occur. for
    --       writes, we actually don't have to use the tagging technique
    --       because everything is concrete! Output will be composed of
    --       Trace and the result AVal; unclear at the moment whether the
    --       trace should contain intermediate values and effects
    --       interleaved, or whether these two should be separate.
    --

    runAndQuery
      goal
      (mkEmptyModel args tm)
      (withExceptT AnalyzeFailure . runAnalysis (mkAnalysis tm))
      (resultQuery goal) -- TODO: possibly nest under new error using withExceptT

  where
    goal :: Goal
    goal = checkGoal check

    mkAnalysis :: ETerm -> Analyze AVal
    mkAnalysis = \case
      ETerm   body' _ -> mkAVal <$> analyzeTerm  body'
      EObject body' _ -> AnObj  <$> analyzeTermO body'

    runAnalysis
      :: Analyze AVal
      -> Model
      -> ExceptT AnalyzeFailure Symbolic (SBV Bool)
    runAnalysis act model = do
      let aEnv   = mkAnalyzeEnv tables args model
          state0 = mkInitialAnalyzeState tables

      (propResult, state1, constraints) <- hoist generalize $
        runRWST (runAnalyze act) aEnv state0

      let qEnv  = mkQueryEnv aEnv state1 propResult
          query = analyzeCheck check

      lift $ runConstraints constraints
      _sSbv <$> runReaderT (queryAction query) qEnv

--
-- TODO: probably inline this function into 'verifyFunction'
--
checkTopFunction
  :: [Table]
  -> TopLevel Node
  -> Check
  -> IO (Either CheckFailure CheckSuccess)
checkTopFunction tables (TopFun (FDefun _ _ _ args body) _) check =
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

  in checkFunctionBody tables args' body check

checkTopFunction _ _ _ = pure $ Left $ CodeCompilationFailed "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

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
      then for props $ \(parsed, check) ->
        first (parsed,) <$> checkTopFunction tables fun check
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
  :: ModuleData     -- ^ the module we're verifying
  -> Text           -- ^ the name of the function
  -> Check          -- ^ the check we're running
  -> IO CheckResult
verifyCheck moduleData funName check = do
  let parsed = dummyParsed
      moduleName = moduleData ^. _1.mName
      modules = HM.fromList [(moduleName, moduleData)]
  tables <- moduleTables modules moduleData
  case moduleFun moduleData funName of
    Just funRef -> head <$> verifyFunction tables funRef [(parsed, check)]
    Nothing     -> pure $ Left
      ( parsed
      , CodeCompilationFailed ("function not found: " ++ T.unpack funName)
      )
