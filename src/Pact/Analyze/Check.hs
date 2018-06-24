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

import           Control.Exception         as E
import           Control.Lens              (Prism', ifoldMap, ifoldr, ifoldrM,
                                            itraversed, ix, toListOf,
                                            traverseOf, traversed, (<&>), (^.),
                                            (^?), (^@..), _1, _2, _Just)
import           Control.Monad             ((>=>), void)
import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT,
                                            throwError, withExcept, withExceptT)
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Bifunctor            (first)
import qualified Data.Default              as Default
import           Data.Either               (lefts, rights)
import qualified Data.HashMap.Strict       as HM
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Monoid               ((<>))
import           Data.SBV                  (SBV, SymWord, Symbolic)
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
import           Pact.Types.Lang           (Parsed, eParsed, mMetas, mName,
                                            renderInfo, renderParsed, tMeta,
                                            _iInfo)
import           Pact.Types.Runtime        (Exp, ModuleData, ModuleName,
                                            Ref (Ref),
                                            Term (TDef, TSchema, TTable),
                                            asString, tInfo, tShow)
import qualified Pact.Types.Runtime        as Pact
import           Pact.Types.Typecheck      (AST,
                                            Fun (FDefun, _fArgs, _fBody, _fInfo),
                                            Named, Node, TcId (_tiInfo),
                                            TopLevel (TopFun, TopTable),
                                            UserType (_utFields, _utName),
                                            runTC, tcFailures)
import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Analyze      hiding (invariants)
import           Pact.Analyze.Parse        (expToCheck, expToInvariant)
import           Pact.Analyze.Term         (ETerm (EObject, ETerm))
import           Pact.Analyze.Translate
import           Pact.Analyze.Types

data CheckSuccess
  = SatisfiedProperty Model
  | ProvedTheorem
  deriving Show

data SmtFailure
  = Invalid Model
  | Unsatisfiable
  | Unknown SBV.SMTReasonUnknown
  | UnexpectedFailure SBV.SMTException
  deriving Show

data CheckFailure
  = NotAFunction Text
  | TypecheckFailure (Set TC.Failure)
  | TranslateFailure TranslateFailure
  | AnalyzeFailure AnalyzeFailure
  | SmtFailure SmtFailure
  deriving Show

describeSmtFailure :: SmtFailure -> Text
describeSmtFailure = \case
  Invalid model -> "Invalidating model found:\n" <> showModel model
  Unsatisfiable  -> "This property is unsatisfiable"
  Unknown reason -> "The solver returned 'unknown':\n" <> tShow reason
  UnexpectedFailure smtE -> T.pack $ show smtE

describeCheckFailure :: Parsed -> CheckFailure -> Text
describeCheckFailure parsed failure =
  case failure of
    TypecheckFailure fails -> T.unlines $ map
      (\(TC.Failure ti s) -> T.pack (renderInfo (_tiInfo ti) ++ ":Warning: " ++ s))
      (Set.toList fails)

    _ ->
      let str = case failure of
            NotAFunction name    -> "No function named " <> name
            TypecheckFailure _   -> error "impossible (handled above)"
            TranslateFailure err -> describeTranslateFailure err
            AnalyzeFailure err   -> describeAnalyzeFailure err
            SmtFailure err       -> describeSmtFailure err
      in T.pack (renderParsed parsed) <> ":Warning: " <> str

showModel :: Model -> Text
showModel (Model args vars reads' writes auths res) = T.intercalate "\n"
    [ "Arguments:"
    , ifoldMap (showVarItem showVar) args
    , "Variables:"
    , ifoldMap (showVarItem showVar) vars
    , "Reads:"
    , ifoldMap (showTaggedItem showAccess) reads'
    , "Writes:"
    , ifoldMap (showTaggedItem showAccess) writes
    , "Authorizations:"
    , ifoldMap (showTaggedItem showAuth) auths
    , "Result:"
    , "  " <> showResult
    ]

  where
    showVarItem :: (a -> Text) -> VarId -> a -> Text
    showVarItem show' (VarId i) var = "  (" <> tShow i <> ") " <> show' var <> "\n"

    showTaggedItem :: (a -> Text) -> TagId -> a -> Text
    showTaggedItem show' (TagId i) var = "  [" <> tShow i <> "] " <> show' var <> "\n"

    showSbv :: (Show a, SymWord a) => SBV a -> Text
    showSbv sbv = fromMaybe "[symbolic]" $ tShow <$> SBV.unliteral sbv

    showS :: (Show a, SymWord a) => S a -> Text
    showS = showSbv . _sSbv

    showTVal :: TVal -> Text
    showTVal (ety, av) = case av of
      OpaqueVal   -> "[opaque]"
      AnObj obj   -> showObject obj
      AVal _ sval -> case ety of
        EObjectTy _ -> error "showModel: impossible object type for AVal"
        EType (_ :: Type t) -> showSbv (SBVI.SBV sval :: SBV t)

    showObject :: Object -> Text
    showObject (Object m) = "{ "
      <> T.intercalate ", "
           (ifoldr (\key val acc -> showObjMapping key val : acc) [] m)
      <> " }"

    showObjMapping :: Text -> TVal -> Text
    showObjMapping key val = key <> ": " <> showTVal val

    showVar :: Located (Text, TVal) -> Text
    showVar (Located _ (nm, tval)) = nm <> " := " <> showTVal tval

    showAccess :: Located (S RowKey, Object) -> Text
    showAccess (Located _ (srk, obj)) = showS srk <> " => " <> showObject obj

    showAuth :: Located (SBV Bool) -> Text
    showAuth = showSbv . _located

    showResult :: Text
    showResult = showTVal $ _located res

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

mkEmptyModel :: Pact.Info -> [Arg] -> ETerm -> [TagAllocation] -> Symbolic Model
mkEmptyModel funInfo args tm tagAllocs = Model
    <$> allocateArgs
    <*> allocateVars
    <*> allocateReads
    <*> allocateWrites
    <*> allocateAuths
    <*> allocateResult

  where
    allocSchema :: Schema -> Symbolic Object
    allocSchema (Schema fieldTys) = Object <$>
      for fieldTys (\ety -> (ety,) <$> alloc ety)

    alloc :: EType -> Symbolic AVal
    alloc = \case
      EObjectTy schema -> AnObj <$> allocSchema schema
      EType (_ :: Type t) -> mkAVal . sansProv <$>
        (SBV.free_ :: Symbolic (SBV t))

    allocateArgs :: Symbolic (Map VarId (Located (Text, TVal)))
    allocateArgs = fmap Map.fromList $ for args $ \(nm, vid, node, ety) -> do
      let info = node ^. TC.aId . TC.tiInfo
      av <- alloc ety
      pure (vid, Located info (nm, (ety, av)))

    allocateVars :: Symbolic (Map VarId (Located (Text, TVal)))
    allocateVars = fmap Map.fromList $
      for (toListOf (traverse._AllocVarTag) tagAllocs) $
        \(Located info (vid, nm, ety)) ->
          alloc ety <&> \av -> (vid, Located info (nm, (ety, av)))

    allocRowKey :: Symbolic (S RowKey)
    allocRowKey = sansProv <$> SBV.free_

    allocAccesses
      :: Prism' TagAllocation (Located (TagId, Schema))
      -> Symbolic (Map TagId (Located (S RowKey, Object)))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) tagAllocs) $ \(Located info (tid, schema)) -> do
        srk <- allocRowKey
        obj <- allocSchema schema
        pure (tid, Located info (srk, obj))

    allocateReads :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocateReads = allocAccesses _AllocReadTag

    allocateWrites :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocateWrites = allocAccesses _AllocWriteTag

    allocateAuths :: Symbolic (Map TagId (Located (SBV Bool)))
    allocateAuths = fmap Map.fromList $
      for (toListOf (traverse._AllocAuthTag) tagAllocs) $ \(Located info tid) ->
        (tid,) . Located info <$> SBV.free_

    allocateResult :: Symbolic (Located TVal)
    allocateResult = Located funInfo <$> case tm of
      ETerm _ ty ->
        let ety = EType ty
        in (ety,) <$> alloc ety
      EObject _ sch ->
        (EObjectTy sch,) . AnObj <$> allocSchema sch

-- | Builds a new 'Model' by querying the SMT model to concretize the initial
-- 'Model'.
saturateModel :: Model -> SBV.Query Model
saturateModel =
    traverseOf (modelArgs.traversed.located._2) fetchTVal   >=>
    traverseOf (modelVars.traversed.located._2) fetchTVal   >=>
    traverseOf (modelReads.traversed.located)   fetchAccess >=>
    traverseOf (modelWrites.traversed.located)  fetchAccess >=>
    traverseOf (modelAuths.traversed.located)   fetchSbv    >=>
    traverseOf (modelResult.located)            fetchTVal

  where
    fetchTVal :: TVal -> SBVI.Query TVal
    fetchTVal (ety, av) = (ety,) <$> go ety av
      where
        go :: EType -> AVal -> SBVI.Query AVal
        go (EType (_ :: Type t)) (AVal _mProv sval) = mkAVal' . SBV.literal <$>
          SBV.getValue (SBVI.SBV sval :: SBV t)
        go (EObjectTy _) (AnObj obj) = AnObj <$> fetchObject obj
        go _ _ = error "fetchTVal: impossible"

    -- NOTE: This currently rebuilds an SBV. Not sure if necessary.
    fetchSbv :: (SymWord a, SBV.SMTValue a) => SBV a -> SBVI.Query (SBV a)
    fetchSbv = fmap SBV.literal . SBV.getValue

    fetchS :: (SymWord a, SBV.SMTValue a) => S a -> SBVI.Query (S a)
    fetchS = traverseOf s2Sbv fetchSbv

    fetchObject :: Object -> SBVI.Query Object
    fetchObject (Object fields) = Object <$> traverse fetchTVal fields

    fetchAccess :: (S RowKey, Object) -> SBVI.Query (S RowKey, Object)
    fetchAccess (sRk, obj) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      pure (sRk', obj')

resultQuery
  :: Goal                                      -- ^ are we in sat or valid mode?
  -> Model                                     -- ^ initial model
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

-- Adaptation of SBV's internal @runWithQuery@ for our needs.
runAndQuery
  :: forall env r
   . Goal                                              -- ^ are we using sat or valid?
  -> ExceptT CheckFailure Symbolic env                 -- ^ initial setup in Symbolic
  -> (env -> ExceptT CheckFailure Symbolic (SBV Bool)) -- ^ produces the Provable to be run
  -> (env -> ExceptT CheckFailure SBV.Query r)         -- ^ produces the Query to be run
  -> ExceptT CheckFailure IO r
runAndQuery goal mkEnv mkProvable mkQuery = ExceptT $
    runSymbolic comp `E.catch` \(e :: SBV.SMTException) ->
      pure $ Left $ SmtFailure $ UnexpectedFailure e

  where
    cfg :: SBV.SMTConfig
    cfg = SBV.z3

    runSymbolic :: Symbolic a -> IO a
    runSymbolic s = fst <$>
      SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup (goal == Satisfaction) cfg) s

    comp :: Symbolic (Either CheckFailure r)
    comp = runExceptT $ do
      env <- mkEnv
      void $ lift . SBVI.output =<< mkProvable env
      hoist SBV.query $ mkQuery env

checkFunction
  :: [Table]
  -> Pact.Info
  -> [Named Node]
  -> [AST Node]
  -> Check
  -> IO (Either CheckFailure CheckSuccess)
checkFunction tables info pactArgs body check = runExceptT $ do
  (args, tm, tagAllocs) <- hoist generalize $ withExcept TranslateFailure $
    runTranslation pactArgs body

  let goal = checkGoal check

  runAndQuery
    goal
    (lift $ mkEmptyModel info args tm tagAllocs)
    (withExceptT AnalyzeFailure . runAnalysis tables tm check)
    (withExceptT SmtFailure . resultQuery goal)

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

      -- TODO: Ideally we wouldn't have any ad-hoc VID generation, but we're
      --       not there yet:
      vids = VarId <$> [0..]

      -- TODO(joel): we probably don't want mapMaybe here and instead should
      -- fail harder if we can't make sense of a type
      --
      -- TODO(joel): this relies on generating the same unique ids as
      -- @checkFunction@. We need to more carefully enforce this is true!
      env :: [(Text, VarId, EType)]
      env = fmap (\(vid, (text, ty)) -> (text, vid, ty))
        $ zip vids
        $ flip mapMaybe argTys $ \(Pact.Arg name ty _info) ->
            case translateType' ty of
              Just ety -> Just (name, ety)
              Nothing  -> Nothing

      nameEnv :: Map Text VarId
      nameEnv = Map.fromList $ fmap (\(name, vid, _) -> (name, vid)) env

      idEnv :: Map VarId EType
      idEnv = Map.fromList $ fmap (\(_, vid, ty) -> (vid, ty)) env

      vidStart = VarId (length env)

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
        case expToCheck vidStart nameEnv idEnv meta of
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
    TopFun (FDefun {_fInfo, _fArgs, _fBody}) _ ->
      if Set.null failures
      then for props $ \(parsed, check) ->
        first (parsed,) <$> checkFunction tables _fInfo _fArgs _fBody check
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
    Nothing     -> pure $ Left (parsed, NotAFunction funName)

  where
    moduleFun :: ModuleData -> Text -> Maybe Ref
    moduleFun (_mod, modRefs) name = name `HM.lookup` modRefs
