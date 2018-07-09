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
  , describeCheckFailure
  , describeCheckResult
  , describeParseFailure
  , showModel
  , CheckFailure(..)
  , CheckFailureNoLoc(..)
  , CheckSuccess(..)
  , CheckResult
  , ModuleResult(..)
  , SmtFailure(..)
  , ParseFailure
  ) where

import           Control.Exception         as E
import           Control.Lens              (Prism', imap, ifoldr, ifoldrM,
                                            itraversed, ix, toListOf,
                                            traverseOf, traversed,
                                            (<&>), (^.), (^?), (^@..), (?~),
                                            _1, _2, _Just)
import           Control.Monad             ((>=>), join, void)
import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT,
                                            throwError, withExcept, withExceptT)
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Either               (lefts, partitionEithers, rights)
import qualified Data.Foldable             as Foldable
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
import           Pact.Types.Lang           (Code(Code), Info(Info), eParsed,
                                            mMetas, mName, renderInfo,
                                            renderParsed, tMeta)
import           Pact.Types.Runtime        (Exp, ModuleData, ModuleName,
                                            Ref (Ref),
                                            Term (TDef, TSchema, TTable),
                                            asString, tShow)
import qualified Pact.Types.Runtime        as Pact
import           Pact.Types.Typecheck      (AST,
                                            Fun (FDefun, _fArgs, _fBody, _fInfo),
                                            Named, Node, TcId (_tiInfo),
                                            TopLevel (TopFun, TopTable,
                                            _tlInfo), UserType (_utFields,
                                            _utName), runTC, tcFailures)
import qualified Pact.Types.Typecheck      as TC

import           Pact.Analyze.Analyze      hiding (invariants)
import           Pact.Analyze.Parse        (expToCheck, expToInvariant)
import           Pact.Analyze.Term         (ETerm (EObject, ETerm))
import           Pact.Analyze.Translate
import           Pact.Analyze.Types
import           Pact.Analyze.Util

data CheckSuccess
  = SatisfiedProperty Model
  | ProvedTheorem
  deriving (Eq, Show)

type ParseFailure = (Exp, String)

data SmtFailure
  = Invalid Model
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

data ModuleResult
  = ModuleParseFailures [ParseFailure]
  | ModuleCheckFailure CheckFailure
  | ModuleChecks
    { propertyChecks  :: HM.HashMap Text [CheckResult]
    , invariantChecks :: HM.HashMap Text (TableMap [CheckResult])
    }
  deriving (Eq, Show)

describeCheckSuccess :: CheckSuccess -> Text
describeCheckSuccess = \case
  SatisfiedProperty model -> "Property satisfied with model:\n"
                          <> showModel model
  ProvedTheorem           -> "Property proven valid"

describeParseFailure :: ParseFailure -> Text
describeParseFailure (exp, info)
  = T.pack (renderParsed (exp ^. eParsed))
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

-- TODO: don't throw out these Infos
translateToCheckFailure :: TranslateFailure -> CheckFailure
translateToCheckFailure (TranslateFailure info err)
  = CheckFailure info (TranslateFailure' err)

analyzeToCheckFailure :: AnalyzeFailure -> CheckFailure
analyzeToCheckFailure (AnalyzeFailure info err)
  = CheckFailure info (AnalyzeFailure' err)

smtToCheckFailure :: Info -> SmtFailure -> CheckFailure
smtToCheckFailure info = CheckFailure info . SmtFailure

-- NOTE: we indent the entire model two spaces so that the atom linter will
-- treat it as one message.
showModel :: Model -> Text
showModel (Model (ModelTags args vars reads' writes auths res) ksProvs) =
    T.intercalate "\n" $ T.intercalate "\n" . map indent <$>
      [ ["Arguments:"]
      , indent <$> fmapToList showVar args
      , []
      , ["Variables:"]
      , indent <$> fmapToList showVar vars
      , []
      , ["Reads:"]
      , indent <$> fmapToList showAccess reads'
      , []
      , ["Writes:"]
      , indent <$> fmapToList showAccess writes
      , []
      , ["Keysets:"]
      , indent <$> imapToList showAuth auths
      , []
      , ["Result:"]
      , indent <$> [showResult]
      ]

  where
    fmapToList f xs = Foldable.toList $ fmap f xs
    imapToList f xs = Foldable.toList $ imap f xs

    indent :: Text -> Text
    indent = ("  " <>)

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

    showKsn :: S KeySetName -> Text
    showKsn sKsn = case SBV.unliteral (_sSbv sKsn) of
      Nothing -> "[unknown]"
      Just (KeySetName ksn) -> "'" <> ksn

    showAuth :: TagId -> Located (SBV Bool) -> Text
    showAuth tid lsb = status <> ksDescription
      where
        status = case SBV.unliteral (_located lsb) of
          Nothing    -> "unknown:      "
          Just True  -> "authorized:   "
          Just False -> "unauthorized: "

        ksDescription = case tid `Map.lookup` ksProvs of
          Nothing ->
            "unknown keyset"
          Just (FromCell (OriginatingCell (TableName tn) (ColumnName cn) sRk _dirty)) ->
            "database keyset at ("
              <> T.pack tn <> ", "
              <> "'" <> T.pack cn <> ", "
              <> showS sRk <> ")"
          Just (FromNamedKs sKsn) ->
            "named keyset " <> showKsn sKsn
          Just (FromInput arg) ->
            "argument " <> arg

    showResult :: Text
    showResult = showTVal $ _located res

--
-- TODO: implement machine-friendly JSON output for CheckResult
--

allocModelTags :: Info -> [Arg] -> ETerm -> [TagAllocation] -> Symbolic ModelTags
allocModelTags funInfo args tm tagAllocs = ModelTags
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
      av <- alloc ety <&> _AVal._1 ?~ FromInput nm
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

-- | Builds a new 'Model' by querying the SMT model to concretize the provided
-- symbolic 'Model'.
saturateModel :: Model -> SBV.Query Model
saturateModel =
    traverseOf (modelTags.mtArgs.traversed.located._2) fetchTVal   >=>
    traverseOf (modelTags.mtVars.traversed.located._2) fetchTVal   >=>
    traverseOf (modelTags.mtReads.traversed.located)   fetchAccess >=>
    traverseOf (modelTags.mtWrites.traversed.located)  fetchAccess >=>
    traverseOf (modelTags.mtAuths.traversed.located)   fetchSbv    >=>
    traverseOf (modelTags.mtResult.located)            fetchTVal   >=>
    traverseOf (modelKsProvs.traversed)                fetchProv

  where
    fetchTVal :: TVal -> SBV.Query TVal
    fetchTVal (ety, av) = (ety,) <$> go ety av
      where
        go :: EType -> AVal -> SBV.Query AVal
        go (EType (_ :: Type t)) (AVal _mProv sval) = mkAVal' . SBV.literal <$>
          SBV.getValue (SBVI.SBV sval :: SBV t)
        go (EObjectTy _) (AnObj obj) = AnObj <$> fetchObject obj
        go _ _ = error "fetchTVal: impossible"

    -- NOTE: This currently rebuilds an SBV. Not sure if necessary.
    fetchSbv :: (SymWord a, SBV.SMTValue a) => SBV a -> SBV.Query (SBV a)
    fetchSbv = fmap SBV.literal . SBV.getValue

    fetchS :: (SymWord a, SBV.SMTValue a) => S a -> SBV.Query (S a)
    fetchS = traverseOf s2Sbv fetchSbv

    fetchObject :: Object -> SBVI.Query Object
    fetchObject (Object fields) = Object <$> traverse fetchTVal fields

    fetchAccess :: (S RowKey, Object) -> SBV.Query (S RowKey, Object)
    fetchAccess (sRk, obj) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      pure (sRk', obj')

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromNamedKs         fetchS

resultQuery
  :: Goal                                      -- ^ are we in sat or valid mode?
  -> Model                                     -- ^ unsaturated/symbolic model
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

verifyFunctionInvariants'
  :: Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants' funInfo tables pactArgs body = runExceptT $ do
    (args, tm, tagAllocs) <- hoist generalize $
      withExcept translateToCheckFailure $ runTranslation funInfo pactArgs body

    ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
      tags <- lift $ allocModelTags funInfo args tm tagAllocs
      resultsTable <- withExceptT analyzeToCheckFailure $
        runInvariantAnalysis tables tm tags funInfo

      -- Iterate through each invariant in a single query so we can reuse our
      -- assertion stack.
      ExceptT $ fmap Right $
        SBV.query $
          for2 resultsTable $ \(Located info (AnalysisResult prop ksProvs)) -> do
            queryResult <- runExceptT $
              inNewAssertionStack $ do
                void $ lift $ SBV.constrain $ SBV.bnot prop
                resultQuery goal $ Model tags ksProvs

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

    -- SBV also provides 'inNewAssertionStack', but in 'Query'
    inNewAssertionStack
      :: ExceptT a SBV.Query CheckSuccess
      -> ExceptT a SBV.Query CheckSuccess
    inNewAssertionStack act = do
      lift $ SBV.push 1
      result <- act
      lift $ SBV.pop 1
      pure result

    -- Discharges impure 'SMTException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SMTException) ->
      pure $ Left $ CheckFailure funInfo $ SmtFailure $ UnexpectedFailure e

    runSymbolic :: Symbolic a -> IO a
    runSymbolic = fmap fst .
      -- SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup (goal == Satisfaction) config)
      SBVI.runSymbolic (SBVI.SMTMode SBVI.ISetup True config)

verifyFunctionProperty
  :: Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> Located Check
  -> IO (Either CheckFailure CheckSuccess)
verifyFunctionProperty funInfo tables pactArgs body (Located propInfo check) =
    runExceptT $ do
      (args, tm, tagAllocs) <- hoist generalize $
        withExcept translateToCheckFailure $
          runTranslation funInfo pactArgs body
      ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
        tags <- lift $ allocModelTags funInfo args tm tagAllocs
        AnalysisResult prop ksProvs <- withExceptT analyzeToCheckFailure $
          runPropertyAnalysis check tables tm tags funInfo
        void $ lift $ SBV.output prop
        hoist SBV.query $ withExceptT (smtToCheckFailure propInfo) $
          resultQuery goal $ Model tags ksProvs

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
  -> IO (Either [ParseFailure] [Table])
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

  eitherTables <- for tables $ \(tabName, tab) -> do
      (TopTable _info _name (Pact.TyUser schema) _meta, _tcState)
        <- runTC 0 False $ typecheckTopLevel (Ref tab)

      let TC.Schema{_utName,_utFields} = schema
          schemaName = asString _utName

          invariants = schemas ^? ix schemaName.tMeta._Just.mMetas.ix "invariants"
          invariant  = schemas ^? ix schemaName.tMeta._Just.mMetas.ix "invariant"

          invInfo = runExpParserOver
            "invariants" invariants invariant $
            \meta -> runReaderT (expToInvariant TBool meta) _utFields

      pure $ Table tabName schema <$> invInfo

  let (failures, tables') = partitionEithers eitherTables
  pure $ if null failures then Right tables' else Left (concat failures)

-- Get the set (HashMap) of refs to functions in this module.
moduleFunRefs :: ModuleData -> HM.HashMap Text Ref
moduleFunRefs (_mod, modRefs) = flip HM.filter modRefs $ \case
  Ref (TDef {}) -> True
  _             -> False

moduleFunChecks
  :: [Table]
  -> HM.HashMap Text (Ref, Pact.FunType TC.UserType)
  -> HM.HashMap Text (Ref, Either [ParseFailure] [Located Check])
moduleFunChecks tables modTys = modTys <&> \(ref@(Ref defn), Pact.FunType argTys _) ->
  -- TODO(joel): right now we can get away with ignoring the result type but we
  -- should use it for type checking

  let properties = defn ^? tMeta . _Just . mMetas . ix "properties"
      property   = defn ^? tMeta . _Just . mMetas . ix "property"

      -- TODO: Ideally we wouldn't have any ad-hoc VID generation, but we're
      --       not there yet:
      vids = VarId <$> [0..]

      -- TODO(joel): we probably don't want mapMaybe here and instead should
      -- fail harder if we can't make sense of a type

      -- TODO(joel): this relies on generating the same unique ids as
      -- @verifyFunctionProperty@. We need to more carefully enforce this is
      -- true!
      env :: [(Text, VarId, EType)]
      env = fmap (\(vid, (text, ty)) -> (text, vid, ty))
        $ zip vids
        $ flip mapMaybe argTys $ \(Pact.Arg name ty _info) ->
            case maybeTranslateType ty of
              Just ety -> Just (name, ety)
              Nothing  -> Nothing

      nameEnv :: Map Text VarId
      nameEnv = Map.fromList $ fmap (\(name, vid, _) -> (name, vid)) env

      idEnv :: Map VarId EType
      idEnv = Map.fromList $ fmap (\(_, vid, ty) -> (vid, ty)) env

      vidStart = VarId (length env)

      tableEnv = TableMap $ Map.fromList $
        tables <&> \Table { _tableName, _tableType } ->
          let fields = _utFields _tableType
              colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
                \(Pact.Arg argName ty _) ->
                  (ColumnName (T.unpack argName),) <$> maybeTranslateType ty
          in (TableName (T.unpack _tableName), colMap)

      checks :: Either [ParseFailure] [Located Check]
      checks = runExpParserOver "properties" properties property
        (expToCheck tableEnv vidStart nameEnv idEnv)

  in (ref, checks)

-- | For both properties and invariants you're allowed to use either the
-- singular ("property") or plural ("properties") name. This helper just
-- collects the properties / invariants in a list.
collectExps :: String -> Maybe Exp -> Maybe Exp -> Either [(Exp, String)] [Exp]
collectExps name multiExp singularExp = case multiExp of
  Just (Pact.ELitList exps') -> Right exps'
  Just exp -> Left [(exp, name ++ " must be a list")]
  Nothing -> case singularExp of
    Just exp -> Right [exp]
    Nothing  -> Right []

expToInfo :: Exp -> Info
expToInfo exp = Info (Just (Code (tShow exp), exp ^. eParsed))

-- | This runs a parser over a collection of 'Exp's, collecting the failures
-- or successes.
runExpParserOver
  :: forall t.
     String
  -> Maybe Exp
  -> Maybe Exp
  -> (Exp -> Either String t)
  -> Either [ParseFailure] [Located t]
runExpParserOver name multiExp singularExp parser =
  let
      exps = collectExps name multiExp singularExp
      parsedList :: Either [ParseFailure] [Either [ParseFailure] (Located t)]
      parsedList = exps <&&> \meta -> case parser meta of
        Left err   -> Left [(meta, err)]
        Right good -> Right (Located (expToInfo meta) good)

      parsedList' :: [Either [ParseFailure] (Located t)]
      parsedList' = join <$> sequence parsedList

      failures :: [ParseFailure]
      failures = join $ lefts parsedList'

  in if null failures then Right (rights parsedList') else Left failures

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
    other -> pure $ Left $ CheckFailure (_tlInfo other) (NotAFunction (tShow ref))

-- | Verifies properties on all functions, and that each function maintains all
-- invariants.
verifyModule
  :: HM.HashMap ModuleName ModuleData   -- ^ all loaded modules
  -> ModuleData                         -- ^ the module we're verifying
  -> IO ModuleResult
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

  runExceptT' $ do
    tables' <- case tables of
      Left errs -> throwError (ModuleParseFailures errs)
      Right tables' -> pure tables'

    let funChecks :: HM.HashMap Text (Ref, Either [ParseFailure] [Located Check])
        funChecks = moduleFunChecks tables' funTypes

        funChecks' :: Either [ParseFailure] (HM.HashMap Text (Ref, [Located Check]))
        funChecks' = sequence (fmap sequence funChecks)

        verifyFunProps :: (Ref, [Located Check]) -> IO [CheckResult]
        verifyFunProps = uncurry (verifyFunctionProps tables')

    funChecks'' <- case funChecks' of
      Left errs         -> throwError (ModuleParseFailures errs)
      Right funChecks'' -> pure funChecks''

    funChecks''' <- lift $ traverse verifyFunProps funChecks''
    invariantChecks <- for funRefs $ \ref -> do
      tabledResults <- lift $ verifyFunctionInvariants tables' ref
      case tabledResults of
        Left err      -> throwError $ ModuleCheckFailure err
        Right results -> pure results

    pure $ ModuleChecks funChecks''' invariantChecks

runExceptT' :: Functor m => ExceptT a m a -> m a
runExceptT' = fmap (either id id) . runExceptT

-- | Verifies a one-off 'Check' for a function.
verifyCheck
  :: ModuleData     -- ^ the module we're verifying
  -> Text           -- ^ the name of the function
  -> Check          -- ^ the check we're running
  -> IO (Either [ParseFailure] CheckResult)
verifyCheck moduleData funName check = do
    let info       = dummyInfo
        moduleName = moduleData ^. _1.mName
        modules    = HM.fromList [(moduleName, moduleData)]

    tables <- moduleTables modules moduleData
    case tables of
      Left failures -> pure $ Left failures
      Right tables' -> Right <$>
        case moduleFun moduleData funName of
          Just funRef -> head <$> verifyFunctionProps tables' funRef [Located info check]
          Nothing     -> pure $ Left $ CheckFailure info $ NotAFunction funName

  where
    moduleFun :: ModuleData -> Text -> Maybe Ref
    moduleFun (_mod, modRefs) name = name `HM.lookup` modRefs
