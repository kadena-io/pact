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
  , renderVerifiedModule
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
import           Control.Lens              (at, each, ifoldrM, ifor,
                                            itraversed, ix, toListOf,
                                            traversed, view, (%~), (&), (<&>),
                                            (?~), (^.), (^?), (^@..), _1, _2,
                                            _Just, _Left)
import           Control.Monad             (void, (<=<))

import           Control.Monad.Except      (Except, ExceptT (ExceptT),
                                            MonadError, catchError, runExceptT,
                                            throwError, withExcept, withExceptT)
import           Control.Monad.Morph       (generalize, hoist)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Either               (partitionEithers)
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (isPrefixOf)
import qualified Data.List                 as List
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
import           Pact.Types.Lang           (pattern ColonExp, pattern CommaExp,
                                            Info, mModel, mName, renderInfo,
                                            renderParsed, tMeta, _tDefName)
import           Pact.Types.Runtime        (Exp, ModuleData, ModuleName,
                                            Ref (Ref),
                                            Term (TConst, TDef, TSchema, TTable),
                                            asString, getInfo, tShow)
import qualified Pact.Types.Runtime        as Pact
import           Pact.Types.Typecheck      (AST,
                                            Fun (FDefun, _fArgs, _fBody, _fInfo),
                                            Named, Node, TcId (_tiInfo),
                                            TopLevel (TopConst, TopFun, TopTable),
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
  | SortMismatch String
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
  | QueryFailure SmtFailure
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
  | FailedConstTranslation String
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
  Invalid model    -> "Invalidating model found:\n" <> showModel model
  Unsatisfiable    -> "This property is unsatisfiable"
  Unknown reason   -> "The solver returned 'unknown':\n" <> tShow reason
  SortMismatch msg -> T.unlines
    [ "The solver returned a sort mismatch:"
    , T.pack msg
    , "This may be the result of a bug in z3 versions 4.8.0 and earlier."
    , "Specifically, before commit a37d05d54b9ca10d4c613a4bb3a980f1bb0c1c4a."
    ]
  UnexpectedFailure smtE -> T.pack $ show smtE

describeQueryFailure :: SmtFailure -> Text
describeQueryFailure = \case
  Invalid model  -> "Wow. We (the compiler) have bad news for you. You know that property / invariant you wrote? It's great. Really. It's just that it divides by zero or somesuch and we don't know what to do with this. Good news is we have a model which may (fingers crossed) help debug the problem:\n" <> showModel model
  Unknown reason -> "You've written a hell of a property here. Usually properties are simple things, like \"is positive\" or \"conserves mass\". But not this bad boy. This here property broke the SMT solver. Wish we could help but you're on your own with this one (actually, please report this as an issue: https://github.com/kadena-io/pact/issues).\n\nGood luck...\n" <> tShow reason
  err@SortMismatch{} -> describeSmtFailure err
  Unsatisfiable  -> "Unsatisfiable query failure: please report this as a bug"
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
            QueryFailure err      -> describeQueryFailure err
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

translateToVerificationFailure :: TranslateFailure -> VerificationFailure
translateToVerificationFailure = ModuleCheckFailure . translateToCheckFailure

analyzeToCheckFailure :: AnalyzeFailure -> CheckFailure
analyzeToCheckFailure (AnalyzeFailure info err)
  = CheckFailure info (AnalyzeFailure' err)

smtToCheckFailure :: Info -> SmtFailure -> CheckFailure
smtToCheckFailure info = CheckFailure info . SmtFailure

smtToQueryFailure :: Info -> SmtFailure -> CheckFailure
smtToQueryFailure info = CheckFailure info . QueryFailure

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
        SBV.Unk   -> throwError . mkUnknown =<< lift SBV.getUnknownReason

    Satisfaction ->
      case satResult of
        SBV.Sat   -> SatisfiedProperty <$> lift (saturateModel model0)
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

verifyFunctionInvariants'
  :: Text
  -> Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants' funName funInfo tables pactArgs body = runExceptT $ do
    (args, tm, graph) <- hoist generalize $
      withExcept translateToCheckFailure $ runTranslation funName funInfo pactArgs body

    ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
      modelArgs' <- lift $ allocArgs args
      tags <- lift $ allocModelTags (Located funInfo tm) graph
      resultsTable <- withExceptT analyzeToCheckFailure $
        runInvariantAnalysis tables (analysisArgs modelArgs') tm tags funInfo

      -- Iterate through each invariant in a single query so we can reuse our
      -- assertion stack.
      ExceptT $ fmap Right $
        SBV.query $
          for2 resultsTable $ \(Located info (AnalysisResult querySucceeds prop ksProvs)) -> do
            _ <- runExceptT $ inNewAssertionStack $ do
              void $ lift $ SBV.constrain $ SBV.bnot $ successBool querySucceeds
              withExceptT (smtToQueryFailure info) $
                resultQuery Validation $ Model modelArgs' tags ksProvs graph

            queryResult <- runExceptT $ inNewAssertionStack $ do
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
    config = SBV.z3 -- { SBVI.verbose = True }

    -- Discharges impure 'SMTException's from sbv.
    catchingExceptions
      :: IO (Either CheckFailure b)
      -> IO (Either CheckFailure b)
    catchingExceptions act = act `E.catch` \(e :: SBV.SMTException) ->
      pure $ Left $ CheckFailure funInfo $ SmtFailure $ UnexpectedFailure e

    runSymbolic :: Symbolic a -> IO a
    runSymbolic = SBV.runSMTWith config

verifyFunctionProperty
  :: Text
  -> Info
  -> [Table]
  -> [Named Node]
  -> [AST Node]
  -> Located Check
  -> IO (Either CheckFailure CheckSuccess)
verifyFunctionProperty funName funInfo tables pactArgs body (Located propInfo check) =
    runExceptT $ do
      (args, tm, graph) <- hoist generalize $
        withExcept translateToCheckFailure $
          runTranslation funName funInfo pactArgs body
      ExceptT $ catchingExceptions $ runSymbolic $ runExceptT $ do
        modelArgs' <- lift $ allocArgs args
        tags <- lift $ allocModelTags (Located funInfo tm) graph
        AnalysisResult _querySucceeds prop ksProvs
          <- withExceptT analyzeToCheckFailure $
            runPropertyAnalysis check tables (analysisArgs modelArgs') tm tags
              funInfo

        -- TODO: bring back the query success check when we've resolved the SBV
        -- query / quantified variables issue:
        -- https://github.com/LeventErkok/sbv/issues/407
        --
        -- _ <- hoist SBV.query $ do
        --   void $ lift $ SBV.constrain $ SBV.bnot $ successBool querySucceeds
        --   withExceptT (smtToQueryFailure (getInfo check)) $
        --     resultQuery Validation model

        void $ lift $ SBV.output prop
        hoist SBV.query $
          withExceptT (smtToCheckFailure propInfo) $
            resultQuery goal $ Model modelArgs' tags ksProvs graph

  where
    goal :: Goal
    goal = checkGoal check

    config :: SBV.SMTConfig
    config = SBV.z3 -- { SBVI.verbose = True }

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
      Just model -> liftEither $ case expToMapping ["invariant", "invariants"] model of
        Nothing -> throwError (model, "expected \"invariant\" or \"invariants\"")
        Just model' -> do
          let expInvariants = model' ^? ix "invariants"
              expInvariant  = model' ^? ix "invariant"
          exps <- collectExps "invariants" expInvariants expInvariant
          runExpParserOver exps $
            flip runReaderT (varIdArgs _utFields) . expToInvariant TBool

    pure $ Table tabName schema invariants

data PropertyScope
  = Everywhere
  | Excluding !(Set Text)
  | Including !(Set Text)

data ModuleProperty = ModuleProperty
  { _moduleProperty      :: !(Exp Info)
  , _modulePropertyScope :: !PropertyScope
  }

-- Does this (module-scoped) property apply to this function?
applicableCheck :: Text -> ModuleProperty -> Bool
applicableCheck funName (ModuleProperty _ propScope) = case propScope of
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
parseModelDecl
  :: Exp Info
  -> Either ParseFailure [Either (Text, DefinedProperty (Exp Info)) ModuleProperty]
parseModelDecl (SquareList exps) = traverse parseDefprops' exps where

  parseDefprops' exp@(ParenList (EAtom' "defproperty" : rest)) = case rest of
    [ EAtom' propname, ParenList args, body ] -> do
      args' <- parseBindings (curry Right) args & _Left %~ (exp,)
      pure $ Left (propname, DefinedProperty args' body)
    [ EAtom' propname,              body ] ->
      pure $ Left (propname, DefinedProperty [] body)
    _ -> Left (exp, "Invalid property definition")
  parseDefprops' exp = do
    (body, propScope) <- parsePropertyExp exp
    pure $ Right $ ModuleProperty body propScope

  parseNames :: Exp Info -> Either ParseFailure (Set Text)
  parseNames exp@(SquareList names) = case normalizeListLit names of
    Just names' -> fmap Set.fromList $ traverse parseName names'
    Nothing     -> Left (exp, "expected a list of names")
  parseNames exp                = Left (exp, "expected a list of names")

  parseName :: Exp Info -> Either ParseFailure Text
  parseName (EAtom' name) = Right name
  parseName exp           = Left (exp, "expected a bare word name")

  parsePropertyExp :: Exp Info -> Either ParseFailure (Exp Info, PropertyScope)
  parsePropertyExp exp = case exp of
    ParenList (EAtom' "property" : rest) -> case rest of
      [ exp' ]
        -> pure (exp', Everywhere)
      [ exp', BraceList [ EStrLiteral' "except", ColonExp, names ] ]
        -> (exp',) . Excluding <$> parseNames names
      [ exp', BraceList [ EStrLiteral' "only",   ColonExp, names ] ]
        -> (exp',) . Including <$> parseNames names
      _ -> Left (exp, "malformed property definition")
    _ -> Left (exp, "expected a set of property / defproperty")

parseModelDecl exp = Left (exp, "expected set of defproperty")

-- Get the set (HashMap) of refs to functions in this module.
moduleTypecheckableRefs :: ModuleData -> HM.HashMap Text Ref
moduleTypecheckableRefs (_mod, modRefs) = flip HM.filter modRefs $ \case
  Ref TDef{}   -> True
  Ref TConst{} -> True
  _            -> False

data ModelDecl = ModelDecl
  { _moduleDefProperties :: !(HM.HashMap Text (DefinedProperty (Exp Info)))
  , _moduleProperties    :: ![ModuleProperty]
  }

-- Get the model defined in this module
moduleModelDecl :: ModuleData -> Either ParseFailure ModelDecl
moduleModelDecl (Pact.Module{Pact._mMeta=Pact.Meta _ model}, _modRefs)
  = case model of
      Just model' -> do
        lst <- parseModelDecl model'
        let (propList, checkList) = partitionEithers lst
        pure $ ModelDecl (HM.fromList propList) checkList
      Nothing     -> Right $ ModelDecl HM.empty []

moduleFunChecks
  :: [Table]
  -> [ModuleProperty]
  -> HM.HashMap Text (Ref, Pact.FunType TC.UserType)
  -> HM.HashMap Text EProp
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -> Except VerificationFailure
       (HM.HashMap Text (Ref, Either ParseFailure [Located Check]))
moduleFunChecks tables modCheckExps funTypes consts propDefs = for funTypes $ \case
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
          ((\(vid, (Unmunged nm, ty)) -> Binding vid (Unmunged nm) (Munged nm) ty) <$>
            zip vids argTys')

        --
        -- TODO: should the map sent to parsing should be Un/Munged, instead of Text?
        --
        nameVids :: Map Text VarId
        nameVids = Map.fromList $ fmap (\(Binding vid (Unmunged nm) _ _) -> (nm, vid)) env

        vidTys :: Map VarId EType
        vidTys = Map.fromList $ fmap (\(Binding vid _ _ ty) -> (vid, ty)) env

        vidStart = VarId (length env)

        tableEnv = TableMap $ Map.fromList $
          tables <&> \Table { _tableName, _tableType } ->
            let fields = _utFields _tableType
                colMap = ColumnMap $ Map.fromList $ flip mapMaybe fields $
                  \(Pact.Arg argName ty _) ->
                    (ColumnName (T.unpack argName),) <$> maybeTranslateType ty
            in (TableName (T.unpack _tableName), colMap)

    checks <- withExcept ModuleParseFailure $ liftEither $ do
      exps <- case defn ^? tMeta . mModel . _Just of
        Just model -> case expToMapping ["property", "properties"] model of
          Nothing -> throwError (model, "expected \"property\" or \"properties\"")
          Just model' ->
            let expProperties = model' ^? ix "properties"
                expProperty   = model' ^? ix "property"
            in collectExps "properties" expProperties expProperty
        Nothing -> pure []
      let funName = _tDefName defn
          applicableModuleChecks = map _moduleProperty $
            filter (applicableCheck funName) modCheckExps
      runExpParserOver (applicableModuleChecks <> exps) $
        expToCheck tableEnv vidStart nameVids vidTys consts propDefs

    pure (ref, Right checks)

-- | Given an exp like '(k v)', and a set (list) of allowed keys, convert it to
-- a singleton map
expToMapping :: [Text] -> Exp Info -> Maybe (Map Text (Exp Info))
expToMapping allowed = \case
  ParenList [EAtom' k, v]
    | k `elem` allowed -> Just $ Map.singleton k v
  _                    -> Nothing

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

verifyFunctionProps :: [Table] -> Ref -> Text -> [Located Check] -> IO [CheckResult]
verifyFunctionProps tables ref name props = do
  (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures

  case fun of
    TopFun FDefun {_fInfo, _fArgs, _fBody} _ ->
      if Set.null failures
      then for props $ verifyFunctionProperty name _fInfo tables _fArgs _fBody
      else pure [Left (CheckFailure _fInfo (TypecheckFailure failures))]
    _ -> pure []

verifyFunctionInvariants
  :: [Table]
  -> Ref
  -> Text
  -> IO (Either CheckFailure (TableMap [CheckResult]))
verifyFunctionInvariants tables ref name = do
  (fun, tcState) <- runTC 0 False $ typecheckTopLevel ref
  let failures = tcState ^. tcFailures

  case fun of
    TopFun FDefun {_fInfo, _fArgs, _fBody} _ ->
      if Set.null failures
      then verifyFunctionInvariants' name _fInfo tables _fArgs _fBody
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

  allModuleModelDecls <-
    withExceptT ModuleParseFailure $ liftEither $
      traverse moduleModelDecl allModules

  ModelDecl _ checkExps <-
    withExceptT ModuleParseFailure $ liftEither $
      moduleModelDecl moduleData

  let allModulePropDefs = fmap _moduleDefProperties allModuleModelDecls

      -- how many times have these names been defined across all in-scope
      -- modules
      allModulePropNameDuplicates =
          HM.keys
        $ HM.filter (> (1 :: Int))
        $ foldl (\acc k -> acc & at k %~ (Just . maybe 0 succ)) HM.empty
        $ concatMap HM.keys allModulePropDefs

      propDefs :: HM.HashMap Text (DefinedProperty (Exp Info))
      propDefs = HM.unions allModulePropDefs

      typecheckedRefs :: HM.HashMap Text Ref
      typecheckedRefs = moduleTypecheckableRefs moduleData

  -- For each ref, if it typechecks as a function (it'll be either a function
  -- or a constant), keep its signature.
  --
  ( funTypes :: HM.HashMap Text (Ref, Pact.FunType TC.UserType),
    consts :: HM.HashMap Text (AST TC.Node)) <- ifoldrM
    (\name ref accum -> do
      maybeFun <- lift $ runTC 0 False $ typecheckTopLevel ref
      pure $ case maybeFun of
        (TopFun (FDefun _info _name funType _args _body) _meta, _tcState)
          -> accum & _1 . at name ?~ (ref, funType)
        (TopConst _info _qualifiedName _type val _doc, _tcState)
          -> accum & _2 . at name ?~ val
        _ -> accum
    )
    (HM.empty, HM.empty)
    typecheckedRefs

  let valueToProp' :: ETerm -> Except VerificationFailure EProp
      valueToProp' tm = case valueToProp tm of
        Right prop -> pure prop
        Left msg   -> throwError $ FailedConstTranslation msg

      translateNodeNoGraph'
        = withExceptT translateToVerificationFailure . translateNodeNoGraph

  consts' <- hoist generalize $
    traverse (valueToProp' <=< translateNodeNoGraph') consts

  (funChecks :: HM.HashMap Text (Ref, Either ParseFailure [Located Check]))
    <- hoist generalize $ moduleFunChecks tables checkExps funTypes consts' propDefs

  let funChecks' :: Either ParseFailure (HM.HashMap Text (Ref, [Located Check]))
      funChecks' = traverse sequence funChecks

      verifyFunProps :: Ref -> Text -> [Located Check] -> IO [CheckResult]
      verifyFunProps = verifyFunctionProps tables

  funChecks'' <- case funChecks' of
    Left errs         -> throwError $ ModuleParseFailure errs
    Right funChecks'' -> pure funChecks''

  funChecks''' <- lift $ ifor funChecks'' $ \name (ref, check) ->
    verifyFunProps ref name check
  invariantChecks <- ifor typecheckedRefs $ \name ref ->
    withExceptT ModuleCheckFailure $ ExceptT $
      verifyFunctionInvariants tables ref name

  let warnings = VerificationWarnings allModulePropNameDuplicates

  pure $ ModuleChecks funChecks''' invariantChecks warnings

renderVerifiedModule :: Either VerificationFailure ModuleChecks -> [Text]
renderVerifiedModule = \case
  Left (ModuleParseFailure failure)  ->
    [describeParseFailure failure]
  Left (ModuleCheckFailure checkFailure) ->
    [describeCheckFailure checkFailure]
  Left (TypeTranslationFailure msg ty) ->
    [msg <> ": " <> tShow ty]
  Left (InvalidRefType) ->
    ["Invalid reference type given to typechecker."]
  Left (FailedConstTranslation msg) ->
    [T.pack msg]
  Right (ModuleChecks propResults invariantResults warnings) ->
    let propResults'      = toListOf (traverse.each)          propResults
        invariantResults' = toListOf (traverse.traverse.each) invariantResults
    in (describeCheckResult <$> propResults' <> invariantResults') <>
         [describeVerificationWarnings warnings]

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
      Right . head <$> verifyFunctionProps tables funRef funName [Located info check]
    Nothing -> pure $ Left $ CheckFailure info $ NotAFunction funName
