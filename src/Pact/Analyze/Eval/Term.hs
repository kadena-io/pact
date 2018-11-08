{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Pact.Analyze.Eval.Term where

import           Control.Applicative         (ZipList (..))
import           Control.Lens                (At (at), Lens', iforM, iforM_,
                                              preview, use, view, (%=), (%~),
                                              (&), (+=), (.=), (.~), (<&>),
                                              (?~), (^.), (^?), _1, _2, _Just)
import           Control.Monad               (void, when)
import           Control.Monad.Except        (Except, ExceptT (ExceptT),
                                              MonadError (throwError),
                                              runExcept)
import           Control.Monad.Reader        (MonadReader (local), runReaderT)
import           Control.Monad.RWS.Strict    (RWST (RWST, runRWST))
import           Control.Monad.State.Strict  (MonadState, modify', runStateT)
import qualified Data.Aeson                  as Aeson
import           Data.ByteString.Lazy        (toStrict)
import           Data.Foldable               (foldl', foldlM)
import           Data.Functor.Identity       (Identity (Identity))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (Boolean (bnot, true, (&&&), (|||)),
                                              EqSymbolic ((.==)),
                                              Mergeable (symbolicMerge), SBV,
                                              SymArray (readArray), SymWord,
                                              constrain, false, ite, (.<))
import qualified Data.SBV.String             as SBV
import           Data.String                 (fromString)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Thyme                  (formatTime, parseTime)
import           Data.Traversable            (for)
import           System.Locale

import qualified Pact.Types.Hash             as Pact
import qualified Pact.Types.Persistence      as Pact
import           Pact.Types.Runtime          (tShow)
import qualified Pact.Types.Runtime          as Pact
import           Pact.Types.Version          (pactVersion)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Eval.Invariant
import           Pact.Analyze.Orphans        ()
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util


newtype Analyze a
  = Analyze
    { runAnalyze :: RWST AnalyzeEnv () EvalAnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState EvalAnalyzeState, MonadError AnalyzeFailure)

instance Analyzer Analyze where
  type TermOf Analyze = Term
  eval             = evalTerm
  evalO            = evalTermO
  evalLogicalOp    = evalTermLogicalOp
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid = view (scope . at vid)
  markFailure b = succeeds %= (&&& sansProv (bnot b))

evalTermLogicalOp
  :: LogicalOp
  -> [Term Bool]
  -> Analyze (S Bool)
evalTermLogicalOp AndOp [a, b] = do
  a' <- eval a
  ite (_sSbv a') (eval b) (pure false)
evalTermLogicalOp OrOp [a, b] = do
  a' <- eval a
  ite (_sSbv a') (pure true) (eval b)
evalTermLogicalOp NotOp [a] = bnot <$> eval a
evalTermLogicalOp op terms = throwErrorNoLoc $ MalformedLogicalOpExec op $ length terms

addConstraint :: S Bool -> Analyze ()
addConstraint s = modify' $ globalState.gasConstraints %~ (<> c)
  where
    c = Constraints $ constrain $ _sSbv s

instance (Mergeable a) => Mergeable (Analyze a) where
  symbolicMerge force test left right = Analyze $ RWST $ \r s -> ExceptT $ Identity $
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged.
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runExcept . runRWST (runAnalyze act) r
    in do
      (lRes, AnalyzeState lls lgs, ()) <- run left s
      (rRes, AnalyzeState rls rgs, ()) <- run right $ s & globalState .~ lgs

      return ( symbolicMerge force test lRes rRes
             , AnalyzeState (symbolicMerge force test lls rls) rgs
             , ()
             )

--
-- NOTE: for these tagging functions, at the moment we allow a "partial" model.
--       we could also decide to 'throwError'; right now we simply don't tag in
--       these cases where a tag is not found.
--

tagAccessKey
  :: Lens' (ModelTags 'Symbolic) (Map TagId (Located Access))
  -> TagId
  -> S RowKey
  -> S Bool
  -> Analyze ()
tagAccessKey lens' tid rowKey accessSucceeds = do
  mAcc <- preview $ aeModelTags.lens'.at tid._Just.located
  case mAcc of
    Nothing -> pure ()
    Just (Access tagRowKey _object tagSuccess) -> do
      addConstraint $ sansProv $ rowKey               .== tagRowKey
      addConstraint $ sansProv $ _sSbv accessSucceeds .== tagSuccess

-- | "Tag" an uninterpreted read value with value from our Model that was
-- allocated in Symbolic.
tagAccessCell
  :: Lens' (ModelTags 'Symbolic) (Map TagId (Located Access))
  -> TagId
  -> Text
  -> AVal
  -> Analyze ()
tagAccessCell lens' tid fieldName av = do
  mTag <- preview $
    aeModelTags.lens'.at tid._Just.located.accObject.objFields.at fieldName._Just._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

tagAssert :: TagId -> S Bool -> Analyze ()
tagAssert tid sb = do
  mTag <- preview $ aeModelTags.mtAsserts.at tid._Just.located
  case mTag of
    Nothing  -> pure ()
    Just sbv -> addConstraint $ sansProv $ sbv .== _sSbv sb

-- | "Tag" an uninterpreted auth value with value from our Model that was
-- allocated in Symbolic.
tagAuth :: TagId -> S KeySet -> S Bool -> Analyze ()
tagAuth tid sKs sb = do
  mAuth <- preview $ aeModelTags.mtAuths.at tid._Just.located
  case mAuth of
    Nothing  -> pure ()
    Just (Authorization ksTag sbv) -> do
      addConstraint $ sansProv $ ksTag .== sKs
      addConstraint $ sansProv $ sbv .== _sSbv sb
      globalState.gasKsProvenances.at tid .= (sKs ^. sProv)

tagFork :: Path -> Path -> S Bool -> S Bool -> Analyze ()
tagFork pathL pathR reachable lPasses = do
    tagSubpathStart pathL $ reachable &&& lPasses
    tagSubpathStart pathR $ reachable &&& bnot lPasses

  where
    tagSubpathStart :: Path -> S Bool -> Analyze ()
    tagSubpathStart p active = do
      mTag <- preview $ aeModelTags.mtPaths.at p._Just
      case mTag of
        Nothing  -> pure ()
        Just sbv -> addConstraint $ sansProv $ sbv .== _sSbv active

tagResult :: AVal -> Analyze ()
tagResult av = do
  tid <- view $ aeModelTags.mtResult._1
  tagReturn tid av
  tag <- view $ aeModelTags.mtResult._2.located._2
  addConstraint $ sansProv $ tag .== av

tagReturn :: TagId -> AVal -> Analyze ()
tagReturn tid av = do
  mTag <- preview $ aeModelTags.mtReturns.at tid._Just._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

tagVarBinding :: VarId -> AVal -> Analyze ()
tagVarBinding vid av = do
  mTag <- preview $ aeModelTags.mtVars.at vid._Just.located._2._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

symKsName :: S String -> S KeySetName
symKsName = unsafeCoerceS

ksAuthorized :: S KeySet -> Analyze (S Bool)
ksAuthorized sKs = do
  -- NOTE: we know that KsAuthorized constructions are only emitted within
  -- Enforced constructions, so we know that this keyset is being enforced
  -- here.
  case sKs ^? sProv._Just._FromCell of
    Just (OriginatingCell tn sCn sRk sDirty) ->
      cellEnforced tn sCn sRk %= (||| bnot sDirty)
    Nothing ->
      pure ()
  fmap sansProv $ readArray <$> view ksAuths <*> pure (_sSbv sKs)

reindex :: Map Text VarId -> Map Text AVal -> Map VarId AVal
reindex varMap = Map.mapKeys (varMap Map.!)

applyInvariants
  :: TableName
  -> Map Text AVal
  -- ^ Mapping from the fields in this table to the @SVal@ holding that field
  --   in this context.
  -> ([S Bool] -> Analyze ())
  -- ^ The function used to apply an invariant in this context. The @SBV Bool@
  --   is an assertion of what it would take for the invariant to be true in
  --   this context.
  -> Analyze ()
applyInvariants tn aValFields addInvariants = do
  mInvariants <- view (invariants . at tn)
  mColumnIds  <- view (aeColumnIds . at tn)
  case (mInvariants, mColumnIds) of
    (Just invariants', Just columnIds) -> do
      let aValFields' = reindex columnIds aValFields
      invariants'' <- for invariants' $ \(Located info invariant) ->
        case runReaderT (runStateT (unInvariantCheck (eval invariant)) true)
                        (Located info aValFields') of
          -- Use the location of the invariant
          Left  (AnalyzeFailure _ err) -> throwError $ AnalyzeFailure info err
          -- though it's important that the query succeeds, we don't check that
          -- here. it's checked when we query invariants. if it passes there,
          -- it'll pass here. if not, we won't get here.
          Right (inv, _querySucceeds) -> pure inv
      addInvariants invariants''
    _ -> pure ()

evalETerm :: ETerm -> Analyze AVal
evalETerm tm = snd <$> evalExistential tm

evalTermO :: Term Object -> Analyze Object
evalTermO = \case
  CoreTerm a -> evalCoreO a

  Read tid tn (Schema fields) rowKey -> do
    sRk <- symRowKey <$> evalTerm rowKey
    tableRead tn .= true
    rowReadCount tn sRk += 1

    readSucceeds <- use $ rowExists id tn sRk
    tagAccessKey mtReads tid sRk readSucceeds
    succeeds %= (&&& readSucceeds)

    aValFields <- iforM fields $ \fieldName fieldType -> do
      let cn = ColumnName $ T.unpack fieldName
      columnRead tn cn .= true
      sDirty <- use $ cellWritten tn cn sRk

      av <- case fieldType of
        EType TInt     -> mkAVal <$> use (intCell     id tn cn sRk sDirty)
        EType TBool    -> mkAVal <$> use (boolCell    id tn cn sRk sDirty)
        EType TStr     -> mkAVal <$> use (stringCell  id tn cn sRk sDirty)
        EType TDecimal -> mkAVal <$> use (decimalCell id tn cn sRk sDirty)
        EType TTime    -> mkAVal <$> use (timeCell    id tn cn sRk sDirty)
        EType TKeySet  -> mkAVal <$> use (ksCell      id tn cn sRk sDirty)
        EType TAny     -> pure OpaqueVal
        --
        -- TODO: if we add nested object support here, we need to install
        --       the correct provenance into AVals all the way down into
        --       sub-objects.
        --
        EObjectTy _    -> throwErrorNoLoc UnsupportedObjectInDbCell

      tagAccessCell mtReads tid fieldName av

      pure (fieldType, av)

    applyInvariants tn (snd <$> aValFields) (mapM_ addConstraint)

    pure $ Object aValFields

  Let _name vid retTid eterm body -> do
    av <- evalETerm eterm
    tagVarBinding vid av
    local (scope.at vid ?~ av) $ do
      obj <- evalTermO body
      tagReturn retTid $ AnObj obj
      pure obj

  Sequence eterm objT -> evalETerm eterm *> evalTermO objT

  IfThenElse cond (thenPath, then') (elsePath, else') -> do
    reachable <- use purelyReachable
    testPasses <- evalTerm cond
    tagFork thenPath elsePath reachable testPasses
    case unliteralS testPasses of
      Just True  -> evalTermO then'
      Just False -> evalTermO else'
      Nothing    -> throwErrorNoLoc "Unable to determine statically the branch taken in an if-then-else evaluating to an object"

validateWrite :: Pact.WriteType -> Schema -> Object -> Analyze ()
validateWrite writeType sch@(Schema sm) obj@(Object om) = do
  -- For now we lump our three cases together:
  --   1. write field not in schema
  --   2. object and schema types don't match
  --   3. unexpected partial write
  let invalid = throwErrorNoLoc $ InvalidDbWrite writeType sch obj

  iforM_ om $ \field (ety, _av) ->
    case field `Map.lookup` sm of
      Nothing -> invalid
      Just ety'
        | ety /= ety' -> invalid
        | otherwise   -> pure ()

  let requiresFullWrite = writeType `elem` [Pact.Insert, Pact.Write]

  when (requiresFullWrite && Map.size om /= Map.size sm) invalid

evalTerm :: (Show a, SymWord a) => Term a -> Analyze (S a)
evalTerm = \case
  CoreTerm a -> evalCore a

  IfThenElse cond (thenPath, then') (elsePath, else') -> do
    reachable <- use purelyReachable
    testPasses <- evalTerm cond
    tagFork thenPath elsePath reachable testPasses
    iteS testPasses
      (evalTerm then')
      (evalTerm else')

  -- TODO: check that the body of enforce is pure
  Enforce mTid cond -> do
    cond' <- evalTerm cond
    maybe (pure ()) (`tagAssert` cond') mTid
    succeeds %= (&&& cond')
    pure true

  EnforceOne (Left tid) -> do
    tagAssert tid false -- in this case (of an empty list), we always fail.
    succeeds .= false
    pure true           -- <- this value doesn't matter.

  -- TODO: check that each cond is pure. checking that @Enforce@ terms are pure
  -- does *NOT* suffice; we can have arbitrary expressions in an @enforce-one@
  -- list.
  EnforceOne (Right conds) -> do
    initSucceeds <- use succeeds

    (result, anySucceeded) <- foldlM
      (\(prevRes, earlierSuccess) ((failTag, passTag), cond) -> do
        succeeds .= true
        res <- evalTerm cond
        currentSucceeded <- use succeeds
        tagFork passTag failTag (bnot earlierSuccess) currentSucceeded

        pure $ iteS earlierSuccess
          (prevRes, true)
          (res,     currentSucceeded))
      (true, false)
      conds

    succeeds .= (initSucceeds &&& anySucceeded)
    pure result

  Sequence eterm valT -> evalETerm eterm *> evalTerm valT

  Write writeType tid tn schema rowKey objT -> do
    obj@(Object fields) <- evalTermO objT
    validateWrite writeType schema obj
    sRk <- symRowKey <$> evalTerm rowKey

    thisRowExists <- use $ rowExists id tn sRk
    let writeSucceeds = case writeType of
          Pact.Insert -> bnot thisRowExists
          Pact.Write  -> true
          Pact.Update -> thisRowExists
    succeeds %= (&&& writeSucceeds)
    rowExists id tn sRk .= true

    tableWritten tn .= writeSucceeds
    rowWriteCount tn sRk += 1
    tagAccessKey mtWrites tid sRk writeSucceeds

    aValFields <- iforM fields $ \colName (fieldType, aval') -> do
      let cn = ColumnName (T.unpack colName)
      cellWritten tn cn sRk .= true
      columnWritten tn cn   .= true
      tagAccessCell mtWrites tid colName aval'

      case aval' of
        AVal mProv sVal -> do
          -- Note: The reason for taking `plus` and `minus` as arguments is to
          -- avoid overlapping instances. GHC is willing to pick `+` and `-`
          -- for each of the two instantiations of this function.
          let writeDelta
                :: forall t. (SymWord t, Num t)
                => (S t -> S t -> S t) -> (S t -> S t -> S t)
                -> (TableName -> ColumnName -> S RowKey -> S Bool -> Lens' EvalAnalyzeState (S t))
                -> (TableName -> ColumnName -> S RowKey ->           Lens' EvalAnalyzeState (S t))
                -> (TableName -> ColumnName ->                       Lens' EvalAnalyzeState (S t))
                -> Analyze ()
              writeDelta plus minus mkCellL mkCellDeltaL mkColDeltaL = do
                let cell :: Lens' EvalAnalyzeState (S t)
                    cell = mkCellL tn cn sRk true
                let next = mkS mProv sVal

                -- (only) in the case of an insert, we know the cell did not
                -- previously exist
                prev <- if writeType == Pact.Insert
                  then pure (literalS 0)
                  else use cell

                cell .= next
                let diff = next `minus` prev
                mkCellDeltaL tn cn sRk %= plus diff
                mkColDeltaL  tn cn     %= plus diff

          case fieldType of
            EType TInt     -> writeDelta (+) (-) (intCell id) intCellDelta intColumnDelta
            EType TBool    -> boolCell   id tn cn sRk true .= mkS mProv sVal
            EType TDecimal -> writeDelta (+) (-) (decimalCell id) decCellDelta decColumnDelta
            EType TTime    -> timeCell   id tn cn sRk true .= mkS mProv sVal
            EType TStr     -> stringCell id tn cn sRk true .= mkS mProv sVal
            EType TKeySet  -> ksCell     id tn cn sRk true .= mkS mProv sVal
            EType TAny     -> void $ throwErrorNoLoc OpaqueValEncountered
            EObjectTy _    -> void $ throwErrorNoLoc UnsupportedObjectInDbCell

          pure aval'

            -- TODO: handle EObjectTy here

        -- TODO(joel): I'm not sure this is the right error to throw
        AnObj obj' -> throwErrorNoLoc $ AValUnexpectedlyObj obj'
        OpaqueVal  -> throwErrorNoLoc OpaqueValEncountered

    applyInvariants tn aValFields $ \invariants' ->
      let fs :: ZipList (Located (SBV Bool) -> Located (SBV Bool))
          fs = ZipList $ (\s -> fmap (_sSbv s &&&)) <$> invariants'
      in maintainsInvariants . at tn . _Just %= (fs <*>)

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literalS "Write succeeded"

  Let _name vid retTid eterm body -> do
    av <- evalETerm eterm
    tagVarBinding vid av
    local (scope.at vid ?~ av) $ do
      res <- evalTerm body
      tagReturn retTid $ mkAVal res
      pure res

  ReadKeySet str -> resolveKeySet =<< symKsName <$> evalTerm str
  ReadDecimal str -> resolveDecimal =<< evalTerm str
  ReadInteger str -> resolveInteger =<< evalTerm str

  KsAuthorized tid ksT -> do
    ks <- evalTerm ksT
    authorized <- ksAuthorized ks
    tagAuth tid ks authorized
    pure authorized

  NameAuthorized tid str -> do
    ksn <- symKsName <$> evalTerm str
    ks <- resolveKeySet ksn
    authorized <- nameAuthorized ksn
    tagAuth tid ks authorized
    pure authorized

  PactVersion -> pure $ literalS $ T.unpack pactVersion

  Format formatStr args -> do
    formatStr' <- eval formatStr
    args' <- for args $ \case
      ESimple TStr  str  -> Left          <$> eval str
      ESimple TInt  int  -> Right . Left  <$> eval int
      ESimple TBool bool -> Right . Right <$> eval bool
      etm                -> throwErrorNoLoc $ fromString $ T.unpack $
        "We can only analyze calls to `format` formatting {string,integer,bool}" <>
        " (not " <> userShow etm <> ")"
    case unliteralS formatStr' of
      Nothing -> throwErrorNoLoc "We can only analyze calls to `format` with statically determined contents (both arguments)"
      Just concreteStr -> case format concreteStr args' of
        Left err -> throwError err
        Right tm -> pure tm

  FormatTime formatStr time -> do
    formatStr' <- eval formatStr
    time'      <- eval time
    case (unliteralS formatStr', unliteralS time') of
      (Just formatStr'', Just time'') -> pure $ literalS $
        formatTime defaultTimeLocale formatStr'' (toPact timeIso time'')
      _ -> throwErrorNoLoc "We can only analyze calls to `format-time` with statically determined contents (both arguments)"

  ParseTime mFormatStr timeStr -> do
    formatStr' <- case mFormatStr of
      Just formatStr -> eval formatStr
      Nothing        -> pure $ literalS Pact.simpleISO8601
    timeStr'   <- eval timeStr
    case (unliteralS formatStr', unliteralS timeStr') of
      (Just formatStr'', Just timeStr'') ->
        case parseTime defaultTimeLocale formatStr'' timeStr'' of
          Nothing   -> succeeds .= false >> pure 0
          Just time -> pure $ literalS $ fromPact timeIso time
      _ -> throwErrorNoLoc "We can only analyze calls to `parse-time` with statically determined contents (both arguments)"

  Hash value -> do
    let sHash = literalS . T.unpack . Pact.asString . Pact.hash
        notStaticErr :: AnalyzeFailure
        notStaticErr = AnalyzeFailure dummyInfo "We can only analyze calls to `hash` with statically determined contents"
    case value of
      -- Note that strings are hashed in a different way from the other types
      ESimple TStr tm -> eval tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just str -> pure $ sHash $ encodeUtf8 $ T.pack str

      -- Everything else is hashed by first converting it to JSON:
      ESimple TInt tm -> eval tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just int -> pure $ sHash $ toStrict $ Aeson.encode int
      ESimple TBool tm -> eval tm <&> unliteralS >>= \case
        Nothing   -> throwError notStaticErr
        Just bool -> pure $ sHash $ toStrict $ Aeson.encode bool

      -- In theory we should be able to analyze decimals -- we just need to be
      -- able to convert them back into Decimal.Decimal decimals (from SBV's
      -- Real representation). This is probably possible if we think about it
      -- hard enough.
      ESimple TDecimal _ -> throwErrorNoLoc "We can't yet analyze calls to `hash` on decimals"

      ESimple _ _        -> throwErrorNoLoc "We can't yet analyze calls to `hash` on non-{string,integer,bool}"
      EObject _ _        -> throwErrorNoLoc "We can't yet analyze calls to `hash on objects"

  n -> throwErrorNoLoc $ UnhandledTerm $ tShow n


-- For now we only allow these three types to be formatted.
--
-- Formatting behavior is not well specified. Its behavior on these types is
-- easy to infer from examples in the docs. We would also like to be able to
-- format decimals, but that's a little harder (we could still make it work).
-- Behavior on structured data is not specified.
type Formattable = Either (S String) (Either (S Integer) (S Bool))

-- This definition was taken from Pact.Native, then modified to be symbolic
format :: String -> [Formattable] -> Either AnalyzeFailure (S String)
format s tms = do
  -- TODO: don't convert to Text and back. splitOn is provided by both the
  -- split and MissingH packages.
  let parts = literalS . T.unpack <$> T.splitOn "{}" (pack s)
      plen = length parts
      rep = \case
        Left  str          -> str
        Right (Right bool) -> ite (_sSbv bool) "true" "false"
        Right (Left int)   ->
          ite (int .< 0) "-" "" .++
          sansProv (SBV.natToStr (_sSbv (abs int)))
  if plen == 1
  then Right (literalS s)
  else if plen - length tms > 1
       then Left (AnalyzeFailure dummyInfo "format: not enough arguments for template")
       else Right $ foldl'
              (\r (e, t) -> r .++ rep e .++ t)
              (head parts)
              (zip tms (tail parts))
