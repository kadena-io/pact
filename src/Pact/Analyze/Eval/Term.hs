{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# options_ghc -fdefer-type-errors #-}

module Pact.Analyze.Eval.Term where

import           Control.Applicative         (ZipList (..))
import           Control.Lens                (At (at), Lens',
                                              preview, use, view, (%=), (%~),
                                              (&), (+=), (.=), (.~), (<&>),
                                              (?~), (^.), (^?), _1, _2, _Just)
-- import           Control.Monad               (void, when)
import           Control.Monad.Except        (Except, MonadError (throwError))
import           Control.Monad.Reader        (MonadReader (local), runReaderT)
import           Control.Monad.RWS.Strict    (RWST (RWST, runRWST))
import           Control.Monad.State.Strict  (MonadState, modify', runStateT)
import qualified Data.Aeson                  as Aeson
import           Data.ByteString.Lazy        (toStrict)
import           Data.Foldable               (foldl', foldlM)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (EqSymbolic ((.==)),
                                              Mergeable (symbolicMerge), SBV,
                                              SymArray (readArray), -- SymWord,
                                              ite, (.<))
import qualified Data.SBV.Internals          as SBVI
import qualified Data.SBV.String             as SBV
import           Data.SBV.Tuple              (mkPair)
import           Data.String                 (fromString)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Thyme                  (formatTime, parseTime)
import           Data.Traversable            (for)
import           GHC.TypeLits
import           System.Locale

import qualified Pact.Types.Hash             as Pact
import qualified Pact.Types.Persistence      as Pact
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
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid = view (scope . at vid)
  withVar vid val m = local (scope . at vid ?~ val) m
  markFailure b = succeeds %= (.&& sansProv (sNot b))
  withMergeableAnalyzer ty f = withSymWord ty f

addConstraint :: S Bool -> Analyze ()
addConstraint b = modify' $ latticeState.lasConstraints %~ (.&& b)

instance (Mergeable a) => Mergeable (Analyze a) where
  symbolicMerge force test left right = Analyze $ RWST $ \r s ->
    --
    -- We explicitly propagate only the "global" portion of the state from the
    -- left to the right computation. And then the only lattice state, and not
    -- global state, is merged.
    --
    -- If either side fails, the entire merged computation fails.
    --
    let run act = runRWST (runAnalyze act) r
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

tagSubpathStart :: Path -> S Bool -> Analyze ()
tagSubpathStart p active = do
  mTag <- preview $ aeModelTags.mtPaths.at p._Just
  case mTag of
    Nothing  -> pure ()
    Just sbv -> addConstraint $ sansProv $ sbv .== _sSbv active

tagFork :: Path -> Path -> S Bool -> S Bool -> Analyze ()
tagFork pathL pathR reachable lPasses = do
  tagSubpathStart pathL $ reachable .&& lPasses
  tagSubpathStart pathR $ reachable .&& sNot lPasses

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
    Just tagAv -> addConstraint $ sansProv $ tagAv .== av

tagVarBinding :: VarId -> AVal -> Analyze ()
tagVarBinding vid av = do
  mTag <- preview $ aeModelTags.mtVars.at vid._Just.located._2._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

symKsName :: S Str -> S KeySetName
symKsName = unsafeCoerceS

ksAuthorized :: S KeySet -> Analyze (S Bool)
ksAuthorized sKs = do
  -- NOTE: we know that KsAuthorized constructions are only emitted within
  -- Enforced constructions, so we know that this keyset is being enforced
  -- here.
  case sKs ^? sProv._Just._FromCell of
    Just (OriginatingCell tn sCn sRk sDirty) ->
      cellEnforced tn sCn sRk %= (.|| sNot sDirty)
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
        case runReaderT (runStateT (unInvariantCheck (eval invariant)) sTrue)
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

-- error "TODO"
-- validateWrite :: Pact.WriteType -> Schema ty -> Object ty -> Analyze ()
-- validateWrite writeType sch@(Schema sm) obj@(Object om) = do
--   -- For now we lump our three cases together:
--   --   1. write field not in schema
--   --   2. object and schema types don't match
--   --   3. unexpected partial write
--   let invalid = throwErrorNoLoc $ InvalidDbWrite writeType sch obj

--   iforM_ om $ \field (ety, _av) ->
--     case field `Map.lookup` sm of
--       Nothing -> invalid
--       Just ety'
--         | ety /= ety' -> invalid
--         | otherwise   -> pure ()

--   let requiresFullWrite = writeType `elem` [Pact.Insert, Pact.Write]

--   when (requiresFullWrite && Map.size om /= Map.size sm) invalid

readFields
  :: TableName -> S RowKey -> TagId -> SingTy ('TyObject ty)
  -> Analyze (S (ConcreteObj ty), Map Text AVal)
readFields _tn _sRk _tid (SObject SNil) = pure (literalS (), Map.empty)
readFields tn sRk tid (SObject (SCons sym fieldType subSchema)) = do
  let fieldName  = symbolVal sym
      tFieldName = T.pack fieldName
      cn         = ColumnName fieldName
      subObjTy   = SObject subSchema
  columnRead tn cn .= sTrue
  sDirty <- use $ cellWritten tn cn sRk
  av     <- readField tn cn sRk sDirty fieldType
  tagAccessCell mtReads tid tFieldName av
  case av of
    OpaqueVal -> error "TODO (readFields OpaqueVal)"
    AVal _prov sval -> withSymWord fieldType $ withSymWord subObjTy $ do
      (S _ obj', avs) <- readFields tn sRk tid subObjTy
      pure (sansProv $ mkPair (SBVI.SBV sval) obj', Map.insert tFieldName av avs)

readField
  :: TableName -> ColumnName -> S RowKey -> S Bool -> SingTy ty -> Analyze AVal
readField tn cn sRk sDirty ty
  = mkAVal <$> use (typedCell ty id tn cn sRk sDirty)
 --
 -- TODO: do we still need to do this?
 -- TODO: if we add nested object support here, we need to install
 --       the correct provenance into AVals all the way down into
 --       sub-objects.
 --

aValsOfObj :: SingTy ('TyObject ty) -> S (ConcreteObj ty) -> Map Text AVal
aValsOfObj _ _ = Map.empty -- error "TODO"

writeFields
  :: Pact.WriteType -> TagId
  -> TableName -> S RowKey -> S (ConcreteObj ty) -> SingTy ('TyObject ty)
  -> Analyze ()
writeFields _ _ _ _ _ (SObject SNil) = pure ()

writeFields _writeType _tid tn sRk _sObj (SObject (SCons sym _fieldType _subObjTy)) = do
  let fieldName  = symbolVal sym
      -- tFieldName = T.pack fieldName
      cn         = ColumnName fieldName
  cellWritten tn cn sRk .= sTrue
  columnWritten tn cn   .= sTrue
  -- tagAccessCell mtWrites tid tFieldName aval'

  -- case aval' of
  --   AVal mProv sVal -> do
  --     -- Note: The reason for taking `plus` and `minus` as arguments is to
  --     -- avoid overlapping instances. GHC is willing to pick `+` and `-`
  --     -- for each of the two instantiations of this function.
  --     let writeDelta
  --           :: forall ty ty'. (SymWord ty', Num ty', Concrete ty ~ ty')
  --           => SingTy ty
  --           -> (S ty' -> S ty' -> S ty') -> (S ty' -> S ty' -> S ty')
  --           -> (TableName -> ColumnName -> S RowKey ->           Lens' EvalAnalyzeState (S ty'))
  --           -> (TableName -> ColumnName ->                       Lens' EvalAnalyzeState (S ty'))
  --           -> Analyze ()
  --         writeDelta ty plus minus mkCellDeltaL mkColDeltaL = do
  --           let cell :: Lens' EvalAnalyzeState (S ty')
  --               cell = typedCell ty id tn cn sRk sTrue
  --               next = mkS mProv sVal

  --           -- (only) in the case of an insert, we know the cell did not
  --           -- previously exist
  --           prev <- if writeType == Pact.Insert
  --             then pure (literalS 0)
  --             else use cell

  --           cell .= next
  --           let diff = next `minus` prev
  --           mkCellDeltaL tn cn sRk %= plus diff
  --           mkColDeltaL  tn cn     %= plus diff

  --     case fieldType of
  --       SInteger -> writeDelta SInteger (+) (-) intCellDelta intColumnDelta
  --       SDecimal -> writeDelta SDecimal (+) (-) decCellDelta decColumnDelta
  --       _        -> typedCell fieldType id tn cn sRk sTrue .= mkS mProv sVal

  --     pure aval'

    -- OpaqueVal  -> throwErrorNoLoc OpaqueValEncountered

evalTerm :: SingI a => Term a -> Analyze (S (Concrete a))
evalTerm = \case
  CoreTerm a -> evalCore a

  IfThenElse ty cond (thenPath, then') (elsePath, else') -> do
    reachable  <- use purelyReachable
    testPasses <- evalTerm cond
    tagFork thenPath elsePath reachable testPasses
    singIte ty (_sSbv testPasses)
      (evalTerm then')
      (evalTerm else')

  -- TODO: check that the body of enforce is pure
  Enforce mTid cond -> do
    cond' <- evalTerm cond
    maybe (pure ()) (`tagAssert` cond') mTid
    succeeds %= (.&& cond')
    pure sTrue

  EnforceOne (Left tid) -> do
    tagAssert tid sFalse -- in this case (of an empty list), we always fail.
    succeeds .= sFalse
    pure sTrue           -- <- this value doesn't matter.

  -- TODO: check that each cond is pure. checking that @Enforce@ terms are pure
  -- does *NOT* suffice; we can have arbitrary expressions in an @enforce-one@
  -- list.
  EnforceOne (Right conds) -> do
    initSucceeds <- use succeeds

    (result, anySucceeded) <- foldlM
      (\(prevRes, earlierSuccess) ((failTag, passTag), cond) -> do
        succeeds .= sTrue
        res <- evalTerm cond
        currentSucceeded <- use succeeds
        tagFork passTag failTag (sNot earlierSuccess) currentSucceeded

        pure $ iteS earlierSuccess
          (prevRes, sTrue)
          (res,     currentSucceeded))
      (sTrue, sFalse)
      conds

    succeeds .= (initSucceeds .&& anySucceeded)
    pure result

  Sequence eterm valT -> evalETerm eterm *> evalTerm valT

  Read objTy tid tn rowKey -> do
    sRk <- symRowKey <$> evalTerm rowKey
    tableRead tn .= sTrue
    rowReadCount tn sRk += 1

    readSucceeds <- use $ rowExists id tn sRk
    tagAccessKey mtReads tid sRk readSucceeds
    succeeds %= (.&& readSucceeds)

    (sObj, aValFields) <- readFields tn sRk tid objTy

    applyInvariants tn aValFields $ mapM_ addConstraint

    pure sObj

  Write schema writeType tid tn rowKey objT -> do
    obj <- withSing schema $ evalTerm objT
    -- validateWrite writeType schema obj
    sRk <- symRowKey <$> evalTerm rowKey

    thisRowExists <- use $ rowExists id tn sRk
    let writeSucceeds = case writeType of
          Pact.Insert -> sNot thisRowExists
          Pact.Write  -> sTrue
          Pact.Update -> thisRowExists
    succeeds %= (.&& writeSucceeds)
    rowExists id tn sRk .= sTrue

    tableWritten tn .= writeSucceeds
    rowWriteCount tn sRk += 1
    tagAccessKey mtWrites tid sRk writeSucceeds

    writeFields writeType tid tn sRk obj schema

    let aValFields = aValsOfObj schema obj
    applyInvariants tn aValFields $ \invariants' ->
      let fs :: ZipList (Located (SBV Bool) -> Located (SBV Bool))
          fs = ZipList $ (\s -> fmap (_sSbv s .&&)) <$> invariants'
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

  ReadKeySet  str -> resolveKeySet  =<< symKsName <$> evalTerm str
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

  PactVersion -> pure $ literalS $ Str $ T.unpack pactVersion

  Format formatStr args -> do
    formatStr' <- eval formatStr
    args' <- for args $ \case
      Existential SStr     str  -> Left          <$> eval str
      Existential SInteger int  -> Right . Left  <$> eval int
      Existential SBool    bool -> Right . Right <$> eval bool
      etm                   -> throwErrorNoLoc $ fromString $ T.unpack $
        "We can only analyze calls to `format` formatting {string,integer,bool}" <>
        " (not " <> userShow etm <> ")"
    case unliteralS formatStr' of
      Nothing -> throwErrorNoLoc "We can only analyze calls to `format` with statically determined contents (both arguments)"
      Just (Str concreteStr) -> case format concreteStr args' of
        Left err -> throwError err
        Right tm -> pure tm

  FormatTime formatStr time -> do
    formatStr' <- eval formatStr
    time'      <- eval time
    case (unliteralS formatStr', unliteralS time') of
      (Just (Str formatStr''), Just time'') -> pure $ literalS $ Str $
        formatTime defaultTimeLocale formatStr'' (toPact timeIso time'')
      _ -> throwErrorNoLoc "We can only analyze calls to `format-time` with statically determined contents (both arguments)"

  ParseTime mFormatStr timeStr -> do
    formatStr' <- case mFormatStr of
      Just formatStr -> eval formatStr
      Nothing        -> pure $ literalS $ Str Pact.simpleISO8601
    timeStr'   <- eval timeStr
    case (unliteralS formatStr', unliteralS timeStr') of
      (Just (Str formatStr''), Just (Str timeStr'')) ->
        case parseTime defaultTimeLocale formatStr'' timeStr'' of
          Nothing   -> succeeds .= sFalse >> pure 0
          Just time -> pure $ literalS $ fromPact timeIso time
      _ -> throwErrorNoLoc "We can only analyze calls to `parse-time` with statically determined contents (both arguments)"

  Hash value -> do
    let sHash = literalS . Str . T.unpack . Pact.asString . Pact.hash
        notStaticErr :: AnalyzeFailure
        notStaticErr = AnalyzeFailure dummyInfo "We can only analyze calls to `hash` with statically determined contents"
    case value of
      -- Note that strings are hashed in a different way from the other types
      Existential SStr tm -> eval tm <&> unliteralS >>= \case
        Nothing        -> throwError notStaticErr
        Just (Str str) -> pure $ sHash $ encodeUtf8 $ T.pack str

      -- Everything else is hashed by first converting it to JSON:
      Existential SInteger tm -> eval tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just int -> pure $ sHash $ toStrict $ Aeson.encode int
      Existential SBool tm -> eval tm <&> unliteralS >>= \case
        Nothing   -> throwError notStaticErr
        Just bool -> pure $ sHash $ toStrict $ Aeson.encode bool

      -- In theory we should be able to analyze decimals -- we just need to be
      -- able to convert them back into Decimal.Decimal decimals (from SBV's
      -- Real representation). This is probably possible if we think about it
      -- hard enough.
      Existential SDecimal _    -> throwErrorNoLoc "We can't yet analyze calls to `hash` on decimals"

      Existential (SList _) _   -> throwErrorNoLoc "We can't yet analyze calls to `hash on lists"
      Existential (SObject _) _ -> throwErrorNoLoc "We can't yet analyze calls to `hash on objects"
      Existential _ _           -> throwErrorNoLoc "We can't yet analyze calls to `hash` on non-{string,integer,bool}"

-- For now we only allow these three types to be formatted.
--
-- Formatting behavior is not well specified. Its behavior on these types is
-- easy to infer from examples in the docs. We would also like to be able to
-- format decimals, but that's a little harder (we could still make it work).
-- Behavior on structured data is not specified.
type Formattable = Either (S Str) (Either (S Integer) (S Bool))

-- This definition was taken from Pact.Native, then modified to be symbolic
format :: String -> [Formattable] -> Either AnalyzeFailure (S Str)
format s tms = do
  -- TODO: don't convert to Text and back. splitOn is provided by both the
  -- split and MissingH packages.
  let parts = coerceS @String @Str . literalS . T.unpack <$> T.splitOn "{}" (pack s)
      plen = length parts
      rep = \case
        Left  str          -> str
        Right (Right bool) -> ite (_sSbv bool) "true" "false"
        Right (Left int)   ->
          ite (int .< 0) "-" "" .++
          coerceS @String @Str (sansProv (SBV.natToStr (_sSbv (abs int))))
  if plen == 1
  then Right (literalS (Str s))
  else if plen - length tms > 1
       then Left (AnalyzeFailure dummyInfo "format: not enough arguments for template")
       else Right $ foldl'
              (\r (e, t) -> r .++ rep e .++ t)
              (head parts)
              (zip tms (tail parts))
