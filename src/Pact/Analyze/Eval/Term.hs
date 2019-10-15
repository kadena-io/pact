{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Symbolic evaluation of program 'Term's (as opposed to the 'Invariant' or
-- 'Prop' languages).
module Pact.Analyze.Eval.Term where

import           Control.Applicative         (ZipList (..))
import           Control.Lens                (At (at), Lens', preview, use,
                                              view, (%=), (%~), (&), (+=), (.=),
                                              (.~), (<&>), (?~), (?=), (^.),
                                              (^?), (<>~), _1, _2, _head,
                                              _Just, ifoldlM)
import           Control.Monad               (void, when)
import           Control.Monad.Except        (Except, MonadError (throwError))
import           Control.Monad.Fail          (MonadFail(..))
import           Control.Monad.Reader        (MonadReader (ask, local), runReaderT)
import           Control.Monad.RWS.Strict    (RWST (RWST, runRWST))
import           Control.Monad.State.Strict  (MonadState, modify', runStateT)
import qualified Data.Aeson                  as Aeson
import           Data.ByteString.Lazy        (toStrict)
import           Data.Constraint             (Dict (Dict), withDict)
import           Data.Default                (def)
import           Data.Foldable               (foldl', foldlM)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (EqSymbolic ((.==), (./=)),
                                              OrdSymbolic ((.>=)),
                                              Mergeable (symbolicMerge), SBV,
                                              SymVal, ite, literal, (.<),
                                              uninterpret, writeArray)
import qualified Data.SBV.Internals          as SBVI
import qualified Data.SBV.Maybe              as SBV
import qualified Data.SBV.String             as SBV
import           Data.SBV.Tuple              (tuple)
import qualified Data.SBV.Tuple              as SBVT
import           Data.String                 (fromString)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Thyme                  (formatTime, parseTime)
import           Data.Traversable            (for)
import           Data.Type.Equality          ((:~:)(Refl))
import           GHC.TypeLits
import           System.Locale

import qualified Pact.Types.Hash             as Pact
import qualified Pact.Types.PactValue        as Pact
import qualified Pact.Types.Persistence      as Pact
import           Pact.Types.Pretty           (renderCompactString', pretty)
import           Pact.Types.Runtime          (tShow)
import qualified Pact.Types.Runtime          as Pact
import           Pact.Types.Version          (pactVersion)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Core
import           Pact.Analyze.Eval.Invariant
import           Pact.Analyze.LegacySFunArray
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util

newtype Analyze a
  = Analyze
    { runAnalyze :: RWST AnalyzeEnv () EvalAnalyzeState (Except AnalyzeFailure) a }
  deriving (Functor, Applicative, Monad, MonadReader AnalyzeEnv,
            MonadState EvalAnalyzeState, MonadError AnalyzeFailure)

instance MonadFail Analyze where
    fail = throwError . AnalyzeFailure def . fromString

instance Analyzer Analyze where
  type TermOf Analyze = Term
  eval                = evalTerm
  throwErrorNoLoc err = do
    info <- view (analyzeEnv . aeInfo)
    throwError $ AnalyzeFailure info err
  getVar vid                 = view (scope . at vid)
  withVar vid val m          = local (scope . at vid ?~ val) m
  markFailure b              = succeeds %= (.&& sansProv (sNot b))
  withMergeableAnalyzer ty f = withSymVal ty f

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

tagGuard :: TagId -> S Guard -> S Bool -> Analyze ()
tagGuard tid sg sPasses = do
  mGEnforcement <- preview $ aeModelTags.mtGuardEnforcements.at tid._Just.located
  case mGEnforcement of
    Nothing  -> pure ()
    Just (GuardEnforcement sg' passes') -> do
      addConstraint $ sansProv $ sg' .== sg
      addConstraint $ sansProv $ passes' .== _sSbv sPasses
      globalState.gasGuardProvenances.at tid .= (sg ^. sProv)

tagGrantRequest :: TagId -> S Bool -> Analyze ()
tagGrantRequest tid sb = do
  mTag <- preview $ aeModelTags.mtGrantRequests.at tid._Just.located
  case mTag of
    Nothing  -> pure ()
    Just (GrantRequest _ sbv) -> addConstraint $ sansProv $ sbv .== _sSbv sb

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

tagCancel :: TagId -> SBV Bool -> Analyze ()
tagCancel tid cancelHappens = do
  mTag <- view $ aeModelTags.mtCancels.at tid
  case mTag of
    Nothing  -> pure ()
    Just tag -> addConstraint $ sansProv $ cancelHappens .== tag

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

tagYield :: TagId -> AVal -> Analyze ()
tagYield tid av = do
  mTag <- preview $ aeModelTags.mtYields.at tid._Just._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ tagAv .== av

tagResume :: TagId -> AVal -> Analyze ()
tagResume tid av = do
  mTag <- preview $ aeModelTags.mtResumes.at tid._Just._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ tagAv .== av

tagVarBinding :: VarId -> AVal -> Analyze ()
tagVarBinding vid av = do
  mTag <- preview $ aeModelTags.mtVars.at vid._Just.located._2._2
  case mTag of
    Nothing    -> pure ()
    Just tagAv -> addConstraint $ sansProv $ av .== tagAv

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
      invariants'' <- for invariants' $ \(Located info invariant) -> do
        case runReaderT (runStateT (unInvariantCheck (eval invariant)) sTrue)
                        (Located info aValFields') of
          -- Use the location of the invariant
          Left (AnalyzeFailure _ err) -> throwError $ AnalyzeFailure info err
          -- though it's important that the query succeeds, we don't check that
          -- here. it's checked when we query invariants. if it passes there,
          -- it'll pass here. if not, we won't get here.
          Right (inv, _querySucceeds) -> pure inv
      addInvariants invariants''
    _ -> pure ()

evalETerm :: ETerm -> Analyze AVal
evalETerm tm = snd <$> evalExistential tm

-- | Read the fields described by the schema of the row indexed by the symbolic
-- RowKey, but without marking columns as read or tagging access to any cells.
peekFields
  :: TableName
  -> S RowKey
  -> SingTy ('TyObject ty)
  -> Analyze (Map Text AVal)
peekFields _tn _sRk (SObjectUnsafe SNil') =
  pure $ Map.empty
peekFields tn sRk (SObjectUnsafe (SCons' sym fieldType subSchema)) = do
  let fieldName  = symbolVal sym
      tFieldName = T.pack fieldName
      cn         = ColumnName fieldName
      subObjTy   = SObjectUnsafe subSchema
  sDirty <- use $ cellWritten tn cn sRk
  av <- readField tn cn sRk sDirty fieldType
  avs <- peekFields tn sRk subObjTy
  pure $ Map.insert tFieldName av avs

readFields
  :: TableName -> S RowKey -> TagId -> SingTy ('TyObject ty)
  -> Analyze (S (ConcreteObj ty), Map Text AVal)
readFields _tn _sRk _tid (SObjectUnsafe SNil') =
  pure (withProv (FromRow Map.empty) (literal ()), Map.empty)
readFields tn sRk tid (SObjectUnsafe (SCons' sym fieldType subSchema)) = do
  let fieldName  = symbolVal sym
      tFieldName = T.pack fieldName
      cn         = ColumnName fieldName
      subObjTy   = SObjectUnsafe subSchema
  columnRead tn cn .= sTrue
  sDirty <- use $ cellWritten tn cn sRk
  av     <- readField tn cn sRk sDirty fieldType
  tagAccessCell mtReads tid tFieldName av
  case av of
    OpaqueVal -> throwErrorNoLoc "Opaque value encountered in table read"
    AVal (Just (FromCell oc)) sval
      -> withSymVal fieldType $ withSymVal subObjTy $ do
        (S (Just (FromRow ocMap)) obj', avs) <- readFields tn sRk tid subObjTy
        pure ( withProv (FromRow $ Map.insert cn oc ocMap) $
                 tuple (SBVI.SBV sval, obj')
             , Map.insert tFieldName av avs
             )
    AVal _ _ -> error
      "impossible: unexpected type of cell provenance in readFields"

mkChainData :: SingTy ('TyObject ty) -> Analyze (S (ConcreteObj ty))
mkChainData (SObjectUnsafe SNil') = pure $ sansProv $ literal ()
mkChainData (SObjectUnsafe (SCons' sym (fieldType :: SingTy v) subSchema)) = do
  let fieldName = symbolVal sym
      subObjTy = SObjectUnsafe subSchema
      sbv = withHasKind fieldType $ uninterpret $ "chain_data_" <>
        (map (\c -> if c == '-' then '_' else c) fieldName)

      mNumDict :: Maybe (Dict (Num (Concrete v)))
      mNumDict = case fieldType of
        SInteger -> Just Dict
        SDecimal -> Just Dict
        _        -> Nothing

  withSymVal fieldType $ do
    when (fieldName `elem` ["block-height", "gas-limit", "gas-price"]) $ do
      case mNumDict of
        Just numDict ->
          let zero = withDict numDict $ literal 0
          in withOrd fieldType $ addConstraint $ sansProv $ sbv .>= zero
        Nothing ->
          error $ "impossible: " ++ fieldName ++ " must be a number"

    S _ obj' <- mkChainData subObjTy
    withSymVal subObjTy $ pure $ sansProv $ tuple (sbv, obj')

readField
  :: TableName -> ColumnName -> S RowKey -> S Bool -> SingTy ty -> Analyze AVal
readField tn cn sRk sDirty ty
  = mkAVal <$> use (typedCell ty id tn cn sRk sDirty)

aValsOfObj :: SingList schema -> SBV (ConcreteObj schema) -> Map Text AVal
aValsOfObj schema obj = foldObject (obj :< schema) $ \sym (SBVI.SBV sval) _
  -> Map.singleton (T.pack (symbolVal sym)) (AVal Nothing sval)

writeFields
  :: Pact.WriteType -> TagId -> TableName -> S RowKey
  -> S (ConcreteObj ty) -> SingTy ('TyObject ty)
  -> Analyze ()
writeFields _ _ _ _ _ SObjectNil = pure ()

writeFields writeType tid tn sRk (S mProv obj)
  (SObjectCons sym fieldType subObjTy)
  = withSymVal fieldType $ withSymVal (SObjectUnsafe subObjTy) $ do
  let fieldName  = symbolVal sym
      tFieldName = T.pack fieldName
      cn         = ColumnName fieldName
      SBVI.SBV sVal = obj SBVT.^. SBVT._1

  cellWritten tn cn sRk .= sTrue
  columnWritten tn cn   .= sTrue
  tagAccessCell mtWrites tid tFieldName $ AVal mProv sVal

  -- Note: The reason for taking `plus` and `minus` as arguments is to
  -- avoid overlapping instances. GHC is willing to pick `+` and `-`
  -- for each of the two instantiations of this function.
  let writeDelta
        :: forall ty ty'. (SymVal ty', Num ty', Concrete ty ~ ty')
        => SingTy ty
        -> (S ty' -> S ty' -> S ty') -> (S ty' -> S ty' -> S ty')
        -> (TableName -> ColumnName -> S RowKey -> Lens' EvalAnalyzeState (S ty'))
        -> (TableName -> ColumnName ->             Lens' EvalAnalyzeState (S ty'))
        -> Analyze ()
      writeDelta ty plus minus mkCellDeltaL mkColDeltaL = do
        let cell :: Lens' EvalAnalyzeState (S ty')
            cell = typedCell ty id tn cn sRk sTrue
            next = mkS mProv sVal

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
    SInteger -> writeDelta SInteger (+) (-) intCellDelta intColumnDelta
    SDecimal -> writeDelta SDecimal (+) (-) decCellDelta decColumnDelta
    _        -> typedCell fieldType id tn cn sRk sTrue .= mkS mProv sVal

  writeFields writeType tid tn sRk (S mProv (obj SBVT.^. SBVT._2))
    (SObjectUnsafe subObjTy)

writeFields _ _ _ _ _ _ = vacuousMatch "the previous two cases are complete"

accumulatingPendingGrants :: Analyze () -> Analyze TokenGrants
accumulatingPendingGrants act = do
    emptyGrants <- view aeEmptyGrants
    previouslyPending <- swapIn emptyGrants
    act
    swapIn previouslyPending

  where
    swapIn :: TokenGrants -> Analyze TokenGrants
    swapIn next = do
      prev <- use $ latticeState.lasPendingGrants
      latticeState.lasPendingGrants .= next
      pure prev

capabilityAppToken :: Capability -> [VarId] -> Analyze Token
capabilityAppToken (Capability schema capName) vids = do
    mAVals <- sequence <$> traverse getVar vids
    case mAVals of
      Nothing    ->
        throwErrorNoLoc "unexpected missing variable in environment"
      Just avals ->
        pure $ Token schema capName $ sansProv $ buildObject schema avals

  where
    buildObject :: SingList schema -> [AVal] -> SBV (ConcreteObj schema)
    buildObject SNil' [] = literal ()
    buildObject (SCons' _sym ty tys) ((AVal _ sval):avals) =
      withSymVal ty $ withSymVal (SObjectUnsafe tys) $ tuple
        ( SBVI.SBV sval
        , buildObject tys avals
        )
    buildObject _ _ = error "invariant violation: list length mismatch"

addPendingGrant :: Token -> Analyze ()
addPendingGrant token = pendingTokenGranted token .= sTrue

extendingGrants :: TokenGrants -> Analyze (S a) -> Analyze (S a)
extendingGrants newGrants = local $ aeActiveGrants %~ (<> newGrants)

isGranted :: Token -> Analyze (S Bool)
isGranted t = view $ activeGrants.tokenGranted t

restrictingDbAccess :: DbRestriction -> Analyze a -> Analyze a
restrictingDbAccess res = local $ aeDbRestriction <>~ Just res

evalTerm :: forall a. SingI a => Term a -> Analyze (S (Concrete a))
evalTerm = \case
  CoreTerm a -> evalCore a

  Sequence eterm valT -> evalETerm eterm *> evalTerm valT

  IfThenElse ty cond (thenPath, then') (elsePath, else') -> do
    reachable  <- use purelyReachable
    testPasses <- evalTerm cond
    tagFork thenPath elsePath reachable testPasses
    singIte ty (_sSbv testPasses)
      (evalTerm then')
      (evalTerm else')

  Enforce mTid cond -> do
    cond' <- restrictingDbAccess DisallowDb $ evalTerm cond
    maybe (pure ()) (`tagAssert` cond') mTid
    succeeds %= (.&& cond')
    pure sTrue

  EnforceOne (Left tid) -> do
    tagAssert tid sFalse -- in this case (of an empty list), we always fail.
    succeeds .= sFalse
    pure sTrue           -- <- this value doesn't matter.

  EnforceOne (Right conds) -> do
    initSucceeds <- use succeeds

    (result, anySucceeded) <- foldlM
      (\(prevRes, earlierSuccess) ((failTag, passTag), cond) -> do
        succeeds .= sTrue
        res <- restrictingDbAccess DisallowWrites $ evalTerm cond
        currentSucceeded <- use succeeds
        tagFork passTag failTag (sNot earlierSuccess) currentSucceeded

        pure $ iteS earlierSuccess
          (prevRes, sTrue)
          (res,     currentSucceeded))
      (sTrue, sFalse)
      conds

    succeeds .= (initSucceeds .&& anySucceeded)
    pure result

  WithCapability app body -> do
    grants <- accumulatingPendingGrants $
      void $ evalETerm app

    extendingGrants grants $
      evalTerm body

  Granting cap vids t -> do
    r <- evalTerm t
    addPendingGrant =<< capabilityAppToken cap vids
    pure r

  HasGrant tid cap (unzip -> (_, vids)) -> do
    granted <- isGranted =<< capabilityAppToken cap vids
    tagGrantRequest tid granted
    pure granted

  Read rowTy objTy mDefault tid tn rowKey -> do
    currRestriction <- view aeDbRestriction
    case currRestriction of
      Just res@DisallowDb -> throwErrorNoLoc $ DisallowedRead tn res
      Just DisallowWrites -> pure ()
      Nothing -> pure ()

    sRk <- symRowKey <$> evalTerm rowKey
    tableRead tn .= sTrue
    rowReadCount tn sRk += 1

    rowExisted <- use $ rowExists id tn sRk

    let readObject = do
          (sObj, aValFields) <- readFields tn sRk tid rowTy
          applyInvariants tn aValFields $ mapM_ addConstraint
          pure $ subObjectS rowTy objTy sObj

        tagAccess :: S Bool -> Analyze ()
        tagAccess readSucceeds = tagAccessKey mtReads tid sRk readSucceeds

    case mDefault of
      Nothing -> do
        tagAccess rowExisted
        succeeds %= (.&& rowExisted)
        readObject

      Just defObj -> withSymVal objTy $ do
        tagAccess sTrue
        iteS rowExisted readObject (eval defObj)

  Write objTy@(SObjectUnsafe schema) writeType tid tn rowKey objT -> do
    currRestriction <- view aeDbRestriction
    case currRestriction of
      Just res -> throwErrorNoLoc $ DisallowedWrite tn writeType res
      Nothing -> pure ()

    obj <- withSing objTy $ evalTerm objT
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

    Just rowETy <- view $ aeTableSchemas.at tn
    prevAVals <- case rowETy of
      EType rowTy@(SObject _) -> do
        rowAVals <- peekFields tn sRk rowTy
        -- assume invariants for values already in the DB:
        applyInvariants tn rowAVals $ mapM_ addConstraint
        pure rowAVals

    writeFields writeType tid tn sRk obj objTy

    let newAVals = aValsOfObj schema (_sSbv obj)
        -- NOTE: left-biased union prefers the new values:
        nextAVals = Map.union newAVals prevAVals

    applyInvariants tn nextAVals $ \invariants' ->
      let fs :: ZipList (Located (SBV Bool) -> Located (SBV Bool))
          fs = ZipList $ (\s -> fmap (_sSbv s .&&)) <$> invariants'
      in maintainsInvariants . at tn . _Just %= (fs <*>)

    --
    -- TODO: make a constant on the pact side that this uses:
    --
    pure $ literalS "Write succeeded"

  Let _name vid eterm body -> do
    av <- evalETerm eterm
    tagVarBinding vid av
    withVar vid av $ do
      res <- evalTerm body
      pure res

  Return tid body -> do
    res <- evalTerm body
    tagReturn tid $ mkAVal res
    pure res

  -- Read values from tx metadata
  ReadKeySet  nameT -> readKeySet  =<< evalTerm nameT
  ReadDecimal nameT -> readDecimal =<< evalTerm nameT
  ReadInteger nameT -> readInteger =<< evalTerm nameT
  ReadString  nameT -> readString  =<< evalTerm nameT

  PactId -> do
    whetherInPact <- view inPact
    succeeds %= (.&& whetherInPact)
    view currentPactId

  ChainData objTy -> do
    mCached <- use $ globalState.gasCachedChainData
    case mCached of
      Just (SomeVal objTy' cd) ->
        case singEq objTy objTy' of
          Just Refl -> pure cd
          Nothing -> error "impossible: type mismatch for chain-data cache"
      Nothing -> do
        cd <- mkChainData objTy
        globalState.gasCachedChainData ?= SomeVal objTy cd
        pure cd

  --
  -- If in the future Pact is able to store guards other than keysets in the
  -- registry, our approach will work for that generally.
  --
  MkKsRefGuard nameT -> resolveGuard =<< symRegistryName <$> evalTerm nameT

  MkPactGuard _nameT -> do
    whetherInPact <- view inPact
    succeeds %= (.&& whetherInPact)
    view aeTrivialGuard

  MkUserGuard guard bodyET -> do
    -- NOTE: a user guard can not access user tables, so we can run it ahead of
    -- its use and store whether the guard permits the tx to succeed. If we
    -- instead deferred the execution to the use (rather than closure) site,
    -- then we would need to capture the environment here for later use.
    prevSucceeds <- use succeeds
    restrictingDbAccess DisallowDb $
      void $ evalETerm bodyET
    postGuardSucceeds <- use succeeds
    succeeds .= prevSucceeds

    let sg = literalS guard
    guardPasses sg .= postGuardSucceeds
    pure sg

  MkModuleGuard _nameT ->
    view moduleGuard

  GuardPasses tid guardT -> do
    guard <- evalTerm guardT

    -- NOTE: we're at an enforcement site; we know that `GuardPasses`
    -- constructions only appear within `Enforce` constructions. so if we have
    -- metadata sitting around for a cell that this guard came from, mark that
    -- cell as enforced.
    case guard ^? sProv._Just._FromCell of
      Just (OriginatingCell tn sCn sRk sDirty) ->
        cellEnforced tn sCn sRk %= (.|| sNot sDirty)
      Nothing ->
        pure ()

    whetherPasses <- use $ guardPasses guard

    tagGuard tid guard whetherPasses
    pure whetherPasses

  PactVersion -> pure $ literalS $ Str $ T.unpack pactVersion

  Format formatStr args -> do
    formatStr' <- eval formatStr
    args' <- for args $ \case
      Some SStr     str  -> Left          <$> eval str
      Some SInteger int  -> Right . Left  <$> eval int
      Some SBool    bool -> Right . Right <$> eval bool
      Some ty       _    -> throwErrorNoLoc $ fromString $
        renderCompactString' $
        "We can only analyze calls to `format` formatting {string,integer,bool}" <>
        " (not " <> pretty ty <> ")"
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
    let sHash = literalS . Str . T.unpack . Pact.asString . Pact.pactHash
        notStaticErr :: AnalyzeFailure
        notStaticErr = AnalyzeFailure dummyInfo "We can only analyze calls to `hash` with statically determined contents"
    case value of
      -- Note that strings are hashed in a different way from the other types
      Some SStr tm -> eval tm <&> unliteralS >>= \case
        Nothing        -> throwError notStaticErr
        Just (Str str) -> pure $ sHash $ encodeUtf8 $ T.pack str

      -- Everything else is hashed by first converting it to JSON:
      Some SInteger tm -> eval tm <&> unliteralS >>= \case
        Nothing  -> throwError notStaticErr
        Just int -> pure $ sHash $ toStrict $ Aeson.encode $ Pact.PLiteral $ Pact.LInteger int
      Some SBool tm -> eval tm <&> unliteralS >>= \case
        Nothing   -> throwError notStaticErr
        Just bool -> pure $ sHash $ toStrict $ Aeson.encode bool

      -- In theory we should be able to analyze decimals -- we just need to be
      -- able to convert them back into Decimal.Decimal decimals (from SBV's
      -- Real representation). This is probably possible if we think about it
      -- hard enough.
      Some SDecimal _    -> throwErrorNoLoc "We can't yet analyze calls to `hash` on decimals"

      Some (SList _) _   -> throwErrorNoLoc "We can't yet analyze calls to `hash` on lists"
      Some (SObject _) _ -> throwErrorNoLoc "We can't yet analyze calls to `hash` on objects"
      Some _ _           -> throwErrorNoLoc "We can't yet analyze calls to `hash` on non-{string,integer,bool}"

  Pact steps -> local (inPact .~ sTrue) $ do
    -- We execute through all the steps once (via a left fold), then we execute
    -- all the rollbacks (via for), in reverse order.

    (rollbacks, _) <- ifoldlM
      (\stepNo (rollbacks, successChoice)
        --                 ^ do we make it to this step?
        (Step (tm :< ty) successPath {- s_n -} mEntity mCancelVid mRollback) ->
        withSing ty $ do

        -- update yielded values
        previouslyYielded <- use $ latticeState . lasYieldedInCurrent
        latticeState . lasYieldedInPrevious .= previouslyYielded
        latticeState . lasYieldedInCurrent  .= Nothing

        -- The first step has no cancel var. All other steps do.
        cancel <- case mCancelVid of

          -- We can never cancel before executing the first step
          Nothing -> pure sFalse

          -- ... otherwise, we nondeterministically either succeed or cancel
          Just (cancelPath {- c_n -}, cancelVid) -> withReset stepNo $ do
            cancel <- view (aeStepChoices . at cancelVid)
              ??? "couldn't find cancel var"

            tagFork successPath cancelPath (sansProv successChoice)
              (sansProv $ sNot cancel)
            tagCancel (_pathTag cancelPath) cancel

            pure $ successChoice .&& cancel

        let -- Trigger the latest rollback if we cancel on this step
            rollbacks' = rollbacks & _head . _2 %~ (.|| cancel)
            rollbacks'' = case mRollback of
              Nothing
                -> rollbacks'
              Just (rollbackPath {- r_n -}, rollback)
                -> (rollbackPath, sFalse, rollback):rollbacks'
                --                ^ should we run this rollback, and every
                --                  rollback before it?

            reachesStep = successChoice .&& sNot cancel

        ite reachesStep
          (void $ evalTermWithEntity mEntity tm)
          (pure ())

        pure (rollbacks'', reachesStep))
      ([], sTrue)
      steps

    -- clear yields before evaluating rollbacks. (though there shouldn't be any
    -- resumes in the rollbacks anyway)
    latticeState . lasYieldedInPrevious .= Nothing
    latticeState . lasYieldedInCurrent  .= Nothing

    void $ foldlM
      (\alreadyRollingBack
        (path {- r_n -}, rollbackTriggered, rollback) -> do
        -- If this rollback was triggered, we execute this and all earlier
        -- rollbacks
        let nowRollingBack = alreadyRollingBack .|| rollbackTriggered
        tagSubpathStart path $ sansProv nowRollingBack
        ite nowRollingBack
          (void $ evalETerm rollback)
          (pure ())
        pure nowRollingBack)
      sFalse
      rollbacks

    pure "INTERNAL: pact done"

  Yield tid tm -> do
    sVal@(S prov val) <- eval tm
    tagYield tid $ mkAVal sVal
    let ty = sing :: SingTy a
    withSymVal ty $
      latticeState . lasYieldedInCurrent ?= PossibleVal ty (S prov (SBV.sJust val))
    pure sVal

  Resume tid -> use (latticeState . lasYieldedInPrevious) >>= \case
    Nothing -> throwErrorNoLoc "No previously yielded value for resume"
    Just (PossibleVal ty (S prov mVal)) -> case singEq ty (sing :: SingTy a) of
      Nothing   -> throwErrorNoLoc "Resume of unexpected type"
      Just Refl -> withSymVal ty $ do
        markFailure $ SBV.isNothing mVal
        tagResume tid $ mkAVal' $ SBV.fromJust mVal
        pure $ S prov $ SBV.fromJust mVal

-- | Private pacts must be evaluated by the right entity. Fail if the current
-- entity doesn't match the provided.
evalTermWithEntity
  :: SingI a => Maybe (Term 'TyStr) -> Term a -> Analyze (S (Concrete a))
evalTermWithEntity mEntity tm = do
  case mEntity of
    Nothing -> pure ()
    Just expectedEntity -> do
      actualEntity    <- view currentEntity
      expectedEntity' <- eval expectedEntity
      markFailure $ actualEntity ./= expectedEntity'
  evalTerm tm

-- | Reset to perform between different transactions.
--
-- This encompasses anything in 'AnalyzeState' or 'AnalyzeEnv' that could have
-- changed between transactions.
withReset :: Int -> Analyze a -> Analyze a
withReset number action = do
  oldState <- use id
  oldEnv   <- ask

  trivialGuard <- view aeTrivialGuard

  let tables = oldEnv ^. aeTables
      txMetadata   = TxMetadata (mkFreeArray $ "txKeySets"  <> tShow number)
                                (mkFreeArray $ "txDecimals" <> tShow number)
                                (mkFreeArray $ "txIntegers" <> tShow number)
                                (mkFreeArray $ "txStrings"  <> tShow number)

      newRegistry = Registry $ mkFreeArray $ "registry" <> tShow number
      newGuardPasses = writeArray (mkFreeArray $ "guardPasses" <> tShow number)
        (_sSbv trivialGuard) sTrue

      -- If `rowExists` is already set to true, then it won't change. If it's
      -- set to false, this will unset it.
      updateTableMap (TableMap tm) = TableMap $ flip Map.mapWithKey tm $
        \(TableName tn) sFunArr ->
          let newArr = mkFreeArray $
                "row_exists__" <> T.pack tn <> "_" <> tShow number
          in eitherArray sFunArr newArr

      newState = oldState
        & latticeState . lasExtra . cvTableCells .~ mkSymbolicCells tables
        & latticeState . lasExtra . cvRowExists  %~ updateTableMap
        & latticeState . lasGuardPasses          .~ newGuardPasses
      newEnv = oldEnv
        & aePactMetadata . pmEntity .~ uninterpretS "entity"
        & aeRegistry                .~ newRegistry
        & aeTxMetadata              .~ txMetadata

  id .= newState
  local (analyzeEnv .~ newEnv) action

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
