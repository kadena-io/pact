{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | 'Symbolic' allocation of quantified variables for arguments and tags,
-- for use prior to evaluation; and functions to saturate and show models from
-- Z3 post-evaluation.
module Pact.Analyze.Model
  ( allocArgs
  , allocModelTags
  , linearizedTrace
  , reachableEdges
  , saturateModel
  , showModel
  ) where

import           Control.Lens         (Lens', Prism', Traversal', at, ifoldr,
                                       to, toListOf, traverseOf, traversed,
                                       (<&>), (?~), (^.), (^?), _1, _2, _Just)
import           Control.Monad        (when, (>=>))
import           Data.Bool            (bool)
import qualified Data.Foldable        as Foldable
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Monoid          ((<>))
import           Data.SBV             (SBV, SymWord, Symbolic)
import qualified Data.SBV             as SBV
import qualified Data.SBV.Control     as SBV
import qualified Data.SBV.Internals   as SBVI
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable     (for)

import qualified Pact.Types.Info      as Pact
import           Pact.Types.Runtime   (tShow)
import qualified Pact.Types.Typecheck as TC

import           Pact.Analyze.Types

alloc :: SymWord a => Symbolic (SBV a)
alloc = SBV.free_

allocSchema :: Schema -> Symbolic Object
allocSchema (Schema fieldTys) = Object <$>
  for fieldTys (\ety -> (ety,) <$> allocAVal ety)

allocAVal :: EType -> Symbolic AVal
allocAVal = \case
  EObjectTy schema -> AnObj <$> allocSchema schema
  EType (_ :: Type t) -> mkAVal . sansProv <$>
    (alloc :: Symbolic (SBV t))

allocArgs :: [Arg] -> Symbolic (Map VarId (Located (Text, TVal)))
allocArgs args = fmap Map.fromList $ for args $ \(Arg nm vid node ety) -> do
  let info = node ^. TC.aId . TC.tiInfo
  av <- allocAVal ety <&> _AVal._1 ?~ FromInput nm
  pure (vid, Located info (nm, (ety, av)))

allocModelTags :: Located ETerm -> ExecutionGraph -> Symbolic (ModelTags 'Symbolic)
allocModelTags locatedTm graph = ModelTags
    <$> allocVars
    <*> allocReads
    <*> allocWrites
    <*> allocAsserts
    <*> allocAuths
    <*> allocResult
    <*> allocPaths

  where
    -- For the purposes of symbolic value allocation, we just grab all of the
    -- events from the graph indiscriminately:
    events :: [TraceEvent]
    events = toListOf (egEdgeEvents.traverse.traverse) graph

    allocVars :: Symbolic (Map VarId (Located (Text, TVal)))
    allocVars = fmap Map.fromList $
      for (toListOf (traverse._TraceBind) events) $
        \(Located info (vid, nm, ety)) ->
          allocAVal ety <&> \av -> (vid, Located info (nm, (ety, av)))

    allocS :: SymWord a => Symbolic (S a)
    allocS = sansProv <$> alloc

    allocAccesses
      :: Prism' TraceEvent (Located (TagId, Schema))
      -> Symbolic (Map TagId (Located (S RowKey, Object)))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) events) $ \(Located info (tid, schema)) -> do
        srk <- allocS
        obj <- allocSchema schema
        pure (tid, Located info (srk, obj))

    allocReads :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocReads = allocAccesses _TraceRead

    allocWrites :: Symbolic (Map TagId (Located (S RowKey, Object)))
    allocWrites = allocAccesses _TraceWrite

    allocAsserts :: Symbolic (Map TagId (Located (SBV Bool)))
    allocAsserts = fmap Map.fromList $
      for (toListOf (traverse._TraceAssert._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$> alloc

    allocAuths :: Symbolic (Map TagId (Located (S KeySet, SBV Bool)))
    allocAuths = fmap Map.fromList $
      for (toListOf (traverse._TraceAuth._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$> ((,) <$> allocS <*> alloc)

    allocResult :: Symbolic (Located TVal)
    allocResult = sequence $ locatedTm <&> \case
      ESimple ty _ ->
        let ety = EType ty
        in (ety,) <$> allocAVal ety
      EObject sch _ ->
        (EObjectTy sch,) . AnObj <$> allocSchema sch

    -- NOTE: the root path we manually set to true. translation only emits the
    -- start of "subpaths" on either side of a conditional. the root path is
    -- always trivially reachable, because it corresponds to the start of a
    -- program.
    allocPaths :: Symbolic (Map TagId (SBV Bool))
    allocPaths = do
      let rootPath = _egRootPath graph
          paths    = rootPath : toListOf (traverse._TraceSubpathStart) events
      fmap Map.fromList $
        for paths $ \tid -> do
          sbool <- alloc
          when (tid == rootPath) $ SBV.constrain sbool
          pure (tid, sbool)

linearizedTrace :: Model 'Concrete -> ExecutionTrace
linearizedTrace model = mkTrace traceEvents

  where
    mkTrace :: [TraceEvent] -> ExecutionTrace
    mkTrace = foldr
      (\event (ExecutionTrace futureEvents mRes) ->
        let continue = ExecutionTrace (event : futureEvents) mRes
            stop     = ExecutionTrace [event] Nothing

            handleEnforce
              :: Recoverability
              -> Traversal' (ModelTags 'Concrete) (SBV Bool)
              -> ExecutionTrace
            handleEnforce recov tagsBool =
              let mPassesEnforce =
                    model ^? modelTags.tagsBool.to (SBV.unliteral)._Just
                 in case mPassesEnforce of
                   Nothing ->
                     error "impossible: missing enforce tag, or symbolic value"
                   Just False ->
                     case recov of
                       Recoverable _resumptionPath ->
                         --
                         -- TODO: instead of just continuing, we should
                         -- actually skip future events (in the same case)
                         -- until we hit this "resumption path".  this would
                         -- produce better output for cases with any events
                         -- after a failed enforce:
                         --
                         --   (enforce-one
                         --     [(let ((x (enforce false)))
                         --        (enforce true)) ; <- we should not see this
                         --      true
                         --      ])
                         --
                         continue
                       Unrecoverable ->
                         stop
                   Just True ->
                     continue
        in case event of
             TraceAssert recov (_located -> tid) ->
               handleEnforce recov $ mtAsserts.at tid._Just.located
             TraceAuth recov (_located -> tid) ->
               handleEnforce recov $ mtAuths.at tid._Just.located._2
             _ ->
               continue)
      (ExecutionTrace [] (Just $ model ^. modelTags.mtResult.located))

    -- NOTE: 'Map' is ordered, so our @(Vertex, Vertex)@ 'Edge' representation
    -- over monotonically increasing 'Vertex's across the execution graph
    -- yields a topological sort. Additionally the 'TraceEvent's on each 'Edge'
    -- are ordered, so we now have a linear trace of events. But we still have
    -- the possibility of 'TraceAssert' and 'TraceAuth' affecting control flow.
    traceEvents :: [TraceEvent]
    traceEvents = concat $ restrictKeys edgeEvents (reachableEdges model)

    edgeEvents :: Map Edge [TraceEvent]
    edgeEvents = model ^. modelExecutionGraph.egEdgeEvents

-- TODO: use Map.restrictKeys once using containers >= 0.5.8
restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys m kset = Map.filterWithKey (\k _v -> k `Set.member` kset) m

reachablePaths :: Model 'Concrete -> Set TagId
reachablePaths model = Map.foldlWithKey'
  (\paths path sbool -> maybe
    (error $ "impossible: found symbolic value in concrete model for path "
          <> show path)
    (bool paths (Set.insert path paths))
    (SBV.unliteral sbool))
  Set.empty
  (model ^. modelTags.mtPaths)

reachableEdges :: Model 'Concrete -> Set Edge
reachableEdges model = Set.fromList . concat $
    restrictKeys pathEdges (reachablePaths model)

  where
    pathEdges :: Map TagId [Edge]
    pathEdges = model ^. modelExecutionGraph.egPathEdges

indent :: Text -> Text
indent = ("  " <>)

showSbv :: (Show a, SymWord a) => SBV a -> Text
showSbv sbv = maybe "[ERROR:symbolic]" tShow (SBV.unliteral sbv)

showS :: (Show a, SymWord a) => S a -> Text
showS = showSbv . _sSbv

showTVal :: TVal -> Text
showTVal (ety, av) = case av of
  OpaqueVal   -> "[opaque]"
  AnObj obj   -> showObject obj
  AVal _ sval -> case ety of
    EObjectTy _         -> error "showModel: impossible object type for AVal"
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

--
-- TODO: this should display the table name
--
showRead :: Located (S RowKey, Object) -> Text
showRead (Located _ (srk, obj)) = "read " <> showObject obj
                               <> " for key " <> showS srk

--
-- TODO: this should display the table name
--
showWrite :: Located (S RowKey, Object) -> Text
showWrite (Located _ (srk, obj)) = "write " <> showObject obj
                                <> " to key " <> showS srk

showKsn :: S KeySetName -> Text
showKsn sKsn = case SBV.unliteral (_sSbv sKsn) of
  Nothing               -> "[unknown]"
  Just (KeySetName ksn) -> "'" <> ksn

showFailure :: Recoverability -> Text
showFailure = \case
  Recoverable _ -> "recovered from failure"
  Unrecoverable -> "failed"

showAssert :: Recoverability -> Located (SBV Bool) -> Text
showAssert recov (Located (Pact.Info mInfo) lsb) = case SBV.unliteral lsb of
    Nothing    -> "[ERROR:symbolic assert]"
    Just True  -> "satisfied assertion" <> context
    Just False -> showFailure recov <> " to satisfy assertion" <> context

  where
    context = maybe "" (\(Pact.Code code, _) -> ": " <> code) mInfo

showAuth
  :: Recoverability
  -> Maybe Provenance
  -> Located (S KeySet, SBV Bool)
  -> Text
showAuth recov mProv (_located -> (srk, sbool)) =
  status <> " " <> ksDescription

  where
    status = case SBV.unliteral sbool of
      Nothing    -> "[ERROR:symbolic auth]"
      Just True  -> "satisfied"
      Just False -> showFailure recov <> " to satisfy"

    ks :: Text
    ks = showS srk

    ksDescription = case mProv of
      Nothing ->
        "unknown " <> ks
      Just (FromCell (OriginatingCell (TableName tn) (ColumnName cn) sRk _)) ->
        ks <> " from database at ("
          <> T.pack tn <> ", "
          <> "'" <> T.pack cn <> ", "
          <> showS sRk <> ")"
      Just (FromNamedKs sKsn) ->
        ks <> " named " <> showKsn sKsn
      Just (FromInput arg) ->
        ks <> " from argument " <> arg

showEvent :: Map TagId Provenance -> ModelTags 'Concrete -> TraceEvent -> [Text]
showEvent ksProvs tags = \case
    TraceRead (_located -> (tid, _)) ->
      [display mtReads tid showRead]
    TraceWrite (_located -> (tid, _)) ->
      [display mtWrites tid showWrite]
    TraceAssert recov (_located -> tid) ->
      [display mtAsserts tid (showAssert recov)]
    TraceAuth recov (_located -> tid) ->
      [display mtAuths tid (showAuth recov $ tid `Map.lookup` ksProvs)]
    TraceBind (_located -> (vid, _, _)) ->
      [display mtVars vid showVar]
    TraceSubpathStart _ ->
      [] -- not shown to end-users

  where
    display
      :: Ord k
      => Lens' (ModelTags 'Concrete) (Map k v)
      -> k
      -> (v -> Text)
      -> Text
    display l ident f = maybe "[ERROR:missing tag]" f $ tags ^. l.at ident

showModel :: Model 'Concrete -> Text
showModel model =
    T.intercalate "\n" $ T.intercalate "\n" . map indent <$>
      [ ["Arguments:"]
      , indent <$> Foldable.toList (showVar <$> (model ^. modelArgs))
      , []
      , ["Program trace:"]
      , indent <$> (showEvent' =<< traceEvents)
      , []
      , ["Result:"]
      , [indent $ maybe
          "Transaction aborted."
          (\tval -> "Return value: " <> showTVal tval)
          mRetval
        ]
      ]

  where
    ExecutionTrace traceEvents mRetval = linearizedTrace model

    showEvent' = showEvent (model ^. modelKsProvs) (model ^. modelTags)

-- | Builds a new 'Model' by querying the SMT model to concretize the provided
-- symbolic 'Model'.
saturateModel :: Model 'Symbolic -> SBV.Query (Model 'Concrete)
saturateModel =
    traverseOf (modelArgs.traversed.located._2)             fetchTVal   >=>
    traverseOf (modelTags.mtVars.traversed.located._2)      fetchTVal   >=>
    traverseOf (modelTags.mtReads.traversed.located)        fetchAccess >=>
    traverseOf (modelTags.mtWrites.traversed.located)       fetchAccess >=>
    traverseOf (modelTags.mtAsserts.traversed.located)      fetchSbv    >=>
    traverseOf (modelTags.mtAuths.traversed.located)        fetchAuth   >=>
    traverseOf (modelTags.mtResult.located)                 fetchTVal   >=>
    traverseOf (modelTags.mtPaths.traversed)                fetchSbv    >=>
    traverseOf (modelKsProvs.traversed)                     fetchProv

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

    fetchAuth :: (S KeySet, SBV Bool) -> SBV.Query (S KeySet, SBV Bool)
    fetchAuth (sKs, sbool) = (,) <$> fetchS sKs <*> fetchSbv sbool

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromNamedKs         fetchS
