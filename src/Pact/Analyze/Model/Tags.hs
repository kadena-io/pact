{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
module Pact.Analyze.Model.Tags
  ( allocArgs
  , allocModelTags
  , saturateModel
  ) where

import           Control.Lens         (Traversal', toListOf, traverseOf,
                                       traversed, (<&>), (?~), (^.), _1, _2,
                                       _3)
import           Control.Monad        ((>=>))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.SBV             (SBV, SymWord)
import qualified Data.SBV             as SBV
import qualified Data.SBV.Control     as SBV
import qualified Data.SBV.Internals   as SBVI
import           Data.Traversable     (for)

import qualified Pact.Types.Typecheck as TC

import           Pact.Analyze.Alloc   (Alloc, free)
import           Pact.Analyze.Types

allocS :: SymWord a => Alloc (S a)
allocS = free

allocSbv :: SymWord a => Alloc (SBV a)
allocSbv = _sSbv <$> allocS

allocSchema :: Schema -> Alloc Object
allocSchema (Schema fieldTys) = Object <$>
  for fieldTys (\ety -> (ety,) <$> allocAVal ety)

allocAVal :: EType -> Alloc AVal
allocAVal = \case
  EObjectTy schema -> AnObj <$> allocSchema schema
  EType (_ :: Type t) -> mkAVal <$> (allocS :: Alloc (S t))

allocTVal :: EType -> Alloc TVal
allocTVal ety = (ety,) <$> allocAVal ety

allocForETerm :: ETerm -> Alloc TVal
allocForETerm (existentialType -> ety) = allocTVal ety

allocArgs :: [Arg] -> Alloc (Map VarId (Located (Unmunged, TVal)))
allocArgs args = fmap Map.fromList $ for args $ \(Arg nm vid node ety) -> do
  let info = node ^. TC.aId . TC.tiInfo
  av <- allocAVal ety <&> _AVal._1 ?~ FromInput nm
  pure (vid, Located info (nm, (ety, av)))

allocModelTags
  :: Map VarId (Located (Unmunged, TVal))
  -> Located ETerm
  -> ExecutionGraph
  -> Alloc (ModelTags 'Symbolic)
allocModelTags argsMap locatedTm graph = ModelTags
    <$> allocVars
    <*> allocReads
    <*> allocWrites
    <*> allocAsserts
    <*> allocAuths
    <*> allocResult
    <*> allocPaths
    <*> allocReturns

  where
    -- For the purposes of symbolic value allocation, we just grab all of the
    -- events from the graph indiscriminately:
    events :: [TraceEvent]
    events = toListOf (egEdgeEvents.traverse.traverse) graph

    -- We only allocate variables here for non-arguments; that is, variables
    -- which are intermediate bindings. Arguments have already been allocated
    -- at this point (by 'allocArgs', with 'FromInput' provenance attached),
    -- and we just reuse those allocations as we come across them in
    -- 'TracePushScope' events.
    allocVars :: Alloc (Map VarId (Located (Unmunged, TVal)))
    allocVars = fmap Map.fromList $
      for (toListOf (traverse._TracePushScope._3.traverse) events) $
        \(Located info (Binding vid nm _ ety)) ->
          case Map.lookup vid argsMap of
            Nothing -> allocTVal ety <&> \tv -> (vid, Located info (nm, tv))
            Just arg -> pure (vid, arg)

    allocAccesses
      :: Traversal' TraceEvent (Schema, Located TagId)
      -> Alloc (Map TagId (Located Access))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) events) $ \(schema, Located info tid) -> do
        srk <- allocS
        obj <- allocSchema schema
        suc <- allocSbv
        pure (tid, Located info (Access srk obj suc))

    allocReads :: Alloc (Map TagId (Located Access))
    allocReads = allocAccesses _TraceRead

    allocWrites :: Alloc (Map TagId (Located Access))
    allocWrites = allocAccesses traceWriteT

      where
        traceWriteT :: Traversal' TraceEvent (Schema, Located TagId)
        traceWriteT f event = case event of
          TraceWrite _writeType schema tid -> const event <$> f (schema, tid)
          _                                -> pure event

    allocAsserts :: Alloc (Map TagId (Located (SBV Bool)))
    allocAsserts = fmap Map.fromList $
      for (toListOf (traverse._TraceAssert._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$> allocSbv

    allocAuths :: Alloc (Map TagId (Located Authorization))
    allocAuths = fmap Map.fromList $
      for (toListOf (traverse._TraceAuth._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$> (Authorization <$> allocS <*> allocSbv)

    allocResult :: Alloc (TagId, Located TVal)
    allocResult = do
      let tid :: TagId = last $ toListOf (traverse._TracePopScope._3) events
      fmap (tid,) $ sequence $ allocForETerm <$> locatedTm

    -- NOTE: the root path we manually set to true. translation only emits the
    -- start of "subpaths" on either side of a conditional. the root path is
    -- always trivially reachable, because it corresponds to the start of a
    -- program.
    allocPaths :: Alloc (Map Path (SBV Bool))
    allocPaths = do
      let rootPath = _egRootPath graph
          paths    = rootPath : toListOf (traverse._TraceSubpathStart) events
      Map.fromList <$> for paths (\p -> (p,) <$> allocSbv)

    allocReturns :: Alloc (Map TagId TVal)
    allocReturns = fmap Map.fromList $
      for (toListOf (traverse._TracePopScope) events) $ \(_, _, tid, ety) ->
        (tid,) <$> allocTVal ety

-- | Builds a new 'Model' by querying the SMT model to concretize the provided
-- symbolic 'Model'.
saturateModel :: Model 'Symbolic -> SBV.Query (Model 'Concrete)
saturateModel =
    traverseOf (modelArgs.traversed.located._2)        fetchTVal   >=>
    traverseOf (modelTags.mtVars.traversed.located._2) fetchTVal   >=>
    traverseOf (modelTags.mtReads.traversed.located)   fetchAccess >=>
    traverseOf (modelTags.mtWrites.traversed.located)  fetchAccess >=>
    traverseOf (modelTags.mtAsserts.traversed.located) fetchSbv    >=>
    traverseOf (modelTags.mtAuths.traversed.located)   fetchAuth   >=>
    traverseOf (modelTags.mtResult._2.located)         fetchTVal   >=>
    traverseOf (modelTags.mtPaths.traversed)           fetchSbv    >=>
    traverseOf (modelTags.mtReturns.traversed)         fetchTVal   >=>
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

    fetchAccess :: Access -> SBV.Query Access
    fetchAccess (Access sRk obj suc) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      suc' <- fetchSbv suc
      pure $ Access sRk' obj' suc'

    fetchAuth :: Authorization -> SBV.Query Authorization
    fetchAuth (Authorization sKs sbool) = Authorization <$>
      fetchS sKs <*> fetchSbv sbool

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromNamedKs         fetchS
