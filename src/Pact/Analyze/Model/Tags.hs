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
module Pact.Analyze.Model.Tags
  ( allocArgs
  , allocModelTags
  , saturateModel
  ) where

import           Control.Lens            (Prism', toListOf, traverseOf,
                                          traversed, (<&>), (?~), (^.), _1, _2)
import           Control.Monad           (when, (>=>))
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.SBV                (SBV, SymWord, Symbolic)
import qualified Data.SBV                as SBV
import qualified Data.SBV.Control        as SBV
import qualified Data.SBV.Internals      as SBVI
import           Data.Text               (Text)
import           Data.Traversable        (for)

import qualified Pact.Types.Typecheck    as TC

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
      -> Symbolic (Map TagId (Located Access))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) events) $ \(Located info (tid, schema)) -> do
        srk <- allocS
        obj <- allocSchema schema
        pure (tid, Located info (Access srk obj))

    allocReads :: Symbolic (Map TagId (Located Access))
    allocReads = allocAccesses _TraceRead

    allocWrites :: Symbolic (Map TagId (Located Access))
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
    allocPaths :: Symbolic (Map Path (SBV Bool))
    allocPaths = do
      let rootPath = _egRootPath graph
          paths    = rootPath : toListOf (traverse._TraceSubpathStart) events
      fmap Map.fromList $
        for paths $ \p -> do
          sbool <- alloc
          when (p == rootPath) $ SBV.constrain sbool
          pure (p, sbool)

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

    fetchAccess :: Access -> SBV.Query Access
    fetchAccess (Access sRk obj) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      pure $ Access sRk' obj'

    fetchAuth :: (S KeySet, SBV Bool) -> SBV.Query (S KeySet, SBV Bool)
    fetchAuth (sKs, sbool) = (,) <$> fetchS sKs <*> fetchSbv sbool

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromNamedKs         fetchS
