{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
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
                                       traversed, (<&>), (?~), (^.), _1, _2, _3)
import           Control.Monad        ((>=>))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.SBV             (SBV, SymVal)
import qualified Data.SBV             as SBV
import qualified Data.SBV.Control     as SBV
import qualified Data.SBV.Internals   as SBVI
import           Data.Text            (pack, unpack)
import           Data.Traversable     (for)
import           GHC.TypeLits         (symbolVal)

import qualified Pact.Types.Typecheck as TC

import           Pact.Analyze.Alloc   (Alloc, free, singFree)
import           Pact.Analyze.Types

allocS :: forall a. SingI a => String -> Alloc (S (Concrete a))
allocS name = free @a ("tag_" ++ name)

allocSbv :: forall a. SingI a => String -> Alloc (SBV (Concrete a))
allocSbv name = _sSbv <$> allocS @a name

allocSchema :: SingList schema -> Alloc UObject
allocSchema = fmap UObject . allocSchema' where
  allocSchema' = foldrSingList (pure Map.empty) $ \k ty m -> do
    let ety  = EType ty
        name = symbolVal k
    val <- allocAVal name ety
    Map.insert (pack name) (ety, val) <$> m

allocAVal :: String -> EType -> Alloc AVal
allocAVal name (EType ty) = mkAVal <$> singFree name ty

allocTVal :: String -> EType -> Alloc TVal
allocTVal name ety = (ety,) <$> allocAVal name ety

allocForETerm :: String -> ETerm -> Alloc TVal
allocForETerm name (existentialType -> ety) = allocTVal name ety

unmungedStr :: Unmunged -> String
unmungedStr (Unmunged name) = unpack name

allocArgs :: [Arg] -> Alloc (Map VarId (Located (Unmunged, TVal)))
allocArgs args = fmap Map.fromList $ for args $ \(Arg nm vid node ety) -> do
  let info = node ^. TC.aId . TC.tiInfo
  av <- allocAVal (unmungedStr nm) ety <&> _AVal._1 ?~ FromInput nm
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
    <*> allocGEs
    <*> allocGrantReqs
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
            Nothing  -> allocTVal ("binding_" ++ unmungedStr nm) ety <&>
              \tv -> (vid, Located info (nm, tv))
            Just arg -> pure (vid, arg)

    allocAccesses
      :: Traversal' TraceEvent (ESchema, Located TagId)
      -> Alloc (Map TagId (Located Access))
    allocAccesses p = fmap Map.fromList $
      for (toListOf (traverse.p) events) $
        \(ESchema schema, Located info tid) -> do
          srk <- allocS @TyRowKey "row_key"
          obj <- allocSchema schema
          suc <- allocSbv @'TyBool "access_success"
          pure (tid, Located info (Access srk obj suc))

    allocReads :: Alloc (Map TagId (Located Access))
    allocReads = allocAccesses _TraceRead

    allocWrites :: Alloc (Map TagId (Located Access))
    allocWrites = allocAccesses traceWriteT

      where
        traceWriteT :: Traversal' TraceEvent (ESchema, Located TagId)
        traceWriteT f event = case event of
          TraceWrite _writeType schema tid -> const event <$> f (schema, tid)
          _                                -> pure event

    allocAsserts :: Alloc (Map TagId (Located (SBV Bool)))
    allocAsserts = fmap Map.fromList $
      for (toListOf (traverse._TraceAssert._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$> allocSbv @'TyBool "assert"

    allocGEs :: Alloc (Map TagId (Located GuardEnforcement))
    allocGEs = fmap Map.fromList $
      for (toListOf (traverse._TraceGuard._2) events) $ \(Located info tid) ->
        (tid,) . Located info <$>
          (GuardEnforcement
            <$> allocS @'TyGuard "guard"
            <*> allocSbv @'TyBool "guard_success")

    allocGrantReqs :: Alloc (Map TagId (Located GrantRequest))
    allocGrantReqs = fmap Map.fromList $
      for (toListOf (traverse._TraceRequireGrant) events) $ \(_, capName, _, Located info tid) ->
        (tid,) . Located info <$>
          (GrantRequest capName <$> allocSbv @'TyBool "grant_request")

    allocResult :: Alloc (TagId, Located TVal)
    allocResult = do
      let tid :: TagId = last $ toListOf (traverse._TracePopScope._3) events
      fmap (tid,) $ sequence $ allocForETerm "result" <$> locatedTm

    -- NOTE: the root path we manually set to true. translation only emits the
    -- start of "subpaths" on either side of a conditional. the root path is
    -- always trivially reachable, because it corresponds to the start of a
    -- program.
    allocPaths :: Alloc (Map Path (SBV Bool))
    allocPaths = do
      let rootPath = _egRootPath graph
          paths    = rootPath : toListOf (traverse._TraceSubpathStart) events
      Map.fromList <$> for paths (\p -> (p,) <$> allocSbv @'TyBool "path")

    allocReturns :: Alloc (Map TagId TVal)
    allocReturns = fmap Map.fromList $
      for (toListOf (traverse._TracePopScope) events) $ \(_, _, tid, ety) ->
        (tid,) <$> allocTVal "trace_pop_scope" ety

-- | Builds a new 'Model' by querying the SMT model to concretize the provided
-- symbolic 'Model'.
saturateModel :: Model 'Symbolic -> SBV.Query (Model 'Concrete)
saturateModel =
    traverseOf (modelArgs.traversed.located._2)                  fetchTVal   >=>
    traverseOf (modelTags.mtVars.traversed.located._2)           fetchTVal   >=>
    traverseOf (modelTags.mtReads.traversed.located)             fetchAccess >=>
    traverseOf (modelTags.mtWrites.traversed.located)            fetchAccess >=>
    traverseOf (modelTags.mtAsserts.traversed.located)           fetchSbv    >=>
    traverseOf (modelTags.mtGuardEnforcements.traversed.located) fetchGE     >=>
    traverseOf (modelTags.mtGrantRequests.traversed.located)     fetchGR     >=>
    traverseOf (modelTags.mtResult._2.located)                   fetchTVal   >=>
    traverseOf (modelTags.mtPaths.traversed)                     fetchSbv    >=>
    traverseOf (modelTags.mtReturns.traversed)                   fetchTVal   >=>
    traverseOf (modelGuardProvs.traversed)                       fetchProv

  where
    fetchTVal :: TVal -> SBV.Query TVal
    fetchTVal (ety, av) = (ety,) <$> go ety av
      where
        go :: EType -> AVal -> SBV.Query AVal
        go (EType (ty :: SingTy t)) (AVal _mProv sval)
          = withSymVal ty $ withSMTValue ty $
            mkAVal' . SBV.literal
              <$> SBV.getValue (SBVI.SBV sval :: SBV (Concrete t))
        go _ OpaqueVal = pure OpaqueVal

    -- NOTE: This currently rebuilds an SBV. Not sure if necessary.
    fetchSbv :: (SymVal a, SBV.SMTValue a) => SBV a -> SBV.Query (SBV a)
    fetchSbv = fmap SBV.literal . SBV.getValue

    fetchS :: (SymVal a, SBV.SMTValue a) => S a -> SBV.Query (S a)
    fetchS = traverseOf s2Sbv fetchSbv

    fetchObject :: UObject -> SBVI.QueryT IO UObject
    fetchObject (UObject fields) = UObject <$> traverse fetchTVal fields

    fetchAccess :: Access -> SBV.Query Access
    fetchAccess (Access sRk obj suc) = do
      sRk' <- fetchS sRk
      obj' <- fetchObject obj
      suc' <- fetchSbv suc
      pure $ Access sRk' obj' suc'

    fetchGE :: GuardEnforcement -> SBV.Query GuardEnforcement
    fetchGE (GuardEnforcement sg sbool) = GuardEnforcement <$>
      fetchS sg <*> fetchSbv sbool

    fetchGR :: GrantRequest -> SBV.Query GrantRequest
    fetchGR (GrantRequest capName sb) = GrantRequest capName <$> fetchSbv sb

    fetchProv :: Provenance -> SBV.Query Provenance
    fetchProv = traverseOf (_FromCell.ocRowKey) fetchS
            >=> traverseOf _FromRegistry        fetchS
            >=> traverseOf _FromMetadata        fetchS
