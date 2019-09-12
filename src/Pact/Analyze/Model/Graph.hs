{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Converts a concrete model and its execution graph to a linearized
-- execution trace. This is converted to textual output in
-- 'Pact.Analyze.Model.Text'.
module Pact.Analyze.Model.Graph
  ( reachablePaths
  , reachableEdges
  , linearize
  ) where

import           Control.Lens       (Traversal', at, to, (^.), (^?), _2, _Just)
import           Data.Bool          (bool)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.SBV           (SBV)
import qualified Data.SBV           as SBV
import           Data.Set           (Set)
import qualified Data.Set           as Set

import           Pact.Analyze.Types

linearize :: Model 'Concrete -> ExecutionTrace
linearize model = go traceEvents
  where
    go :: [TraceEvent] -> ExecutionTrace
    go = foldr
      (\event (ExecutionTrace futureEvents mRes) ->
        let continue = ExecutionTrace (event : futureEvents) mRes
            continueUntil path = ExecutionTrace
              (event : dropWhile (/= TraceSubpathStart path) futureEvents)
              mRes
            stop = ExecutionTrace [event] Nothing

            handleDbAccess
              :: Traversal' (ModelTags 'Concrete) (SBV Bool)
              -> ExecutionTrace
            handleDbAccess tagsBool =
              let mSucceeds = model ^? modelTags . tagsBool . to SBV.unliteral . _Just
              in case mSucceeds of
                   Nothing    ->
                     error "impossible: missing db access tag, or symbolic value"
                   Just False -> stop
                   Just True  -> continue

            handleEnforce
              :: Recoverability
              -> Traversal' (ModelTags 'Concrete) (SBV Bool)
              -> ExecutionTrace
            handleEnforce recov tagsBool =
              let mPassesEnforce =
                    model ^? modelTags . tagsBool . to SBV.unliteral . _Just
                 in case mPassesEnforce of
                   Nothing ->
                     error "impossible: missing enforce tag, or symbolic value"
                   Just False ->
                     case recov of
                       Recoverable resumptionPath ->
                         continueUntil resumptionPath
                       Unrecoverable ->
                         stop
                   Just True ->
                     continue
        in case event of
             TraceAssert recov (_located -> tid) ->
               handleEnforce recov $ mtAsserts.at tid._Just.located
             TraceGuard recov (_located -> tid) ->
               handleEnforce recov $
                 mtGuardEnforcements.at tid._Just.located.geSuccess
             TraceRequireGrant recov _ _ (_located -> tid) ->
               handleEnforce recov $
                 mtGrantRequests.at tid._Just.located.grSuccess
             TraceRead _tname _schema (Located _i tid) ->
               handleDbAccess $ mtReads.at tid._Just.located.accSuccess
             TraceWrite _writeType _tname _schema (Located _i tid) ->
               handleDbAccess $ mtWrites.at tid._Just.located.accSuccess
             _ ->
               continue)
      (ExecutionTrace [] (Just $ model ^. modelTags.mtResult._2.located))

    -- NOTE: 'Map' is ordered, so our @(Vertex, Vertex)@ 'Edge' representation
    -- over monotonically increasing 'Vertex's across the execution graph
    -- yields a topological sort. Additionally the 'TraceEvent's on each 'Edge'
    -- are ordered, so we now have a linear trace of events. But we still have
    -- the possibility of events like 'TraceAssert' and 'TraceGuard' affecting
    -- control flow.
    traceEvents :: [TraceEvent]
    traceEvents = concat $ Map.restrictKeys edgeEvents (reachableEdges model)

    edgeEvents :: Map Edge [TraceEvent]
    edgeEvents = model ^. modelExecutionGraph.egEdgeEvents

reachablePaths :: Model 'Concrete -> Set Path
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
    Map.restrictKeys pathEdges (reachablePaths model)

  where
    pathEdges :: Map Path [Edge]
    pathEdges = model ^. modelExecutionGraph.egPathEdges
