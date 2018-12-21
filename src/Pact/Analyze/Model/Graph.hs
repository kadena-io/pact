{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

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
            stop     = ExecutionTrace [event] Nothing

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
               handleEnforce recov $ mtAuths.at tid._Just.located.authSuccess
             TraceRead _schema (Located _i tid) ->
               handleDbAccess $ mtReads.at tid._Just.located.accSuccess
             TraceWrite _writeType _schema (Located _i tid) ->
               handleDbAccess $ mtWrites.at tid._Just.located.accSuccess
             _ ->
               continue)
      (ExecutionTrace [] (Just $ model ^. modelTags.mtResult._2.located))

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
    restrictKeys pathEdges (reachablePaths model)

  where
    pathEdges :: Map Path [Edge]
    pathEdges = model ^. modelExecutionGraph.egPathEdges
