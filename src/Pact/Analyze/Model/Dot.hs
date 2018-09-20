{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pact.Analyze.Model.Dot
  ( compileDot
  , renderDot
  ) where

import qualified Algebra.Graph            as Alga
import qualified Algebra.Graph.Export.Dot as Alga
import           Algebra.Graph.Export.Dot (Style (..), Attribute ((:=)))
import           Control.Lens             ((^.))
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text.IO             as Text

import           Pact.Types.Util          (tShow)

import qualified Pact.Analyze.Model.Graph as Model
import           Pact.Analyze.Types       (Concreteness (Concrete), Edge,
                                           Model, Path, Vertex, _pathTag,
                                           modelExecutionGraph, egGraph,
                                           egPathEdges)

-- | Compile to DOT format
compileDot :: Model 'Concrete -> Text
compileDot m = Alga.export style graph
  where
    graph :: Alga.Graph Vertex
    graph = m ^. modelExecutionGraph.egGraph

    reachableEdges :: Set Edge
    reachableEdges = Model.reachableEdges m

    reachable :: Edge -> Bool
    reachable = flip Set.member reachableEdges

    edgePaths :: Map Edge Path
    edgePaths = Map.fromList $ do
      (path, edges) <- Map.toList $ m ^. modelExecutionGraph.egPathEdges
      (,path) <$> edges

    style :: Alga.Style Vertex Text
    style = Alga.Style
      { graphName =
          mempty
      , preamble =
          mempty
      , graphAttributes =
          []
      , defaultVertexAttributes =
          [ "shape" := "circle"
          , "style" := "filled"
          ]
      , defaultEdgeAttributes =
          []
      , vertexName =
          tShow . fromEnum
      , vertexAttributes =
          const []
      , edgeAttributes = curry $ \e ->
          [ "color" := "blue"
          | reachable e
          ] ++
          [ "label" := tShow (fromEnum $ _pathTag $ edgePaths Map.! e)
          | True -- show path id on edge?
          ]
      }

-- | Render to a DOT file
renderDot :: FilePath -> Model 'Concrete -> IO ()
renderDot fp m = Text.writeFile fp $ compileDot m
