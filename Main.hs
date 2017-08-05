{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Args
import           Data.GraphViz
import           Data.GraphViz.Printing
import qualified Data.GraphViz.Types.Generalised as G
import           Data.List (find, partition)
import           Data.Maybe
import           Data.Sequence ((><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as L


main:: IO ()
main = runWithArgs $ \Args{..} -> do
  xDotText <- L.getContents
  let xDotGraph = parseDotGraphLiberally xDotText :: G.DotGraph Text

  let matchNodeId nid = isJust $ find (`T.isInfixOf` nid) argsKeywords

  let newEdges = filterEdges matchNodeId Set.empty [] (graphEdges xDotGraph)
      newNodeIds = Set.fromList $ (fromNode <$> newEdges) ++ (toNode <$> newEdges)
      newNodes = filter (\DotNode{..} -> Set.member nodeID newNodeIds) $ graphNodes xDotGraph

  L.putStrLn $ renderDot $ toDot $ xDotGraph {
    G.graphStatements =
        Seq.fromList (G.DE <$> newEdges) ><
        Seq.fromList (G.DN <$> newNodes)
  }


filterEdges:: (Text -> Bool) -- ^ matches nodes by ID
           -> Set Text -- ^ vertices to keep
           -> [DotEdge Text] -- ^ edges to keep
           -> [DotEdge Text] -- ^ edges to process
           -> [DotEdge Text]
filterEdges matchNodeId vs es es' =
  let (m,nm) = partition isMatchingEdge es'
   in if null m
      then es
      else filterEdges
             matchNodeId
             (Set.union vs (Set.fromList ((toNode <$> m) ++ (fromNode <$> m))))
             (es ++ m)
             nm

  where isMatchingEdge DotEdge{..} =
          matchNodeId toNode || Set.member toNode vs
