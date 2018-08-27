module Lina.Graphy (Edge, Graph, aGraph2Gr, mkImage, module Data.GraphViz) where

import Data.List    
import qualified Data.Text.Lazy as T    
import qualified Data.Text.Internal.Lazy as L
import qualified Data.Graph.Inductive.Graph as I
import Data.Graph.Inductive.PatriciaTree    
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

type Node n = n    
type Edge n e = (n,n,e)    
type Graph n e = [Edge n e]

-- Label instances:
instance Labellable () where
  toLabelValue () = toLabelValue T.empty
    
lookupNode :: Eq n => n -> [I.LNode n] -> Maybe Int
lookupNode _ [] = Nothing
lookupNode n ((n',l'):ns) | n == l' = Just n'
                          | otherwise = lookupNode n ns

nodesGr :: [Node n] -> [I.LNode n]
nodesGr nodes = zip [1..numNodes] nodes
 where
   numNodes = length nodes
            
edgeGr :: Eq n => [Node n] -> Edge n e -> I.LEdge e
edgeGr nodes (n1, n2, l) = case (n' , n'') of
                             (Nothing, _) -> error "Node in edge that is not in set of nodes."
                             (_, Nothing) -> error "Node in edge that is not in set of nodes."
                             (Just n3, Just n4) -> (n3,n4,l)
                                               
    where
      nodes' = nodesGr nodes
      n' = lookupNode n1 nodes'
      n'' = lookupNode n2 nodes'
             
edgesGr :: Eq n => [Node n] -> [Edge n e] -> [I.LEdge e]
edgesGr nodes edges = map (edgeGr nodes) edges
                         
aGraph2Gr :: ((Eq n), (Eq e)) => Graph n e -> Gr n e
aGraph2Gr edges = I.mkGraph ns es
  where
    nodes = nub $ foldr (\(x,y,_) r -> x:y:r) [] edges
    ns = nodesGr nodes
    es = edgesGr nodes edges
                         
mkDotGraph :: (Ord e, Labellable n, Labellable e)
              => Double -- Node size
              -> Double -- Node separation
              -> Double -- Distance of the edge label from the edge itself
              -> Gr n e
              -> DotGraph I.Node
mkDotGraph nodeSize nodeSep edgeLDist = setDirectedness graphToDot params
 where
   params :: (Labellable n, Labellable e) => GraphvizParams I.Node n e () n
   params = blankParams { isDirected = False
                        , globalAttributes = [EdgeAttrs [edgeEnds NoDir,
                                                        LabelDistance edgeLDist],
                                              NodeAttrs [shape Circle,
                                                         Width nodeSize,
                                                         FixedSize SetNodeSize],
                                             GraphAttrs [Size (GSize 100.0 Nothing True),
                                                         RankDir FromLeft,
                                                         NodeSep nodeSep]]
                        , clusterBy = N
                        , isDotCluster = const False
                        , clusterID = const $ Str L.empty
                        , fmtCluster = const []
                        , fmtNode = \(_,l) -> [Label $ toLabelValue l]
                        , fmtEdge = \(_,_,l) -> [Label $ toLabelValue l]
                        }
            
mkImage :: (Eq n, Eq e, Ord e, Labellable n, Labellable e)
           => GraphvizCommand  -- Output algorithm: Fdp, Dot
           -> GraphvizOutput   -- Output format: Jpeg, Png, DotOutput
           -> Double           -- Node size
           -> Double           -- Node separation
           -> Double           -- Distance of the edge label from the edge itself
           -> Graph n e
           -> FilePath         -- Output file path
           -> IO FilePath
mkImage oc ot nodeSize nodeSep edgeLDist graph ofile =
    runGraphvizCommand oc (mkDotGraph nodeSize nodeSep edgeLDist gr) ot ofile
 where
   gr = aGraph2Gr graph
