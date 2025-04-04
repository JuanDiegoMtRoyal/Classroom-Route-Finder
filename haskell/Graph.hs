module Graph
(
    Graph(..),
    constructorGraph,
    graphGetAllNodes,
    graphAddNode,
    graphAddHallway,
    graphAddIntersection,
    graphGetNodesAlongHallway,
    graphGetNode,
    graphGetIntersections,
    graphGetHallways
)
where

import Data.List (sortOn)
import Data.Map as Map
import Node
import Hallway
-- import Classroom
import Intersection
--import Stairs
--import Elevators

constructorGraph :: Graph
graphGetAllNodes :: Graph -> [Node]
graphAddNode :: Graph -> Node -> Graph
graphAddHallway :: Graph -> Hallway -> Graph
graphAddIntersection :: Graph -> Intersection -> Graph
graphGetNodesAlongHallway :: Hallway -> [Node]
graphGetNode :: Graph -> String -> Maybe Node
graphGetIntersections :: Graph -> [Intersection]
graphGetHallways :: Graph -> [Hallway]

data Graph = Graph
    { graphNodes :: Map.Map String Node,
      graphHallways :: [Hallway],
      graphIntersections :: [Intersection]
    } deriving (Show)

-- constructor
constructorGraph = Graph Map.empty [] []  -- initially empty lists

-- functions
graphGetAllNodes graph = Map.elems (graphNodes graph)

graphAddNode graph node = graph { graphNodes = Map.insert (nodeName node) node (graphNodes graph) }

graphAddHallway graph hallway = graph { graphHallways = hallway : (graphHallways graph) }

graphAddIntersection graph intersection = graph { graphIntersections = intersection : (graphIntersections graph) }

graphGetNodesAlongHallway hallway = hallwayGetNodes hallway

graphGetNode graph nodeName = Map.lookup nodeName (graphNodes graph)

graphGetIntersections graph = graphIntersections graph

graphGetHallways graph = graphHallways graph

