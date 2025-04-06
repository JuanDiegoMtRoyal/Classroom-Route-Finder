module Graph where

import DataTypes
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- Graph representation
data Graph = Graph 
    { gNodes :: Map.Map String Node,
      gHallways :: [Hallway],           
      gIntersections :: [Intersection]  
    } deriving (Show)

-- Create an empty graph
emptyGraph :: Graph
emptyGraph = Graph Map.empty [] []

-- Get all nodes
getAllNodes :: Graph -> Map.Map String Node
getAllNodes graph = gNodes graph

-- Add a node to the graph
addNode :: Graph -> Node -> Graph
addNode graph node = graph { gNodes = Map.insert (nodeName node) node (gNodes graph) }

--add a hallway to the graph
addHallway :: Graph -> Hallway -> Graph
addHallway graph hallway = graph { gHallways = hallway : gHallways graph }

--add a intersection to the graph
addIntersection :: Graph -> Intersection -> Graph
addIntersection graph intersection = 
    graph { gIntersections = intersection : gIntersections graph, gNodes = 
            Map.insert (iName intersection) (IntersectionNode intersection) (gNodes graph)
    }

-- Get a node by name
getNode :: Graph -> String -> Maybe Node
getNode graph name = Map.lookup name (gNodes graph)

-- Gets the intersections in Graph
getIntersections :: Graph -> [Intersection]
getIntersections graph = gIntersections graph

-- Gets the hallways in Graph
getHallways :: Graph -> [Hallway]
getHallways graph = gHallways graph


-- Resolve connections for Stairs
resolveStairsConnections :: Graph -> Node -> Node
resolveStairsConnections graph node | (StairsNode name floor intersection hallway pos _ connectedNames) <- node =
    let connectedNodes = Maybe.mapMaybe (getNode graph) connectedNames
        updatedConnectedNodes = map (resolveNodeConnection graph node) connectedNodes
    in StairsNode name floor intersection hallway pos updatedConnectedNodes connectedNames
resolveStairsConnections _ node = node

-- Resolve connections for Elevator
resolveElevatorConnections :: Graph -> Node -> Node
resolveElevatorConnections graph node | (ElevatorNode name floor intersection hallway pos _ connectedNames) <- node =
    let connectedNodes = Maybe.mapMaybe (getNode graph) connectedNames
        updatedConnectedNodes = map (resolveNodeConnection graph node) connectedNodes
    in ElevatorNode name floor intersection hallway pos updatedConnectedNodes connectedNames
resolveElevatorConnections _ node = node

-- Helper function to update connection between nodes
resolveNodeConnection :: Graph -> Node -> Node -> Node
resolveNodeConnection graph source target =
    case target of
        IntersectionNode i -> 
            let updatedConnectedNodes = (source : iConnectedNodes i)
                updatedIntersection = i { iConnectedNodes = updatedConnectedNodes }
            in IntersectionNode updatedIntersection
        StairsNode {} ->
            -- Add bidirectional connection for stairs
            case source of
                StairsNode {} -> 
                    let updatedConnectedNodes = source : sConnectedNodes target
                    in target { sConnectedNodes = updatedConnectedNodes }
                _ -> target
        ElevatorNode {} ->
            -- Add bidirectional connection for elevators
            case source of
                ElevatorNode {} -> 
                    let updatedConnectedNodes = source : eConnectedNodes target
                    in target { eConnectedNodes = updatedConnectedNodes }
                _ -> target
        _ -> target

-- Resolve all connections in the graph
resolveAllConnections :: Graph -> Graph
resolveAllConnections graph =
    let nodes = Map.elems (gNodes graph)
        resolvedNodes = map (resolveNodeConnections graph) nodes
        newGraph = Graph (foldr (\node map -> Map.insert (nodeName node) node map) Map.empty resolvedNodes) (gHallways graph) (gIntersections graph)
    in newGraph

-- Resolve connections for a specific node
resolveNodeConnections :: Graph -> Node -> Node
resolveNodeConnections graph node =
    case node of
        StairsNode {} -> resolveStairsConnections graph node
        ElevatorNode {} -> resolveElevatorConnections graph node
        _ -> node