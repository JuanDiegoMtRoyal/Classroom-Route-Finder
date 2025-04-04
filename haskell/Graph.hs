module Graph where

import DataTypes
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- Graph representation
data Graph = Graph 
    { gNodes :: Map.Map String Node
    } deriving (Show)

-- Create an empty graph
emptyGraph :: Graph
emptyGraph = Graph Map.empty

-- Add a node to the graph
addNode :: Graph -> Node -> Graph
addNode graph node = Graph (Map.insert (nodeName node) node (gNodes graph))

-- Get a node by name
getNode :: Graph -> String -> Maybe Node
getNode graph name = Map.lookup name (gNodes graph)

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
            let updatedConnectedNodes = source : iConnectedNodes i
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
        newGraph = Graph (foldr (\node map -> Map.insert (nodeName node) node map) Map.empty resolvedNodes)
    in newGraph

-- Resolve connections for a specific node
resolveNodeConnections :: Graph -> Node -> Node
resolveNodeConnections graph node =
    case node of
        StairsNode {} -> resolveStairsConnections graph node
        ElevatorNode {} -> resolveElevatorConnections graph node
        _ -> node