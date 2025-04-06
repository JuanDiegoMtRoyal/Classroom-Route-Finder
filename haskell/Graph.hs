module Graph
    ( Graph(..)  -- This exports the Graph type and all its constructors
    , emptyGraph
    , addNode
    , addHallway
    , addIntersection
    , getNode
    , resolveAllConnections
    -- ... other exports ...
    ) where
import DataTypes
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- Graph representation
data Graph = Graph 
    { gNodes :: Map.Map String Node,     
      gHallways :: [Hallway],           
      gIntersections :: [Intersection]  
    }deriving (Show)

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

--add 
addIntersection :: Graph -> Intersection -> Graph
addIntersection graph intersection = 
  graph { 
    gIntersections = intersection : gIntersections graph,
    gNodes = Map.insert (iName intersection) (IntersectionNode intersection) (gNodes graph)
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



-- Resolve all connections in the graph
resolveAllConnections :: Graph -> Graph
resolveAllConnections graph = foldl resolveConnections graph (Map.elems (gNodes graph))
  where
    resolveConnections :: Graph -> Node -> Graph
    resolveConnections g node = case node of
      StairsNode {} -> resolveStairs g node
      ElevatorNode {} -> resolveElevator g node
      _ -> g


resolveStairs :: Graph -> Node -> Graph
resolveStairs graph node@(StairsNode name _ _ _ _ _ cNames) =
  let (newNodes, updatedConnections) = resolveConnectionsForNode graph cNames
      -- Update this node's connections
      updatedNode = node { sConnectedNodes = updatedConnections }
      -- Update connected nodes to point back
      graphWithUpdatedNode = addNode graph updatedNode

--bidirectional connection
  in foldr (addConnectionToPeer updatedNode) graphWithUpdatedNode updatedConnections
  where
    resolveConnectionsForNode :: Graph -> [String] -> (Map.Map String Node, [Node])
    resolveConnectionsForNode g names =
      let nodes = Maybe.mapMaybe (flip Map.lookup (gNodes g)) names
      in (gNodes g, nodes)

    addConnectionToPeer :: Node -> Node -> Graph -> Graph
    addConnectionToPeer source target g = case target of
      StairsNode {} -> updateTargetWith (\t -> t { sConnectedNodes = source : sConnectedNodes t })
      ElevatorNode {} -> updateTargetWith (\t -> t { eConnectedNodes = source : eConnectedNodes t })
      IntersectionNode i -> 
        let updatedI = i { iConnectedNodes = source : iConnectedNodes i }
        in addNode g (IntersectionNode updatedI)
      _ -> g
      where
        updateTargetWith fn = 
          case Map.lookup (nodeName target) (gNodes g) of
            Just t -> addNode g (fn t)
            Nothing -> g


resolveElevator :: Graph -> Node -> Graph
resolveElevator graph node@(ElevatorNode name _ _ _ _ _ cNames) =
  let (newNodes, updatedConnections) = resolveConnectionsForNode graph cNames
      -- Update this node's connections
      updatedNode = node { eConnectedNodes = updatedConnections }
      -- Update connected nodes to point back
      graphWithUpdatedNode = addNode graph updatedNode
      
--bidirectional connection
  in foldr (addConnectionToPeer updatedNode) graphWithUpdatedNode updatedConnections
  where
    resolveConnectionsForNode :: Graph -> [String] -> (Map.Map String Node, [Node])
    resolveConnectionsForNode g names =
      let nodes = Maybe.mapMaybe (flip Map.lookup (gNodes g)) names
      in (gNodes g, nodes)

    addConnectionToPeer :: Node -> Node -> Graph -> Graph
    addConnectionToPeer source target g = case target of
      StairsNode {} -> updateTargetWith (\t -> t { sConnectedNodes = source : sConnectedNodes t })
      ElevatorNode {} -> updateTargetWith (\t -> t { eConnectedNodes = source : eConnectedNodes t })
      IntersectionNode i -> 
        let updatedI = i { iConnectedNodes = source : iConnectedNodes i }
        in addNode g (IntersectionNode updatedI)
      _ -> g
      where
        updateTargetWith fn = 
          case Map.lookup (nodeName target) (gNodes g) of
            Just t -> addNode g (fn t)
            Nothing -> g




