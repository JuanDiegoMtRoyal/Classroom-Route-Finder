module ClassroomFinder where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.IO

import DataTypes
import Graph
import HallwayOps
import IntersectionOps

-- The CRF (Classroom Route Finder) data type
data CRF = CRF 
    { crfGraph :: Graph
    } deriving (Show)

-- Constructor for CRF
createCRF :: Graph -> CRF
createCRF graph = CRF graph

-- Find a route between two classrooms
findRoute :: CRF -> String -> String -> Int -> Bool -> IO (Maybe [Node])
findRoute crf startRoom endRoom timeConstraint mobilityConstraints = do
    let graph = crfGraph crf
    let maybeStartNode = getNode graph startRoom
    let maybeEndNode = getNode graph endRoom
    
    case (maybeStartNode, maybeEndNode) of
        (Nothing, _) -> do
            putStrLn $ "Classroom does not exist. ERROR with: " ++ startRoom
            return Nothing
        (_, Nothing) -> do
            putStrLn $ "Classroom does not exist. ERROR with: " ++ endRoom
            return Nothing
        (Just startNode, Just endNode) -> do
            let route = dfs startNode endNode (timeConstraint * 60) mobilityConstraints graph
            if null route
                then do
                    putStrLn "No accessible route available."
                    return Nothing
                else return $ Just (reverse route)

-- DFS search for finding a path
dfs :: Node -> Node -> Int -> Bool -> Graph -> [Node]
dfs startNode endNode remainingTime mobilityConstraints graph =
    dfsHelper startNode endNode remainingTime mobilityConstraints graph Set.empty []

-- Helper function for DFS
dfsHelper :: Node -> Node -> Int -> Bool -> Graph -> Set.Set String -> [Node] -> [Node]
dfsHelper current end remainingTime mobilityConstraints graph visited path
    | nodeName current == nodeName end = current : path
    | Set.member (nodeName current) visited = []
    | otherwise = 
        let newVisited = Set.insert (nodeName current) visited
            neighbors = getNeighbors current end mobilityConstraints graph
            validPaths = foldr (\neighbor acc -> 
                                let timeCost = calculateTimeCost current neighbor
                                in if remainingTime - timeCost >= 0 
                                   then 
                                       let result = dfsHelper neighbor end (remainingTime - timeCost) 
                                                    mobilityConstraints graph newVisited (current : path)
                                       in if not (null result) then result : acc else acc
                                   else acc
                               ) [] neighbors
        in if null validPaths then [] else head validPaths

-- Calculate the time cost to move between nodes
calculateTimeCost :: Node -> Node -> Int
calculateTimeCost from to =
    case (from, to) of
        (StairsNode {}, StairsNode {}) | nodeFloor from /= nodeFloor to -> 30
        (StairsNode {}, ElevatorNode {}) | nodeFloor from /= nodeFloor to -> 30
        (ElevatorNode {}, StairsNode {}) | nodeFloor from /= nodeFloor to -> 30
        (ElevatorNode {}, ElevatorNode {}) | nodeFloor from /= nodeFloor to -> 30
        
        -- Same floor stairs/elevators/intersections have no cost
        (StairsNode {}, StairsNode {}) -> 0
        (StairsNode {}, ElevatorNode {}) -> 0
        (StairsNode {}, IntersectionNode {}) -> 0
        (ElevatorNode {}, StairsNode {}) -> 0
        (ElevatorNode {}, ElevatorNode {}) -> 0
        (ElevatorNode {}, IntersectionNode {}) -> 0
        (IntersectionNode {}, StairsNode {}) -> 0
        (IntersectionNode {}, ElevatorNode {}) -> 0
        (IntersectionNode {}, IntersectionNode {}) -> 0
        
        -- Default case: distance along hallway
        _ -> abs (nodePositionAlongHallway to - nodePositionAlongHallway from)

-- Get neighboring nodes based on current node type
getNeighbors :: Node -> Node -> Bool -> Graph -> [Node]
getNeighbors current end mobilityConstraints graph =
    let currentFloor = nodeFloor current
        endFloor = nodeFloor end
    in case current of
        -- For classroom nodes or generic nodes in a hallway
        node | (not (isIntersectionNode node)) && (not (isStairsNode node)) && (not (isElevatorNode node)) ->
            case getHallwayFromNode node of
                Just hallway -> 
                    let hallwayNodes = getHallwayNodes hallway
                        currentIndex = findIndexInList node hallwayNodes
                    in case currentIndex of
                        Just idx -> 
                            let prevNodes = if idx > 0 then [hallwayNodes !! (idx - 1)] else []
                                nextNodes = if idx < length hallwayNodes - 1 then [hallwayNodes !! (idx + 1)] else []
                            in prevNodes ++ nextNodes
                        Nothing -> []
                Nothing -> []
        
        -- For intersection nodes
        IntersectionNode i -> 
            let connectedNodes = getConnectedNodes i
            in if currentFloor == endFloor
               then -- Same floor: exclude stairs/elevators
                   filter (\n -> not (isStairsNode n || isElevatorNode n)) connectedNodes
               else -- Different floor: include only elevators if mobility constraints
                   filter (\n -> 
                       if mobilityConstraints 
                       then isElevatorNode n
                       else isStairsNode n) connectedNodes
        
        -- For stairs nodes
        StairsNode {} -> 
            if currentFloor == endFloor
            then -- Same floor: exit to connected intersection
                case getIntersectionFromNode current of
                    Just i -> [IntersectionNode i]
                    Nothing -> []
            else -- Different floor: handle connections to other floors
                let connectedNodes = sConnectedNodes current
                in filter (\n -> (isStairsNode n || isElevatorNode n) && nodeFloor n /= currentFloor) connectedNodes
        
        -- For elevator nodes
        ElevatorNode {} -> 
            if currentFloor == endFloor
            then -- Same floor: exit to connected intersection
                case getIntersectionFromNode current of
                    Just i -> [IntersectionNode i]
                    Nothing -> []
            else -- Different floor: handle connections to other floors
                let connectedNodes = eConnectedNodes current
                in filter (\n -> (isStairsNode n || isElevatorNode n) && nodeFloor n /= currentFloor) connectedNodes
        
        -- Default case
        _ -> []

-- Helper functions
isIntersectionNode :: Node -> Bool
isIntersectionNode (IntersectionNode _) = True
isIntersectionNode _ = False

isStairsNode :: Node -> Bool
isStairsNode (StairsNode {}) = True
isStairsNode _ = False

isElevatorNode :: Node -> Bool
isElevatorNode (ElevatorNode {}) = True
isElevatorNode _ = False

getHallwayFromNode :: Node -> Maybe Hallway
getHallwayFromNode (ClassroomNode _ _ _ hallway _ _) = hallway
getHallwayFromNode (StairsNode _ _ _ hallway _ _ _) = hallway
getHallwayFromNode (ElevatorNode _ _ _ hallway _ _ _) = hallway
getHallwayFromNode _ = Nothing

getIntersectionFromNode :: Node -> Maybe Intersection
getIntersectionFromNode (ClassroomNode _ _ intersection _ _ _) = intersection
getIntersectionFromNode (StairsNode _ _ intersection _ _ _ _) = intersection
getIntersectionFromNode (ElevatorNode _ _ intersection _ _ _ _) = intersection
getIntersectionFromNode _ = Nothing

findIndexInList :: Eq a => a -> [a] -> Maybe Int
findIndexInList _ [] = Nothing
findIndexInList x (y:ys)
    | x == y = Just 0
    | otherwise = case findIndexInList x ys of
                      Just i -> Just (i + 1)
                      Nothing -> Nothing

-- Display the route
displayRoute :: [Node] -> IO ()
displayRoute [] = putStrLn "No route found."
displayRoute route = do
    putStrLn "\n\t\tRoute Directions:\n"
    
    let totalTimeSeconds = calculateTotalTime route
    
    -- Display each node in the route
    mapM_ displayInfo route
    
    let minutes = totalTimeSeconds `div` 60
    let seconds = totalTimeSeconds `mod` 60
    putStrLn $ "\nYou have arrived. Total time taken: " ++ show minutes ++ " minutes and " ++ show seconds ++ " seconds."

-- Calculate total time for the route
calculateTotalTime :: [Node] -> Int
calculateTotalTime [] = 0
calculateTotalTime [_] = 0
calculateTotalTime (n1:n2:rest) = calculateTimeCost n1 n2 + calculateTotalTime (n2:rest)