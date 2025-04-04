module ClassroomFinder where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import DataTypes
import HallwayOps
import IntersectionOps
import Graph

-- Main CRF data type (a graph)
data CRF = CRF 
    { crfGraph :: Graph
    } deriving (Show)

-- Constructor for CRF
createCRF :: Graph -> CRF
createCRF graph = CRF graph

-- Find a route from starting classroom to destination classroom
findRoute :: CRF -> String -> String -> Int -> Bool -> Maybe [Node]
findRoute crf startRoom endRoom timeConstraint mobilityConstraints =
    case (getNode (crfGraph crf) startRoom, getNode (crfGraph crf) endRoom) of
        (Nothing, _) -> do
            --putStrLn $ "Classroom does not exist. ERROR with: " ++ startRoom
            return []
        (_, Nothing) -> do
            --putStrLn $ "Classroom does not exist. ERROR with: " ++ endRoom
            return []
        (Just startNode, Just endNode) -> do
            let route = []
                visited = Set.empty
                found = dfs startNode endNode (timeConstraint * 60) mobilityConstraints visited route
            case found of
                (True, resultRoute) -> return $ extractValue (Just (reverse resultRoute))
                (False, _) -> do
                    --putStrLn "No accessible route available."
                    return []

-- DFS algorithm implementation (with backtracking)
dfs :: Node -> Node -> Int -> Bool -> Set.Set String -> [Node] -> (Bool, [Node])
dfs current end remainingTime mobilityConstraints visited route
    | nodeName current == nodeName end = (True, current : route)
    | otherwise = 
        let visited' = Set.insert (nodeName current) visited
            neighbors = getNeighbors current end mobilityConstraints
            exploreNeighbors [] = (False, route)
            exploreNeighbors (neighbor:rest) =
                if Set.member (nodeName neighbor) visited'
                then exploreNeighbors rest
                else
                    let timeCost = calculateTimeCost current neighbor
                    in if remainingTime - timeCost >= 0
                       then
                           let (found, newRoute) = dfs neighbor end (remainingTime - timeCost) mobilityConstraints visited' (current : route)
                           in if found
                              then (True, newRoute)
                              else exploreNeighbors rest
                       else exploreNeighbors rest
        in exploreNeighbors neighbors

-- Get neighboring nodes based on the current node type and constraints
getNeighbors :: Node -> Node -> Bool -> [Node]
getNeighbors current end mobilityConstraints =
    case current of
        -- Case 1: Nodes in current hallway (except for intersections, stairs, elevators)
        ClassroomNode _ _ _ (Just hallway) _ _ ->
            let hallwayNodes = getHallwayNodes hallway
                currentIndex = List.findIndex (\n -> nodeName n == nodeName current) hallwayNodes
            in case currentIndex of
                Nothing -> []
                Just idx -> 
                    let prevNode = if idx > 0 then [hallwayNodes !! (idx - 1)] else []
                        nextNode = if idx < length hallwayNodes - 1 then [hallwayNodes !! (idx + 1)] else []
                    in prevNode ++ nextNode
                    
        -- Case 2: Intersection nodes
        IntersectionNode intersection ->
            let connectedNodes = getConnectedNodes intersection
                -- Filter based on floor constraints
                floorFiltered = if nodeFloor current == nodeFloor end
                               then filter (\n -> not (isStairsOrElevator n)) connectedNodes
                               else filter (\n -> 
                                      if mobilityConstraints 
                                      then isElevator n
                                      else isStairs n) connectedNodes
            in floorFiltered
            
        -- Case 3: Stairs or Elevator nodes
        StairsNode _ _ (Just intersection) _ _ connectedNodes _ ->
            if nodeFloor current == nodeFloor end
            then [IntersectionNode intersection]
            else filter (\n -> nodeFloor n /= nodeFloor current) connectedNodes
            
        ElevatorNode _ _ (Just intersection) _ _ connectedNodes _ ->
            if nodeFloor current == nodeFloor end
            then [IntersectionNode intersection]
            else filter (\n -> nodeFloor n /= nodeFloor current) connectedNodes
            
        -- Default case
        _ -> []

-- Helper functions to check if a node is a stair or elevator
isStairsOrElevator :: Node -> Bool
isStairsOrElevator node =
    case node of
        StairsNode {} -> True
        ElevatorNode {} -> True
        _ -> False

-- Helper function to check if a node is a stair
isStairs :: Node -> Bool
isStairs node =
    case node of
        StairsNode {} -> True
        _ -> False

-- Helper to check if a node is an elevator
isElevator :: Node -> Bool
isElevator node =
    case node of
        ElevatorNode {} -> True
        _ -> False

-- Calculate time cost between nodes
calculateTimeCost :: Node -> Node -> Int
calculateTimeCost from to
    | (isStairsOrElevator from) && (isStairsOrElevator to) && (nodeFloor from /= nodeFloor to) = 30
    | (isStairsOrElevator from || isIntersection from) && (isStairsOrElevator to || isIntersection to) && 
      (nodeFloor from == nodeFloor to) = 0
    | otherwise = abs (nodePositionAlongHallway to - nodePositionAlongHallway from)

-- Helper to check if node is intersection
isIntersection :: Node -> Bool
isIntersection node =
    case node of
        IntersectionNode {} -> True
        _ -> False

-- Display route information
displayRoute :: Maybe [Node] -> IO ()
displayRoute Nothing = putStrLn "No route found."
displayRoute (Just []) = putStrLn "No route found."
displayRoute (Just route) = do
    putStrLn "\n\t\tRoute Directions:\n"
    let (totalTime, _) = displayRouteNodes route 0
    let minutes = totalTime `div` 60
    let seconds = totalTime `mod` 60
    putStrLn $ "\nYou have arrived. Total time taken: " ++ show minutes ++ " minutes and " ++ show seconds ++ " seconds."

-- Helper function to display route nodes and calculate total time
displayRouteNodes :: [Node] -> Int -> (Int, [Node])
displayRouteNodes [] totalTime = (totalTime, [])
displayRouteNodes [node] totalTime = do
    --displayInfo node
    (totalTime, [node])
displayRouteNodes (current:next:rest) totalTime = do
    --displayInfo current
    let newTime = totalTime + calculateTimeCost current next
    displayRouteNodes (next:rest) newTime