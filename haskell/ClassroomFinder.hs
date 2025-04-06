module ClassroomFinder where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import DataTypes
import Graph
import HallwayOps
import IntersectionOps

data CRF = CRF
    { crfGraph :: Graph 
    } deriving (Show)

-- Constructor for graph
newCRF :: Graph -> CRF
newCRF graph = CRF graph

-- Finds a route based off of user input
findRoute :: CRF -> String -> String -> Int -> Bool -> Maybe [Node]
findRoute crf startRoom endRoom timeConstraint mobilityConstraints =
    let graph = crfGraph crf
        maybeStartNode = getNode graph startRoom
        maybeEndNode = getNode graph endRoom
    in case (maybeStartNode, maybeEndNode) of
            (Nothing, _) -> do
                --putStrLn ("Classroom does not exist. ERROR with: " ++ startRoom)
                return Nothing []
            (_, Nothing) -> do
                --putStrLn ("Classroom does not exist. ERROR with: " ++ endRoom)
                return Nothing []
            (Just startNode, Just endNode) -> do
                let visited = Set.empty  -- starts with an empty set
                    route = []
                    result = dfs startNode endNode (timeConstraint * 60) mobilityConstraints visited route
                case result of
                    Just path -> return path
                    Nothing -> do
                        --putStrLn ("No accessible route available.")
                        return Nothing []

-- dfs for route finding
dfs :: Node -> Node -> Int -> Bool -> Set.Set String -> [Node] -> Maybe [Node]
dfs current end remainingTime mobilityConstraints visited route =
    let currentName = nodeName current
        updatedVisited = Set.insert currentName visited
        updatedRoute = current : route
    in 
        if current == end 
        then Just (reverse updatedRoute)  -- correct order
        else
            let neighbors = getNeighbors current end mobilityConstraints  -- find all neighbors from current node to look for path
                validNeighbors = [n | n <- neighbors, not (Set.member (nodeName n) updatedVisited)]
            in findFirstValidPath validNeighbors end remainingTime mobilityConstraints updatedVisited updatedRoute  -- call helper function for neighbors/paths

-- Search the neighbors for a possible path/solution
findFirstValidPath :: [Node] -> Node -> Int -> Bool -> Set.Set String -> [Node] -> Maybe [Node]
findFirstValidPath [] _ _ _ _ _ = Nothing  -- If no neighbors
findFirstValidPath (neighbor:rest) end remainingTime mobilityConstraints visited route =
    let current = head route  -- Current node is the first in the route
        timeCost = calculateTimeCost current neighbor
    in 
        if (remainingTime - timeCost) >= 0 
        then case dfs neighbor end (remainingTime - timeCost) mobilityConstraints visited route of
                  Just foundRoute -> Just foundRoute  -- if a route is found, return it
                  Nothing -> findFirstValidPath rest end remainingTime mobilityConstraints visited route  -- otherwise continue searching...
        else findFirstValidPath rest end remainingTime mobilityConstraints visited route  -- recursion

-- Get neighboring nodes based on current node type and constraints
getNeighbors :: Node -> Node -> Bool -> [Node]
getNeighbors current end mobilityConstraints =
    case current of
        -- Node is an instance of classroom or a node in hallway
        node | hasHallway node && not (isIntersection node || isStairs node || isElevator node) ->
             let maybeHallway = getNodeHallway node
             in case maybeHallway of
                     Nothing -> []  -- not a/in hallway instance
                     Just hallway -> 
                         let hallwayNodes = getHallwayNodes hallway
                             currentIndex = List.findIndex (== node) hallwayNodes  -- finding node through equivalency
                         in case currentIndex of
                                 Nothing -> []
                                 Just index -> 
                                     (if index > 0 then [hallwayNodes !! (index - 1)] else []) ++
                                     (if index < (length hallwayNodes - 1) then [hallwayNodes !! (index + 1)] else [])

        -- Node is an instance of Intersection
        IntersectionNode intersection ->
            let connectedNodes = getConnectedNodes intersection
            in if nodeFloor current == nodeFloor end
               then [n | n <- connectedNodes, not (isStairs n || isElevator n)]  -- ensuring it is an intersection and not an instance of stairs/elevator
               else [n | n <- connectedNodes, (mobilityConstraints && isElevator n) || (not mobilityConstraints && isStairs n)]  -- stairs or elevator based off mobility constraints
        
        -- Node is an instance of either Stairs or Elevator
        node | isStairs node || isElevator node ->
            if nodeFloor current == nodeFloor end
            then case getNodeIntersection node of  -- if it is on the same floor, set as intersection instead (since not going up/down)
                      Nothing -> []
                      Just intersection -> [IntersectionNode intersection]  -- if it is an intersection, add it as IntersectionNode type
            else
                let transportNodes = if isStairs node 
                                     then sConnectedNodes node
                                     else eConnectedNodes node
                in [n | n <- transportNodes, (isStairs n || isElevator n), nodeFloor n /= nodeFloor current]

       -- _ -> []  -- default case (might delete)

-- Calculates the time cost between nodes
calculateTimeCost :: Node -> Node -> Int
calculateTimeCost from to | (isStairs from || isElevator from) && (isStairs to || isElevator to) && nodeFloor from /= nodeFloor to = 30
                          | (isStairs from || isElevator from || isIntersection from) && (isStairs to || isElevator to || isIntersection to) && nodeFloor from == nodeFloor to = 0
                          | otherwise = abs (nodePositionAlongHallway to - nodePositionAlongHallway from)

-- Display the route directions, includes time and route through helper functions
displayRoute :: [Node] -> IO ()
displayRoute [] = putStrLn "No route found."
displayRoute route = do
    putStrLn "\n\t\tRoute Directions:\n"
    displayRouteSteps route 0 0

-- Helper function to display route steps with time calculation
displayRouteSteps :: [Node] -> Int -> Int -> IO ()

-- Displays the route info (based off displayInfo from DataTypes module)
displayRouteSteps [node] totalTime _ = do
    displayInfo node
    displayRouteSteps [] totalTime 0

-- Calculating total time cost of route for display
displayRouteSteps (current:next:rest) totalTime index = do
    displayInfo current
    let timeCost = calculateTimeCost current next
        newTotalTime = totalTime + timeCost
    displayRouteSteps (next:rest) newTotalTime (index + 1)

-- Final display of route
displayRouteSteps [] totalTime _ = do
    let minutes = totalTime `div` 60
        seconds = totalTime `mod` 60
    putStrLn ("\nYou have arrived. Total time taken: " ++ show minutes ++ " minutes and " ++ show seconds ++ " seconds.")

