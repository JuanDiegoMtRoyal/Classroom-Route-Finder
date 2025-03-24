module Intersection
(
    Intersection,
    
    addConnectedNode,
    getConnectedNode,
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))

data Intersection = Intersection
    { intersectionName :: String,
      intersectionHallway :: Hallway,
      intersectionPositionAlongHallway :: Int,
      intersectionFloor :: Int
	  intersectionConnectedNodes :: [Node]
    } deriving (Show)

-- construtor
createIntersection :: Sring -> Hallway -> Int -> Int -> Intersection
createIntersection name hallway positionAlongHallway floor

-- functions
addConnectedNode :: Node -> Intersection -> Intersection
addConnectedNode node intersection = intersection { connectedNodes = node : connectedNodes intersection }

getConnectedNode :: Intersection -> [Node]
getConnectedNode intersection = connectedNodes intersection

-- fix
-- use mapM_ for displaying connected nodes?
displayInfo :: Intersection -> IO()
displayInfo intersection = do putStrLn ("Classroom: " ++ intersectionName intersection ++ " at " ++ show (intersectionPositionAlongHallway intersection) ++ "m along " ++ hallwayName (intersectionHallway intersection))
                              putStrLn ("Connected Nodes: ")
                              mapM_ 