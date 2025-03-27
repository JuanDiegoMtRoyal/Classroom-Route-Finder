module Stairs
(
    Stairs,

    addConnectedNodeName,
    resolveConnections,
    getConnectedNodes,
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))
import intersection (intersection(..))

constructorStairs :: String -> Int -> Int -> Stairs
addConnectedNodeName :: String -> [String] -> [String]
resolveConnections :: Graph -> [Node] -> [Node]
getConnectedNodes :: [Node]
displayInfo :: Stairs -> IO()

data Stairs = Stairs
    { stairsName :: String,
      stairsIntersection :: intersection,
	  stairsHallway :: Hallway,
      stairsPositionAlongHallway :: Int,
      stairsFloor :: Int
      stairsConnectedNodes :: [Node]
      stairsConnectedNodeNames :: [String]
    } deriving (Show)

-- constructor
constructorStairs name hallway positionAlongHallway floor

-- functions
addConnectedNodeName nodeName connectedNodeNames = nodeName : connectedNodeNames

-- fix
resolveConnections graph = 

getConnectedNodes connectedNodes = connectedNodes

displayInfo stairs = putStrLn ("- Take stairs: " ++ stairName stairs ++ " at intersection: " ++ show (stairPositionAlongHallway stairs))