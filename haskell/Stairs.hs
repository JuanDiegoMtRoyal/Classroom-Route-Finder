module Stairs
(
    Stairs,
    stairsName,
    stairsConnectedNodes,
    stairsConnectedNodeNames,
    constructorStairs,
    stairsAddConnectedNodeName,
    --stairsResolveConnections,
    stairsGetConnectedNodes,
    stairsDisplayInfo
)
where

import Data.List (sortOn)
import Node (Node(..))
import Hallway (Hallway(..))
import Intersection (Intersection(..))

constructorStairs :: String -> Int -> Int -> Stairs
stairsAddConnectedNodeName :: Stairs -> String -> Stairs
--stairsResolveConnections :: Graph -> [Node] -> [Node]
stairsGetConnectedNodes :: Stairs -> [Node]
stairsDisplayInfo :: Stairs -> IO()

data Stairs = Stairs
    { stairsName :: String,
      stairsIntersection :: Maybe Intersection,
      stairsHallway :: Maybe Hallway,
      stairsPositionAlongHallway :: Int,
      stairsFloor :: Int,
      stairsConnectedNodes :: [Node],
      stairsConnectedNodeNames :: [String]
    } deriving (Show)

-- constructor
constructorStairs name positionAlongHallway floor = Stairs name Nothing Nothing positionAlongHallway floor [] []

-- functions
stairsAddConnectedNodeName stairs nodeName = if nodeName `elem` (stairsConnectedNodeNames stairs)
                                  then stairs
                                  else stairs { stairsConnectedNodeNames = nodeName : (stairsConnectedNodeNames stairs) }

-- fix
-- stairsResolveConnections graph = 

stairsGetConnectedNodes stairs = sortOn nodePositionAlongHallway (stairsConnectedNodes stairs)

stairsDisplayInfo stairs = putStrLn ("- Take stairs: " ++ stairsName stairs ++ " at intersection: " ++ show (stairsPositionAlongHallway stairs))