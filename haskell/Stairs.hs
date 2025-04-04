module Stairs
(
    Stairs(..),
    constructorStairs,
    stairsAddConnectedNodeName,
    stairsResolveConnections,
    stairsGetConnectedNodes,
    stairsDisplayInfo
)
where

import Data.List (sortOn)
import Node
import Hallway
import Intersection
import Graph

constructorStairs :: String -> Int -> Int -> Stairs
stairsAddConnectedNodeName :: Stairs -> String -> Stairs
stairsResolveConnections :: Stairs -> Graph -> Stairs
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
stairsResolveConnections stairs graph = let resolvedNodes = mapMaybe (graphGetNode graph) (stairsConnectedNodeNames stairs)
                                            updatedStairs = foldl helperGraphAddConnectionStairs resolvedNodes in updatedStairs

-- helper function for stairsResolveConnections
helperGraphAddConnectionStairs :: Stairs -> Node -> Stairs
helperGraphAddConnectionStairs stairs node | isStairs node = stairsAddConnection stairs node
                                           | isIntersection node = intersectionAddConnection stairs node
                                           | otherwise = stairs { stairsConnectedNodes = node : (stairsConnectedNodes stairs) }


stairsGetConnectedNodes stairs = sortOn nodePositionAlongHallway (stairsConnectedNodes stairs)

stairsDisplayInfo stairs = putStrLn ("- Take stairs: " ++ stairsName stairs ++ " at intersection: " ++ intersectionName extractedIntersection)
    where extractedIntersection :: Intersection
          extractedIntersection = extractValue (stairsIntersection stairs)
