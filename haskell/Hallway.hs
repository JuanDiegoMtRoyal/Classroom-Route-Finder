module Hallway
(
    Hallway,

    addNode,
    getNodes,
    getStartIntersection,
    getLength
)
where

import Node (Node(..))
import Intersection (Intersection(..))

data Hallway = Hallway
    { hallwayName :: String,
      hallwayBuilding :: String,
      hallwayStartIntersection :: Intersection,
      hallwayDirection1 :: String,
      hallwayDirection2 :: String,
      hallwayFloor :: Int,
      hallwayLength :: Int,
      hallwayNodes :: [Node]
    ) deriving (Show)

-- constructor
constructorHallway :: String -> String -> Intersection -> String -> String -> Int -> Int -> Hallway
constructorHallway name building startIntersection direction1 direction2 floor length = Hallway name building startIntersection direction1 direction2 floor length []

-- functions
addNode :: Hallway -> Node -> Hallway
addNode hallway node = hallway {nodeList = node : nodeList hallway }

getNodes :: Hallway -> [Node]
getNodes hallway = nodeList hallway

getStartIntersection :: Hallway -> Intersection
getStartIntersection hallway = startIntersection hallway

getLength :: Hallway -> Int
getLength hallway = length hallway
