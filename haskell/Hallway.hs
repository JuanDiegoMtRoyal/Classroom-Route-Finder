module Hallway
(
    Hallway,

    addNode,
    getNodes,
    getStartIntersection,
    getAllIntersections,
    addIntersection,
    getLength
)
where

import Node (Node(..))
import Intersection (Intersection(..))

constructorHallway :: String -> String -> Intersection -> String -> String -> Int -> Int -> Hallway
addNode :: Hallway -> Node -> Hallway
getNodes :: Hallway -> [Node]
getStartIntersection :: Hallway -> Intersection
getAllIntersections :: Hallway -> [Intersection]
addIntersection :: Hallway -> Intersection -> Hallway
getLength :: Hallway -> Int

data Hallway = Hallway
    { hallwayName :: String,
      hallwayBuilding :: String,
      hallwayStartIntersection :: Intersection,
      hallwayDirection1 :: String,
      hallwayDirection2 :: String,
      hallwayFloor :: Int,
      hallwayLength :: Int,
      hallwayNodes :: [Node],
	  hallwayIntersections :: [Intersection]
    ) deriving (Show)

-- constructor
constructorHallway name building startIntersection direction1 direction2 floor length = Hallway name building startIntersection direction1 direction2 floor length [] []

-- functions
addNode hallway node = hallway { hallwayNodes = node : hallwayNodes hallway }

-- fix, might want to use sorting
getNodes 

getStartIntersection hallway = startIntersection

getAllIntersections hallway -> hallwayIntersections

addIntersection hallway intersection = hallway { hallwayIntersections = intersection : hallwayIntersections hallway }

getLength hallway = hallwayLength
