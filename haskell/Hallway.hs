-- Intersection has been replace with String type for convenience as of now
module Hallway
(
    Hallway,
    constructorHallway,
    addNode,
    getNodes,
    getStartIntersection,
    getAllIntersections,
    addIntersection,
    getLength
)
where

import Node
import Data.List (sortOn)
-- import Intersection (Intersection(..))

constructorHallway :: String -> String -> String -> String -> String -> Int -> Int -> Hallway
addNode :: Hallway -> Node -> Hallway
getNodes :: Hallway -> [Node]
getStartIntersection :: Hallway -> String
getAllIntersections :: Hallway -> [String]
addIntersection :: Hallway -> String -> Hallway
getLength :: Hallway -> Int

data Hallway = Hallway
    { hallwayName :: String,
      hallwayBuilding :: String,
      hallwayStartIntersection :: String,
      hallwayDirection1 :: String,
      hallwayDirection2 :: String,
      hallwayFloor :: Int,
      hallwayLength :: Int,
      hallwayNodes :: [Node],
      hallwayIntersections :: [String]
    } deriving (Show)

-- constructor
constructorHallway name building startIntersection direction1 direction2 floor length = Hallway name building startIntersection direction1 direction2 floor length [] []

-- functions
-- NOTE: if using addNode, to test must use "it" as a reference to the current list
--       when you are running in main program, use a let call.
addNode hallway node = hallway { hallwayNodes = node : (hallwayNodes hallway) }

getNodes hallway = sortOn nodePositionAlongHallway (hallwayNodes hallway)

getStartIntersection hallway = hallwayStartIntersection hallway

getAllIntersections hallway = hallwayIntersections hallway

addIntersection hallway intersection = hallway { hallwayIntersections = intersection : (hallwayIntersections hallway) }

getLength hallway = hallwayLength hallway
