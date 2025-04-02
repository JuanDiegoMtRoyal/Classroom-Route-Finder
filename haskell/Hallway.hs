-- Intersection has been replace with String type for convenience as of now
module Hallway
(
    Hallway,
    hallwayName,
    constructorHallway,
    hallwayAddNode,
    hallwayGetNodes,
    hallwayGetStartIntersection,
    hallwayGetAllIntersections,
    hallwayAddIntersection,
    hallwayGetLength
)
where

import Data.List (sortOn)
import Node
-- import Intersection (Intersection(..))

constructorHallway :: String -> String -> String -> String -> String -> Int -> Int -> Hallway
hallwayAddNode :: Hallway -> Node -> Hallway
hallwayGetNodes :: Hallway -> [Node]
hallwayGetStartIntersection :: Hallway -> String
hallwayGetAllIntersections :: Hallway -> [String]
hallwayAddIntersection :: Hallway -> String -> Hallway
hallwayGetLength :: Hallway -> Int

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
hallwayAddNode hallway node = hallway { hallwayNodes = node : (hallwayNodes hallway) }

hallwayGetNodes hallway = sortOn nodePositionAlongHallway (hallwayNodes hallway)

hallwayGetStartIntersection hallway = hallwayStartIntersection hallway

hallwayGetAllIntersections hallway = hallwayIntersections hallway

hallwayAddIntersection hallway intersection = hallway { hallwayIntersections = intersection : (hallwayIntersections hallway) }

hallwayGetLength hallway = hallwayLength hallway

instance Eq Hallway
    where hallway1 == hallway2 = hallwayName hallway1 == hallwayName hallway2