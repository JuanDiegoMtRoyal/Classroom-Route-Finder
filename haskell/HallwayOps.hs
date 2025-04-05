module HallwayOps where

import DataTypes
import Data.List as List

-- Function to add a node to a hallway
addNodeToHallway :: Hallway -> Node -> Hallway
addNodeToHallway hallway node = 
    hallway { hNodes = node : hNodes hallway }

-- Function to add an intersection to a hallway
addIntersectionToHallway :: Hallway -> Intersection -> Hallway
addIntersectionToHallway hallway intersection = 
    hallway { hIntersections = intersection : hIntersections hallway }

-- Gets nodes in the hallway based on sorted position
getHallwayNodes :: Hallway -> [Node]
getHallwayNodes hallway = 
    List.sortOn nodePositionAlongHallway (hNodes hallway)

-- Gets the start intersection of a hallway
getStartIntersection :: Hallway -> Maybe Intersection
getStartIntersection = hStartIntersection

-- Gets all the intersections in a hallway
getAllIntersections :: Hallway -> [Intersection]
getAllIntersections = hIntersections