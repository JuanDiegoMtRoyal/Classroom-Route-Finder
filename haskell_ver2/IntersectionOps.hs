module IntersectionOps where

import DataTypes
import qualified Data.List as List

-- Function to add a hallway to an intersection
addHallwayToIntersection :: Intersection -> Hallway -> Intersection
addHallwayToIntersection intersection hallway = 
    if hallway `elem` iConnectedHallways intersection
        then intersection
        else intersection { iConnectedHallways = hallway : iConnectedHallways intersection }

-- Function to add a connected node to an intersection
addConnectedNodeToIntersection :: Intersection -> Node -> Intersection
addConnectedNodeToIntersection intersection node = 
    case node of
        IntersectionNode _ -> intersection -- Don't connect intersections to intersections
        _ -> if node `elem` iConnectedNodes intersection
                then intersection
                else intersection { iConnectedNodes = node : iConnectedNodes intersection }

-- Gets the connected hallways
getConnectedHallways :: Intersection -> [Hallway]
getConnectedHallways = iConnectedHallways

-- Gets the sorted connected nodes
getConnectedNodes :: Intersection -> [Node]
getConnectedNodes intersection =
    List.sortOn nodePositionAlongHallway (iConnectedNodes intersection)