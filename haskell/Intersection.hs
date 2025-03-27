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

constructorIntersection :: Sring -> Int -> Int -> Intersection
addHallway :: Intersection -> Hallway -> Intersection
getConnectedHallways :: Intersection -> [Hallway]
addConnectedNode :: Intersection -> Node -> Intersection
getConnectedNode :: Intersection -> [Node]
displayInfo :: Intersection -> IO()

data Intersection = Intersection
    { intersectionName :: String,
      intersectionIntersection :: Intersection,
      intersectionHallway :: Hallway,
      intersectionPositionAlongHallway :: Int,
      intersectionFloor :: Int
	  intersectionConnectedHallways :: [Hallway]
	  intersectionConnectedNodes :: [Node]
    } deriving (Show)

-- construtor
constructorIntersection name positionAlongHallway floor = Intersection name position floor [] []

-- functions
addHallway intersection hallway = if hallway `elem` (intersectionConnectedHallways intersection)
                                  then intersection
                                  else intersection { intersectionConnectedHallways = hallway : (intersectionConnectedHallways intersection) }

getConnectedHallways intersection = intersectionConnectedHallways

-- fix, helper function isIntersection
addConnectedNode intersection node = if node `elem` intersectionConnectedNodes intersection -- || isIntersection node
                                     then intersection
                                     else intersection { intersectionConnectedNodes = node : (intersectionConnectedNodes intersection) }
        where isIntersection :: Node -> Bool
              isIntersection _ = False

-- fix, might want to use a sorting function
getConnectedNode intersection = intersectionConnectedNodes intersection

displayInfo intersection = putStrLn ("-Arrive at Intersection: " ++ intersectionName intersection ++ " *From here: ")