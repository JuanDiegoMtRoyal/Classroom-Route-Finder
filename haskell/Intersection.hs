module Intersection
(
    Intersection,
    constructorIntersection,
    addHallway,
    getConnectedHallways,
    addConnectedNode,
    getConnectedNode,
    intersectionDisplayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))
import Data.List (sortOn)

constructorIntersection :: String -> Int -> Int -> Intersection
addHallway :: Intersection -> Hallway -> Intersection
getConnectedHallways :: Intersection -> [Hallway]
addConnectedNode :: Intersection -> Node -> Intersection
getConnectedNode :: Intersection -> [Node]
intersectionDisplayInfo :: Intersection -> IO()

data Intersection = Intersection
    { intersectionName :: String,
      intersectionIntersection :: Maybe Intersection,
      intersectionHallway :: Maybe Hallway,
      intersectionPositionAlongHallway :: Int,
      intersectionFloor :: Int,
      intersectionConnectedHallways :: [Hallway],
      intersectionConnectedNodes :: [Node]
    } deriving (Show)

-- construtor
constructorIntersection name positionAlongHallway floor = Intersection name Nothing Nothing positionAlongHallway floor [] []

-- functions
-- fix, alternative to `elem` or fix `elem`
addHallway intersection hallway = if hallway `elem` (intersectionConnectedHallways intersection)
                                  then intersection
                                  else intersection { intersectionConnectedHallways = hallway : (intersectionConnectedHallways intersection) }

getConnectedHallways intersection = intersectionConnectedHallways intersection

-- fix, helper function isIntersection
-- fix, find alternative to `elem` or fix `elem`
addConnectedNode intersection node = if node `elem` (intersectionConnectedNodes intersection) -- || isIntersection node
                                     then intersection
                                     else intersection { intersectionConnectedNodes = node : (intersectionConnectedNodes intersection) }
        where isIntersection :: Node -> Bool
              isIntersection _ = False

getConnectedNode intersection = sortOn nodePositionAlongHallway (intersectionConnectedNodes intersection)

intersectionDisplayInfo intersection = putStrLn ("-Arrive at Intersection: " ++ intersectionName intersection ++ " *From here: ")