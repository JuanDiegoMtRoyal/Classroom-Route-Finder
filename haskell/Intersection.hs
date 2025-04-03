module Intersection
(
    Intersection(..),
    extractValue,
    constructorIntersection,
    intersectionAddHallway,
    intersectionGetConnectedHallways,
    intersectionAddConnectedNode,
    intersectionGetConnectedNodes,
    intersectionDisplayInfo
)
where

import Data.List (sortOn)
import Node (Node(..))
import Hallway (Hallway(..))

extractValue :: Maybe a -> a

constructorIntersection :: String -> Int -> Int -> Intersection
intersectionAddHallway :: Intersection -> Hallway -> Intersection
intersectionGetConnectedHallways :: Intersection -> [Hallway]
intersectionAddConnectedNode :: Intersection -> Node -> Intersection
intersectionGetConnectedNodes :: Intersection -> [Node]
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

-- helper functions
extractValue maybeValue = case maybeValue of
                              Just maybeValue -> maybeValue
                              -- Nothing -> defaultValue

-- construtor
constructorIntersection name positionAlongHallway floor = Intersection name Nothing Nothing positionAlongHallway floor [] []

-- functions
intersectionAddHallway intersection hallway = if hallway `elem` (intersectionConnectedHallways intersection)
                                  then intersection
                                  else intersection { intersectionConnectedHallways = hallway : (intersectionConnectedHallways intersection) }

intersectionGetConnectedHallways intersection = intersectionConnectedHallways intersection

intersectionAddConnectedNode intersection node = if node `elem` (intersectionConnectedNodes intersection) -- || isIntersection node
                                     then intersection
                                     else intersection { intersectionConnectedNodes = node : (intersectionConnectedNodes intersection) }
        --where isIntersection :: Node -> Bool
        --      isIntersection _ = False

intersectionGetConnectedNodes intersection = sortOn nodePositionAlongHallway (intersectionConnectedNodes intersection)

intersectionDisplayInfo intersection = putStrLn ("-Arrive at Intersection: " ++ intersectionName intersection ++ " *From here: ")