module Hallway
(
    Hallway,

    addNode,
    getNodes,
    getStartIntersection,
    getLength
)
where

data Hallway = Hallway
    { name :: String,
      building :: String,
      startIntersection :: Intersection,
      direction1 :: String,
      direction2 :: String,
      floor :: Int,
      length :: Int,
      nodes :: [Node]
    ) deriving (Show)

-- constructor
createHallway :: String -> String -> Intersection -> String -> String -> Int -> Int -> Hallway
createHallway name building startIntersection direction1 direction2 floor length = Hallway name building startIntersection direction1 direction2 floor length []

-- functions
addNode :: Hallway -> Node -> Hallway
addNode hallway node = hallway {nodeList = node : nodeList hallway }

getNodes :: Hallway -> [Node]
getNodes hallway = nodeList hallway

getStartIntersection :: Hallway -> Intersection
getStartIntersection hallway = startIntersection hallway

getLength :: Hallway -> Int
getLength hallway = length hallway
