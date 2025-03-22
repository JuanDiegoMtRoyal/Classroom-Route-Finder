module Intersection
(
    Intersection,
    
    addConnectedNode,
    getConnectedNode,
    displayInfo
)
where

data Intersection = Intersection
    { name :: String,
      hallway :: Hallway,
      positionAlongHallway :: Int,
      floor :: Int
	  connectedNodes :: [Node]
    } deriving (Show)

-- construtor
createIntersection :: Sring -> Hallway -> Int -> Int -> Intersection
createIntersection name hallway positionAlongHallway floor

-- functions
addConnectedNode :: Node -> Intersection -> Intersection
addConnectedNode node intersection = intersection { connectedNodes = node : connectedNodes intersection }

getConnectedNode :: Intersection -> [Node]
getConnectedNode intersection = connectedNodes intersection

displayInfo :: Intersection -> IO()
-- fix