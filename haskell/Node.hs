module Node
(
    Node,

    equals,
    displayInfo
)
where

import Hallway (Hallway(..))

data Node = Node
    { nodeName :: String,
      nodeIntersection :: Intersection,
      nodeHallway :: Hallway,
      nodePositionAlongHallway :: Int,
      nodeFloor :: Int
    } deriving (Show)

-- constructor
createNode :: String -> Intersection -> Hallway -> Int -> Floor -> Node
createNode name intersection hallway positionAlongHallway floor

-- compares the name of 2 nodes => returns true if the same, else false
equals :: Node -> Node -> Bool
equals (Node name1 _ _ _) (Node name2 _ _ _) = name1 == name2

displayInfo :: Node -> IO()
displayInfo node = putStrLn ("Node Info:\nName: " ++ (nodeName node) ++ " Hallway: " ++ (nodeHallway node) ++ " nodePositionAlongHallway node) ++ " Floor: " ++ (nodeFloor node))