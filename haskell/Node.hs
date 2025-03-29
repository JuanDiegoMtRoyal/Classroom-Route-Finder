module Node
(
    Node,

    equals,
    displayInfo
)
where

import Hallway (Hallway(..))
import Intersection (Intersection(..))

constructorNode :: String -> Intersection -> Hallway -> Int -> Floor -> Node
equals :: Node -> Node -> Bool
displayInfo :: Node -> IO()

data Node = Node
    { nodeName :: String,
      nodeIntersection :: Intersection,
      nodeHallway :: Hallway,
      nodePositionAlongHallway :: Int,
      nodeFloor :: Int
    } deriving (Show)

-- constructor
constructorNode name intersection hallway positionAlongHallway floor = Node name intersection hallway positionAlongHallway floor

-- compares the name of 2 nodes => returns true if the same, else false
equals node1 node2 = nodeName node1 == nodeName node2

displayInfo node = putStrLn ("Node Info:\nName: " ++ (nodeName node) ++ " Hallway: " ++ (nodeHallway node) ++ " Hallway Position: " ++ ( nodePositionAlongHallway node) ++ " Floor: " ++ (nodeFloor node))