-- Hallway & Intersection have been replaced as String type for convenience as of now
module Node
(
    Node,
    nodePositionAlongHallway,
    constructorNode,
    equals
    --displayInfo
)
where

constructorNode :: String -> String -> String -> Int -> Int -> Node
equals :: Node -> Node -> Bool
-- displayInfo :: Node -> IO()

data Node = Node
    { nodeName :: String,
      nodeIntersection :: String,
      nodeHallway :: String,
      nodePositionAlongHallway :: Int,
      nodeFloor :: Int
    } deriving (Show)

-- constructor
constructorNode name intersection hallway positionAlongHallway floor = Node name intersection hallway positionAlongHallway floor

-- compares the name of 2 nodes => returns true if the same, else false
equals node1 node2 = nodeName node1 == nodeName node2 &&
                     nodeIntersection node1 == nodeIntersection node2 &&
                     nodeHallway node1 == nodeHallway node2 &&
                     nodePositionAlongHallway node1 == nodePositionAlongHallway node2 &&
                     nodeFloor node1 == nodeFloor node2