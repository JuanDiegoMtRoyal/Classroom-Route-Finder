module Node
(
    Node,

    displayInfo,
    equals
)
where

data Node = Node
    { name :: String,
      hallway :: Hallway,
      positionAlongHallway :: Int,
      floor :: Int
    } deriving (Show)

-- compares the name of 2 nodes => returns true if the same, else false
equals :: Node -> Node -> Bool
equals (Node name1 _ _ _) (Node name2 _ _ _) = name1 == name2