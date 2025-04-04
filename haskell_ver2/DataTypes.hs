module DataTypes where

import Data.List as List
import Data.Maybe as Maybe

-- Data types that aren't extended from Node have their own definitions
data Intersection = Intersection 
    { iName :: String,
      iFloor :: Int,
      iPositionAlongHallway :: Int,
      iConnectedHallways :: [Hallway],
      iConnectedNodes :: [Node]
    } deriving (Show)

data Hallway = Hallway 
    { hName :: String,
      hBuilding :: String,
      hStartIntersection :: Maybe Intersection,
      hDirection1 :: String,
      hDirection2 :: String,
      hFloor :: Int,
      hLength :: Int,
      hNodes :: [Node],
      hIntersections :: [Intersection]
    } deriving (Show)

-- Main Node data type defined as a sum type of all possible nodes (Java implementation used "extended"
data Node =
    ClassroomNode 
    { cName :: String,
      cFloor :: Int,
      cIntersection :: Maybe Intersection,
      cHallway :: Maybe Hallway,
      cPositionAlongHallway :: Int,
      cCompassDirection :: String
    }
    | StairsNode 
    { sName :: String,
      sFloor :: Int,
      sIntersection :: Maybe Intersection, 
      sHallway :: Maybe Hallway,
      sPositionAlongHallway :: Int,
      sConnectedNodes :: [Node],
      sConnectedNodeNames :: [String]
    }
    | ElevatorNode 
    { eName :: String,
      eFloor :: Int,
      eIntersection :: Maybe Intersection,
      eHallway :: Maybe Hallway,
      ePositionAlongHallway :: Int,
      eConnectedNodes :: [Node],
      eConnectedNodeNames :: [String]
    }
    | IntersectionNode Intersection
    | HallwayNode Hallway
    deriving (Show)

-- Extract common fields from any Node
-- name
nodeName :: Node -> String
nodeName (ClassroomNode name _ _ _ _ _) = name
nodeName (StairsNode name _ _ _ _ _ _) = name
nodeName (ElevatorNode name _ _ _ _ _ _) = name
nodeName (IntersectionNode i) = iName i
nodeName (HallwayNode h) = hName h

-- floor
nodeFloor :: Node -> Int
nodeFloor (ClassroomNode _ floor _ _ _ _) = floor
nodeFloor (StairsNode _ floor _ _ _ _ _) = floor
nodeFloor (ElevatorNode _ floor _ _ _ _ _) = floor
nodeFloor (IntersectionNode i) = iFloor i
nodeFloor (HallwayNode h) = hFloor h

-- positionAlongHallway
nodePositionAlongHallway :: Node -> Int
nodePositionAlongHallway (ClassroomNode _ _ _ _ pos _) = pos
nodePositionAlongHallway (StairsNode _ _ _ _ pos _ _) = pos
nodePositionAlongHallway (ElevatorNode _ _ _ _ pos _ _) = pos
nodePositionAlongHallway (IntersectionNode i) = iPositionAlongHallway i

-- For handling equality based on name
-- implementation of Node (mainly for traversal purposes)
instance Eq Node where
    n1 == n2 = nodeName n1 == nodeName n2

-- implementation for Hallway (mainly for traversal purposes)
instance Eq Hallway where
    h1 == h2 = hName h1 == hName h2

-- helper function for extracting value from Maybe types in Just a
extractValue :: Maybe a -> a
extractValue maybeVal = case maybeVal of
                             Just maybeVal -> maybeVal

-- Display information for each type of node
displayInfo :: Node -> IO ()

-- CLASSROOM DISPLAY
-- case where classroom does exist in a hallway
displayInfo (ClassroomNode name _ _ (Just hallway) pos compassDir) = 
    putStrLn $ "-Go " ++ compassDir ++ " towards Classroom: " ++ name ++ " at " ++ show pos ++ "m along " ++ hName hallway
-- case where classroom does not have a hallway (such as standing in the same position where no traversal is needed).
displayInfo (ClassroomNode name _ _ Nothing pos compassDir) = 
    putStrLn $ "-Go " ++ compassDir ++ " towards Classroom: " ++ name ++ " at " ++ show pos ++ "m"

-- STAIRS DISPLAY
-- case where stairs exist at an intersection point
displayInfo (StairsNode name _ (Just intersection) _ _ _ _) = 
    putStrLn $ "-Take Stairs: " ++ name ++ " at intersection: " ++ iName intersection
-- case where stairs are not at any intersection point
displayInfo (StairsNode name _ Nothing _ _ _ _) = 
    putStrLn $ "-Take Stairs: " ++ name

-- ELEVATOR DISPLAY
-- case where Elevator exists at an intersection point
displayInfo (ElevatorNode name _ (Just intersection) _ _ _ _) = 
    putStrLn $ "-Take Elevator: " ++ name ++ " at intersection: " ++ iName intersection
-- case where Elevator is not at any intersection point
displayInfo (ElevatorNode name _ Nothing _ _ _ _) = 
    putStrLn $ "-Take Elevator: " ++ name

-- INTERSECTION DISPLAY
displayInfo (IntersectionNode i) = do
    putStrLn $ "\n-Arrive at Intersection: " ++ iName i ++ "\n*From here: "