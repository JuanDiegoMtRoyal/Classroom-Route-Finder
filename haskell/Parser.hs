module BuildingParser where

import System.IO
import Data.List (find)
import Data.Char (isSpace)
import Control.Monad (when)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Direction = SW | NE | SE | NW | 

data Node = Node
    { nodeName :: String
    , nodeBuilding :: String
    , nodeFloor :: Int
    } deriving (Show, Eq)

data Hallway = Hallway
    { hallwayName :: String
    , hallwayBuilding :: String
    , hallwayStartIntersection :: String
    , hallwayDirection1 :: Direction
    , hallwayDirection2 :: Direction
    , hallwayFloor :: Int
    , hallwayLength :: Int
    , hallwayNodes :: [String]  -- Names of nodes along this hallway
    , hallwayIntersections :: [String]  -- Names of intersections on this hallway
    } deriving (Show)

data Classroom = Classroom
    { classroomName :: String
    , classroomBuilding :: String
    , classroomHallway :: String
    , classroomPosition :: Int
    , classroomDirection :: Direction
    , classroomFloor :: Int
    } deriving (Show)

    data Stairs = Stairs
    { stairsName :: String
    , stairsBuilding :: String
    , stairsIntersection :: String
    , stairsPosition :: Int
    , stairsDirection :: Direction
    , stairsTeleportTo :: [String]
    , stairsFloor :: Int
    } deriving (Show)

data Elevator = Elevator
    { elevatorName :: String
    , elevatorBuilding :: String
    , elevatorIntersection :: String
    , elevatorPosition :: Int
    , elevatorDirection :: Direction
    , elevatorTeleportTo :: [String]
    , elevatorFloor :: Int
    } deriving (Show)

data Intersection = Intersection
    { intersectionName :: String
    , intersectionFloor :: Int
    , intersectionPosition :: Int
    , intersectionHallways :: [String]
    , intersectionConnectedNodes :: [String]
    } deriving (Show)

data BuildingGraph = BuildingGraph
    { graphNodes :: Map.Map String Node
    , graphHallways :: Map.Map String Hallway
    , graphClassrooms :: Map.Map String Classroom
    , graphStairs :: Map.Map String Stairs
    , graphElevators :: Map.Map String Elevator
    , graphIntersections :: Map.Map String Intersection
    } deriving (Show)

    emptyGraph :: BuildingGraph
    emptyGraph = BuildingGraph Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

    --Parser 
    parseBuildingMap :: FilePath -> IO BuildingGraph

    parseBuildingMap filename = do
    contents <- readFile filename
    let lines' = filter (not . isCommentOrEmpty) $ lines contents
    let sections = groupSections lines'
    foldM parseSection emptyGraph sections
      where
    isCommentOrEmpty line = null (trim line) || "#" `isPrefixOf` trim line
    isPrefixOf prefix str = prefix `eqOnLength` take (length prefix) str
    eqOnLength a b = length a == length b && a == b



