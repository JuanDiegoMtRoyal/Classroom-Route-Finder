module Elevator
(
    Elevator(..),
    constrcutorElevator,
    elevatorAddConnectedNodeName,
    --elevatorResolveConnections,
    elevatorGetConnectedNodes,
    elevatorDisplayInfo
)
where

import Data.List (sortOn)
import Node (Node(..))
import Hallway (Hallway(..))
import Intersection (Intersection(..))

constructorElevator :: String -> Hallway -> Int -> Int -> Elevator
elevatorAddConnectedNodeName :: Elevator -> String -> Elevator
--elevatorResolveConnections :: Elevator -> Graph -> Elevator
elevatorGetConnectedNodes :: Elevator -> [Node]
elevatorDisplayInfo :: Elevator -> IO()

data Elevator = Elevator
    { elevatorName :: String,
      elevatorIntersection :: Maybe Intersection,
      elevatorHallway :: Maybe Hallway,
      elevatorPositionAlongHallway :: Int,
      elevatorFloor :: Int,
      elevatorConnectNodes :: [Node],
      elevatorConnectedNodeNames :: [Node]
    } deriving (Show)

-- constructor
constrcutorElevator name hallway positionAlongHallway floor = name Nothing Nothing positionAlongHallway floor [] []

-- functions
elevatorAddConnectedNodeName elevator nodeName = if nodeName `elem` (elevatorConnectedNodeNames elevator)
                                                 then elevator
                                                 else elevator { elevatorConnectedNodeNames = nodeName : (elevatorConnectedNodeNames elevator) }

-- fix
--elevatorResolveConnections 

elevatorGetConnectedNodes elevator = sortOn nodePositionAlongHallway (elevatorGetConnectedNodes elevator)

elevatorDisplayInfo elevator = putStrLn ("-Take Elevator: " ++ elevatorName elevator ++ " at intersection: " ++ (intersectionName elevatorIntersection))