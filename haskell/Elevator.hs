module Elevator
(
    Elevator,
    constrcutorElevator,
    addConnectedNodeName,
    resolveConnections,
    getConnectedNodes,
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))
import Intersection (Intersection(..))

constructorElevator :: String -> Hallway -> Int -> Int -> Elevator
addConnectedNodeName :: Elevator -> String -> Elevator
resolveConnections :: Elevator -> Graph -> Elevator
getConnectedNodes :: Elevator -> [Node]
displayInfo :: Elevator -> IO()

data Elevator = Elevator
    { elevatorName :: String,
      elevatorIntersection :: Intersection,
      elevatorHallway :: Hallway,
      elevatorPositionAlongHallway :: Int,
      elevatorFloor :: Int,
      elevatorConnectNodes :: [Node],
      elevatorConnectedNodeNames :: [Node]
    } deriving (Show)

-- constructor
constrcutorElevator name hallway positionAlongHallway floor

-- functions
addConnectedNodeName elevator nodeName = 

resolveConnections 

getConnectedNodes elevator = elevator 

displayInfo elevator = putStrLn ("Elevator: " ++ elevatorName elevator ++ " at " ++ show (elevatorPositionAlongHallway elevator) ++ "m along " ++ hallwayName (elevatorHallway elevator))