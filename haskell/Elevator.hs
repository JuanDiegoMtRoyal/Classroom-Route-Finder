module Elevator
(
    Elevator,

    addConnectedNodeName,
    resolveConnections,
    getConnectedNodes,
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))

data Elevator = Elevator
    { elevatorName :: String,
      elevatorHallway :: Hallway,
      elevatorPositionAlongHallway :: Int,
      elevatorFloor :: Int,
      elevatorConnectNodes :: [Node],
      elevatorConnectedNodeNames :: [Node]
    } deriving (Show)

-- constructor
constructorElevator :: String -> Hallway -> Int -> Int -> Elevator
constrcutorElevator name hallway positionAlongHallway floor

-- functions
addConnectedNodeName :: Elevator -> String -> Elevator
addConnectedNodeName elevator nodeName = 

resolveConnections :: Elevator -> Graph -> Elevator
resolveConnections 

getConnectedNodes :: Elevator -> [Node]
getConnectedNodes elevator = elevator 

displayInfo :: Elevator -> IO()
displayInfo elevator = putStrLn ("Elevator: " ++ elevatorName elevator ++ " at " ++ show (elevatorPositionAlongHallway elevator) ++ "m along " ++ hallwayName (elevatorHallway elevator))