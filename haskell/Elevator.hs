module Elevator
(
    Elevator,

    displayInfo
)
where

data Elevator = Elevator
    { elevatorName :: String,
      elevatorHallway :: Hallway,
      elevatorPositionAlongHallway :: Int,
      elevatorFloor :: Int
    } deriving (Show)

-- constructor
createElevator :: String -> Hallway -> Int -> Int -> Elevator
createElevator name hallway positionAlongHallway floor

displayInfo :: Elevator -> IO()
displayInfo elevator = putStrLn ("Elevator: " ++ elevatorName elevator ++ " at " ++ show (elevatorPositionAlongHallway elevator) ++ "m along " ++ hallwayName (elevatorHallway elevator))