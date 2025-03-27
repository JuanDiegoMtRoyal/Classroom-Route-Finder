module Classroom
(
    Classroom,
    
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))

data Classroom = Classroom
    { classroomName :: String
      classroomBuilding :: String
      classroomHallway :: Hallway
      classroomPositionAlongHallway :: Int
      classroomCompassDirection :: String
      classroomFloor :: Int
    } deriving (Show)

-- constructor
constructorClassroom :: String -> String -> Hallway -> Int -> String -> Int
constructorClassroom name building hallway positionAlongHallway compassDirection floor

-- functions
displayInfo :: Classroom -> IO()
displayInfo classroom = putStrLn ("-Go " ++ classroomCompassDirection classroom ++ " towards classroom: " ++ classroomName classroom ++ " at " ++ show (classroomPositionAlongHallway classroom) ++ "m along " ++ hallwayName (classroomHallway classroom))