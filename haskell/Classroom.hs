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
createClassroom :: String -> String -> Hallway -> Int -> String -> Int
createClassroom name building hallway positionAlongHallway compassDirection floor

-- functions
displayInfo :: Classroom -> IO()
displayInfo classroom = putStrLn ("Classroom: " ++ classroomName classroom ++ " at " ++ show (classroomPositionAlongHallway classroom) ++ "m along " ++ hallwayName (classroomHallway classroom))