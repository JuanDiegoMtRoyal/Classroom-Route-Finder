module Classroom
(
    Classroom,
    
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))
import Intersection (Intersection(..))

constructorClassroom :: String -> String -> Hallway -> Int -> String -> Int -> Classroom
displayInfo :: Classroom -> IO()

data Classroom = Classroom
    { classroomName :: String,
      classroomIntersection :: Intersection,
      classroomHallway :: Hallway,
      classroomPositionAlongHallway :: Int,
      classroomFloor :: Int,
      classroomBuilding :: String,
      classroomCompassDirection :: String
    } deriving (Show)

-- constructor
constructorClassroom name building hallway positionAlongHallway compassDirection floor = Classroom name building hallway positionAlongHallway compassDirection floor 

-- functions
displayInfo classroom = putStrLn ("-Go " ++ classroomCompassDirection classroom ++ " towards classroom: " ++ classroomName classroom ++ " at " ++ show (classroomPositionAlongHallway classroom) ++ "m along " ++ hallwayName (classroomHallway classroom))