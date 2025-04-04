module Classroom
(
    Classroom(..),
    constructorClassroom,
    classroomDisplayInfo
)
where

import Node
import Hallway

constructorClassroom :: String -> String -> Hallway -> Int -> String -> Int -> Classroom
classroomDisplayInfo :: Classroom -> IO()

data Classroom = Classroom
    { classroomName :: String,
      classroomHallway :: Hallway,
      classroomPositionAlongHallway :: Int,
      classroomFloor :: Int,
      classroomBuilding :: String,
      classroomCompassDirection :: String
    } deriving (Show)

-- constructor
constructorClassroom name building hallway positionAlongHallway compassDirection floor = Classroom name hallway positionAlongHallway floor building compassDirection

-- functions
classroomDisplayInfo classroom = putStrLn ("-Go " ++ classroomCompassDirection classroom ++ " towards classroom: " ++ classroomName classroom ++ " at " ++ show (classroomPositionAlongHallway classroom) ++ "m along " ++ hallwayName (classroomHallway classroom))