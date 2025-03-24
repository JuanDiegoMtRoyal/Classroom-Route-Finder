module Stairs
(
    Stairs,

    addConnectedFloor,
    displayInfo
)
where

import Node (Node(..))
import Hallway (Hallway(..))

data Stairs = Stairs
    { stairName :: String,
      stairHallway :: Hallway,
      stairPositionAlongHallway :: Int,
      stairFloor :: Int
	  stairsConnectedFloors :: [String]
    } deriving (Show)

-- constructor
createStairs :: String -> Hallway -> Int -> Int -> Stairs
createStairs name hallway positionAlongHallway floor

-- functions
addConnectedFloor :: String -> [String] -> [String]
addConnectedFloor stair connectedFloors = stair : connectedFloors

displayInfo :: Stairs -> IO()
displayInfo stairs = putStrLn ("Stairs: " ++ stairName stairs ++ " at " ++ show (stairPositionAlongHallway stairs) ++ "m along " ++ hallwayName (stairHallway stairs))