module Stairs
(
    Stairs,
    
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
    } deriving (Show)

createStairs :: String -> Hallway -> Int -> Int -> Stairs
createStairs name hallway positionAlongHallway floor

displayInfo :: Stairs -> IO()
displayInfo stairs = putStrLn ("Stairs: " ++ stairName stairs ++ " at " ++ show (stairPositionAlongHallway stairs) ++ "m along " ++ hallwayName (stairHallway stairs))