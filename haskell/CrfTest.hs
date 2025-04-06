module Main where

import DataTypes
import Graph
import ClassroomFinder

import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Testing - Classroom Route Finder (CRF)"
    
    -- Create a test graph
    let testGraph = createTestGraph
        crf = newCRF testGraph
    
    -- Test finding a route
    putStrLn "\nTest 1: Finding a route from EB1113 to EB1113 with no mobility constraints"
    case findRoute crf "EB1113" "EB1113" 5 False of
        Just route -> displayRoute route
        Nothing -> putStrLn "Failed to find route"
    
    -- Test with mobility constraints
    putStrLn "\nTest 2: Finding a route from EB1113 to EB2138 with mobility constraints"
    case findRoute crf "EB1113" "EB2138" 5 True of
        Just route -> displayRoute route
        Nothing -> putStrLn "Failed to find route"
    
    -- Test with a non-existent room
    putStrLn "\nTest 3: Testing with non-existent room"
    case findRoute crf "EB1113" "XX0000" 5 False of
        Just route -> displayRoute route
        Nothing -> putStrLn "Failed to find route (failed to find room)"
    
    -- Test with insufficient time
    putStrLn "\nTest 4: Testing with 0 as the time constraint"
    case findRoute crf "EB1113" "EB1112" 0 False of
        Just route -> displayRoute route
        Nothing -> putStrLn "Failed to find route (invalid time constraint)"


-- Create a test graph with sample buildings, hallways, and classrooms
createTestGraph :: Graph
createTestGraph = resolveAllConnections $ addTestNodes emptyGraph

-- Add all test nodes to the graph
addTestNodes :: Graph -> Graph
addTestNodes graph =
    let -- Create intersections
        int1 = Intersection "bisset_South_Intersection_F1" 1 0 [] []  -- base1
        int2 = Intersection "bisset_NorthEast_Intersection_F1" 1 50 [] []
        int3 = Intersection "bisset_NorthWest_Intersection_F1" 1 60 [] []

        int4 = Intersection "bisset_South_Intersection_F2" 2 0 [] []  -- base2
        int5 = Intersection "bisset_NorthEast_Intersection_F2" 2 50 [] []
        int6 = Intersection "bisset_NorthWest_Intersection_F2" 2 60 [] []

        int7 = Intersection "bisset_South_Intersection_F3" 3 0 [] []  -- base3
        int8 = Intersection "bisset_NorthEast_Intersection_F3" 3 50 [] []
        int9 = Intersection "bisset_NorthWest_Intersection_F3" 3 60 [] []

        -- Create hallways
        hall1 = Hallway "hallwayEB1_SW_NE" "bisset" (Just int1) "sw" "ne" 1 50 [] []
        hall2 = Hallway "hallwayEB1_SE_NW" "bisset" (Just int1) "se" "nw" 1 60 [] []

        hall3 = Hallway "hallwayEB2_SW_NE" "bisset" (Just int4) "sw" "ne" 2 50 [] []
        hall4 = Hallway "hallwayEB2_SE_NW" "bisset" (Just int4) "se" "nw" 2 60 [] []

        hall5 = Hallway "hallwayEB3_SW_NE" "bisset" (Just int7) "sw" "ne" 3 50 [] []
        hall6 = Hallway "hallwayEB3_SW_NW" "bisset" (Just int7) "se" "nw" 3 60 [] []
        
        -- Create nodes
        classroom1 = ClassroomNode "EB1113" 1 (Just int1) (Just hall1) 28 "NE"
        classroom2 = ClassroomNode "EB1112" 1 (Just int1) (Just hall1) 27 "NE"
        classroom3 = ClassroomNode "EB2138" 2 (Just int4) (Just hall3) 50 "NE"
        classroom4 = ClassroomNode "EB2136" 2 (Nothing) (Just hall3) 46 "NE"
        classroom5 = ClassroomNode "EB3135" 3 (Just int7) (Just hall5) 50 "NE"
        classroom6 = ClassroomNode "EB3134" 3 (Just int7) (Just hall5) 49 "NE"
        
        -- Create stairs and elevators
        stairs1 = StairsNode "stairsEB01_F1" 1 (Just int1) Nothing 4 [] ["stairsEB01_F2"]
        stairs2 = StairsNode "stairsEB01_F2" 2 (Just int4) Nothing 4 [] ["stairsEB01_F1", "stairsEB01_F3"]
        stairs3 = StairsNode "stairsEB01_F3" 3 (Just int4) Nothing 4 [] ["stairsEB01_F2"]
        
        elevator1 = ElevatorNode "elevatorEB02_F1" 1 (Just int2) Nothing 50 [] ["elevatorEB02_F2"]
        elevator2 = ElevatorNode "elevatorEB02_F2" 2 (Just int5) Nothing 50 [] ["elevatorEB02_F1", "elevatorEB02_F3"]
        elevator3 = ElevatorNode "elevatorEB02_F3" 3 (Just int8) Nothing 50 [] ["elevatorEB02_F2"]

        -- Add all nodes to the graph
        updatedGraph = foldl addNode graph [
            IntersectionNode int1, 
            IntersectionNode int2,
            IntersectionNode int4, 
            IntersectionNode int7,
            HallwayNode hall1, 
            HallwayNode hall3, 
            HallwayNode hall6,
            classroom1, 
            classroom2, 
            classroom3,
            classroom4, 
            classroom5, 
            classroom6,
            stairs1, 
            stairs2,
            stairs3,
            elevator1, 
            elevator2,
            elevator3
            ]
    in updatedGraph