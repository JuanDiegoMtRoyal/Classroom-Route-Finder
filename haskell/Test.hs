import DataTypes
import Graph
import HallwayOps
import IntersectionOps
import qualified Data.Maybe as Maybe

-- Example usage
main :: IO ()
main = do
    -- Create intersections
    let intersection1 = Intersection "I1" 1 0 [] []
        intersection2 = Intersection "I2" 1 50 [] []
    
    -- Create a hallway connecting the intersections
    let hallway = Hallway "H1" "Building A" (Just intersection1) "North" "South" 1 50 [] [intersection1, intersection2]
    
    -- Create a classroom
    let classroom = ClassroomNode "C101" 1 (Just intersection1) (Just hallway) 25 "East"
    
    -- Create stairs connecting floors
    let stairs1 = StairsNode "S1" 1 (Just intersection1) Nothing 10 [] ["S2"]
        stairs2 = StairsNode "S2" 2 Nothing Nothing 10 [] ["S1"]
    
    -- Create an elevator
    let elevator1 = ElevatorNode "E1" 1 (Just intersection2) Nothing 40 [] ["E2"]
        elevator2 = ElevatorNode "E2" 2 Nothing Nothing 40 [] ["E1"]
    
    -- Build the graph
    let graph = addNode (addNode (addNode (addNode (addNode (addNode emptyGraph 
                 (IntersectionNode intersection1)) 
                 (IntersectionNode intersection2)) 
                 classroom) 
                 stairs1) 
                 stairs2) 
                 elevator1
        graphWithElevator2 = addNode graph elevator2
    
    -- Resolve all connections
    let resolvedGraph = resolveAllConnections graphWithElevator2
    
    -- Display information
    putStrLn "Example Navigation:"
    case getNode resolvedGraph "C101" of
        Just node -> displayInfo node
        Nothing -> putStrLn "Classroom not found"
    
    case getNode resolvedGraph "I1" of
        Just node -> displayInfo node
        Nothing -> putStrLn "Intersection not found"
    
    case getNode resolvedGraph "S1" of
        Just node -> displayInfo node
        Nothing -> putStrLn "Stairs not found"