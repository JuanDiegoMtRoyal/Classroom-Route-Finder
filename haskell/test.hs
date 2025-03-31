-- import files
import Node
import Hallway
import Classroom

-- ---------------------------------------------------------------------------------------
-- HERE IS A LIST OF TEST RUNS YOU MAY CALL
-- RUN IN GHCi AND THE PRINT WILL SHOW YOU A LIST OF WHAT YOU CAN CALL
mainTest :: IO()
mainTest = do
              putStrLn("Here is a list of test runs you may call:\n")
              putStrLn("For Node:\n1.) testEqualsA\n2.) testEqualsAB\n3.) testEqualsCC\n")
              putStrLn("For Hallway:\n1.) testHallwayAddNode\n2.) testHallwayGetNode\n3.) testHallwayAddIntersection\n4.) testHallwayGetAllIntersections\n5.) testHallwayGetLength\n")
              putStrLn("For Classroom:\n1.) testClassroomDisplay\n")

-- ---------------------------------------------------------------------------------------
-- ******** NODES ********
-- FINISHED 03/28/2025
-- tests constructor for nodes
nodeA :: Node
nodeA = constructorNode "nodeA" "intersectionA" "hallwayA" 1 2

nodeA2 :: Node
nodeA2 = constructorNode "nodeA" "intersectionA" "hallwayA" 1 2

nodeB :: Node
nodeB = constructorNode "nodeB" "intersectionB" "hallwayB" 3 4

nodeC :: Node
nodeC = constructorNode "nodeC" "intersectionC" "hallwayC" 5 6


-- tests equals for 2 sets of identical nodes (TRUE)
testEqualsA :: IO()
testEqualsA = do
                putStrLn ("Running equals test on A + A2...\nShould be TRUE:\n")
                putStrLn ("Is nodeA and nodeA2 the same?\n" ++ show(equals nodeA nodeA2))

-- tests equals for 2 sets of different nodes (FALSE)
testEqualsAB :: IO()
testEqualsAB = do
                putStrLn ("Running equals test on A + B...\nShould be FALSE:\n")
                putStrLn ("Is nodeA and nodeB the same?\n" ++ show(equals nodeA nodeB))

-- tests equals for nodes against itself (TRUE)
testEqualsCC :: IO()
testEqualsCC = do
                putStrLn ("Running equals test on C + C...\nShould be TRUE:\n")
                putStrLn ("Is nodeC and nodeC the same?\n" ++ show(equals nodeC nodeC))

-- WORKS: equals
-- ---------------------------------------------------------------------------------------

-- ******** HALLWAY ********
-- FINISHED 03/30/2025
-- tests constructor for hallway
hallwayA :: Hallway
hallwayA = constructorHallway "hallwayA" "buildingA" "startIntersectionA" "dirA1" "dirA2" 1 2 -- [] []

hallwayB :: Hallway
hallwayB = constructorHallway "hallwayB" "buildingB" "startIntersectionB" "dirB1" "dirB2" 3 4 -- [] []

hallwayC :: Hallway
hallwayC = constructorHallway "hallwayC" "buildingC" "startIntersectionC" "dirC1" "dirC2" 5 6 -- [] []

-- tests addNode
-- NOTE: follow this format to UPDATE the previous list
testHallwayAddNode :: IO ()
testHallwayAddNode = do
    putStrLn ("Running addNode test..." ++ "\n\nOriginal: \n" ++ show(hallwayC) ++ "\n")
    let hallwayC'  = addNode hallwayC nodeA  -- First update
    putStrLn ("Added nodeA: \n" ++ show(hallwayC') ++ "\n")
    let hallwayC'' = addNode hallwayC' nodeB -- Second update
    putStrLn ("Added nodeB: \n" ++ show(hallwayC''))

-- tests getNodes
testHallwayGetNode :: IO()
testHallwayGetNode = do
    putStrLn "Running getNodes test....\n"
    let hallwayC'  = addNode hallwayC nodeA  -- First update
    let hallwayC'' = addNode hallwayC' nodeB -- Second update
    let sorted = getNodes hallwayC''
    putStrLn ("Sorted Nodes: \n" ++ show sorted)


-- tests addIntersection
-- NOTE: follow this format to UPDATE the previous list
testHallwayAddIntersection :: IO ()
testHallwayAddIntersection = do
    putStrLn ("Running addIntersection test..." ++ "\n\nOriginal: \n" ++ show(hallwayA) ++ "\n")
    let hallwayA'  = addIntersection hallwayA "intersectionAA"  -- First update
    putStrLn ("Adding intersectionAA as Intersection: \n" ++ show(hallwayA') ++ "\n")
    let hallwayA'' = addIntersection hallwayA' "intersectionAAAA" -- Second update
    putStrLn ("Adding intersectionAAAA as Intersection: " ++ show(hallwayA'') ++ "\n")

-- tests getAllIntersections
testHallwayGetAllIntersections :: IO ()
testHallwayGetAllIntersections = do
    putStrLn ("Running getAllIntersections test..." ++ "\n\nOriginal: \n" ++ show(hallwayB) ++ "\n")
    let hallwayB'  = addIntersection hallwayB "intersectionAA"  -- First update
    let hallwayB'' = addIntersection hallwayB' "intersectionAAAA" -- Second update
    putStrLn ("Added 2 intersections...\n")
    let intersection = getAllIntersections hallwayB''
    putStrLn ("All Intersections: \n" ++ show(intersection))

-- tests getLength
testHallwayGetLength :: IO ()
testHallwayGetLength = do
    putStrLn ("Running getLength test..." ++ "\n\nOriginal is: \n" ++ show(getLength hallwayB) ++ "\n")
    let hallwayB'  = addIntersection hallwayB "intersectionAA"  -- First update
    let hallwayB'' = addIntersection hallwayB' "intersectionAAAA" -- Second update
    let length = getLength hallwayB
    putStrLn ("Length of hallwayB is: \n" ++ show(length))

-- WORKS: addNode
-- WORKS: getNodes
-- WORKS: getStartIntersection
-- WORKS: getAllIntersections
-- WORKS: addIntersection
-- WORKS: getLength
-- ---------------------------------------------------------------------------------------

-- ******** CLASSROOM ********
-- test constructor for classroom
classA :: Classroom
classA = constructorClassroom "classA" "buildingA" hallwayA 5 "NE" 10

classB :: Classroom
classB = constructorClassroom "classB" "buildingB" hallwayB 15 "SW" 20

testClassroomDisplay :: IO()
testClassroomDisplay = do
                     putStrLn ("Running displayInfo for Classroom...\n")
                     putStrLn ("Displaying classA:")
                     displayInfo classA
                     putStrLn ("\nDisplaying classB:")
                     displayInfo classB