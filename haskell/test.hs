-- import files
import Node
import Hallway
import Classroom
import Intersection
import Stairs
--import Elevator
import Graph
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
                putStrLn ("Is nodeA and nodeA2 the same?\n" ++ show(nodeA == nodeA2))

-- tests equals for 2 sets of different nodes (FALSE)
testEqualsAB :: IO()
testEqualsAB = do
                putStrLn ("Running equals test on A + B...\nShould be FALSE:\n")
                putStrLn ("Is nodeA and nodeB the same?\n" ++ show(nodeA == nodeB))

-- tests equals for nodes against itself (TRUE)
testEqualsCC :: IO()
testEqualsCC = do
                putStrLn ("Running equals test on C + C...\nShould be TRUE:\n")
                putStrLn ("Is nodeC and nodeC the same?\n" ++ show(nodeC == nodeC))

-- tests extractValue
notMaybeIntersection :: Intersection
notMaybeIntersection = extractValue maybeIntersection
testExtractValue :: IO()
testExtractValue = do
                      putStrLn("Running extractValue for Intersection...\n\nDisplaying maybeIntersection:\n" ++ show(notMaybeIntersection))

-- WORKS: equals
-- WORKS: extractValue

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
    let hallwayC'  = hallwayAddNode hallwayC nodeA  -- First update
    putStrLn ("Added nodeA: \n" ++ show(hallwayC') ++ "\n")
    let hallwayC'' = hallwayAddNode hallwayC' nodeB -- Second update
    putStrLn ("Added nodeB: \n" ++ show(hallwayC''))

-- tests getNodes
testHallwayGetNode :: IO()
testHallwayGetNode = do
    putStrLn "Running getNodes test....\n"
    let hallwayC'  = hallwayAddNode hallwayC nodeA  -- First update
    let hallwayC'' = hallwayAddNode hallwayC' nodeB -- Second update
    let sorted = hallwayGetNodes hallwayC''
    putStrLn ("Sorted Nodes: \n" ++ show sorted)


-- tests addIntersection
-- NOTE: follow this format to UPDATE the previous list
testHallwayAddIntersection :: IO ()
testHallwayAddIntersection = do
    putStrLn ("Running addIntersection test..." ++ "\n\nOriginal: \n" ++ show(hallwayA) ++ "\n")
    let hallwayA'  = hallwayAddIntersection hallwayA "intersectionAA"  -- First update
    putStrLn ("Adding intersectionAA as Intersection: \n" ++ show(hallwayA') ++ "\n")
    let hallwayA'' = hallwayAddIntersection hallwayA' "intersectionAAAA" -- Second update
    putStrLn ("Adding intersectionAAAA as Intersection: " ++ show(hallwayA'') ++ "\n")

-- tests getAllIntersections
testHallwayGetAllIntersections :: IO ()
testHallwayGetAllIntersections = do
    putStrLn ("Running getAllIntersections test..." ++ "\n\nOriginal: \n" ++ show(hallwayB) ++ "\n")
    let hallwayB'  = hallwayAddIntersection hallwayB "intersectionAA"  -- First update
    let hallwayB'' = hallwayAddIntersection hallwayB' "intersectionAAAA" -- Second update
    putStrLn ("Added 2 intersections...\n")
    let intersection = hallwayGetAllIntersections hallwayB''
    putStrLn ("All Intersections: \n" ++ show(intersection))

-- tests getLength
testHallwayGetLength :: IO ()
testHallwayGetLength = do
    putStrLn ("Running getLength test..." ++ "\n\nOriginal is: \n" ++ show(hallwayGetLength hallwayB) ++ "\n")
    let hallwayB'  = hallwayAddIntersection hallwayB "intersectionAA"  -- First update
    let hallwayB'' = hallwayAddIntersection hallwayB' "intersectionAAAA" -- Second update
    let length = hallwayGetLength hallwayB
    putStrLn ("Length of hallwayB is: \n" ++ show(length))

-- WORKS: hallwayAddNode
-- WORKS: hallwayGetNodes
-- WORKS: hallwayGetStartIntersection
-- WORKS: hallwayGetAllIntersections
-- WORKS: hallwayAddIntersection
-- WORKS: hallwayGetLength

-- ---------------------------------------------------------------------------------------
-- ******** CLASSROOM ********
-- FINISHED 03/31/2025
-- test constructor for classroom
classA :: Classroom
classA = constructorClassroom "classA" "buildingA" hallwayA 5 "NE" 10

classB :: Classroom
classB = constructorClassroom "classB" "buildingB" hallwayB 15 "SW" 20

-- test displayInfo
testClassroomDisplay :: IO()
testClassroomDisplay = do
                     putStrLn ("Running displayInfo for Classroom...\n")
                     putStrLn ("Displaying classA:")
                     classroomDisplayInfo classA
                     putStrLn ("\nDisplaying classB:")
                     classroomDisplayInfo classB

-- WORKS: classroomDisplayInfo

-- ---------------------------------------------------------------------------------------
-- ******** INTERSECTION ********
-- FINISHED 03/31/2025
-- test constructor for intersection
intersectionA :: Intersection
intersectionA = constructorIntersection "intersectionA" 50 1

intersectionB :: Intersection
intersectionB = constructorIntersection "intersectionB" 75 2

maybeIntersection :: Maybe Intersection
maybeIntersection = Just (constructorIntersection "test" 1 2)

-- tests addHallway
testIntersectionAddHallway :: IO()
testIntersectionAddHallway = do
                                putStrLn ("Running addHallway for Intersection...\n")
                                putStrLn ("Displaying original Intersection A:\n" ++ show(intersectionA) ++ "\n")
                                let testIntA = intersectionAddHallway intersectionA hallwayA
                                let testIntAA = intersectionAddHallway testIntA hallwayA
                                putStrLn ("Added 2 identical hallways to intersectionA...\nHallway list should have only one element:\nDisplaying hallways for Intersection A:\n" ++ show(testIntAA) ++ "\n")
                                putStrLn ("Displaying original Intersection B:\n" ++ show(intersectionB) ++ "\n")
                                let testIntB = intersectionAddHallway intersectionB hallwayA
                                let testIntBB = intersectionAddHallway testIntB hallwayB
                                putStrLn ("Added 2 different hallways to intersectionB...\nDisplaying hallways for Intersection B:\n" ++ show(testIntBB) ++ "\n")
                                putStrLn ("Calling getConnectedHallways for intersectionB...")
                                let testIntBBgcn = intersectionGetConnectedHallways testIntBB
                                putStrLn ("Hallways existing in intersectionB...\nThere should be 2 different hallways:\n" ++ show(testIntBBgcn))

-- tests getConnectedHallways
testIntersectionGetConnectedHallways :: IO()
testIntersectionGetConnectedHallways = do
                                          putStrLn ("Running getConnectedHallways for Intersection...\n")
                                          let testIntA = intersectionGetConnectedHallways intersectionA
                                          let testIntA = intersectionGetConnectedHallways intersectionA
                                          putStrLn ("Displaying for Intersection A:\n" ++ show(testIntA) ++ "\n")
                                          let testIntB = intersectionGetConnectedHallways intersectionB
                                          putStrLn ("Displaying for Intersection B:\n" ++ show(testIntB) ++ "\n")

-- tests addConnectedNode
testIntersectionAddConnectedNode :: IO()
testIntersectionAddConnectedNode = do
                                      putStrLn ("Running addConnectedNode for Intersection...\n")
                                      putStrLn ("Displaying original Intersection A:\n" ++ show(intersectionA) ++ "\n")
                                      let testIntA = intersectionAddConnectedNode intersectionA nodeA
                                      let testIntAA = intersectionAddConnectedNode testIntA nodeA
                                      putStrLn ("Added 2 identical nodes to intersectionA...\nNode list should have only one element:\nDisplaying nodes for Intersection A:\n" ++ show(testIntAA) ++ "\n")
                                      putStrLn ("Displaying original Intersection B:\n" ++ show(intersectionB) ++ "\n")
                                      let testIntB = intersectionAddConnectedNode intersectionB nodeA
                                      let testIntBB = intersectionAddConnectedNode testIntB nodeB
                                      putStrLn ("Added 2 different nodes to intersectionB...\nDisplaying nodes for Intersection B:\n" ++ show(testIntBB) ++ "\n")

-- tests getConnectedNodes
testIntersectionGetConnectedNodes :: IO()
testIntersectionGetConnectedNodes = do
                                      putStrLn ("Running getConnectedNode for Intersection...\n")
                                      let testIntB = intersectionAddConnectedNode intersectionB nodeA
                                      let testIntBB = intersectionAddConnectedNode testIntB nodeB
                                      putStrLn ("Added 2 different nodes to intersectionB...\nDisplaying nodes for Intersection B:\n" ++ show(testIntBB) ++ "\n")
                                      putStrLn ("Calling getConnectedNodes for intersectionB...")
                                      let testIntBBgcn = intersectionGetConnectedNodes testIntBB
                                      putStrLn ("Nodes existing in intersectionB...\nThere should be 2 different nodes:\n" ++ show(testIntBBgcn))

-- tests displayInfo
testIntersectionDisplay :: IO()
testIntersectionDisplay = do
                     putStrLn ("Running displayInfo for Intersection...\n")
                     putStrLn ("Displaying intersectionA:")
                     intersectionDisplayInfo intersectionA
                     putStrLn ("\nDisplaying intersectionB:")
                     intersectionDisplayInfo intersectionB

-- WORKS: intersectionAddHallway
-- WORKS: intersectionGetConnectedHallways
-- WORKS: intersectionAddConnectedNode
-- WORKS: intersectionGetConnectedNode
-- WORKS: intersectionDisplayInfo
-- WORKS: extractValue

-- ---------------------------------------------------------------------------------------
-- ******** STAIRS ********
-- DATE
-- test constructor for stairs
stairsA :: Stairs
stairsA = constructorStairs "stairsA" 50 1

stairsB :: Stairs
stairsB = constructorStairs "stairsB" 75 2

-- tests addConnectedNodeName
testStairsAddConnectedNodeName :: IO()
testStairsAddConnectedNodeName = do
                                    putStrLn ("Running addConnectedNodeName for Stairs...\n")
                                    putStrLn ("Displaying original Stairs A:\n" ++ show(stairsA) ++ "\n")
                                    let testStairsA = stairsAddConnectedNodeName stairsA (stairsName stairsA)
                                    let testStairsAA = stairsAddConnectedNodeName testStairsA (stairsName stairsA)
                                    putStrLn ("Added 2 identical nodes to stairsA...\nDisplaying nodes for Stairs A:\n" ++ show(testStairsAA) ++ "\n")                                    
                                    let testStairsAB = stairsAddConnectedNodeName testStairsAA (stairsName stairsB)
                                    putStrLn ("Adding Stairs B...\n" ++ show(testStairsAB))

-- tests resolveConnections
-- fix
-- stairsResolveConnections graph = 

-- tests getConnectedNodes
testStairsGetConnectedNodes :: IO()
testStairsGetConnectedNodes = do
                                  putStrLn ("Running getConnectedNodes for Stairs...\n")
                                  putStrLn ("Displaying current nodes in Stairs A:\n" ++ show(stairsConnectedNodes stairsA) ++ "\n")
                                  let testStairsA = stairsA {stairsConnectedNodes = [nodeA, nodeB]}
                                  let testStairsGCN = stairsGetConnectedNodes testStairsA
                                  putStrLn ("Added 2 entries...\nDisplaying getconnectNodes on stairsA:\n" ++ show(testStairsGCN))

-- tests displayInfo
testStairsDisplay :: IO()
testStairsDisplay = do
                     putStrLn ("Running displayInfo for Stairs...\n")
                     putStrLn ("Displaying StairsA:")
                     stairsDisplayInfo stairsA
                     putStrLn ("\nDisplaying stairsB:")
                     stairsDisplayInfo stairsB


-- WORKS: stairsAddConnectedNodeName
-- FIX: stairsResolveConnections
-- WORKS: stairsGetConnectedNodes
-- FIX: stairsDisplayInfo

-- ---------------------------------------------------------------------------------------
