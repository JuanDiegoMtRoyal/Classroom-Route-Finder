-- import files
import Node
import Hallway

-- ---------------------------------------------------------------------------------------

-- ******** NODES ********
-- FINISHED 03/28/2025
-- tests constructor for nodes
nodeA :: Node
nodeA = constructorNode "a" "b" "c" 1 2
nodeA2 :: Node
nodeA2 = constructorNode "a" "b" "c" 1 2

-- tests equals for 2 sets of identical nodes (TRUE)
isEqualsA2 = equals nodeA nodeA2

-- tests equals for 2 sets of different nodes (FALSE)
nodeB :: Node
nodeB = constructorNode "d" "e" "f" 3 4
isEqualsAB = equals nodeA nodeB

-- tests equals for nodes against itself (TRUE)
nodeC :: Node
nodeC = constructorNode "g" "h" "i" 5 6
isEqualsCC = equals nodeC nodeC  -- true

-- ---------------------------------------------------------------------------------------

-- ******** HALLWAY ********
-- WORK IN PROGRESS
-- tests constructor for hallway
hallwayA :: Hallway
hallwayA = constructorHallway "x" "y" "z" "dir1" "dir2" 1 2 -- [] []

hallwayB :: Hallway
hallwayB = constructorHallway "w" "u" "v" "dir3" "dir4" 3 4 -- [] []

hallwayC :: Hallway
hallwayC = constructorHallway "test" "add" "intersectionssss" "dir5" "dir6" 5 6 -- [] []

-- tests addNode
-- NOTE: follow this format to UPDATE the previous list
main :: IO ()
main = do
    putStrLn ("Running addNode test..." ++ "\n\nOriginal: \n" ++ show(hallwayC) ++ "\n")
    let hallwayC'  = addNode hallwayC nodeA  -- First update
    putStrLn ("Added nodeA: \n" ++ show(hallwayC') ++ "\n")
    let hallwayC'' = addNode hallwayC' nodeB -- Second update
    putStrLn ("Added nodeB: \n" ++ show(hallwayC''))

-- tests getNodes
mainGN :: IO()
mainGN = do
    putStrLn "Running getNodes test....\n"
    let hallwayC'  = addNode hallwayC nodeA  -- First update
    let hallwayC'' = addNode hallwayC' nodeB -- Second update
    let sorted = getNodes hallwayC''
    putStrLn ("Sorted Nodes: \n" ++ show sorted)


-- tests addIntersection
-- NOTE: follow this format to UPDATE the previous list
main2 :: IO ()
main2 = do
    putStrLn ("Running addIntersection test..." ++ "\n\nOriginal: \n" ++ show(hallwayA) ++ "\n")
    let hallwayA'  = addIntersection hallwayA "SOMETHING1"  -- First update
    putStrLn ("Adding SOMETHING1 as Intersection: \n" ++ show(hallwayA') ++ "\n")
    let hallwayA'' = addIntersection hallwayA' "SOMETHING2" -- Second update
    putStrLn ("Adding SOMETHING2 as Intersection: " ++ show(hallwayA'') ++ "\n")

-- tests getAllIntersections
main3 :: IO ()
main3 = do
    putStrLn ("Running getAllIntersections test..." ++ "\n\nOriginal: \n" ++ show(hallwayB) ++ "\n")
    let hallwayB'  = addIntersection hallwayB "SOMETHING1"  -- First update
    let hallwayB'' = addIntersection hallwayB' "SOMETHING2" -- Second update
    putStrLn ("Added 2 intersections...\n")
    let intersection = getAllIntersections hallwayB''
    putStrLn ("All Intersections: \n" ++ show(intersection))

-- tests getLength
main4 :: IO ()
main4 = do
    putStrLn ("Running getLength test..." ++ "\n\nOriginal is: \n" ++ show(getLength hallwayB) ++ "\n")
    let hallwayB'  = addIntersection hallwayB "SOMETHING1"  -- First update
    let hallwayB'' = addIntersection hallwayB' "SOMETHING2" -- Second update
    let length = getLength hallwayB
    putStrLn ("Length of hallwayB is: \n" ++ show(length))

-- WORKS: addNode
-- WORKS:   getNodes
-- WORKS: getStartIntersection
-- WORKS:  getAllIntersections
-- WORKS: addIntersection
-- WORKS:  getLength
