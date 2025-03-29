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
    let hallwayC'  = addNode hallwayC nodeA  -- First update
    let hallwayC'' = addNode hallwayC' nodeB -- Second update
    print hallwayC''  -- Only this has both nodes

-- tests addIntersection
-- NOTE: follow this format to UPDATE the previous list
main2 :: IO ()
main2 = do
    let hallwayA'  = addIntersection hallwayA "SOMETHING1"  -- First update
    let hallwayA'' = addIntersection hallwayA' "SOMETHING2" -- Second update
    print hallwayA''  -- Only this has both nodes

-- tests getAllIntersections
main3 :: IO ()
main3 = do
    let hallwayB'  = addIntersection hallwayB "SOMETHING1"  -- First update
    let hallwayB'' = addIntersection hallwayB' "SOMETHING2" -- Second update
    print (getAllIntersections hallwayB'')

-- tests getLength
main4 :: IO ()
main4 = do
    let hallwayB'  = addIntersection hallwayB "SOMETHING1"  -- First update
    let hallwayB'' = addIntersection hallwayB' "SOMETHING2" -- Second update
    print (getLength hallwayB'')

-- WORKS: addNode
-- FIX:   getNodes
-- WORKS: getStartIntersection
-- WORKS:  getAllIntersections
-- WORKS: addIntersection
-- WORKS:  getLength
