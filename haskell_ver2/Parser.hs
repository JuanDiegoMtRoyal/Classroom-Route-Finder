module Parser where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Char
import System.IO
import Control.Monad
import Debug.Trace -- used as a temporary solution to error printing

import DataTypes
import Graph
import HallwayOps
import IntersectionOps

data Parser = Parser 
    { pGraph :: Graph
    } deriving (Show)

-- Constructor for Parser
createParser :: Graph -> Parser
createParser graph = Parser graph

-- Initialize building map from file.txt
initializeBuildingMap :: Parser -> FilePath -> IO Graph
initializeBuildingMap parser filename = do
    contents <- readFile filename
    let lines = filter (not . null) (map trim (splitLines contents))
    let (graph, _) = parseLines lines (pGraph parser) "Unknown"
    let resolvedGraph = resolveAllConnections graph
    return resolvedGraph

-- Split a string into lines, easier for parsing
splitLines :: String -> [String]
splitLines = lines

-- Trim whitespace from a string
-- Note: it is reversed in case there is existing whitespace at the end!
trim :: String -> String
trim s = dropWhile isSpace(reverse(dropWhile isSpace(reverse s)))

-- Parse lines from the file
parseLines :: [String] -> Graph -> String -> (Graph, String)
parseLines [] graph _ = (graph, "Unknown")
parseLines (line:rest) graph currentSection
    | isPrefixOf "#" line = 
        let newSection = if "Intersection Format" `isInfixOf` line then "Intersection"
                         else if "Hallway Format" `isInfixOf` line then "Hallway"
                         else if "Classroom Format" `isInfixOf` line then "Classroom"
                         else if "Stairs Format" `isInfixOf` line then "Stairs"
                         else if "Elevator Format" `isInfixOf` line then "Elevator"
                         else currentSection
        in parseLines rest graph newSection
        
    | otherwise = 
        let newGraph = case currentSection of
                         "Hallway" -> parseHallway graph line
                         "Classroom" -> parseClassroom graph line
                         "Stairs" -> parseStairs graph line
                         "Elevator" -> parseElevator graph line
                         "Intersection" -> parseIntersection graph line
                         _ -> graph
        in parseLines rest newGraph currentSection

-- Helper function to check if a string is a prefix of another
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Helper function to check if a string is an infix of another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails l | (_:xs) <- l = l : tails xs

-- Parse hallway information
parseHallway :: Graph -> String -> Graph
parseHallway graph line =
    let parts = splitParts line
        name = parts !! 0
        building = parts !! 1
        startIntersectionName = parts !! 2
        direction1 = parts !! 3
        direction2 = parts !! 4
        floor = read (parts !! 5) :: Int
        length = read (parts !! 6) :: Int
        
        maybeStartIntersection = case getNode graph startIntersectionName of
            Just (IntersectionNode i) -> Just i
            Nothing -> Just (Intersection startIntersectionName floor 0 [] [])
            _ -> Nothing
            
        hallway = Hallway name building maybeStartIntersection direction1 direction2 floor length [] []
        
        -- Add the hallway to the graph
        updatedGraph = case maybeStartIntersection of
            Just intersection -> 
                let updatedIntersection = addHallwayToIntersection intersection hallway
                    graphWithIntersection = addNode graph (IntersectionNode updatedIntersection)
                in addNode graphWithIntersection (HallwayNode hallway)
            Nothing -> graph
    in updatedGraph

-- Parse classroom information
parseClassroom :: Graph -> String -> Graph
parseClassroom graph line =
    let parts = splitParts line
        name = parts !! 0
        building = parts !! 1
        hallwayName = parts !! 2
        position = read (parts !! 3) :: Int
        compassDirection = parts !! 4
        floor = read (parts !! 5) :: Int
        
        maybeHallway = findHallwayByName graph hallwayName
        
        updatedGraph = case maybeHallway of
            Just hallway ->
                let classroom = ClassroomNode name floor Nothing (Just hallway) position compassDirection
                    updatedHallway = addNodeToHallway hallway classroom
                    graphWithClass = addNode graph classroom
                in addNode graphWithClass (HallwayNode updatedHallway)

            Nothing -> 
                trace ("ERROR FINDING HALLWAY/ADDING CLASSROOM: " ++ line) graph

    in updatedGraph

-- Parse stairs information
parseStairs :: Graph -> String -> Graph
parseStairs graph line =
    let parts = splitParts line
        name = parts !! 0
        building = parts !! 1
        intersectionName = parts !! 2
        position = read (parts !! 3) :: Int
        compassDirection = parts !! 4
        floor = read (parts !! (length parts - 1)) :: Int
        
        connectedNodeNames = intersectionName : [parts !! i | i <- [5..(length parts - 2)]]
        
        stairs = StairsNode name floor Nothing Nothing position [] connectedNodeNames
        
        updatedGraph = addNode graph stairs
    in updatedGraph

-- Parse elevator information
parseElevator :: Graph -> String -> Graph
parseElevator graph line =
    let parts = splitParts line
        name = parts !! 0
        building = parts !! 1
        intersectionName = parts !! 2
        position = read (parts !! 3) :: Int
        compassDirection = parts !! 4
        floor = read (parts !! (length parts - 1)) :: Int
        
        connectedNodeNames = intersectionName : [parts !! i | i <- [5..(length parts - 2)]]
        
        elevator = ElevatorNode name floor Nothing Nothing position [] connectedNodeNames
        
        updatedGraph = addNode graph elevator
    in updatedGraph

-- Parse intersection information
parseIntersection :: Graph -> String -> Graph
parseIntersection graph line =
    let parts = splitParts line
        name = parts !! 0
        floor = read (parts !! 1) :: Int
        posOnHallway = read (parts !! 2) :: Int
        
        connectedItems = [parts !! i | i <- [3..(length parts - 1)]]
        
        maybeIntersection = case getNode graph name of
            Just (IntersectionNode i) -> Just i
            Nothing -> Just (Intersection name floor posOnHallway [] [])
            _ -> Nothing
            
        processConnectedItems inter [] = inter
        processConnectedItems inter (item:rest) =
            if "hallway" `isPrefixOf` item then
                case findHallwayByName graph item of
                    Just hallway -> 
                        let updatedInter = addHallwayToIntersection inter hallway
                            updatedHallway = addIntersectionToHallway hallway inter
                        in processConnectedItems updatedInter rest
                    Nothing -> processConnectedItems inter rest
            else
                case getNode graph item of
                    Just node ->
                        let updatedInter = addConnectedNodeToIntersection inter node
                        in processConnectedItems updatedInter rest

                    Nothing -> 
                        {- trace ("Node was null: " ++ item) -} (processConnectedItems inter rest)

        updatedGraph = case maybeIntersection of
            Just intersection -> 
                let processedIntersection = processConnectedItems intersection connectedItems
                    graphWithInter = addNode graph (IntersectionNode processedIntersection)
                in connectAdjacentNodes graphWithInter processedIntersection
            Nothing -> graph
    in updatedGraph

-- Helper function to connect intersection to adjacent nodes in hallways
connectAdjacentNodes :: Graph -> Intersection -> Graph
connectAdjacentNodes graph intersection =
    let hallways = getConnectedHallways intersection
        
        connectInHallway graph hallway =
            let hallwayNodes = getHallwayNodes hallway
                maybeIndex = List.findIndex (\n -> nodeName n == iName intersection) hallwayNodes
            in case maybeIndex of
                Nothing -> graph
                Just idx ->
                    let prevNodes = if idx > 0 
                                   then [hallwayNodes !! (idx - 1)]
                                   else []
                        nextNodes = if idx < length hallwayNodes - 1
                                   then [hallwayNodes !! (idx + 1)]
                                   else []
                        nodesToConnect = prevNodes ++ nextNodes
                        updatedInter = foldl addConnectedNodeToIntersection intersection nodesToConnect
                    in addNode graph (IntersectionNode updatedInter)
                    
    in foldl connectInHallway graph hallways

-- Split a line into parts (handling parentheses)
splitParts :: String -> [String]
splitParts line =
    let withoutParens = map (\c -> if c == '(' || c == ')' then ',' else c) line
    in map trim (split ',' withoutParens)

-- Split a string by a delimiter
split :: Char -> String -> [String]
split delimiter str = 
    let splitHelper [] acc current = reverse (reverse current : acc)
        splitHelper (x:xs) acc current
            | x == delimiter = splitHelper xs (reverse current : acc) []
            | otherwise = splitHelper xs acc (x:current)
    in splitHelper str [] []

-- Find a hallway by name
findHallwayByName :: Graph -> String -> Maybe Hallway
findHallwayByName graph name =
    let nodes = Map.elems (gNodes graph)
        hallways = [h | HallwayNode h <- nodes]
    in List.find (\h -> hName h == name) hallways
