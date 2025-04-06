module Parser where

import DataTypes
import Graph
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Read (readMaybe)
import Control.Monad (foldM)
import System.IO (hPutStrLn, stderr)

-- Main parser function
parseBuildingMap :: FilePath -> Graph -> IO Graph
parseBuildingMap filename initialGraph = do
    contents <- readFile filename
    let sections = groupSections (cleanLines $ lines contents)
    foldM parseSection initialGraph sections
  where
    cleanLines = filter (not . isCommentOrEmpty) . map trim
    isCommentOrEmpty l = null l || "#" `List.isPrefixOf` l

-- Group lines into sections
groupSections :: [String] -> [(String, [String])]
groupSections [] = []
groupSections (l:ls)
    | "#" `List.isPrefixOf` l = 
        let (body, rest) = break (List.isPrefixOf "#") ls
        in (l, body) : groupSections rest
    | otherwise = groupSections ls  -- Skip lines without headers

-- Parse a section with its lines
parseSection :: Graph -> (String, [String]) -> IO Graph
parseSection graph (header, lines') 
    | "Intersection" `List.isInfixOf` header = foldM parseIntersection graph lines'
    | "Hallway" `List.isInfixOf` header = foldM parseHallway graph lines'
    | "Classroom" `List.isInfixOf` header = foldM parseClassroom graph lines'
    | "Stairs" `List.isInfixOf` header = foldM parseStairs graph lines'
    | "Elevator" `List.isInfixOf` header = foldM parseElevator graph lines'
    | otherwise = do
        hPutStrLn stderr $ "Unknown section: " ++ header
        return graph

-- Helper functions
trim :: String -> String
trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (a, _:b) -> a : splitOn c b
    (a, _)   -> [a]

-- Hallway parser
parseHallway :: Graph -> String -> IO Graph
parseHallway graph line = 
    case splitOn ',' line of
        [name, building, startIntersectionName, dir1, dir2, floorStr, lengthStr] ->
            case (readMaybe floorStr, readMaybe lengthStr) of
                (Just floor, Just length) -> do
                    let (intersection, graph') = findOrCreateIntersection graph startIntersectionName floor
                        hallway = Hallway {
                            hName = name,
                            hBuilding = building,
                            hStartIntersection = Just intersection,
                            hDirection1 = dir1,
                            hDirection2 = dir2,
                            hFloor = floor,
                            hLength = length,
                            hNodes = [],
                            hIntersections = []
                        }
                        -- Update intersection with hallway
                        updatedIntersection = intersection {
                            iConnectedHallways = hallway : iConnectedHallways intersection
                        }
                    return $ graph'
                        & addHallway hallway
                        & addIntersection updatedIntersection
                _ -> parseError "Invalid floor/length" line graph
        _ -> parseError "Invalid hallway format" line graph

-- Intersection parser
parseIntersection :: Graph -> String -> IO Graph
parseIntersection graph line = 
    case splitOn ',' line of
        (name:floorStr:posStr:connections) ->
            case (readMaybe floorStr, readMaybe posStr) of
                (Just floor, Just pos) -> do
                    let intersection = findOrCreateIntersection' graph name floor pos
                        graph' = addIntersection graph intersection
                    foldM (parseIntersectionConnection intersection) graph' connections
                _ -> parseError "Invalid floor/position" line graph
        _ -> parseError "Invalid intersection format" line graph

parseIntersectionConnection :: Intersection -> Graph -> String -> IO Graph
parseIntersectionConnection intersection graph conn =
    case conn of
        'h':'a':'l':'l':'w':'a':'y':':':hallwayName -> 
            case findHallwayByName graph (trim hallwayName) of
                Just hallway -> do
                    let updatedHallway = hallway { hIntersections = intersection : hIntersections hallway }
                        updatedIntersection = intersection {
                            iConnectedHallways = hallway : iConnectedHallways intersection
                        }
                    return $ graph
                        & updateHallway updatedHallway
                        & updateIntersection updatedIntersection
                Nothing -> parseError ("Hallway not found: " ++ hallwayName) "" graph
        _ -> case getNode graph (trim conn) of
                Just node -> return $ graph
                    & updateIntersection (intersection {
                        iConnectedNodes = node : iConnectedNodes intersection
                    })
                    & addNode (case node of
                        StairsNode n f i h p cs cns -> StairsNode n f (Just intersection) h p cs cns
                        ElevatorNode n f i h p cs cns -> ElevatorNode n f (Just intersection) h p cs cns
                        _ -> node
                    )
                Nothing -> parseError ("Connection not found: " ++ conn) "" graph

-- Classroom parser
parseClassroom :: Graph -> String -> IO Graph
parseClassroom graph line =
    case splitOn ',' line of
        [name, building, hallwayName, posStr, dir, floorStr] ->
            case (readMaybe posStr, readMaybe floorStr) of
                (Just pos, Just floor) ->
                    case findHallwayByName graph hallwayName of
                        Just hallway -> do
                            let classroom = ClassroomNode {
                                cName = name,
                                cFloor = floor,
                                cIntersection = Nothing,
                                cHallway = Just hallway,
                                cPositionAlongHallway = pos,
                                cCompassDirection = dir
                            }
                                updatedHallway = hallway {
                                    hNodes = classroom : hNodes hallway
                                }
                            return $ graph
                                & addNode classroom
                                & updateHallway updatedHallway
                        Nothing -> parseError ("Hallway not found: " ++ hallwayName) line graph
                _ -> parseError "Invalid position/floor" line graph
        _ -> parseError "Invalid classroom format" line graph

-- Stairs parser
parseStairs :: Graph -> String -> IO Graph
parseStairs graph line =
    case splitOn ',' line of
        (name:building:intersectionName:posStr:dir:conns) ->
            case (readMaybe posStr, readMaybe (last conns)) of
                (Just pos, Just floor) -> do
                    let stairs = StairsNode {
                        sName = name,
                        sFloor = floor,
                        sIntersection = Nothing,
                        sHallway = Nothing,
                        sPositionAlongHallway = pos,
                        sConnectedNodes = [],
                        sConnectedNodeNames = intersectionName : init conns
                    }
                    return $ addNode graph stairs
                _ -> parseError "Invalid position/floor" line graph
        _ -> parseError "Invalid stairs format" line graph

-- Elevator parser (similar to stairs)
parseElevator :: Graph -> String -> IO Graph
parseElevator graph line =
    case splitOn ',' line of
        (name:building:intersectionName:posStr:dir:conns) ->
            case (readMaybe posStr, readMaybe (last conns)) of
                (Just pos, Just floor) -> do
                    let elevator = ElevatorNode {
                        eName = name,
                        eFloor = floor,
                        eIntersection = Nothing,
                        eHallway = Nothing,
                        ePositionAlongHallway = pos,
                        eConnectedNodes = [],
                        eConnectedNodeNames = intersectionName : init conns
                    }
                    return $ addNode graph elevator
                _ -> parseError "Invalid position/floor" line graph
        _ -> parseError "Invalid elevator format" line graph

-- Connection resolver
resolveAllConnections :: Graph -> Graph
resolveAllConnections graph = foldr resolveNode graph (Map.elems $ gNodes graph)
  where
    resolveNode node g = case node of
        StairsNode name floor _ hallway pos _ cnames ->
            let connected = Maybe.mapMaybe (flip getNode g) cnames
                updatedNode = node {
                    sConnectedNodes = connected,
                    sIntersection = findIntersectionForTransport g name
                }
            in foldr (addConnectionToNode updatedNode) (addNode g updatedNode) connected
        ElevatorNode name floor _ hallway pos _ cnames ->
            let connected = Maybe.mapMaybe (flip getNode g) cnames
                updatedNode = node {
                    eConnectedNodes = connected,
                    eIntersection = findIntersectionForTransport g name
                }
            in foldr (addConnectionToNode updatedNode) (addNode g updatedNode) connected
        _ -> g

addConnectionToNode :: Node -> Node -> Graph -> Graph
addConnectionToNode source target graph = case target of
    IntersectionNode i -> 
        let updated = i { iConnectedNodes = source : iConnectedNodes i }
        in addNode graph (IntersectionNode updated)
    StairsNode {} -> updateNode graph target $ \t -> t { sConnectedNodes = source : sConnectedNodes t }
    ElevatorNode {} -> updateNode graph target $ \t -> t { eConnectedNodes = source : eConnectedNodes t }
    _ -> graph

-- Helper functions
findOrCreateIntersection :: Graph -> String -> Int -> (Intersection, Graph)
findOrCreateIntersection graph name floor =
    case List.find (\i -> iName i == name && iFloor i == floor) (gIntersections graph) of
        Just i -> (i, graph)
        Nothing -> 
            let newIntersection = Intersection name floor 0 [] []
            in (newIntersection, addIntersection graph newIntersection)

findIntersectionForTransport :: Graph -> String -> Maybe Intersection
findIntersectionForTransport graph name =
    listToMaybe [i | IntersectionNode i <- Map.elems (gNodes graph), iName i == name]

findHallwayByName :: Graph -> String -> Maybe Hallway
findHallwayByName graph name =
    List.find (\h -> hName h == name) (gHallways graph)

updateNode :: Graph -> Node -> (Node -> Node) -> Graph
updateNode graph node fn = case Map.lookup (nodeName node) (gNodes graph) of
    Just existing -> addNode graph (fn existing)
    Nothing -> graph

updateHallway :: Hallway -> Graph -> Graph
updateHallway hallway graph = graph {
    gHallways = hallway : filter (\h -> hName h /= hName hallway) (gHallways graph)
}

updateIntersection :: Intersection -> Graph -> Graph
updateIntersection intersection graph = graph {
    gIntersections = intersection : filter (\i -> iName i /= iName intersection) (gIntersections graph)
}

parseError :: String -> String -> Graph -> IO Graph
parseError errDesc line graph = do
    hPutStrLn stderr $ "Parse error: " ++ errDesc ++ " in line: " ++ line
    return graph

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 1 &
































{-
module Parser where

import Data.List as List
import Data.Maybe as Maybe
import System.IO
import Control.Monad

import DataTypes
import HallwayOps
import IntersectionOps
import Graph

data Parser = Parser
    { pGraph = Graph
    }

createParser :: Parser
createParser = Parser emptyGraph

initializeBuildingMap :: Parser -> String -> IO Graph
initializeBuildingMap parser filename = do
    content <- readFile filename
    -- FIX

parseLine :: Parser -> String -> Parser
parseLine parser line | line `isPrefixOf` "hallway" -> parseHallway parser line
<<<<<<< HEAD
                      | line `isPrefixOf` "EB" -> parseClassrom line
=======
                      | line `isPrefixOf` "EB" -> parseClassroom line
>>>>>>> 7d2f487597de7e9a797835c0cdda7c41b9b4f2ee
                      | line `isPrefixOf` "stairs" -> parseStairs parser line
                      | line `isPrefixOf` "elevator" -> parseElevator parser line
                      | line `isPrefixOf` "bisset" -> parseIntersection parser line

parseHallway :: Parser -> String -> Parser

parseClassroom :: Parser -> String -> Parser

parseStairs :: Parser -> String -> Parser

parseElevator :: Parser -> String -> Parser

parseIntersection :: Parser -> String -> Parser

-- Might be useful?
-- Helper function that checks the type of the Parser
-- ie.) # Hallway Format:
-- ie.) # Classroom Format: etc..
getSectionType :: String -> Maybe String
getSectionType line
    | "# Intersection Format" `isPrefixOf` line = Just "Intersection"
    | "# Hallway Format" `isPrefixOf` line = Just "Hallway"
    | "# Classroom Format" `isPrefixOf` line = Just "Classroom"
    | "# Stairs Format" `isPrefixOf` line = Just "Stairs"
    | "# Elevator Format" `isPrefixOf` line = Just "Elevator"
    | otherwise = Nothing

----------------------------------------------------------------------------------------
module Parser where

import DataTypes
import Graph
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Read (readMaybe)

parseBuildingMap :: FilePath -> Graph -> IO Graph
parseBuildingMap filename initialGraph = do
    contents <- readFile filename
    let lines = filter (not . isCommentLine) (map trim (lines contents))
        sections = groupSections lines
    foldM parseSection initialGraph sections
  where
    isCommentLine line = "#" `List.isPrefixOf` line
    trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

    groupSections [] = []
    groupSections (l:ls)
        | "#" `List.isPrefixOf` l = let (sec, rest) = span (not . isCommentLine) ls
                                    in (l, sec) : groupSections rest
        | otherwise = groupSections ls

parseSection :: Graph -> (String, [String]) -> IO Graph
parseSection graph (header, lines) 
    | "Intersection" `List.isPrefixOf` header = foldM parseIntersection graph lines
    | "Hallway" `List.isPrefixOf` header = foldM parseHallway graph lines
    | "Classroom" `List.isPrefixOf` header = foldM parseClassroom graph lines
    | "Stairs" `List.isPrefixOf` header = foldM parseStairs graph lines
    | "Elevator" `List.isPrefixOf` header = foldM parseElevator graph lines
    | otherwise = return graph  -- Skip unknown sections

-- Example: Parse a hallway
parseHallway :: Graph -> String -> IO Graph
parseHallway graph line = 
    case splitOn ',' line of
        [name, building, startIntersectionName, dir1, dir2, floorStr, lengthStr] -> do
            let floor = read floorStr
                length = read lengthStr
                startIntersection = findOrCreateIntersection graph startIntersectionName floor
                hallway = Hallway name building (Just startIntersection) dir1 dir2 floor length [] []
            -- Update graph with hallway and intersection
            return $ graph 
                & addHallway hallway 
                & addIntersection startIntersection{ iConnectedHallways = hallway : iConnectedHallways startIntersection }
        _ -> error ("Invalid hallway line: " ++ line)

-- Helper to find/create intersections
findOrCreateIntersection :: Graph -> String -> Int -> Intersection
findOrCreateIntersection graph name floor = 
    case List.find (\i -> iName i == name && iFloor i == floor) (gIntersections graph) of
        Just i -> i
        Nothing -> Intersection name 0 floor [] []

-- Similar functions for parseClassroom, parseStairs, etc.

-}