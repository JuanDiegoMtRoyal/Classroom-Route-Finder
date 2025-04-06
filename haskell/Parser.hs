module Parser where

import Data.List as List
import Data.Maybe as Maybe
import System.IO

import DataTypes
import HallwayOps
import IntersectionOps
import Graph

data Parser = Parser
    { pGraph :: Graph
    } deriving (Show)

createParser :: Parser
createParser = Parser emptyGraph

initializeBuildingMap :: Parser -> String -> IO Graph
initializeBuildingMap parser filename = do
    content <- readFile filename
    -- FIX

parseLine :: Parser -> String -> Parser
parseLine parser line | line `isPrefixOf` "hallway" = parseHallway parser line
                      | line `isPrefixOf` "EB" = parseClassroom line
                      | line `isPrefixOf` "stairs" = parseStairs parser line
                      | line `isPrefixOf` "elevator" = parseElevator parser line
                      | line `isPrefixOf` "bisset" = parseIntersection parser line
                      | otherwise = parser

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
