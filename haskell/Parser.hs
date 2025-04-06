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
    let lines = filter
    -- FIX

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

