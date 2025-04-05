module Main where

import System.Environment
import System.Exit
import System.IO
import Data.Char
import Control.Exception
import Data.Maybe
import Parser
import Graph
import DataTypes
import ClassroomFinder

main :: IO ()
main = do
    args <- getArgs
    
    if length args < 4
        then do
            putStrLn "Usage: ./main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>"
            putStrLn "Example: ./main EB1113 EB1112 2 false"
            exitFailure
        else do
            let startRoom = map toUpper (args !! 0)  -- Starting classroom
            let endRoom = map toUpper (args !! 1)    -- Destination classroom
            
            -- Parse time constraint
            timeConstraint <- case reads (args !! 2) of
                [(time, "")] -> if time > 0 && time <= 6
                                then return time
                                else error "Time constraint must be between 1 and 6 minutes"
                _ -> error "Time constraint must be a valid integer"
            
            -- Parse mobility constraints
            mobilityConstraints <- case map toLower (args !! 3) of
                "true" -> return True
                "false" -> return False
                _ -> error "Mobility constraints must be 'true' or 'false'"
            
            putStrLn ("\nNavigating from Classroom " ++ startRoom ++ " to Classroom " ++ endRoom
                      ++ " with a time constraint of " ++ show timeConstraint
                      ++ " minutes and mobility constraint: " ++ show mobilityConstraints)
            
            -- Create graph and parser
            let graph = emptyGraph
            let parser = createParser graph
            
            -- Initialize building map
            initializedGraph <- initializeBuildingMap parser "../data/bissetBuilding.txt"
            
            -- Create CRF
            let crf = createCRF initializedGraph
            
            -- Find route
            maybeRoute <- findRoute crf startRoom endRoom timeConstraint mobilityConstraints
            
            -- Display route
            case maybeRoute of
                Just route -> displayRoute route
                Nothing -> putStrLn "Could not find a route."