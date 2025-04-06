module Main where

import System.Environment ( getArgs ) 
import System.Exit (exitFailure, exitWith, ExitCode(..))
import Control.Exception (catch, IOException)
import Data.Char (toUpper, toLower) 
import Graph
import Parser 
import ClassroomFinder


main :: IO ()
main = do
    args <- getArgs
    if length args < 4
        then do
            putStrLn "Usage: ./Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>"
            putStrLn "Example: ./Main EB1113 EB1112 2 FALSE"
            exitFailure
        else do
            let startRoom = map toUpper (args !! 0)
                endRoom = map toUpper (args !! 1) 
            if startRoom == endRoom
                then do
                    putStrLn "Error: Start Classroom must be different than Destination Classroom"
                    exitWith (ExitFailure 1)
                else do 
                    timeConstraint <- case reads (args !! 2) of
                        [(tc, "")] ->
                            if tc > 0 && tc <= 6
                                then return tc
                                else do
                                    putStrLn "Error: Time Constraint must be between 1 and 6 minutes"
                                    exitWith (ExitFailure 1)
                        _ -> do 
                            putStrLn "Error: Time constraint must be a valid integer"
                            exitWith (ExitFailure 1)

                    mobilityConstraints <- case map toLower (args !! 3) of
                        "true" -> return True
                        "false" -> return False
                        _ -> do
                            putStrLn "Error: Mobility constraints must be 'true' or 'false'"
                            exitWith (ExitFailure 1) 

                    putStrLn $ "\nNavigating from Classroom " ++ startRoom ++ " to Classroom " ++ endRoom ++ " with a time constraint of " ++ show timeConstraint ++ " minutes and mobility constraint: " ++ show mobilityConstraints

                    catch(do
                         putStrLn "Route calculation would happen here"
                        --let graph = Graph.empty
                        --parser <- Parser.initializeBuildingMap graph "../data/bissetBuilding.txt"
                       -- Parser.resolveAllConnections parser
                        
                       -- let crf = CRF.new graph
                       -- route <- CRF.findRoute crf startRoom endRoom timeConstraint mobilityConstraints
                       -- CRF.displayRoute route
                        ) handleException

handleException :: IOException -> IO ()
handleException e = do
    putStrLn $ "An unexpected error occured: " ++ show e
    exitWith (ExitFailure 1)

