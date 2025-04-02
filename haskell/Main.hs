module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (toUpper, toLower)
import BuildingParser (parseBuildingMap, resolveAllConnections, BuildingGraph)
import CRF (findRoute, displayRoute)  

main :: IO ()
main = do
    args <- getArgs
    when (length args < 4) $ do 
        putStrLn "Usage: ./Main <startRoom> <endRoom> <timeConstraint> <mobilityConstraints>"
        putStrLn "Example: ./Main EB1113 EB1112 2 false"
        die "Insufficient arguments"

    let startRoom = map toUpper (args !! 0) 
        endRoom = map toUpper (args !! 1)
        timeConstraintStr = args !! 2
        mobilityConstraintStr = args !! 3

    timeConstraint <- case reads timeConstraintStr of
        [(tc, "")] -> if tc >= 1 && tc <= 6
            then return tc
            else die "Time constraint must be between 1 and 6 minutes"
        _ -> die "Time constraint must be a valid integer"

        
    mobilityConstraint <- case map toLower mobilityConstraintStr of
        "true"  -> return True
        "false" -> return False
        _       -> die "Mobility constraints must be 'true' or 'false'"

    putStrLn $ "\nNavigating from Classroom " ++ startRoom ++ " to Classroom " ++ endRoom
        ++ " with a time constraint of " ++ show timeConstraint
        ++ " minutes and mobility constraint: " ++ show mobilityConstraint

    graph <- parseBuildingMap "../data/bissetBuilding.txt"
    resolvedGraph <- resolveAllConnections graph

    case findRoute resolvedGraph startRoom endRoom timeConstraint mobilityConstraint of
        Just route -> displayRoute route
        Nothing    -> putStrLn "No valid route found"
