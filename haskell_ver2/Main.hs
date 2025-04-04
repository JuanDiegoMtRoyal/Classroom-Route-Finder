module Main where

import System.Environment
import System.IO
import Data.Maybe
import DataTypes
import Graph
import ClassroomFinder
import Parser

-- Main function to run the application
main :: IO ()
main = do
    args <- getArgs
    let buildingFile = if null args then "Building.txt" else head args
    
    putStrLn $ "Loading building map from: " ++ buildingFile
    
    -- Initialize the graph and parser
    let graph = emptyGraph
    let parser = createParser graph
    
    -- Parse the building file
    loadedGraph <- initializeBuildingMap parser buildingFile
    
    -- Create the CRF with the loaded graph
    let crf = createCRF loadedGraph
    
    -- Start the interactive console
    runConsole crf

-- Run an interactive console for route finding
runConsole :: CRF -> IO ()
runConsole crf = do
    putStrLn "\n==== Classroom Route Finder ===="
    putStrLn "Enter starting room:"
    startRoom <- getLine
    
    putStrLn "Enter destination room:"
    endRoom <- getLine
    
    putStrLn "Enter maximum time allowed (in minutes):"
    timeStr <- getLine
    let timeConstraint = read timeStr :: Int
    
    putStrLn "Do you require mobility assistance? (y/n):"
    mobilityStr <- getLine
    let mobilityConstraints = mobilityStr == "y" || mobilityStr == "Y"
    
    -- Find and display the route
    let routeResult = findRoute crf startRoom endRoom timeConstraint mobilityConstraints
    case routeResult of
        Just route -> displayRoute (Just route)
        Nothing -> putStrLn "No route could be found with the given constraints."
    
    -- Ask if the user wants to find another route
    putStrLn "\nWould you like to find another route? (y/n):"
    cont <- getLine
    if cont == "y" || cont == "Y"
        then runConsole crf
        else putStrLn "Thank you for using Classroom Route Finder. Goodbye!"