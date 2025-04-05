module Main where

import Tournament
import Match
import Time
import Constants
import ReadInput
import PrintOutput
import Scheduler

import System.Environment (getArgs)
import System.IO

-- | Main function - entry point of the program
main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            putStrLn "Please provide an input file as follows:\n\n ./round_robin <input_file>\n"
            putStrLn "Note: The program will automatically look for the file in the 'testcases' directory."
        else do
            let inputFilePath = "../testcases/" ++ head args
            
            -- Try to read the tournament data
            result <- readInputFile inputFilePath
            case result of
                Left err -> putStrLn err
                Right tournament -> do
                    -- Count participants and generate matchups
                    let numParticipants = length (getParticipants tournament)
                        totalMatchups = calculateTotalMatchups numParticipants (getType tournament)
                    
                    if numParticipants == 0
                        then putStrLn "Error: No participants found."
                    else if totalMatchups > maxMatchups
                        then putStrLn "Error: Too many matchups for the current configuration."
                    else do
                        let matches = generateMatchups tournament                       
                        -- Schedule the matches
                        scheduleResult <- scheduleMatches matches tournament
                        case scheduleResult of
                            Nothing -> putStrLn "Failed to generate schedule"
                            Just scheduledMatches -> do
                                putStrLn "\nRound Robin Tournament Schedule"
                                putStrLn "==============================="
                                printSchedule scheduledMatches

-- | Calculate total number of matchups
calculateTotalMatchups :: Int -> Int -> Int
calculateTotalMatchups numParticipants tournamentType = 
    (numParticipants^2 - numParticipants) `div` 2 * tournamentType

-- | Generate all matchups based on tournament type (N rounds)
generateMatchups :: Tournament -> [Match]
generateMatchups tournament =
    let teams = getParticipants tournament
        numRounds = getType tournament
    in concat (replicate numRounds (generateSingleRound teams))
  where
    generateSingleRound :: [String] -> [Match]
    generateSingleRound teams =
        [ newMatch (newTime 0 0) (newTime 0 0) t1 t2 0 0 False
        | (i, t1) <- zip [0..] teams
        , (j, t2) <- zip [0..] teams 
        , i < j
        ]