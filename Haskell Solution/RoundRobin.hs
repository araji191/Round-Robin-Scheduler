-- File: RoundRobin.hs

{- |
Module      :  Main 
Description :  This module implements a round-robin tournament scheduler.
               It reads input from a file, generates matchups, and schedules matches.
    
    The program expects an input file with the following format:
    - The first line contains the number of participants.
    - The second line contains the type of tournament (1 for single round, 2 for double round).
    - The subsequent lines contain the names of the participants.
    - The last line contains the start time, end time, match length, rest period, and number of venues.
    - The program will output the schedule of matches, including the day, venue, start time, and end time for each match.
    
    The program uses a backtracking algorithm to find a valid schedule that meets the constraints of the tournament.
    The program also checks for valid time parameters and ensures that the match length, rest period, and tournament time 
    are divisible by 30 minutes.
    The program will print an error message if the input file is not formatted correctly or if a valid schedule cannot be generated.
    The program is designed to be run from the command line with the input file as an argument.


Authors :     Abiola Raji, Ochihai Omuha

-}

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