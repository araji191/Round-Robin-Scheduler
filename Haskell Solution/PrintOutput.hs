-- File: PrintOutput.hs

{- |
Module      : PrintOutput
Description : Module for printing match schedules. 
              
              It provides functions to compare matches, print individual matches,
              and print the entire schedule grouped by day and venue.
              The comparison is based on the day, venue, and start time of the matches.
              The schedule is printed in a formatted manner for better readability.
              The module uses the Match and Time modules for match representation and time handling.    

Authors :     Abiola Raji, Ochihai Omuha

-}

module PrintOutput
(
    compareMatches,
    printSchedule
) where

import Match
import Time

import Data.List (sortBy, groupBy)
import Data.Function (on)

-- | Compare two matches for sorting purposes
compareMatches :: Match -> Match -> Ordering
compareMatches a b
    | day a /= day b       = compare (day a) (day b)
    | venue a /= venue b   = compare (venue a) (venue b)
    | getHour (start a) /= getHour (start b) = compare (getHour (start a)) (getHour (start b))
    | otherwise            = compare (getMinute (start a)) (getMinute (start b))

-- | Format a time with leading zero if needed
formatTime :: Int -> String
formatTime t = if t < 10 then "0" ++ show t else show t

-- | Print the schedule in a formatted way
printSchedule :: [Match] -> IO ()
printSchedule matches = do
    let sortedMatches = sortBy compareMatches matches
        groupedByDay = groupBy ((==) `on` day) sortedMatches
    mapM_ printDay groupedByDay
  where
    -- Print all matches for a day
    printDay :: [Match] -> IO ()
    printDay dayMatches = do
        putStrLn ("\nDAY " ++ show (day (head dayMatches)) ++ ":")
        let groupedByVenue = groupBy ((==) `on` venue) dayMatches
        mapM_ printVenue groupedByVenue
    
    -- Print all matches for a venue
    printVenue :: [Match] -> IO ()
    printVenue venueMatches = do
        putStrLn ("Venue " ++ show (venue (head venueMatches)) ++ ":")
        mapM_ printMatch venueMatches
    
    printMatch :: Match -> IO ()
    printMatch match = do
        let startH = getHour (start match)
            startM = getMinute (start match)
            endH = getHour (end match)
            endM = getMinute (end match)
        putStrLn (" " ++ formatTime startH ++ ":" ++ formatTime startM ++ "-" ++
                formatTime endH ++ ":" ++ formatTime endM ++ ": " ++
                participant1 match ++ " vs " ++ participant2 match)
