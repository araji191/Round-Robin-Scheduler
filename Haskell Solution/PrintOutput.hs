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
    printMatch, 
    printSchedule
) where

import Time
import Match
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Text.Printf (printf)


-- Comparison function using the provided accessor functions.

compareMatches :: Match.Match -> Match.Match -> Ordering
compareMatches a b =
    compare (Match.getDay a) (Match.getDay b) <>
    compare (Match.getVenue a) (Match.getVenue b) <>
    compare (Time.getHour (Match.getStartTime a)) (Time.getHour (Match.getStartTime b)) <>
    compare (Time.getMinute (Match.getStartTime a)) (Time.getMinute (Match.getStartTime b))

-- Print a single match with formatted times.

printMatch :: Match.Match -> IO ()
printMatch m =
    printf " %02d:%02d-%02d:%02d: %s vs %s\n"
        (Time.getHour (Match.getStartTime m))
        (Time.getMinute (Match.getStartTime m))
        (Time.getHour (Match.getEndTime m))
        (Time.getMinute (Match.getEndTime m))
        (Match.getParticipant1 m)
        (Match.getParticipant2 m)

-- Print the schedule by grouping matches by day and then by venue.

printSchedule :: [Match.Match] -> IO ()
printSchedule matches = do
    let sortedMatches = sortBy compareMatches matches
        days = groupBy ((==) `on` Match.getDay) sortedMatches
    mapM_ printDayGroup days
  where
    printDayGroup :: [Match.Match] -> IO ()
    printDayGroup ms@(m:_) = do
        putStrLn $ "\nDAY " ++ show (Match.getDay m) ++ ":"
        let venues = groupBy ((==) `on` Match.getVenue) ms
        mapM_ printVenueGroup venues
    printDayGroup [] = return ()

 -- Print matches grouped by venue

printVenueGroup :: [Match.Match] -> IO ()
printVenueGroup ms@(m:_) = do
    putStrLn $ "Venue " ++ show (Match.getVenue m) ++ ":"
    mapM_ printMatch ms
printVenueGroup [] = return ()



