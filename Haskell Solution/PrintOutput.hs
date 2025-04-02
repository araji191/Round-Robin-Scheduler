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
        putStrLn $ "\nDAY " ++ show (day (head dayMatches)) ++ ":"
        let groupedByVenue = groupBy ((==) `on` venue) dayMatches
        mapM_ printVenue groupedByVenue
    
    -- Print all matches for a venue
    printVenue :: [Match] -> IO ()
    printVenue venueMatches = do
        putStrLn $ "Venue " ++ show (venue (head venueMatches)) ++ ":"
        mapM_ printMatch venueMatches
    
    printMatch :: Match -> IO ()
    printMatch m = do
        let startH = getHour (start m)
            startM = getMinute (start m)
            endH = getHour (end m)
            endM = getMinute (end m)
        putStrLn $ " " ++ formatTime startH ++ ":" ++ formatTime startM ++ "-" ++
                formatTime endH ++ ":" ++ formatTime endM ++ ": " ++
                participant1 m ++ " vs " ++ participant2 m