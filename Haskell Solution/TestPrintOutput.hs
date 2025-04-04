
import Time 
import Match 

import PrintOutput (compareMatches, printMatch, printSchedule)
import Data.List (sortBy, groupBy)

-- Test cases for the PrintOutput module
-- This module is for testing the PrintOutput module, which includes
-- functions to compare matches, print a single match, and print a schedule.
-- The test cases will check the functionality of these functions
-- against expected outputs.

-- The test cases will include:

-- 1. compareMatches: Check if the matches are sorted correctly.
-- 2. printMatch: Check if the match is printed in the correct format.
-- 3. printSchedule: Check if the schedule is printed correctly by day and venue.           




-- Example matches
match1 :: Match
match1 = Match.newMatch (Time.newTime 9 0) (Time.newTime 10 0) "Alice" "Bob" 1 1
match2 :: Match
match2 = Match.newMatch (Time.newTime 10 0) (Time.newTime 11 0) "Charlie" "Dana" 2 1
match3 :: Match
match3 = Match.newMatch (Time.newTime 8 0) (Time.newTime 9 0) "Eve" "Frank" 1 2



-- Main function to test
main :: IO ()
main = do
    
    putStrLn "Testing compareMatches:"
    let matches = [match2, match1, match3]
        sortedMatches = sortBy compareMatches matches
    print sortedMatches -- Verify the order is correct

    putStrLn "\nTesting printMatch:"
    printMatch match1 -- Should print: " 10:30-12:30: Team X vs Team Y"

    putStrLn "\nTesting printSchedule:"
    printSchedule [match1, match2, match3]
    -- Expected output:
    -- DAY 1:
    -- Venue Stadium A:
    --  10:30-12:30: Team X vs Team Y
    -- Venue Stadium B:
    --  14:00-16:00: Team A vs Team B
    --
    -- DAY 2:
    -- Venue Stadium A:
    --  09:00-11:00: Team C vs Team D