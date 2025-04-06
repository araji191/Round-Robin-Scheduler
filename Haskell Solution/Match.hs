-- File: Match.hs

{- |
Module      :  Match
Description :  This module defines a data structure for representing a match in a tournament.
               It includes fields for the start and end times of the match, the participants,
               the venue, the day of the match, and a scheduled status.
               The module provides functions to create a new match, retrieve its properties,
               and set its scheduled status.

Authors :     Abiola Raji, Ochihai Omuha

-}

module Match
(
    Match(..),
    newMatch,
    getMatchStartTime,
    getMatchEndTime,
    getParticipant1,
    getParticipant2,
    getVenue,
    getDay,
    isScheduled,
    setScheduled
) where

import Time ( Time )

-- The Match data type represents a match in a tournament.
data Match = Match
    { start :: Time
    , end :: Time
    , participant1 :: String
    , participant2 :: String
    , venue :: Int
    , day :: Int
    , scheduled :: Bool
    } deriving (Show)


-- - st: The start time of the match (of type Time).
-- - et: The end time of the match (of type Time).
-- - p1: The first participant of the match (of type String).
-- - p2: The second participant of the match (of type String).
-- - v: The venue of the match (of type Int).
-- - d: The day of the match (of type Int).
-- - sched: The scheduled status of the match (of type Bool).



-- The function takes these parameters and creates a new Match object with the provided values.
newMatch :: Time -> Time -> String -> String -> Int -> Int -> Bool -> Match
newMatch st et p1 p2 v d sched = Match st et p1 p2 v d sched

-- The function takes a Match object and returns the start time of the match.
getMatchStartTime :: Match -> Time
getMatchStartTime = start

-- The function takes a Match object and returns the end time of the match.
getMatchEndTime :: Match -> Time
getMatchEndTime = end

-- The function takes a Match object and returns the first participant of the match.
getParticipant1 :: Match -> String
getParticipant1 = participant1

-- The function takes a Match object and returns the second participant of the match.
getParticipant2 :: Match -> String
getParticipant2 = participant2

-- The function takes a Match object and returns the venue of the match.
getVenue :: Match -> Int
getVenue = venue

-- The function takes a Match object and returns the day of the match.
getDay :: Match -> Int
getDay = day

-- The function takes a Match object and returns the scheduled status of the match.
isScheduled :: Match -> Bool
isScheduled = scheduled

-- The function takes a Match object and a Boolean value, and sets the scheduled status of the match.
setScheduled :: Bool -> Match -> Match
setScheduled s m = m { scheduled = s }