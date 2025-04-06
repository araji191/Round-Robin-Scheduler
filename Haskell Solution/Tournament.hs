-- File: Tournament.hs

{- |
Module      :  Tournament
Description :  This module defines a data structure for representing a tournament.
                It includes fields for the tournament type, participants, number of days,
                start and end times, match length, number of venues, and rest period.
                The module provides functions to create a new tournament, retrieve its properties,
                and count the number of participants.
Description :  

Authors :     Abiola Raji, Ochihai Omuha

-}

module Tournament
(
    Tournament,
    newTournament, 
    getType,
    getParticipants,
    countParticipants,   
    getNumDays,
    getStartTime,
    getEndTime,
    getMatchLength,
    getNumVenues,
    getRestPeriod
) where

import Time
import Constants

data Tournament = Tournament
    { tournamentType :: Int -- Type of tournament (e.g., round robin, knockout)
    , participants :: [String] -- List of participants in the tournament
    , numDays :: Int -- Number of days the tournament will be held
    , startTime :: Time -- Start time of the tournament
    , endTime :: Time -- End time of the tournament
    , matchLength :: Int -- Length of each match in minutes
    , numVenues :: Int  -- Number of venues available for the tournament
    , restPeriod :: Int -- Rest period between matches in minutes
    } deriving (Show)  
 
-- The function takes the following parameters:
-- - t: The type of tournament (of type Int).
-- - ps: A list of participants (of type [String]).
-- - nd: The number of days the tournament will be held (of type Int).
-- - st: The start time of the tournament (of type Time).
-- - et: The end time of the tournament (of type Time).
-- - ml: The length of each match in minutes (of type Int).
-- - nv: The number of venues available for the tournament (of type Int).
-- - rp: The rest period between matches in minutes (of type Int).

newTournament :: Int -> [String] -> Int -> Time -> Time -> Int -> Int -> Int -> Tournament
newTournament t ps nd st et ml nv rp = 
    Tournament t (take maxParticipants ps) nd st et ml nv rp

-- The function takes a Tournament object and returns the type of tournament.
getType :: Tournament -> Int
getType = tournamentType

-- The function takes a Tournament object and returns the list of participants.
getParticipants :: Tournament -> [String]
getParticipants = participants

-- The function takes a Tournament object and returns the number of days the tournament will be held.
getNumDays :: Tournament -> Int
getNumDays = numDays

-- The function takes a Tournament object and returns the start time of the tournament.
getStartTime :: Tournament -> Time
getStartTime = startTime

-- The function takes a Tournament object and returns the end time of the tournament.
getEndTime :: Tournament -> Time
getEndTime = endTime

-- The function takes a Tournament object and returns the length of each match in minutes.
getMatchLength :: Tournament -> Int
getMatchLength = matchLength

-- The function takes a Tournament object and returns the number of venues available for the tournament.
getNumVenues :: Tournament -> Int
getNumVenues = numVenues

-- The function takes a Tournament object and returns the rest period between matches in minutes.
getRestPeriod :: Tournament -> Int
getRestPeriod = restPeriod

-- The function takes a Tournament object and returns the number of participants in the tournament.
countParticipants :: Tournament -> Int
countParticipants = length . getParticipants