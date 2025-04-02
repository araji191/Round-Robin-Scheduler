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
    { tournamentType :: Int
    , participants :: [String]
    , numDays :: Int
    , startTime :: Time
    , endTime :: Time
    , matchLength :: Int
    , numVenues :: Int
    , restPeriod :: Int
    } deriving (Show)

newTournament :: Int -> [String] -> Int -> Time -> Time -> Int -> Int -> Int -> Tournament
newTournament t ps nd st et ml nv rp = 
    Tournament t (take maxParticipants ps) nd st et ml nv rp

getType :: Tournament -> Int
getType = tournamentType

getParticipants :: Tournament -> [String]
getParticipants = participants

getNumDays :: Tournament -> Int
getNumDays = numDays

getStartTime :: Tournament -> Time
getStartTime = startTime

getEndTime :: Tournament -> Time
getEndTime = endTime

getMatchLength :: Tournament -> Int
getMatchLength = matchLength

getNumVenues :: Tournament -> Int
getNumVenues = numVenues

getRestPeriod :: Tournament -> Int
getRestPeriod = restPeriod

countParticipants :: Tournament -> Int
countParticipants = length . getParticipants