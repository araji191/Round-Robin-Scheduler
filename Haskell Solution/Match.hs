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

import Time

data Match = Match
    { start :: Time
    , end :: Time
    , participant1 :: String
    , participant2 :: String
    , venue :: Int
    , day :: Int
    , scheduled :: Bool
    } deriving (Show)

newMatch :: Time -> Time -> String -> String -> Int -> Int -> Bool -> Match
newMatch st et p1 p2 v d sched = Match st et p1 p2 v d sched

getMatchStartTime :: Match -> Time
getMatchStartTime = start

getMatchEndTime :: Match -> Time
getMatchEndTime = end

getParticipant1 :: Match -> String
getParticipant1 = participant1

getParticipant2 :: Match -> String
getParticipant2 = participant2

getVenue :: Match -> Int
getVenue = venue

getDay :: Match -> Int
getDay = day

isScheduled :: Match -> Bool
isScheduled = scheduled

setScheduled :: Bool -> Match -> Match
setScheduled s m = m { scheduled = s }