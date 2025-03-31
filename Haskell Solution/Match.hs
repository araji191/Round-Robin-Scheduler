module Match
(
    Match,
    newMatch,
    getStartTime,
    getEndTime,
    getParticipant1,
    getParticipant2,
    getVenue,
    getDay,
    isScheduled,
    setScheduled
) where

import Time

-- | Represents a tournament match with timing, participants, venue, and status
data Match = Match
    { start :: Time
    , end :: Time
    , participant1 :: String
    , participant2 :: String
    , venue :: Int
    , day :: Int
    , scheduled :: Bool
    } deriving (Show)

-- | Creates a new unscheduled Match
newMatch :: Time -> Time -> String -> String -> Int -> Int -> Match
newMatch st et p1 p2 v d = Match st et p1 p2 v d False

-- Accessor functions
getStartTime :: Match -> Time
getStartTime = start

getEndTime :: Match -> Time
getEndTime = end

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

-- | Updates the scheduled status of a match
setScheduled :: Bool -> Match -> Match
setScheduled s m = m { scheduled = s }