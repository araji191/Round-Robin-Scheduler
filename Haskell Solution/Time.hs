module Time
(
    Time,
    newTime,
    getHour,
    getMinute,
    getInterval
) where

-- | Represents a specific time of day with hours and minutes
data Time = Time { hour :: Int, minute :: Int } deriving (Show)

-- | Creates a new Time value
newTime :: Int -> Int -> Time
newTime h m = Time h m

-- | Gets the hour component of a Time
getHour :: Time -> Int
getHour = hour

-- | Gets the minute component of a Time
getMinute :: Time -> Int
getMinute = minute

-- | Calculates the interval in minutes between two Time values
getInterval :: Time -> Time -> Int
getInterval (Time h1 m1) (Time h2 m2) = (h2 - h1) * 60 + (m2 - m1)