module Time
(
    Time,
    newTime,
    getHour,
    getMinute,
    getInterval
) where

data Time = Time {
    hour :: Int,
    minute :: Int
    } deriving (Show)

newTime :: Int -> Int -> Time
newTime h m = Time h m

getHour :: Time -> Int
getHour = hour

getMinute :: Time -> Int
getMinute = minute

getInterval :: Time -> Time -> Int
getInterval (Time h1 m1) (Time h2 m2) = (h2 - h1) * 60 + (m2 - m1)