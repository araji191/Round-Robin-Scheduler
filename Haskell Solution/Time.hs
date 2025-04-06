-- File: Time.hs

{- |
Module      : Time  
Description : This module defines a Time data type and provides functions to create and manipulate time objects.
              The Time data type represents a specific time of day, with hour and minute components.
    
    Functions:
    - newTime: Creates a new Time object given hour and minute.
    - getHour: Retrieves the hour from a Time object.
    - getMinute: Retrieves the minute from a Time object.
    - getInterval: Calculates the interval in minutes between two Time objects. 

Authors :     Abiola Raji, Ochihai Omuha

-}

module Time
(
    Time,
    newTime,
    getHour,
    getMinute,
    getInterval
) where

-- The Time data type represents a specific time of day.
-- It contains two fields: hour and minute, both of type Int.
-- The hour field represents the hour of the day (0-23).
-- The minute field represents the minute of the hour (0-59).

data Time = Time {
    hour :: Int,  
    minute :: Int
    } deriving (Show)

-- The function takes two parameters: h (hour) and m (minute).
newTime :: Int -> Int -> Time
newTime h m = Time h m

-- The function takes a Time object and returns the hour component of the time.
getHour :: Time -> Int
getHour = hour

-- The function takes a Time object and returns the minute component of the time.
getMinute :: Time -> Int
getMinute = minute

-- The function takes two Time objects and calculates the interval between them in minutes.   
getInterval :: Time -> Time -> Int
getInterval (Time h1 m1) (Time h2 m2) = (h2 - h1) * 60 + (m2 - m1)