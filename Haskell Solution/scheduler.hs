module Scheduler
(
    scheduleMatches,
    calculateMatchStartTime,
    calculateMatchEndTime,
    solve,
    hasVenueTimeConflict,
    hasParticipantConflict,
    isValid
) where

import Match
import Tournament
import Time
import Constants

import Control.Monad.State

-- Track backtrack count
type SchedulerState = Int
type Scheduler a = State SchedulerState a

-- | Schedule all matches for a tournament
scheduleMatches :: [Match] -> Tournament -> IO (Maybe [Match])
scheduleMatches matches tournament = do
    -- Validate time parameters are divisible by 30
    if not (validTimeParameters tournament)
        then do
            putStrLn "Error: Match length, rest period, and tournament time must be divisible by 30 minutes."
            return Nothing
        else do
            -- Initialize all matches as unscheduled
            let initializedMatches = map unschedule matches
            -- Start the backtracking solver with initial backtrack count
            let (result, backtracks) = runState (solve initializedMatches 0 tournament) 0
            case result of
                Just solution -> do
                    putStrLn ("Schedule created successfully!")
                    return (Just solution)
                Nothing -> do
                    putStrLn "A schedule was not able to be generated based on the input"
                    return Nothing
  where
    validTimeParameters t =
        getMatchLength t `mod` 30 == 0 &&
        getRestPeriod t `mod` 30 == 0 &&
        getInterval (getStartTime t) (getEndTime t) `mod` 30 == 0

    unschedule match = match { venue = 0, day = 0, start = newTime 0 0, end = newTime 0 0, scheduled = False }

-- | Backtracking solver for scheduling matches
solve :: [Match] -> Int -> Tournament -> Scheduler (Maybe [Match])
solve matches currentIndex tournament = do
    backtracks <- get
    if backtracks > maxBacktracks 
        then return Nothing
        else if currentIndex >= length matches 
            then return (Just matches)
            else tryOptions matches currentIndex tournament

tryOptions :: [Match] -> Int -> Tournament -> Scheduler (Maybe [Match])
tryOptions matches currentIndex tournament = do
    let dayMinutes = getInterval (getStartTime tournament) (getEndTime tournament)
        currentMatch = matches !! currentIndex
        days = [1 .. getNumDays tournament]
        venues = [1 .. getNumVenues tournament]
        timeSlots = [0, 30 .. dayMinutes - getMatchLength tournament]
    
    tryAllDays days venues timeSlots currentMatch matches currentIndex tournament

tryAllDays :: [Int] -> [Int] -> [Int] -> Match -> [Match] -> Int -> Tournament -> Scheduler (Maybe [Match])
tryAllDays [] _ _ _ _ _ _ = return Nothing
tryAllDays (day:days) venues timeSlots currentMatch matches currentIndex tournament = do
    result <- tryAllVenues venues day timeSlots currentMatch matches currentIndex tournament
    case result of
        Just solution -> return (Just solution)
        Nothing -> tryAllDays days venues timeSlots currentMatch matches currentIndex tournament

tryAllVenues :: [Int] -> Int -> [Int] -> Match -> [Match] -> Int -> Tournament -> Scheduler (Maybe [Match])
tryAllVenues [] _ _ _ _ _ _ = return Nothing
tryAllVenues (venue:venues) day timeSlots currentMatch matches currentIndex tournament = do
    result <- tryAllTimeSlots timeSlots day venue currentMatch matches currentIndex tournament
    case result of
        Just solution -> return (Just solution)
        Nothing -> tryAllVenues venues day timeSlots currentMatch matches currentIndex tournament

tryAllTimeSlots :: [Int] -> Int -> Int -> Match -> [Match] -> Int -> Tournament -> Scheduler (Maybe [Match])
tryAllTimeSlots [] _ _ _ _ _ _ = return Nothing
tryAllTimeSlots (startMinute:slots) day venue currentMatch matches currentIndex tournament = do
    let start = calculateMatchStartTime tournament startMinute
        end = calculateMatchEndTime start (getMatchLength tournament)
        updatedMatch = currentMatch {
            day = day,
            venue = venue,
            start = start,
            end = end,
            scheduled = True
        }
        updatedMatches = take currentIndex matches ++ [updatedMatch] ++ drop (currentIndex + 1) matches
    
    if isValid updatedMatches currentIndex tournament
        then do
            result <- solve updatedMatches (currentIndex + 1) tournament
            case result of
                Just solution -> return (Just solution)
                Nothing -> do
                    modify (+1) -- Increment backtrack count
                    tryAllTimeSlots slots day venue currentMatch matches currentIndex tournament
        else do
            modify (+1) -- Increment backtrack count
            tryAllTimeSlots slots day venue currentMatch matches currentIndex tournament

-- | Calculate the start time based on tournament start time and minutes offset
calculateMatchStartTime :: Tournament -> Int -> Time
calculateMatchStartTime tournament startMinute =
    let totalMinutes = getHour (getStartTime tournament) * 60 + getMinute (getStartTime tournament) + startMinute
    in newTime (totalMinutes `div` 60) (totalMinutes `mod` 60)

-- | Calculate the end time based on start time and match length
calculateMatchEndTime :: Time -> Int -> Time
calculateMatchEndTime start len =
    let totalMinutes = getHour start * 60 + getMinute start + len
    in newTime (totalMinutes `div` 60) (totalMinutes `mod` 60)

-- | Check if a venue has a time conflict between two matches
hasVenueTimeConflict :: Match -> Match -> Bool
hasVenueTimeConflict current other =
    let currentStart = getHour (start current) * 60 + getMinute (start current)
        currentEnd = getHour (end current) * 60 + getMinute (end current)
        otherStart = getHour (start other) * 60 + getMinute (start other)
        otherEnd = getHour (end other) * 60 + getMinute (end other)
    in (currentStart >= otherStart && currentStart < otherEnd) ||
       (otherStart >= currentStart && otherStart < currentEnd)

-- | Check if two matches share any participants
hasParticipantConflict :: Match -> Match -> Bool
hasParticipantConflict current other =
    participant1 current == participant1 other ||
    participant1 current == participant2 other ||
    participant2 current == participant1 other ||
    participant2 current == participant2 other

-- | Validate if the current match can be scheduled without conflicts
isValid :: [Match] -> Int -> Tournament -> Bool
isValid matches currentIndex tournament =
    let current = matches !! currentIndex
        previousMatches = filter scheduled (take currentIndex matches)
    in all (checkConflicts current tournament) previousMatches
  where
    checkConflicts current tournament other =
        let venueConflict = 
                -- Check venue and time conflict if same day and venue
                if day current == day other && venue current == venue other
                then not (hasVenueTimeConflict current other)
                else True
            participantConflict = 
                -- Check participant rest period if same day and participants conflict
                if day current == day other && hasParticipantConflict current other
                then checkRestPeriod current other tournament
                else True
        in venueConflict && participantConflict  -- Both must be true
    
    checkRestPeriod current other tournament =
        let currentStart = getHour (start current) * 60 + getMinute (start current)
            currentEnd = getHour (end current) * 60 + getMinute (end current)
            otherStart = getHour (start other) * 60 + getMinute (start other)
            otherEnd = getHour (end other) * 60 + getMinute (end other)
            rest = getRestPeriod tournament
        in (currentStart >= otherEnd + rest) || (otherStart >= currentEnd + rest)