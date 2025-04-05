module ReadInput
(
    readInputFile,
    validateTournament,
    validateTournamentType,
    validateParticipants,
    validateNumDays,
    validateMatchLength,
    validateNumVenues,
    validateRestPeriod
) where

import Tournament
import Time
import Constants

import Data.Char (isSpace)
import System.IO
import Text.Read (readMaybe)

-- Helper functions for parsing
readInt :: String -> Int
readInt s = case readMaybe s of
    Just n -> n
    Nothing -> error ("Invalid number: " ++ s)

parseTime :: String -> Either String Time
parseTime s = case splitOn ':' s of
    [hour, minute] -> Right (newTime (readInt hour) (readInt minute))
    _ -> Left ("Invalid time format: " ++ s)

parseParticipants :: [String] -> Either String ([String], [String])
parseParticipants [] = Left "Unexpected end of input while reading participants"
parseParticipants ("END":rest) = Right ([], rest)
parseParticipants (participant:rest) = do
    let cleanP = dropWhile isSpace participant  -- Clean whitespace
    (participants, remaining) <- parseParticipants rest
    return (cleanP:participants, remaining)

parseTournamentParams :: [String] -> Either String (Int, Time, Time, Int, Int, Int)
parseTournamentParams (days:start:end:matchLen:venues:restPeriod:rest) = do
    day <- validateNumDays (readInt days)
    startTime <- parseTime start
    endTime <- parseTime end
    matchLength <- validateMatchLength (readInt matchLen)
    venue <- validateNumVenues (readInt venues)
    restPeriod <- validateRestPeriod (readInt restPeriod)
    return (day, startTime, endTime, matchLength, venue, restPeriod)
parseTournamentParams _ = Left "Insufficient tournament parameters"

-- | Read and parse tournament data from file
readInputFile :: FilePath -> IO (Either String Tournament)
readInputFile filePath = do
    content <- readFile filePath
    let fileLines = lines content
    case parseTournamentData fileLines of
        Left err -> return (Left err)
        Right t -> return (validateTournament t)

-- Add this to ReadInput.hs if not using Data.List.Split
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str = 
    let (before, remainder) = break (== delimiter) str
        after = dropWhile (== delimiter) remainder
    in before : if null after then [] else splitOn delimiter after

-- | Parse tournament data from lines of text
parseTournamentData :: [String] -> Either String Tournament
parseTournamentData [] = Left "Empty input file"
parseTournamentData (t:rest) = do
    tournamentType <- validateTournamentType (readInt t)
    (participants, remaining) <- parseParticipants rest
    (days, startTime, endTime, matchLen, venues, restPeriod) <- parseTournamentParams remaining
    return (newTournament tournamentType participants days startTime endTime matchLen venues restPeriod)

-- | Validate tournament type
validateTournamentType :: Int -> Either String Int
validateTournamentType tournamentType
    | tournamentType <= 0 || tournamentType > maxTournamentType = Left "Error: Invalid tournament type."
    | otherwise = Right tournamentType

-- | Validate participant list
validateParticipants :: [String] -> Either String [String]
validateParticipants participants
    | length participants < minParticipants = Left ("Error: At least " ++ show minParticipants ++ " participants required.")
    | length participants > maxParticipants = Left ("Error: Maximum " ++ show maxParticipants ++ " participants allowed.")
    | otherwise = Right participants

-- | Validate number of days
validateNumDays :: Int -> Either String Int
validateNumDays numDays
    | numDays <= 0 = Left "Error: Number of days must be positive."
    | numDays > maxDays = Left ("Error: Maximum " ++ show maxDays ++ " days allowed.")
    | otherwise = Right numDays

-- | Validate match length
validateMatchLength :: Int -> Either String Int
validateMatchLength matchLength
    | matchLength <= 0 = Left "Error: Match length must be positive."
    | matchLength > maxMatchLength = Left ("Error: Match length exceeds maximum (" ++ show maxMatchLength ++ " minutes).")
    | otherwise = Right matchLength

-- | Validate number of venues
validateNumVenues :: Int -> Either String Int
validateNumVenues numVenues
    | numVenues <= 0 = Left "Error: Number of venues must be positive."
    | numVenues > maxVenues = Left ("Error: Maximum " ++ show maxVenues ++ " venues allowed.")
    | otherwise = Right numVenues

-- | Validate rest period
validateRestPeriod :: Int -> Either String Int
validateRestPeriod restPeriod
    | restPeriod < 0 || restPeriod > maxRestPeriod = Left ("Error: Invalid rest period (must be between 0 and " ++ show maxRestPeriod ++ " minutes).")
    | otherwise = Right restPeriod

-- | Validate the complete tournament configuration
validateTournament :: Tournament -> Either String Tournament
validateTournament tournament
    | length (getParticipants tournament) < minParticipants = Left ("Error: At least " ++ show minParticipants ++ " participants required.")
    | getType tournament > maxTournamentType = Left ("Error: Invalid tournament type.")
    | getNumDays tournament <= 0 || getNumDays tournament > maxDays = Left ("Error: Invalid number of days.")
    | getMatchLength tournament <= 0 || getMatchLength tournament > maxMatchLength = Left ("Error: Invalid match length.")
    | getNumVenues tournament <= 0 || getNumVenues tournament > maxVenues = Left ("Error: Invalid number of venues.")
    | getRestPeriod tournament < 0 || getRestPeriod tournament > maxRestPeriod = Left ("Error: Invalid rest period.")
    | otherwise = Right tournament