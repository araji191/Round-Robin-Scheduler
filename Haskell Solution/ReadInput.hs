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

import Data.List (intercalate, isPrefixOf)
import Data.Char (isSpace)
import Control.Monad (when)
import System.IO
import Text.Read (readMaybe)

-- Helper functions for parsing
readInt :: String -> Int
readInt s = case readMaybe s of
    Just n -> n
    Nothing -> error $ "Invalid number: " ++ s

parseTime :: String -> Either String Time
parseTime s = case splitOn ':' s of
    [h, m] -> Right $ newTime (readInt h) (readInt m)
    _ -> Left $ "Invalid time format: " ++ s

parseParticipants :: [String] -> Either String ([String], [String])
parseParticipants [] = Left "Unexpected end of input while reading participants"
parseParticipants ("END":rest) = Right ([], rest)
parseParticipants (p:rest) = do
    let cleanP = dropWhile isSpace p  -- Clean whitespace
    (ps, remaining) <- parseParticipants rest
    return (cleanP:ps, remaining)

parseTournamentParams :: [String] -> Either String (Int, Time, Time, Int, Int, Int)
parseTournamentParams (days:start:end:len:venues:restPeriod:rest) = do
    d <- validateNumDays (readInt days)
    st <- parseTime start
    et <- parseTime end
    ml <- validateMatchLength (readInt len)
    v <- validateNumVenues (readInt venues)
    rp <- validateRestPeriod (readInt restPeriod)
    return (d, st, et, ml, v, rp)
parseTournamentParams _ = Left "Insufficient tournament parameters"

-- | Read and parse tournament data from file
readInputFile :: FilePath -> IO (Either String Tournament)
readInputFile filePath = do
    content <- readFile filePath
    let fileLines = filter (not . isCommentLine) $ lines content
    case parseTournamentData fileLines of
        Left err -> return $ Left err
        Right t -> return $ validateTournament t
  where
    isCommentLine line = "--" `isPrefixOf` strip line
    strip = dropWhile isSpace

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
    return $ newTournament tournamentType participants days startTime endTime matchLen venues restPeriod

-- | Validate tournament type
validateTournamentType :: Int -> Either String Int
validateTournamentType t
    | t <= 0 || t > maxTournamentType = Left "Error: Invalid tournament type."
    | otherwise = Right t

-- | Validate participant list
validateParticipants :: [String] -> Either String [String]
validateParticipants ps
    | length ps < minParticipants = Left $ "Error: At least " ++ show minParticipants ++ " participants required."
    | length ps > maxParticipants = Left $ "Error: Maximum " ++ show maxParticipants ++ " participants allowed."
    | otherwise = Right ps

-- | Validate number of days
validateNumDays :: Int -> Either String Int
validateNumDays nd
    | nd <= 0 = Left "Error: Number of days must be positive."
    | nd > maxDays = Left $ "Error: Maximum " ++ show maxDays ++ " days allowed."
    | otherwise = Right nd

-- | Validate match length
validateMatchLength :: Int -> Either String Int
validateMatchLength ml
    | ml <= 0 = Left "Error: Match length must be positive."
    | ml > maxMatchLength = Left $ "Error: Match length exceeds maximum (" ++ show maxMatchLength ++ " minutes)."
    | otherwise = Right ml

-- | Validate number of venues
validateNumVenues :: Int -> Either String Int
validateNumVenues nv
    | nv <= 0 = Left "Error: Number of venues must be positive."
    | nv > maxVenues = Left $ "Error: Maximum " ++ show maxVenues ++ " venues allowed."
    | otherwise = Right nv

-- | Validate rest period
validateRestPeriod :: Int -> Either String Int
validateRestPeriod rp
    | rp < 0 || rp > maxRestPeriod = Left $ "Error: Invalid rest period (must be between 0 and " ++ show maxRestPeriod ++ " minutes)."
    | otherwise = Right rp

-- | Validate the complete tournament configuration
validateTournament :: Tournament -> Either String Tournament
validateTournament t
    | length (getParticipants t) < minParticipants = Left $ "Error: At least " ++ show minParticipants ++ " participants required."
    | getType t > maxTournamentType = Left "Error: Invalid tournament type."
    | getNumDays t <= 0 || getNumDays t > maxDays = Left $ "Error: Invalid number of days."
    | getMatchLength t <= 0 || getMatchLength t > maxMatchLength = Left $ "Error: Invalid match length."
    | getNumVenues t <= 0 || getNumVenues t > maxVenues = Left $ "Error: Invalid number of venues."
    | getRestPeriod t < 0 || getRestPeriod t > maxRestPeriod = Left $ "Error: Invalid rest period."
    | otherwise = Right t