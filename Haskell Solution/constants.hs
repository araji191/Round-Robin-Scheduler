module Constants
(
    maxParticipants,
    maxMatchups,
    maxBacktracks,
    maxVenues,
    maxDays,
    maxTournamentType,
    minParticipants,
    maxMatchLengthMinutes,
    maxRestPeriodMinutes
) where

-- | Maximum number of participants in a tournament
maxParticipants :: Int
maxParticipants = 100

-- | Maximum number of matchups to consider
maxMatchups :: Int
maxMatchups = 100

-- | Maximum number of backtracking attempts
maxBacktracks :: Int
maxBacktracks = 10000

-- | Maximum number of venues
maxVenues :: Int
maxVenues = 10

-- | Maximum number of days for a tournament
maxDays :: Int
maxDays = 30

-- | Maximum tournament type identifier
maxTournamentType :: Int
maxTournamentType = 5

-- | Minimum number of participants required
minParticipants :: Int
minParticipants = 2

-- | Maximum length of a match in minutes
maxMatchLengthMinutes :: Int
maxMatchLengthMinutes = 300

-- | Maximum rest period between matches in minutes
maxRestPeriodMinutes :: Int
maxRestPeriodMinutes = 480