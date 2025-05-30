--File: Constants.hs

{- |
Module      :  Constants
Description :  Constants for the tournament scheduling system

Authors :     Abiola Raji, Ochihai Omuha

-}


module Constants where 

-- Constants for the tournament scheduling system

maxParticipants :: Int
maxParticipants = 100

minParticipants :: Int
minParticipants = 2

maxMatchups :: Int
maxMatchups = 100

maxVenues :: Int
maxVenues = 10

maxDays :: Int
maxDays = 30

maxTournamentType :: Int
maxTournamentType = 5

maxMatchLength :: Int
maxMatchLength = 300

maxRestPeriod :: Int
maxRestPeriod = 480

maxBacktracks :: Int
maxBacktracks = 100000
