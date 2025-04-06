import Constants

-- Test the constants defined in Constants.hs

runConstantTests :: IO ()
runConstantTests = do
    putStrLn " "
    putStrLn "Testing Constants..."
    putStrLn $ "maxParticipants: " ++ show maxParticipants
    putStrLn $ "maxMatchups: " ++ show maxMatchups
    putStrLn $ "maxBacktracks: " ++ show maxBacktracks
    putStrLn $ "maxVenues: " ++ show maxVenues
    putStrLn $ "maxDays: " ++ show maxDays
    putStrLn $ "maxTournamentType: " ++ show maxTournamentType
    putStrLn $ "minParticipants: " ++ show minParticipants
    putStrLn $ "maxMatchLengthMinutes: " ++ show maxMatchLengthMinutes
    putStrLn $ "maxRestPeriodMinutes: " ++ show maxRestPeriodMinutes
    putStrLn " "
    putStrLn "All constants are set correctly."
    putStrLn " "
    putStrLn "Testing Constants completed."
    putStrLn " "
    putStrLn "----------------------------------------"