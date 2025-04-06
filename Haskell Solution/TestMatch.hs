import Time 
import Match

-- Test driver for the Match module
runMatchTests :: IO ()
runMatchTests = do
    let startTime = Time.newTime 9 0
        endTime = Time.newTime 10 30
        match = newMatch startTime endTime "Alice" "Bob" 2 1

    putStrLn " "
    putStrLn "Testing Match module:"
    putStrLn $ "Start Time: " ++ show (getStartTime match)
    putStrLn $ "End Time: " ++ show (getEndTime match)
    putStrLn $ "Participant 1: " ++ getParticipant1 match
    putStrLn $ "Participant 2: " ++ getParticipant2 match
    putStrLn $ "Venue: " ++ show (getVenue match)
    putStrLn $ "Day: " ++ show (getDay match)
    putStrLn $ "Scheduled (initial): " ++ show (isScheduled match)
    
    let scheduledMatch = setScheduled True match
    putStrLn $ "Scheduled (after setScheduled True): " ++ show (isScheduled scheduledMatch)
    putStrLn " "