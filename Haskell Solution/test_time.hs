import Time

-- Test function to check if the time interval is valid
testTime :: Int
testTime = do 
            getInterval (newTime 9 30) (newTime 10 45)
            getInterval (newTime 9 30) (newTime 10 45)