import Data.Time

main :: IO ()
main = do
    -- Record the start time
    startTime <- getCurrentTime
    
    -- Perform the computation
    let result = sum [1..9_123_456_789]  -- Replace this with your computation
    
    -- Record the end time
    endTime <- getCurrentTime
    
    -- Calculate and display the elapsed time
    let elapsedTime = diffUTCTime endTime startTime
    putStrLn $ "The result of the computation is: " ++ show result
    putStrLn $ "Time taken: " ++ show elapsedTime ++ " seconds"
    
