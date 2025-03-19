-- Grok 3
-- Linear Congruential Generator returning a Double in [0,1]
randomLCG :: Integer -> (Double, Integer)
randomLCG seed = (fromIntegral next / fromIntegral modulus, next)
  where
    multiplier = 1103515245
    increment  = 12345
    modulus    = 2^63  
    next       = (multiplier * seed + increment) `mod` modulus

-- Generate an infinite list of random Doubles in [0,1]
randomList :: Integer -> [Double]
randomList seed = r : randomList nextSeed
  where
    (r, nextSeed) = randomLCG seed

-- Check if a point (x, y) is inside the unit quarter circle
inQuarterCircle :: Double -> Double -> Bool
inQuarterCircle x y = x * x + y * y <= 1.0

-- Generate random pairs and count those inside the quarter circle
monteCarloPi :: Integer -> Int -> Double
monteCarloPi seed n = 4.0 * fromIntegral inside / fromIntegral n
  where
    randoms = randomList seed  -- Infinite list of random numbers
    pairs   = zip (take n randoms) (take n (drop n randoms))  -- n pairs (x, y)
    inside  = length $ filter (uncurry inQuarterCircle) pairs

-- Example usage
main :: IO ()
main = do
  let seed = 42  -- Arbitrary starting seed
      n = 1_000_000  -- Number of points to simulate
      piApprox = monteCarloPi seed n
  putStrLn $ "Approximation of π with " ++ show n ++ " points: " ++ show piApprox
