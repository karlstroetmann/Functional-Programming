-- Provide by Grok 3

-- This needs to be compiled for tail recursion optimization to take effect.
-- Otherwise there will be a stack overflow.

-- Linear Congruential Generator returning a Double in [0,1] and next seed
randomLCG :: Integer -> (Double, Integer)
randomLCG seed = (fromIntegral next / fromIntegral modulus, next)
  where
    multiplier = 1103515245
    increment  = 12345
    modulus    = 2^63  -- 2147483648
    next       = (multiplier * seed + increment) `mod` modulus

-- Check if a point (x, y) is inside the unit quarter circle
inQuarterCircle :: Double -> Double -> Bool
inQuarterCircle x y = x * x + y * y <= 1.0

-- Monte Carlo simulation with strict accumulation
monteCarloPi :: Integer -> Int -> Double
monteCarloPi seed n = 4.0 * fromIntegral (go seed n 0) / fromIntegral n
  where
    go _ 0 acc = acc  -- Base case: no more points, return accumulator
    go s k acc = 
      let (x, s1) = randomLCG s      -- Generate x and next seed
          (y, s2) = randomLCG s1     -- Generate y and next seed
          newAcc = if inQuarterCircle x y then acc + 1 else acc
      in go s2 (k - 1) newAcc        -- Recurse with new seed and decremented count

-- Example usage
main :: IO ()
main = do
  let seed = 42
      n    = 10_000_000  
      piApprox = monteCarloPi seed n
  putStrLn $ "Approximation of π with " ++ show n ++ " points: " ++ show piApprox
