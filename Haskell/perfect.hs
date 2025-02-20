-- A natural number is *perfect* if is is the sum of all its *proper* divisors.
-- The natural number `t' is a *proper* divisors of the natural number `n` iff
--     t < n  &&  mod n t == 0.
-- For example, 6 is perfect because the proper divisors of 6 are 1, 2, and 3
-- and we have:
--     6 = 1 + 2 + 3

-- check whether a number is perfect
isPerfect :: Integer -> Bool
isPerfect n = sum [t | t <- [1.. n-1], mod n t == 0] == n

-- the list of all perfect numbers
perfect :: [Integer]
perfect = [n | n <- [1..], isPerfect n]

main :: IO ()
main = print $ take 4 perfect
