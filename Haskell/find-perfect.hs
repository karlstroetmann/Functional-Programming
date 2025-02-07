-- Function to check if a number is perfect
perfect :: Int -> Bool
perfect n = sum [x | x <- [1 .. n - 1], n `mod` x == 0] == n

-- Function to find all perfect numbers up to 10,000
findPerfect :: [Int]
findPerfect = [n | n <- [1 .. 10000], perfect n]

main :: IO ()
main = print findPerfect
