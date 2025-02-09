sumSquares :: [Int] -> Int
sumSquares xs = (sum . (map (\x -> x * x))) xs

sumSquaresPF :: [Int] -> Int
sumSquaresPF = sum . map (\x -> x * x)
