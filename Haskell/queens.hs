import Data.List (intercalate)

-- Check if it's safe to place a queen at column 'col' in row 'row' given previous placements
isSafe :: Int -> Int -> [Int] -> Bool
isSafe col row prev = all (\r -> col /= prev !! (r-1) && abs (col - prev !! (r-1)) /= row - r) [1..row-1]

-- Recursively place queens up to the given row
placeQueens :: Int -> Int -> [[Int]]
placeQueens n row | row == 0 = [ [] ]
placeQueens n row = do
    prev <- placeQueens n (row - 1)
    col <- [1..n]
    if isSafe col row prev then
        return (prev ++ [col])
    else
        []

-- Solve N-Queens for the full board
solveNQueens :: Int -> [[Int]]
solveNQueens n = placeQueens n n

-- Everything below is I/O

-- Display a single board for a given solution
showBoard :: [Int] -> Int -> String
showBoard solution n = unlines [ intercalate " " [ [if solution !! (r-1) == c then 'Q' else '.'] | c <- [1..n] ] | r <- [1..n] ]

-- Display all solutions with numbering
showSolutions :: [[Int]] -> Int -> String
showSolutions [] n = "No solutions for N=" ++ show n ++ "\n"
showSolutions solutions n = unlines $ zipWith (\i sol -> "Solution " ++ show i ++ ":\n" ++ showBoard sol n ++ "\n") [1..] solutions

-- Main function to run the program
main :: IO ()
main = do
    putStrLn "Enter N:"
    n <- readLn
    let solutions = solveNQueens n
    putStr $ showSolutions solutions n
