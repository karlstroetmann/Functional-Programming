import Data.List (find)  -- For finding the first empty cell

-- A Sudoku grid is represented 0 represents an empty cell
type Grid = [[Int]]

-- Main solver function that returns a Maybe Grid
solve :: Grid -> Maybe Grid
solve g = case findEmptyCell g of
            Nothing     -> Just g   -- If no empty cells, puzzle is solved
            Just (r, c) -> tryPossibilities (possibleNumbers g r c) g r c  

-- Helper function to try all possibilities for a cell
tryPossibilities :: [Int] -> Grid -> Int -> Int -> Maybe Grid
tryPossibilities []     _ _ _ = Nothing  -- No possibilities left = dead end
tryPossibilities (x:xs) g r c =
  case solve (placeNumber g r c x) of 
    Just solved -> Just solved 
    Nothing     -> tryPossibilities xs g r c  

-- Finds the first empty cell (0) in row-major order (top-left to bottom-right)
findEmptyCell :: Grid -> Maybe (Int, Int)
findEmptyCell grid = find (\(r, c) -> (grid !! r) !! c == 0) cells
  -- Generate all (row, column) indices from (0,0) to (8,8)
  where cells = [(r, c) | r <- [0..8], c <- [0..8]]
        
-- Calculates possible valid numbers for a given cell position
possibleNumbers :: Grid -> Int -> Int -> [Int]
possibleNumbers g r c =
  let used = rowNums ++ colNums ++ boxNums  -- All prohibited numbers
  in filter (`notElem` used) [1..9]         -- Filter valid numbers from 1-9
  where rowNums = g !! r                    -- All numbers in current row
        colNums = map (!! c) g              -- All numbers in current column
        boxNums = getBox g r c              -- All numbers in current 3x3 subgrid

-- Extracts numbers from the 3x3 subgrid containing the given cell
getBox :: Grid -> Int -> Int -> [Int]
getBox g r c = [ g !! y !! x | y <- [rs..rs+2], x <- [cs..cs+2] ]
               where rs = 3 * (r `div` 3)   -- Starting row of 3x3 subgrid
                     cs = 3 * (c `div` 3)   -- Starting column of 3x3 subgrid

-- ite c x y == if c then x else y
ite :: Bool -> a -> a -> a
ite True  x _ = x
ite False _ y = y

-- Places a number in the grid and returns a new grid 
placeNumber :: Grid -> Int -> Int -> Int -> Grid
placeNumber g r c n =
  [ [ ite ((x,y) == (c,r)) n ((g !! y) !! x) | x <- [0..8] ] | y <- [0..8] ]

-- A difficult Sudoku
diabolical :: Grid
diabolical = [ [0,9,0, 7,0,0, 8,6,0],
               [0,3,1, 0,0,5, 0,2,0],
               [8,0,6, 0,0,0, 0,0,0],
               [0,0,7, 0,5,0, 0,0,6],
               [0,0,0, 3,0,7, 0,0,0],
               [5,0,0, 0,1,0, 7,0,0],
               [0,0,0, 0,0,0, 1,0,9],
               [0,2,0, 6,0,0, 3,5,0],
               [0,5,4, 0,0,8, 0,7,0]
             ]

-- Helper function to print the board in a readable format
printBoard :: Grid -> IO ()
printBoard = mapM_ print

main :: IO ()
main = do
  putStrLn "Original board:"
  printBoard diabolical
  case solve diabolical of
    Nothing -> putStrLn "No solution found."
    Just solution -> do
      putStrLn "\nSolution:"
      printBoard solution
