-- The algorithm employs a backtracking approach enhanced by the Most
-- Constrained Value (MCV) heuristic. It first identifies the empty
-- cell with the fewest possible valid numbers based on Sudoku rules.
-- For that cell, it generates a list of valid candidates by ensuring
-- no conflicts in the corresponding row, column, or 3x3 subgrid. The
-- algorithm then recursively attempts to fill the grid by placing
-- each candidate and exploring further solutions. If a configuration
-- leads to a fully filled, valid grid, it is recorded as a solution;
-- otherwise, the algorithm backtracks to try different numbers. This
-- process ultimately collects all possible solutions, returning an
-- empty list if none exist.

import Data.List (minimumBy, (\\), intercalate)
import Data.Ord (comparing)

type Grid = [[Int]]

-- The new solve function returns all solutions as a list.
solve :: Grid -> [Grid]
solve grid =
  case findBestCell grid of
    []           -> [grid]  -- Puzzle solved; return the completed grid as the only solution.
    [(row, col)] -> concatMap (\x -> solve (placeNumber grid row col x)) (possibleNumbers grid row col)

-- Finds the most constrained empty cell using the Minimum Remaining Value (MRV) heuristic.
findBestCell :: Grid -> [(Int, Int)]
findBestCell grid =
  case emptyCellsWithCounts of
    []    -> []
    cells -> [ fst $ minimumBy (comparing snd) cells ]
  where
    emptyCells = [(r, c) | r <- [0..8], c <- [0..8], (grid !! r) !! c == 0]
    emptyCellsWithCounts = map (\cell@(r,c) -> (cell, length (possibleNumbers grid r c))) emptyCells

-- Calculates valid numbers for a given cell based on Sudoku rules.
possibleNumbers :: Grid -> Int -> Int -> [Int]
possibleNumbers grid row col = [1..9] \\ (rowNums ++ colNums ++ subgridNums)
  where 
    rowNums    = grid !! row
    colNums    = map (!! col) grid
    subgridNums = getSubgrid grid row col

-- Retrieves the numbers from the 3x3 subgrid that contains the specified cell.
getSubgrid :: Grid -> Int -> Int -> [Int]
getSubgrid grid row col = [ grid !! r !! c | r <- [rowStart..rowStart+2],
                                             c <- [colStart..colStart+2] ]
  where 
    rowStart = 3 * (row `div` 3)
    colStart = 3 * (col `div` 3)

-- Creates a new grid with the specified number placed at the given position.
placeNumber :: Grid -> Int -> Int -> Int -> Grid
placeNumber grid row col x =
  [ [ if (r,c) == (row,col) then x else grid !! r !! c | c <- [0..8] ] | r <- [0..8] ]

-- Prints the Sudoku grid in a human-readable format with borders between 3x3 subgrids and rows.
printBoard :: Grid -> IO ()
printBoard grid = do
  let groups = chunksOf3Rows grid
  mapM_ (\g -> do putStrLn "-------------"; mapM_ (putStrLn . formatRow) g) groups
  putStrLn "-------------"
  where
    chunksOf3Rows :: Grid -> [Grid]
    chunksOf3Rows [] = []
    chunksOf3Rows grid = take 3 grid : chunksOf3Rows (drop 3 grid)
    
    formatRow :: [Int] -> String
    formatRow row = "|" ++ intercalate "|" (map (concatMap show) (chunksOf3 row)) ++ "|"
    
    chunksOf3 :: [Int] -> [[Int]]
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

-- Some example Sudokus

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

-- A minimal Sudoku
minimal :: Grid
minimal = [ [0,9,8, 0,0,0, 0,0,0],
            [0,0,0, 0,7,0, 0,0,0],
            [0,0,0, 0,1,5, 0,0,0],
            
            [1,0,0, 0,0,0, 0,0,0],
            [0,0,0, 2,0,0, 0,0,9],
            [0,0,0, 9,0,6, 0,8,2],
            
            [0,0,0, 0,0,0, 0,3,0],
            [5,0,1, 0,0,0, 0,0,0],
            [0,0,0, 4,0,0, 0,2,0]
          ]

main :: IO ()
main = do
  putStrLn "Original board:"
  printBoard minimal
  let solutions = solve minimal
  if null solutions then
    putStrLn "No solution found."
  else do
    putStrLn "\nSolutions:"
    mapM_ (\sol -> do 
             printBoard sol
             putStrLn ""
          ) solutions
