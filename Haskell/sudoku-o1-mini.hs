import Data.List (nub)
import Data.Maybe (listToMaybe, catMaybes)

type Board = [[Int]]

-- Find the first empty cell (represented by 0)
findEmpty :: Board -> Maybe (Int, Int)
findEmpty board = listToMaybe [ (r, c) | (r, row) <- zip [0..] board
                                       , (c, val) <- zip [0..] row
                                       , val == 0
                              ]

-- Check if placing n at position (r, c) is valid
valid :: Board -> (Int, Int) -> Int -> Bool
valid board (r, c) n = notElem n (board !! r) &&
                       notElem n [ board !! i !! c | i <- [0..8] ] &&
                       notElem n [ board !! i !! j | i <- [baseRow .. baseRow + 2]
                                                   , j <- [baseCol .. baseCol + 2]
                                 ]
                       where baseRow = (r `div` 3) * 3
                             baseCol = (c `div` 3) * 3

-- Update the board by placing n at position (r, c)
updateBoard :: Board -> (Int, Int) -> Int -> Board
updateBoard board (r, c) n = take r board ++
                             [take c (board !! r) ++ [n] ++ drop (c+1) (board !! r)] ++
                             drop (r+1) board

-- The backtracking solver
solve :: Board -> Maybe Board
solve board = case findEmpty board of
                  Nothing     -> Just board  -- no empty cells, solution found
                  Just (r, c) -> listToMaybe $ catMaybes
                                 [ if valid board (r, c) n
                                   then solve (updateBoard board (r, c) n)
                                   else Nothing
                                 | n <- [1..9]
                                 ]

-- Helper function to print the board in a readable format
printBoard :: Board -> IO ()
printBoard = mapM_ print

-- Example board (0 represents an empty cell)
example :: Board
example = [ [5,3,0, 0,7,0, 0,0,0],
            [6,0,0, 1,9,5, 0,0,0],
            [0,9,8, 0,0,0, 0,6,0],
            [8,0,0, 0,6,0, 0,0,3],
            [4,0,0, 8,0,3, 0,0,1],
            [7,0,0, 0,2,0, 0,0,6],
            [0,6,0, 0,0,0, 2,8,0],
            [0,0,0, 4,1,9, 0,0,5],
            [0,0,0, 0,8,0, 0,7,9]
          ]

diabolical :: Board
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

main :: IO ()
main = do
  putStrLn "Original board:"
  printBoard diabolical
  case solve diabolical of
    Nothing -> putStrLn "No solution found."
    Just solution -> do
      putStrLn "\nSolution:"
      printBoard solution
