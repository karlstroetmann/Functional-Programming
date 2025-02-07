------------------------------------------
--  Implementation of the game Nim      --
------------------------------------------

import System.IO     -- import hSetEcho
type Board  = [Int]
type Player = Int

size :: Int
size = 8

play :: Board -> IO ()
play b =
  do printBoard b
     putStr ("\nEnter your move in the format (line, number): ")
     hSetEcho stdin False
     pair <- getLine
     let (row, n) = read pair :: (Int, Int)
     let newBoard = move b row n
     if and (map (==0) newBoard)
         then do putStrLn ("The human player has won!\n")
                 printBoard newBoard
                 hSetEcho stdin True
         else playComputer newBoard

playComputer :: Board -> IO ()
playComputer b =
  do printBoard b
     let (row, n) = bestMove b
     putStrLn ("The computer takes " ++ show n ++ " from line " ++ show row ++ ".")
     let newBoard = move b row n
     if and (map (==0) newBoard)
         then do putStrLn ("The computer has won!\n")
                 printBoard newBoard
                 hSetEcho stdin True
         else play newBoard

printBoard :: Board -> IO ()
printBoard []     = do return ()
printBoard (x:xs) = do let n = length xs
                       putStrLn (show (size - n) ++ ": " ++ showStars x)
                       printBoard xs

showStars :: Int -> String
showStars n = concat $ replicate n "* "

move :: Board -> Int -> Int -> Board
move (x:xs) 1 n
  | x >= n    = x - n : xs
  | otherwise = 0 : xs
move (x:xs) r n = x : move xs (r - 1) n 

-- return the best move for a given board
bestMove :: Board -> (Int, Int)
bestMove b = bestMoveHelper b (nimSum b) 1 b
    where
        bestMoveHelper :: Board -> Int -> Int -> Board -> (Int, Int)
        bestMoveHelper (x:xs) sum row oldBoard
            | let target = xor x sum,
                  0 <= target && target < x = (row, x - target)
            | otherwise                     = bestMoveHelper xs sum (row + 1) oldBoard
        bestMoveHelper [] sum row oldBoard = firstNonZero oldBoard 1
        -- no winning strategy, take one from the first nonempty row
        firstNonZero :: Board -> Int -> (Int, Int)
        firstNonZero (x:xs) row
            | x > 0     = (row, 1)
            | otherwise = firstNonZero xs (row + 1)
        
-- compute the nim sum of the board
nimSum :: Board -> Int
nimSum = foldr xor 0

-- bitwise xor of two natural numbers
xor :: Int -> Int -> Int
xor 0 y = y
xor x 0 = x
xor x y
  | mod x 2 == 0 && mod y 2 == 0 = 2 * xor (div x 2) (div y 2)
  | mod x 2 == 1 && mod y 2 == 0 = 2 * xor (div x 2) (div y 2) + 1
  | mod x 2 == 0 && mod y 2 == 1 = 2 * xor (div x 2) (div y 2) + 1
  | mod x 2 == 1 && mod y 2 == 1 = 2 * xor (div x 2) (div y 2) 
  
main :: IO ()
main = play [size, size-1 .. 1]

