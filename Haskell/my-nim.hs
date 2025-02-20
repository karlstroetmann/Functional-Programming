{-# LANGUAGE UnicodeSyntax #-}
------------------------------------------
--  Implementation of the game Nim      --
------------------------------------------

import System.IO (stdin, hSetEcho)
import Data.Bits (xor)

(⊕) :: Int -> Int -> Int
(⊕) = xor

type Board  = [Int]

size :: Int
size = 8

-- The function `move b r n` performs the move of taking `n` matches from
-- row `r` on the board `b` and returns the resultimng board.
move :: Board -> Int -> Int -> Board
move (x:xs) 1 n = x - n : xs
move (x:xs) r n = x : move xs (r - 1) n 

-- Check whether there is a winning move for a row containing `n` matches.
-- The parameter `s` is the Nim sum of the board.
isWinning :: Int -> Int -> Bool
isWinning s n = 0 <= n ⊕ s && n ⊕ s < n

-- all wining moves for the board `b`
winningMoves :: Board -> [(Int, Int)]
winningMoves b = [ (r, n - n ⊕ s) | (r, n) <- zip [1..] b, isWinning s n ]
  where s = nimSum b

-- minimal possible moves for the board b
-- This function is needed in case there is no winning move.
nonZero :: Board -> [(Int, Int)]
nonZero b = [ (r, 1) | (r, n) <- zip [1..] b, n > 0 ]

-- get the best move for the board `b`
bestMove :: Board -> (Int, Int)
bestMove b = head $ winningMoves b ++ nonZero b
       
-- compute the nim sum of the board
nimSum :: Board -> Int
nimSum = foldr (⊕) 0

-- Everything from here is just Input / Output

-- let the human make a move
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

-- let the computer make a move on the given board
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

-- print the given board
printBoard :: Board -> IO ()
printBoard []     = do return ()
printBoard (x:xs) = do let n = length xs
                       putStrLn (show (size - n) ++ ": " ++ stars x)
                       printBoard xs

-- create a string of n stars interleaved with blanks
stars :: Int -> String
stars n = concat $ replicate n "* "

main :: IO ()
main = play [size, size-1 .. 1]

