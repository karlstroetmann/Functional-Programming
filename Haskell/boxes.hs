-- SUDOKU IN HASKELL
-- Graham Hutton, January 2025
-- Based upon notes by Richard Bird

--import Data.List 
import Data.List (intercalate, transpose, (\\), intersperse)

import qualified Data.Set as Set

type Grid     = Matrix Value

type Matrix a = [Row a]

type Row a    = [a]

type Value    = Char


-- Basic definitions 
boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '.')

-- Example grids
easy                  :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]

-- First gentle example from sudoku.org.uk:
gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]

-- First diabolical example:
diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

-- First "unsolvable" (requires backtracking) example:
unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

-- Minimal sized grid (17 values) with a unique solution:
minimal               :: Grid
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

-- Empty grid:
blank :: Grid
blank = replicate n (replicate n '.')
        where n = boxsize ^ 2


-- Extracting rows, columns and boxes
-- Extracting rows is trivial:
rows :: Matrix a -> [Row a]
rows =  id

-- We also have, trivially, that rows . rows = id.  This property (and
-- similarly for cols and boxs) will be important later on.

-- Extracting columns is just matrix transposition:
cols :: Matrix a -> [Row a]
cols =  transpose

-- We also have that cols . cols = id.

boxs :: Matrix a -> [Row a]
boxs m = [ box a b | a <- l, b <- l ]
         where box a b = [m !! (3*a+r) !! (3*b+c) | r <- l, c <- l]
               l       = [0..boxsize-1]

-- A grid is valid if there are no duplicates in any row, column or box:
valid :: Grid -> Bool
valid g =  all nodups (rows g) &&
           all nodups (cols g) &&
           all nodups (boxs g)

-- Check that the given list has no duplicates.
nodups :: Ord a => [a] -> Bool
nodups xs = length (Set.fromList xs) == length xs

type Choices =  [Value]

-- The function choices replaces blank squares in a grid by all possible
-- values for that square, giving a matrix of choices:
choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v
                  | v == '.'  = ['1'..'9']
                  | otherwise = [v]

-- Reducing a matrix of choices to a choice of matrices can be defined 
-- in terms of the normal cartesian product of a list of lists, which
-- generalises the cartesian product of two lists:
cp :: [[a]] -> [[a]]
cp []       =  [[]]
cp (xs:xss) =  [y:ys | y <- xs, ys <- cp xss]

-- It is now simple to collapse a matrix of choices:
collapse :: Matrix [a] -> [Matrix a]
collapse m = cp $ map cp m

-- Given a list, check if it contains exactly one element
single :: [a] -> Bool
single [_] =  True
single  _  =  False

-- Given a row of choices, remove all choices that are impossible.
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
             where singles = concat (filter single xss)

-- Unless xs consists of a single element, all elements of
-- ys are removed from xs,
minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

-- Our first step to making things better is to introduce the idea
-- of "pruning" the choices that are considered for each square.
-- Prunes go well with wholemeal programming!  In particular, from
-- the set of all possible choices for each square, we can prune
-- out any choices that already occur as single entries in the
-- associated row, column, and box, as otherwise the resulting
-- grid will be invalid.  Here is the code for this:
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where pruneBy :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
        pruneBy f = f . map reduce . f

-- Note that pruneBy relies on the fact that rows . rows = id, and
-- similarly for the functions cols and boxs, in order to decompose
-- a matrix into its rows, operate upon the rows in some way, and
-- then reconstruct the matrix.  

-- In turn, we use the term "safe" for matrix for which all rows,
-- columns and boxes are consistent, in the sense that they do not
-- contain more than one occurrence of the same single choice:
safe :: Matrix Choices -> Bool
safe cm = all consistent (rows cm) &&
          all consistent (cols cm) &&
          all consistent (boxs cm)

consistent :: Row Choices -> Bool
consistent = nodups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m =  void m || not (safe m)

-- A matrix is "complete" if each square contains a single choice:
complete :: Matrix Choices -> Bool
complete = all (all single)

-- Similarly, a matrix is "void" if some square contains no choices:
void :: Matrix Choices -> Bool
void =  any (any null)

-- Solve a given Sudoku
solve :: Grid -> [Grid]
solve = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked  m =  []
  | complete m =  collapse m
  | otherwise  =  [g | m' <- expand m, g  <- search (prune m')]

-- The function expand behaves in the same way as collapse, except that
-- it only collapses the first square with more than one choice:
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
           where (rows1, row:rows2) = break (any (not . single)) m
                 (row1,  cs:row2  ) = break (not . single)       row




showGrid :: Grid -> String
showGrid xs = unlines $ dashes : concatMap insertDashes (chunksOf 3 (map showLine xs))
  where dashes = replicate 13 '-'
        insertDashes block = block ++ [dashes] 

showLine :: Row Value -> String
showLine xs = "|" ++ intercalate "|" (chunksOf 3 xs) ++ "|"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main :: IO ()
main = (putStr . showGrid . head . solve) minimal
