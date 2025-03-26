import Data.List (sort, words)
import Data.Char (isAlpha, toLower)
import qualified Data.Set as Set

-- Counts consecutive occurrences of elements in a sorted list.
countRuns :: Eq a => [a] -> [(Int, a)]
countRuns [] = []
countRuns (x:xs) =
    let (run, rest) = break (/= x) xs
    in (1 + length run, x) : countRuns rest

-- Computes the frequency of a word.
--  * `c` is the number of occurrences (the count) of the word
--  * `n` is the total number of words.
-- It returns the fraction c / n.
frequency :: Int -> Int -> Double
frequency c n = fromIntegral c / fromIntegral n 

-- Computes the frequency of each word.
-- n is the total number of words
-- ps is a list of pairs (c, x) where x is a string and c is the count of x
divide :: Int -> [(Int, a)] -> [(Double, a)]
divide n ps = [(frequency c n, x) | (c, x) <- ps]

-- Splits a string into words and removes non-alphabetic characters.
myWords :: String -> [String]
myWords = map (filter isAlpha) . words
-- The previous line could habe been written as follows:
-- myWords s = (map (filter isAlpha) . words) s
-- The process of dropping the argument 's' form both sides of this equation is called
-- eta reduction.

-- Turn a pair of the form (frequency, word) into a string.
showPair :: (Double, String) -> String 
showPair (f, w) = w ++ ": " ++ show f ++ "\n"

-- Extracts the `n` most common words from a book b and formats them.
comnWrds :: Int -> String -> String
comnWrds n b = concatMap showPair $ divide nw . take n . reverse . sort . countRuns . sort $ aw
  where
    aw = (myWords . map toLower) b
    nw = length aw

-- find the number of distinct words
numDistinctWords :: String -> Int
numDistinctWords = Set.size . Set.fromList . myWords 
         
-- Reads the book "Moby Dick", computes the 100 most common words, and outputs them.
main :: IO ()
main = do
    contents <- readFile "moby-dick.txt"
    putStrLn $ comnWrds 100 contents
    putStrLn $ "number of distinct words: " ++ show (numDistinctWords contents)
