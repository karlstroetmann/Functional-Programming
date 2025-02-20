import Data.List (sort, words, dropWhileEnd)
import Data.Char (isAlpha, toLower)

-- Counts consecutive occurrences of elements in a sorted list.
countRuns :: Eq a => [a] -> [(Int, a)]
countRuns [] = []
countRuns (x:xs) =
    let (run, rest) = break (/= x) xs
    in (1 + length run, x) : countRuns rest

-- Computes the frequency of a word.
--  * `c` is the count of the word
--  * `n` is the total number of words.
-- It returns the fraction c / n truncated to three significant digits.
frequency :: Int -> Int -> Double
frequency c n = fromIntegral c / fromIntegral n 

-- Computes the frequency of each word.
divide :: Int -> [(Int, a)] -> [(Double, a)]
divide n ps = [(frequency c n, x) | (c, x) <- ps]

-- Splits a string into words and removes non-alphabetic characters.
myWords :: String -> [String]
myWords = map (filter isAlpha) . words

-- Turn a pair of the form (frequency, word) into a string.
showPair :: (Double, String) -> String 
showPair (f, w) = w ++ ": " ++ show f ++ "\n"

-- Extracts the `n` most common words from a text and formats them.
comnWrds :: Int -> String -> String
comnWrds n b = concatMap showPair $ divide nw . take n . reverse . sort . countRuns . sort $ aw
      where
    aw = (myWords . map toLower) b
    nw = length aw
                 
-- Reads a file and returns its content.
readBook :: FilePath -> IO String
readBook filename = readFile filename

-- Reads the book "Moby Dick", computes the 100 most common words, and outputs them.
main :: IO ()
main = do
    contents <- readBook "moby-dick.txt"
    putStrLn $ comnWrds 100 contents
