import Data.List (sort, words, dropWhileEnd)
import Data.Char (isAlpha, toLower)
import Text.Printf (printf)
import qualified Data.Set as Set

type Text = [Char]
type Word = [Char]

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
frequency c n = fromIntegral (round (result * scale)) / scale
  where
    result = fromIntegral c / fromIntegral n :: Double
    scale  = 10 ^^ decimalPlaces result -- Scale for two significant digits

decimalPlaces :: Double -> Int
decimalPlaces f = 2 - floor (logBase 10 f)

-- Converts a floating-point number to a formatted string using printf.
formatNumber :: Double -> String
formatNumber f = printf ("%." ++ show (decimalPlaces f) ++ "f") f

-- Formats a word with its frequency, ensuring right alignment.
showFreq :: Int -> (Double, String) -> String
showFreq maxLen (f, w) = printf "%*s: %s\n" maxLen w (formatNumber f)

-- Splits a string into words and removes non-alphabetic characters.
myWords :: String -> [String]
myWords = map (filter isAlpha) . words

-- Computes the frequency of each word.
divide :: Int -> [(Int, a)] -> [(Double, a)]
divide n ps = [(frequency c n, x) | (c, x) <- ps]

-- Extracts the `n` most common words from a text and formats them.
comnWrds :: Int -> Text -> String
comnWrds n b = concatMap (showFreq (maximum (map (length . snd) wf))) wf
  where
    wf = divide nw . take n . reverse . sort . countRuns . sort $ aw
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
