{-# LANGUAGE UnicodeSyntax #-}
import Data.Set (Set, (\\), fromList, toList, findMin, insert, empty)
import qualified Data.Set as Set

import Bfs (search)
import SetUtils (power, (∪), (∩), (∈), (∉))
  
-- Define the problem
problem :: Set String -> Bool
problem s =
    "farmer" ∉ s &&
    (("goat" ∈ s && "cabbage" ∈ s) ||  -- Goat eats cabbage
     ("wolf" ∈ s && "goat" ∈ s))       -- Wolf eats goat

-- Define the universe of all items
allItems :: Set String
allItems = fromList ["farmer", "wolf", "goat", "cabbage"]

noProblem :: Set String -> Bool
noProblem s = not (problem s) && not (problem $ allItems \\ s)

-- Define all valid states (in this case, subsets of `allItems`)
states :: Set (Set String)
states = Set.filter noProblem (power allItems)

stateList :: [Set String]
stateList = toList states

-- Generate relations r1 and r2
r1 :: [(Set String, Set String)]
r1 = [ (s, diff)
     | s <- stateList
     , b <- toList $ power s
     , let diff = s \\ b 
     , diff ∈ states
     , "farmer" ∈ b
     , length b <= 2
     ]

r2 :: [(Set String, Set String)]
r2 = map (\(s1, s2) -> (s2, s1)) r1

-- Combine relations
r :: [(Set String, Set String)]
r = r1 ++ r2

-- Define start and goal states
start :: Set String
start = allItems

goal :: Set String
goal = empty

-- Define the path using the search function
-- Assuming `search` is implemented as provided earlier
path :: Maybe [Set String]
path = search r start goal

-- Function to print each list in the path
printPath :: [Set String] -> IO ()
printPath path = mapM_ (putStrLn . show . toList) path

main :: IO ()
main = do
    let path = search r start goal 
    case path of
        Nothing -> putStrLn "No path found."
        Just p  -> printPath p

-- Solution:
-- ["cabbage","farmer","goat","wolf"]
-- ["cabbage","wolf"]
-- ["cabbage","farmer","wolf"]
-- ["cabbage"]
-- ["cabbage","farmer","goat"]
-- ["goat"]
-- ["farmer","goat"]
-- []

