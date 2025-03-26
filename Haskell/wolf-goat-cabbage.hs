{-# LANGUAGE UnicodeSyntax #-}
import Data.Set (Set, (\\), fromList, toList, filter)
import Bfs (search)
import SetUtils (power, (∈), (∉))
  
-- Define the problem
problem :: Set String -> Bool
problem s = "farmer" ∉ s &&
            (("goat" ∈ s && "cabbage" ∈ s) ||  -- goat eats cabbage
             ("wolf" ∈ s && "goat"    ∈ s))    -- wolf eats goat

-- Define the universe of all items
allItems :: Set String
allItems = fromList ["farmer", "wolf", "goat", "cabbage"]

noProblem :: Set String -> Bool
noProblem s = not (problem s) && not (problem $ allItems \\ s)

-- Define all valid states (in this case, subsets of `allItems`)
states :: Set (Set String)
states = Data.Set.filter noProblem (power allItems)

-- Generate relations r1 and r2
r1 :: [(Set String, Set String)]
r1 = [ (s, s \\ b) | s <- toList states,
                     b <- toList $ power s, s \\ b ∈ states,
                     "farmer" ∈ b, length b <= 2
     ]

r2 :: [(Set String, Set String)]
r2 = [ (s1, s2) | (s2, s1) <- r1 ]

-- Combine the relations
r :: [(Set String, Set String)]
r = r1 ++ r2

-- Define start and goal states
start :: Set String
start = allItems

goal :: Set String
goal = fromList []

main :: IO ()
main = mapM_ (putStrLn . show) $ head $ search r start goal 

-- Solution:
-- ["cabbage","farmer","goat","wolf"]
-- ["cabbage","wolf"]
-- ["cabbage","farmer","wolf"]
-- ["cabbage"]
-- ["cabbage","farmer","goat"]
-- ["goat"]
-- ["farmer","goat"]
-- []


