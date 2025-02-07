{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bfs (search) where

import Data.Set (Set, fromList, singleton)
import qualified Data.Set as Set 

(∪) :: Ord a ⇒ Set a → Set a → Set a
(∪) = Set.union

(∩) :: Ord a ⇒ Set a → Set a → Set a
(∩) = Set.intersection

(∈) :: (Ord a) ⇒ a → Set a → Bool
(∈ ) = Set.member

(∉) :: (Ord a) ⇒ a → Set a → Bool
x ∉ s = not (x ∈ s)

type Path a = [a]

search :: forall a. (Ord a) ⇒ [(a, a)] → a → a → Maybe (Path a)
search relation start goal = go [[start]] (singleton start)
  where
    go :: [Path a] → Set a → Maybe (Path a)
    go [] _ = Nothing
    go paths visited
      | null newPaths = Nothing
      | otherwise =
          case findGoal newPaths of
            Just path → Just path
            Nothing → go newPaths (visited ∪ fromList [last path | path <- newPaths])
      where
        -- Generate new paths
        newPaths :: [Path a]
        newPaths = [ path ++ [z] 
                   | path <- paths, 
                     (y, z) <- relation, 
                     last path == y, 
                     z ∉ visited
                   ]
        
        -- Check if the goal is reached
        findGoal :: [Path a] → Maybe (Path a)
        findGoal ps = case filter (\p → last p == goal) ps of
                        []    → Nothing
                        (p:_) → Just p

main :: IO ()
main = do
    let relation = [("a", "b"), ("a", "c"), ("b", "d"), ("c", "d"), ("d", "e")]
        start = "a"
        goal = "e"
    case search relation start goal of
        Just path → putStrLn $ "Path found: " ++ show path
        Nothing   → putStrLn "No path found."
