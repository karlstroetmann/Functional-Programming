{-# LANGUAGE UnicodeSyntax #-}
module Bfs (search) where

import Data.Set (Set)
import qualified Data.Set as Set

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = Set.union

(∈) :: (Ord a) => a -> Set a -> Bool
(∈ ) = Set.member

(∉) :: (Ord a) => a -> Set a -> Bool
x ∉ s = not (x ∈ s)

type Path a = [a]

-- take a binary relation and two points start and goal and compute a path from start to goal
search :: forall a. Ord a => [(a, a)] -> a -> a -> [Path a]
search relation start goal = go [[start]] (Set.singleton start)
  where
    go :: [Path a] -> Set a -> [Path a]
    go [] _ = []
    go paths visited
      | null goalPaths = go newPaths newVisited
      | otherwise = goalPaths
      where
        newPaths   = [ path ++ [z] | path <- paths, (y, z) <- relation, 
                                     last path == y, z ∉ visited       ]
        newVisited = visited ∪ Set.fromList [last path | path <- newPaths]
        goalPaths  = filter (\p -> last p == goal) newPaths

main :: IO ()
main = do
    let relation = [("a", "b"), ("a", "c"), ("b", "d"), ("c", "d"), ("d", "e")]
        start = "a"
        goal = "e"
        paths = search relation start goal
    if null paths 
        then putStrLn "No path found."
        else putStrLn $ "Paths found: " ++ show paths
