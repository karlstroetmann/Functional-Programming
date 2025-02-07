{-# LANGUAGE UnicodeSyntax #-}
module SetUtils (power, (∪), (∩), (∈), (∉)) where

import Data.Set (Set, (\\), fromList, toList, findMin, insert, empty)
import qualified Data.Set as Set

-- the empty set
(∅) :: Ord a ⇒ Set a
(∅) = empty

-- union of sets
(∪) :: Ord a ⇒ Set a → Set a → Set a
(∪) = Set.union

-- intersection of sets
(∩) :: Ord a ⇒ Set a → Set a → Set a
(∩) = Set.intersection

-- is x an element of s
(∈) :: (Ord a) ⇒ a → Set a → Bool
x ∈ s = x `Set.member` s

-- is x not an element of s
(∉) :: (Ord a) ⇒ a → Set a → Bool
x ∉ s = not (x ∈ s)

-- compute the power set of the set s
power :: Ord a ⇒ Set a → Set (Set a)
power s
  | Set.null s = fromList [ empty ]
  | otherwise  = p ∪ Set.map (\x → insert m x) p 
    where m = findMin s
          p = power (s \\ fromList [m])
 
-- compute a list of all subsets of s that have k elements
subsets :: Ord a ⇒ Set a → Int → [Set a]
subsets s 0 = [ (∅) ]
subsets s k = [ insert x a | a <- subsets s (k-1), x <- toList s, x ∉ a]
                
