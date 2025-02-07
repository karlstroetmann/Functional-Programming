{-# LANGUAGE UnicodeSyntax #-}
import Data.Set
import qualified Data.Set as Set

primes :: Int -> Set Int
primes n = s \\ fromList [p * q | p <- toList s, q <- toList s]
  where s = fromList [2..n] 

primes' :: Int -> Set Int
primes' n = s \\ Set.map (uncurry (*)) (cartesianProduct s s)
  where s = fromList [2..n] 
