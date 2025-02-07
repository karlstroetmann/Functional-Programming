{-# LANGUAGE UnicodeSyntax #-}
import Data.List (sort, nub)
import Data.Set (Set, fromList, toList, findMax)
import SetUtils ((∩))


-- define the operator `%` as an alias for `mod`
(%) :: Integer -> Integer -> Integer
x % y = x `mod` y

(÷) :: Integer -> Integer -> Integer
x ÷ y = x `div` y

-- find the divisors of n
divisors :: Integer -> [Integer]
divisors n = [k | k <- [1..n], n % k == 0] 

-- find the first divisor of a given number greater than 1
first_divisor :: Integer -> Integer
first_divisor n = first_divisor_gt n 2

-- find the first divisor of a given number greater or equal than k
first_divisor_gt :: Integer -> Integer -> Integer
first_divisor_gt n k 
  | k * k > n  = n
  | n % k == 0 = k
  | otherwise  = first_divisor_gt n (k + 1)
  
-- find the divisors of n
prime_divisors :: Integer -> [Integer]
prime_divisors n
  | k == n    = [n]
  | otherwise = k : prime_divisors (n ÷ k)
  where k = first_divisor n

-- find all sublists of a given list  
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = rest ++ map (x:) rest
  where rest = sublists xs

-- find all divisors of n
fast_divisors :: Integer -> [Integer]
fast_divisors n = (nub . sort) [ product l | l <- (sublists . prime_divisors) n]

-- find all divisors of n
divisors_set :: Integer -> Set Integer
divisors_set n = fromList [ product l | l <- (sublists . prime_divisors) n]

-- common divisors
common_divisors :: Integer -> Integer -> Set Integer
common_divisors m n = (divisors_set m) ∩ (divisors_set n)

-- greatest common divisor
greatest_common_divisor m n = findMax $ common_divisors m n
