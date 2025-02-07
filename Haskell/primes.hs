--primes :: [Integer]
primes = sieve [2..]

--sieve :: [Integer] -> [Integer]
sieve (p:ns) = p : sieve [n | n <- ns, mod n p /= 0]

-- compute the ten thousandth prime number
main :: IO ()
main = print $ primes !! 9999
