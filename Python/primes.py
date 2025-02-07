def sieve_of_eratosthenes(n):
    """Return a list of prime numbers up to n (inclusive)."""
    if n < 2:
        return []
    
    # Initialize a boolean array of True values.
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False  # 0 and 1 are not primes
    
    # Only need to check factors up to sqrt(n)
    p = 2
    while p * p <= n:
        if is_prime[p]:
            # Mark multiples of p as non-prime.
            for i in range(p * p, n + 1, p):
                is_prime[i] = False
        p += 1
    
    # Extract and return the list of primes.
    return [i for i, prime in enumerate(is_prime) if prime]
