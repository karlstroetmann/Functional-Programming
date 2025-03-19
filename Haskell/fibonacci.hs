-- Haskell: Fibonacci function
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    let n = 40  -- Calculate the 40th Fibonacci number
    print (fib n)
    
