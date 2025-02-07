-- This code is adapted from "Real World Haskell: Code You Can Believe In"
-- by Bryan O'Sullivan, John Goerzen, and Don Stewart, published by O'Reilly Media.
-- Copyright © 2008 Bryan O'Sullivan, John Goerzen, and Don Stewart.
-- The code is used here for educational purposes under the doctrine of fair use.
-- For more details, see the book at: https://www.oreilly.com/library/view/real-world-haskell/9780596514983/

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
          args <- getArgs
          case args of
               [input, output] -> interactWith function input output
               _ -> putStrLn "error: exactly two arguments needed"
          -- replace "id" with the name of our function below
          myFunction = id


