cp :: [a] -> [b] -> [(a,b)]
cp xs ys = do x <- xs
              y <- ys
              [(x,y)]

       
