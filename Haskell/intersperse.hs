intersperse :: a -> [[a]] -> [a]
intersperse a []       = []
intersperse a [x]      = x
intersperse a (x:y:ys) = x ++ [a] ++ intersperse a (y:ys)
