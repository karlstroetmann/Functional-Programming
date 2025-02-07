wordCount :: String -> (Int, Int, Int)
wordCount s = (length $ lines s, length $ words s, length s)     

showTriple :: (Int, Int, Int) -> String
showTriple (a, b, c) = show a ++ " " ++ show b ++ " " ++ show c ++ "\n"

main = interact (showTriple . wordCount)
