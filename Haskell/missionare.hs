{-# LANGUAGE UnicodeSyntax #-}
-- Die Bekehrung der Ungläubigen

-- Drei Missionare und drei Ungläubige wollen zusammen einen Fluss überqueren, denn die Ungläubigen sollen
-- in der Kirche, die sich auf dem anderen Ufer befindet, getauft werden. Sie haben nur ein Boot, das Platz
-- für **maximal zwei**  Passagiere bietet.  Sowohl die Ungläubigen als auch die Missionare können rudern.
-- Es ist zusätzlich bekannt, dass die Ungläubigen zum Kannibalismus neigen. Die Kannibalen sind hungrig,
-- wenn die Missionare an einem der beiden Ufer in der Unterzahl sind, haben sie ein **Problem**.  Ihre
-- Aufgabe besteht darin, einen Fahrplan zu erstellen, so dass einerseits alle Personen das andere Ufer
-- erreichen und andererseits die Missionare zwischendurch kein Problem bekommen. 

import Data.Set (Set, (\\), fromList, toList, findMin, insert, empty)
import qualified Data.Set as Set

import Bfs (search)
import SetUtils (power, (∪), (∩), (∈), (∉))

-- States are described as triples of integers
type State = (Int, Int, Int)
  
-- Define the problem
problem :: Int -> Int -> Bool
problem m k = 0 < m && m < k

no_problem :: Int -> Int -> Bool
no_problem m k = not (problem m k) && not (problem (3-m) (3-k))

boat_ok :: Int -> Int -> Bool
boat_ok m k = 1 <= m + k && m + k <= 2

-- Generate relations r1 and r2
r1 :: [(State, State)]
r1 = [ ((m, k, 1), (m - mb, k - kb, 0))
     | m <- [0..3], k <- [0..3], mb <- [0..m], kb <- [0..k],
       no_problem m k,
       no_problem (m - mb) (k - kb),
       boat_ok mb kb
     ]

r2 :: [(State, State)]
r2 = map (\(s1, s2) -> (s2, s1)) r1

-- Combine relations
r :: [(State, State)]
r = r1 ++ r2

-- Define start and goal states
start :: State
start = (3, 3, 1)

goal :: State
goal = (0, 0, 0)

-- Compute the path using the search function
path :: Maybe [State]
path = search r start goal

-- Function to print each list in the path
printPath :: [State] -> IO ()
printPath path = mapM_ (putStrLn . show) path

main :: IO ()
main = do
    let path = search r start goal 
    case path of
        Nothing -> putStrLn "No path found."
        Just p  -> printPath p

-- Solution
-- (3,3,1)
-- (3,1,0)
-- (3,2,1)
-- (3,0,0)
-- (3,1,1)
-- (1,1,0)
-- (2,2,1)
-- (0,2,0)
-- (0,3,1)
-- (0,1,0)
-- (0,2,1)
-- (0,0,0)


