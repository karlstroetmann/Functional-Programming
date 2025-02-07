import Data.List (permutations, elemIndex)
import Control.Monad (guard)

-- Define the attributes
colors        = ["red", "green", "ivory", "yellow", "blue"]
nationalities = ["Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"]
beverages     = ["coffee", "tea", "milk", "orange juice", "water"]
cigarettes    = ["Old Gold", "Kools", "Chesterfields", "Lucky Strike", "Parliaments"]
pets          = ["dog", "snails", "fox", "horse", "zebra"]

-- Helper function to find the index of a value in a list (1-based)
findIndex :: Eq a => [a] -> a -> Int
findIndex lst value = case elemIndex value lst of
    Just idx -> idx + 1
    Nothing  -> error "Value not found in list"

-- Auxiliary function to simplify attribute lookups
attrAt :: Eq a => [a] -> a -> [b] -> b
attrAt lst value target = target !! (findIndex lst value - 1)

-- Auxiliary function to check if two attributes are neighbors
areNeighbors :: Eq a => [a] -> a -> [a] -> a -> Bool
areNeighbors lst1 value1 lst2 value2 = abs (findIndex lst1 value1 - findIndex lst2 value2) == 1

-- Solve the puzzle
solveZebraPuzzle :: [([String], [String], [String], [String], [String])]
solveZebraPuzzle = do
    color <- permutations colors
    -- Constraint 4: The green house is immediately to the right of the ivory house
    guard (findIndex color "green" == findIndex color "ivory" + 1)
    nationality <- permutations nationalities
    -- Constraint 9: The Norwegian lives in the first house
    guard (head nationality == "Norwegian")
    -- Constraint 15: The Norwegian lives next to the blue house
    guard (areNeighbors color "blue" nationality "Norwegian")
    beverage <- permutations beverages
    -- Constraint 8: Milk is drunk in the middle house
    guard (beverage !! 2 == "milk")
    cigarette <- permutations cigarettes
    -- Constraint 7: Kools are smoked in the yellow house
    guard (attrAt color "yellow" cigarette == "Kools")
    -- Constraint 12: The Lucky Strike smoker drinks orange juice
    guard (attrAt cigarette "Lucky Strike" beverage == "orange juice")
    -- Constraint 14: The Japanese smokes Parliaments
    guard (attrAt nationality "Japanese" cigarette == "Parliaments")
    pet <- permutations pets
    -- Constraint 2: The Spaniard owns the dog
    guard (attrAt nationality "Spaniard" pet == "dog")
    -- Constraint 6: The Old Gold smoker owns snails
    guard (attrAt cigarette "Old Gold" pet == "snails")
    -- Constraint 10: The man who smokes Chesterfields lives next to the man with the fox
    guard (areNeighbors cigarette "Chesterfields" pet "fox")
    -- Constraint 11: Kools are smoked in the house next to the house where the horse is kept
    guard (areNeighbors cigarette "Kools" pet "horse")
    -- Constraint 1: The Englishman lives in the red house
    guard (attrAt nationality "Englishman" color == "red")
    -- Constraint 3: Coffee is drunk in the green house
    guard (attrAt color "green" beverage == "coffee")
    -- Constraint 5: The Ukrainian drinks tea
    guard (attrAt nationality "Ukrainian" beverage == "tea")
    -- Return the valid arrangement
    return (color, nationality, beverage, cigarette, pet)

-- Main function to print the solution
main :: IO ()
main = case solveZebraPuzzle of
    [] -> putStrLn "No solution found."
    ((color, nationality, beverage, cigarette, pet):_) -> do
        putStrLn "Solution found:"
        putStrLn $ "Colors: " ++ show color
        putStrLn $ "Nationalities: " ++ show nationality
        putStrLn $ "Beverages: " ++ show beverage
        putStrLn $ "Cigarettes: " ++ show cigarette
        putStrLn $ "Pets: " ++ show pet
        -- Answer the questions
        let waterDrinker = attrAt beverage "water" nationality
        let zebraOwner = attrAt pet "zebra" nationality
        putStrLn $ "\nWho drinks water? " ++ waterDrinker
        putStrLn $ "Who owns the zebra? " ++ zebraOwner
