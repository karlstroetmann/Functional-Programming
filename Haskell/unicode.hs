{-# LANGUAGE UnicodeSyntax #-}

-- Define a data type using Unicode operator constructors
data Result a b = a :✓: b | a :✗: b deriving (Show)

-- Example function using the operator constructors
evaluate :: Bool -> Result String String
evaluate True  = "Success" :✓: "All tests passed"
evaluate False = "Error"   :✗: "Some tests failed"

-- Example usage
main :: IO ()
main = do
    print (evaluate True)   -- Output: "Success" :✓: "All tests passed"
    print (evaluate False)  -- Output: "Error" :✗: "Some tests failed"

