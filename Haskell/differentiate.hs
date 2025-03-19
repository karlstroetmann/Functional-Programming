{-# LANGUAGE UnicodeSyntax #-}
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Exp Expr
          | Ln  Expr
          | Var Char
          | Num Double

-- Make Expr an instance of Show
instance Show Expr where
    show = prettyPrint

-- Helper function to determine if parentheses are needed
needsParens :: Expr -> Bool
needsParens (Num _) = False
needsParens (Var _) = False
needsParens (Exp _) = False
needsParens (Ln  _) = False
needsParens _ = True

-- Pretty printing function
prettyPrint :: Expr -> String
prettyPrint (Num n) = 
    if n == fromInteger (round n) 
    then show (round n)  -- Show integers without decimal
    else show n          -- Show decimals as is
prettyPrint (Var x) = [x]
prettyPrint (Add e1 e2) = 
    prettyPrint e1 ++ " + " ++ prettyPrint e2
prettyPrint (Sub e1 e2) = 
    prettyPrint e1 ++ " - " ++ 
    (if needsParens e2 then "(" ++ prettyPrint e2 ++ ")" else prettyPrint e2)
prettyPrint (Mul e1 e2) = 
    let e1Str = if needsParens e1 then "(" ++ prettyPrint e1 ++ ")" else prettyPrint e1
        e2Str = if needsParens e2 then "(" ++ prettyPrint e2 ++ ")" else prettyPrint e2
    in e1Str ++ " * " ++ e2Str
prettyPrint (Div e1 e2) = 
    let e1Str = if needsParens e1 then "(" ++ prettyPrint e1 ++ ")" else prettyPrint e1
        e2Str = if needsParens e2 then "(" ++ prettyPrint e2 ++ ")" else prettyPrint e2
    in e1Str ++ " / " ++ e2Str
prettyPrint (Pow e1 e2) = 
    let e1Str = if needsParens e1 then "(" ++ prettyPrint e1 ++ ")" else prettyPrint e1
        e2Str = if needsParens e2 then "(" ++ prettyPrint e2 ++ ")" else prettyPrint e2
    in e1Str ++ "^" ++ e2Str
prettyPrint (Exp e) = "exp(" ++ prettyPrint e ++ ")"
prettyPrint (Ln  e) = "ln("  ++ prettyPrint e ++ ")"

-- symbolic differentiation
diff :: Expr → Char → Expr
diff (Add f g) x = Add fs gs
  where fs = diff f x; gs = diff g x
diff (Sub f g) x = Sub fs gs
  where fs = diff f x; gs = diff g x
diff (Mul f g) x = Add (Mul fs g) (Mul f gs)
  where fs = diff f x; gs = diff g x
diff (Div f g) x = Div (Sub (Mul fs g) (Mul f gs)) (Mul g g)
  where fs = diff f x; gs = diff g x
diff (Pow f g) x = diff (Exp (Mul g (Ln f))) x 
diff (Exp f) x = Mul (Exp f) fs
  where fs = diff f x
diff (Ln f) x = (Div fs f)
  where fs = diff f x
diff (Var v) x = if v == x then Num 1 else Num 0
diff (Num _) _ = Num 0

main :: IO ()
main = (putStrLn . show) $ diff (Pow (Var 'x') (Var 'x')) 'x'
