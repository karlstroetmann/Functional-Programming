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

instance Show Expr where
  show :: Expr -> String
  show (Add x y) = "(" ++ (show x) ++ "+" ++ (show y) ++ ")"
  show (Sub x y) = "(" ++ (show x) ++ "-" ++ (show y) ++ ")"
  show (Mul x y) = "(" ++ (show x) ++ "*" ++ (show y) ++ ")"
  show (Div x y) = "(" ++ (show x) ++ "/" ++ (show y) ++ ")"
  show (Pow x y) = "(" ++ (show x) ++ "^" ++ (show y) ++ ")"
  show (Exp x)   = "exp(" ++ show(x) ++ ")"
  show (Ln  x)   = "ln("  ++ show(x) ++ ")"
  show (Var c)   = [c]
  show (Num x)   = show x

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
diff (Var v) x   = if v == x then Num 1 else Num 0
diff (Num _) _   = Num 0

main :: IO ()
main = (putStrLn . show) $ diff (Pow (Var 'x') (Var 'x')) 'x'
