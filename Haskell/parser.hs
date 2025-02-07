import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

-- take a parser and uses it on a string
parse :: Parser a -> String -> [(a,String)]
parse (P p) s = p s

-- a parser that reads one character
item :: Parser Char
item = P (\s -> case s of
                  []     -> []
                  (x:xs) -> [(x, xs)])
                  

-- turn `Parser` into a `Functor`
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\s -> case parse p s of
                        []       -> []
                        [(v, r)] -> [(g v, r)])
             
-- turn `Parser` into an `Applicative`
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\s -> [(v,s)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\s -> case parse pg s of
                         []       -> []
                         [(g, r)] -> parse (g <$> px) r)
              
-- turn `Parser` into a `Monad`
instance Monad Parser where
  -- (<*>) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\s -> case parse p s of
                       []       -> []
                       [(v, r)] -> parse (f v) r)
  
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\s -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\s -> case parse p s of
                       []       -> parse q s
                       [(v, r)] -> [(v, r)])
    
-- compute a parser that reads a character if it satisfies the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty
           
-- read a single digit
digit :: Parser Char
digit = sat isDigit

-- read a lower case character
lower :: Parser Char
lower = sat isLower

-- read an upper case character
upper :: Parser Char
upper = sat isUpper

-- read an alphabetic character
letter :: Parser Char
letter = sat isAlpha

-- read alphanumeric character
alphnum :: Parser Char
alphnum = sat isAlphaNum

-- read a specifixc character
char :: Char -> Parser Char
char x = sat (== x)

-- read a given string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return $ x:xs
                   
ident :: Parser String
ident = do x  <- lower
           xs <- many alphnum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return $ read xs

space :: Parser ()
space = do many $ sat isSpace
           return ()
           
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token $ string xs

expr :: Parser Int
expr = do t <- term
          restExpr t

restExpr :: Int -> Parser Int
restExpr t = (do symbol "+"
                 a <- term
                 restExpr (t + a))
         <|> (do symbol "-"
                 s <- term
                 restExpr (t - s))
         <|> return t  -- This allows parsing to stop when there are no more operators

term :: Parser Int
term = do f <- factor
          restTerm f

restTerm :: Int -> Parser Int
restTerm f = (do symbol "*"
                 g <- power
                 restTerm (f * g))
         <|> (do symbol "/"
                 g <- power
                 restTerm (f `div` g))
         <|> return f  -- Stop when no more operators

power :: Parser Int
power = (do f <- factor
            symbol "^"
            p <- power
            return (f ^ p))
        <|> factor

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> natural
            
