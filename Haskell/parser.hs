-- This is a parser for arithmetic expression that is based on the parser discussed
-- in the book "Programming in Haskell" by Graham Hutton.
-- It is implements the following EBNF grammar.
-- 
-- expr     ::= term restExpr
--           ;
--
-- restExpr ::= "+" term restExpr
--           |  "-" term restExpr
--           |  
--           ; 
-- 
-- term     ::= factor restTerm
--           ;
-- 
-- restTerm ::= "*" power restTerm
--           |  "/" power restTerm
--           |
--           ;
-- 
-- power    ::= factor "^" power
--           |  factor
--           ;
-- 
-- factor   ::= "(" expr ")"
--           |  natural
--           ;
--
-- natural  ::= digit*
--           ;
--
-- In this grammar, white space is silently discarded.

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
    
-- computes a parser that reads a character iff that character satisfies
-- the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty
           
-- read a single digit
digit :: Parser Char
digit = sat isDigit

-- read a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- read a specific string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- read a natural number                   
nat :: Parser Int
nat = do xs <- some digit
         return $ read xs

-- read whitespace characters and discard them
space :: Parser ()
space = do many $ sat isSpace
           return ()

-- read a token possibly surrounded by whitespace
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- parse a natural number possibly surrounded by whitespace
natural :: Parser Int
natural = token nat

-- read a given string that is possibly surrounded by whitespace
symbol :: String -> Parser String
symbol xs = token $ string xs

-- parse an arithemtic expression and evaluate it to an integer
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

-- Ask for an arithemtic expression and parse and evaluate it.            
main :: IO ()
main = do putStr "Enter an arithmetic expression: "
          s <- getLine
          let [(n, _)] = parse expr s
          putStr $ "The expression " ++ show s ++ " evaluates to " ++ show n ++ ".\n"
          
