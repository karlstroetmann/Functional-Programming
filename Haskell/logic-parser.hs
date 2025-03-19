module PropositionalParser where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Abstract Syntax Tree for Propositional Logic
data Prop
    = Var String           -- Variables
    | Not Prop             -- Negation
    | And Prop Prop        -- Conjunction
    | Or Prop Prop         -- Disjunction
    | Imply Prop Prop      -- Implication
    | Equiv Prop Prop      -- Equivalence
    deriving (Show, Eq)

-- Parser for a variable (identifier)
parseVar :: Parser Prop
parseVar = Var <$> many1 letter

-- Parser for negation
parseNot :: Parser Prop
parseNot = do
    _ <- char '!'
    Not <$> parseFactor

-- Parser for parentheses
parseParens :: Parser Prop
parseParens = do
    _ <- char '('
    expr <- parseExpr
    _ <- char ')'
    return expr

-- Parser for a factor (variable, negation, or parentheses)
parseFactor :: Parser Prop
parseFactor = parseParens <|> parseNot <|> parseVar

-- Operator parsers
parseAnd, parseOr, parseImply, parseEquiv :: Parser (Prop -> Prop -> Prop)
parseAnd   = And   <$ string "&"
parseOr    = Or    <$ string "|"
parseImply = Imply <$ string "->"
parseEquiv = Equiv <$ string "<->"

-- Helper to parse left-associative binary operators
parseBinary :: Parser a -> Parser (a -> a -> a) -> Parser a
parseBinary subParser opParser = chainl1 subParser opParser

-- Expression parsers
parseTerm :: Parser Prop
parseTerm = parseBinary parseFactor parseAnd

parseSimpleExpr :: Parser Prop
parseSimpleExpr = parseBinary parseTerm parseOr

parseExpr :: Parser Prop
parseExpr = parseBinary parseSimpleExpr (parseImply <|> parseEquiv)

-- Top-level parser
parseProp :: String -> Either ParseError Prop
parseProp = parse (parseExpr <* eof) ""
