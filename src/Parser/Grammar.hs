-- | Defines language grammatical constructs
module Parser.Grammar where
    
import Parser.Parser
import Data.Char
import Control.Applicative hiding (some, many)

-- | A kitty-lang expression
data Expr = Wrapped Expr | Variable String | Literal String deriving (Show)

-- | Parses a valid expression
parseExpr :: Parser Expr
parseExpr = parseLiteral <|> parseIdentifier <|> parseWrapped

-- | Parses a valid string or numeric literal
parseLiteral :: Parser Expr
parseLiteral = Literal <$> (number <|> quoted)

-- | Parses a variable identifier 
parseIdentifier :: Parser Expr
parseIdentifier = Variable <$> do
    c <- satisfy isAlpha
    cs <- many (satisfy isAlphaNum)
    return (c:cs)
    
-- | Parses a wrapped expression
parseWrapped :: Parser Expr
parseWrapped = Wrapped <$> (parens $ parseExpr)