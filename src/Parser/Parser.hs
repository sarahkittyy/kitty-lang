{-# LANGUAGE InstanceSigs #-}

-- | Defines basic parser logic
module Parser.Parser where

import Control.Applicative hiding (some, many)
import Data.Char
import Text.Read (readMaybe)

-- | Takes in a string and either returns an error or the parsed tokens
newtype Parser a = Parser { parse :: String -> Either String (a, String) }

-- | Runs a parser over an input string, simplifying the output.
runParser :: Parser a -> String -> Either String a
runParser parser input = case parse parser input of
                            Left err -> Left err
                            Right (a, rest) -> Right a

-- | Applies a function to the matched result of the parser
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fn parser = Parser $ \input ->
        case parse parser input of
            Right (a, rest) -> Right (fn a, rest)
            Left err -> Left err
    
-- | Applies a function parser's function to the output of another parser
instance Applicative Parser where
    pure :: n -> Parser n
    pure c = Parser $ \input -> Right (c, input)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    fp <*> p = Parser $ \input -> case parse fp input of
                                    Left err -> Left err
                                    Right (fn, rest) -> parse (fn <$> p) rest
    
-- | Sends the output of the first parser into a function that returns a new parser
instance Monad Parser where
    return :: n -> Parser n
    return = pure
    
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= fn = Parser $ \input -> case parse p input of
                                    Left err -> Left err
                                    Right (out, rest) -> parse (fn out) rest
                                
-- | A parser which automatically fails, returning an error
failure :: String -> Parser a
failure err = Parser $ \_ -> Left err

-- | Allow an alternative route if one parser fails
instance Alternative Parser where
    empty :: Parser a
    empty = failure "Parser error."
    
    (<|>) :: Parser a -> Parser a -> Parser a
    main <|> alt = Parser $ \input ->
        case parse main input of
            Left err -> parse alt input
            res -> res

-- | Returns a single char
item :: Parser Char
item = Parser $ \input ->
    if length input == 0
        then Left "No input left for item to consume chars"
        else Right (head input, tail input)
        
-- | Returns a single char only if it matches a given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item >>= \ch -> if pred ch
                                    then pure ch
                                    else failure "Could not satisfy given predicate."
                                    
-- | Matches a given char
char :: Char -> Parser Char
char = satisfy . (==)

-- | Matches an exact string
string :: String -> Parser String
string "" = pure ""
string (c:cs) = char c >> string cs >> return (c:cs)
                                    
-- | One or more of a parser
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> rest
    where rest = some p <|> pure []
    
-- | Zero or more
many :: Parser a -> Parser [a]
many p = some p <|> pure []

-- | Exactly n of a given parser
nOf :: Int -> Parser a -> Parser [a]
nOf n p = if n == 0
            then pure []
            else (:) <$> p <*> nOf (n - 1) p

-- | Zero or one of a given parser
maybeOne :: Parser a -> Parser [a]
maybeOne p = nOf 1 p <|> pure []

-- | A character that matches any of the given characters
oneOf :: [Char] -> Parser Char
oneOf [] = failure "Could not match any characters given in parser oneOf"
oneOf (x:xs) = satisfy (==x) <|> oneOf xs

-- | Matches a parser wrapped in parenthesis
parens :: Parser a -> Parser a
parens p = char '(' >> p >>= \res -> (char ')' >> return res)

-- | Matches a single digit
digit :: Parser Char
digit = satisfy isDigit

-- | Matches a natural number
natural :: Parser String
natural = some digit

-- | Matches an integer
integer :: Parser String
integer = do
    sign <- maybeOne (char '-')
    num <- natural
    
    return $ sign ++ num
    
-- | Matches a floating point number
number :: Parser String
number = do
    sign <- maybeOne (char '-')
    whole <- (natural <|> pure "0")
    fractional <- maybeOne $ do
        char '.'
        n <- natural
        return $ "." ++ n
        
    return $ sign ++ whole ++ if null fractional
                                then ""
                                else head fractional