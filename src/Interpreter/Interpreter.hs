module Interpreter.Interpreter where

import Parser.Parser
import Parser.Grammar
import Interpreter.Stack

-- | Evalutes a single line of the interpreter
evalLine :: String -> Stack -> Either String Stack
evalLine str stack = case runParser parseStatement str of
                Left err -> Left err
                Right res -> evalStatement res stack

-- | Evaluates the given expression.
evalExpr :: Expr -> Stack -> Either String String
evalExpr (Wrapped expr) stack = evalExpr expr stack
evalExpr (Variable name) stack =
    case lookup name (vars stack) of
        Nothing -> Left $ "Variable " ++ name ++ " not found."
        Just val -> Right val
evalExpr (Literal val) _ = Right val

-- | Evaluates the given statement.
evalStatement :: Statement -> Stack -> Either String Stack
evalStatement (Assignment var expr) stack =
    case evalExpr expr stack of
        Right res -> Right $ setVar var res stack
        Left err -> Left err