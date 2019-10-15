module Interpreter.Stack where
    
data Stack = Stack { vars :: [(String, String)]
                   }
   
-- | Defines an empty stack.
emptyStack :: Stack
emptyStack = Stack []
                   
-- | Sets a variable in a stack to a value
setVar :: String -> String -> Stack -> Stack
setVar name value stack =
    case lookup name (vars stack) of
        Nothing -> Stack ((name, value):(vars stack))
        (Just v) -> Stack
            [(iden, new_val) | (iden, a_value) <- (vars stack), let new_val = if iden == name
                                                                                then value
                                                                                else a_value]