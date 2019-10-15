module Main where
    
import System.Environment
import System.IO
import Parser.Parser
import Parser.Grammar
import Interpreter.Interpreter
import Interpreter.Stack

main :: IO ()
main = interpret emptyStack
    
-- | Run a persistent interpreter
interpret :: Stack -> IO ()
interpret stack = do
    statement <- prompt
    let newStack = case evalLine statement stack of
                    Left err -> error $ err
                    Right s -> s
    putStrLn $ show newStack
    interpret newStack
    
-- | Main prompt for the interactive interpreter 
prompt :: IO String
prompt = do
    putStr $ ">>> "
    hFlush stdout
    getLine
