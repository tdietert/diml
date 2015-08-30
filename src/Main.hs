module Main where

import Parser
import Syntax
import Eval
import Typecheck

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Reader
import System.Console.Haskeline

-- parses, type checks, and evaluates file
parseFile :: String -> IO ()
parseFile filename = do
    program <- readFile filename
    process program

-- REMEMBER: escape backslash when writing lambdas in string (in ghci, arg to process)
process :: String -> IO ()
process line = do
	-- parse it first!
    case parseExpr line of 
        Left err -> print err
        Right expr -> 
            -- type check it!
            case typeCheck [] expr of
        	    Left err -> print err >> print expr
        	    Right typ -> displayResult result typ
            where result = eval [] expr
                  displayResult :: Value -> Type -> IO ()
                  displayResult e t = putStrLn outStr
                      where outStr = "it = " ++ show e ++ " : " ++ show t

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "diML> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input ->(liftIO $ process input) >> loop