module Main where

import Parser
import Syntax
import Eval
import Typecheck
import Codegen
import EmitLLVM
--import Emit

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Reader
import System.Console.Haskeline
import System.Environment

import qualified LLVM.General.AST as AST

-- parses, type checks, and evaluates file
evalProgram :: String -> IO ()
evalProgram filename = do
    program <- readFile filename
    evalInHaskell program

-- REMEMBER: escape backslash when writing lambdas
-- in string (in ghci, arg to process)
evalInHaskell :: String -> IO ()
evalInHaskell line = do
    -- parse it first!
    case parseExpr line of 
        Left err -> print err
        Right expr -> 
            -- type check it!
            case typeCheck [] expr of
                Left err -> print err >> putStrLn "expr with failure:" >> print expr
                Right typ -> displayResult result typ
            where result = eval [] expr
                  displayResult :: Value -> Type -> IO ()
                  displayResult e t = putStrLn outStr
                      where outStr = "it = " ++ show e ++ " : " ++ show t

procLlvmModule :: AST.Module -> String -> IO (Maybe AST.Module)
procLlvmModule base source = do
    case parseExpr source of
        Left err -> print err >> return Nothing
        Right ex -> print ex >> codegen base ex >>= (\x -> return $ Just x)

processfile :: String -> IO (Maybe AST.Module)
processfile fname = readFile fname >>= procLlvmModule initModule

initModule :: AST.Module
initModule = emptyModule "dimlProgram"

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
    where loop mod' = do
              minput <- getInputLine "diML> "
              case minput of
                  Nothing -> outputStrLn "Goodbye."
                  Just input -> do
                        modn <- liftIO $ procLlvmModule mod' input
                        case modn of
                            Just modn -> loop modn
                            Nothing -> loop mod'

-- either opens repl or compiles file
-- depending on if file name provided to diml
main :: IO ()
main = do
    args <- getArgs
    case args of 
        []      -> repl
        [fname] -> processfile fname >> return ()