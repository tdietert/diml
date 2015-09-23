module Main where

import Parser
import Syntax
import IR
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
    simpleTypecheck program

simpleLambdaLift :: String -> IO ()
simpleLambdaLift line = do
    case parseExpr line of 
        Left err -> print err
        Right expr ->
            case typeCheck [] expr of
                Left err -> print err >> putStrLn "expr with failure:" >> print expr
                Right typ -> print $ buildIRTree expr
                  

-- REMEMBER: escape backslash when writing lambdas
-- in string (in ghci, arg to process)
simpleTypecheck :: String -> IO ()
simpleTypecheck line = do
    case parseExpr line of 
        Left err -> print err
        Right expr -> 
            case typeCheck [] expr of
                Left err -> print err >> putStrLn "expr with failure:" >> print expr
                Right typ -> print expr

-- add type checking
procLlvmModule :: AST.Module -> String -> IO (Maybe AST.Module)
procLlvmModule base source = do
    case parseExpr source of
        Left err -> print err >> return Nothing
        Right dimlExpr -> let irExpr = buildIRTree dimlExpr
                    in do 
                      print dimlExpr 
                      print irExpr 
                      Just <$> codegen base irExpr
        -- Note:
        --    need to get type checking to work with context
        --    from AST.Module, right now the typing context is
        --    simply line by line, but we need the repl to carry
        --    a context around with it, as AST.Module is already
        
            --case typeCheck [] ex of
            --    Left err -> print err >> return Nothing
            --    Right typ -> do
            --        print ex                        -- prints resulting expr
            --        displayResult (eval [] ex) typ  -- prints interpreted expr
            --        Just <$> codegen base ex        -- returns llvm module AST

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