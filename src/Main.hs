module Main where

import Parser
import Syntax
import IR
import TypeInfer
import Type
import Codegen
import EmitLLVM

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Reader
import System.Console.Haskeline
import System.Environment

import Debug.Trace

import qualified LLVM.General.AST as AST

simpleLambdaLift :: String -> IO ()
simpleLambdaLift line = do
    case parseExpr line of 
        Left err -> print err
        Right expr ->
            case inferExpr empty expr of
                Left err -> print err >> putStrLn "expr with failure:" >> print expr
                Right typ -> print $ buildIRTree expr
                  

-- REMEMBER: escape backslash when writing lambdas
--           in string (in ghci, arg to process)
simpleTypecheck :: String -> IO ()
simpleTypecheck line = do
    case parseExpr line of 
        Left err -> print err
        Right expr -> 
            case inferExpr empty expr of
                Left err -> print err >> putStrLn "expr with failure:" >> print expr
                Right typ -> print expr

procLlvmModule :: AST.Module -> String -> IO (Maybe AST.Module)
procLlvmModule base source = do
    case parseExpr source of
        Left err -> print err >> return Nothing
        Right dimlExpr -> do
            case inferExpr empty dimlExpr of
                Left err -> print err >> return Nothing
                Right typeScheme -> do
                    let irExpr = buildIRTree dimlExpr
                    putStrLn "\nDimlExpr AST:\n"
                    putStrLn $ "    " ++ show dimlExpr 
                    putStrLn "\nDimlIR AST:\n"
                    putStrLn $ "    " ++ show irExpr 
                    putStr "\n"
                    Just <$> codegen base irExpr

-- for single file compilation
processfile :: String -> IO ()
processfile fname = do 
    file <- readFile fname 
    case parseExpr file of 
        Left err -> print err
        Right dimlExpr -> do
            -- just plug in "inferExpr" when done testing type inference
            case constraintsExpr empty dimlExpr of
                Left err -> print err 
                Right (cs,sub,typ,typSch) -> do
                    putStrLn $ "Inferred Type: " ++ show typ
                    putStrLn $ "Constraints: " ++ show cs
                    putStrLn $ "Substitution: " ++ show sub
                    putStrLn $ "Type Scheme: " ++ show typSch
                    let irExpr = buildIRTree dimlExpr
                    putStrLn "\nDimlExpr AST:\n"
                    print dimlExpr 
                    putStrLn "\nDimlIR AST:\n"
                    print irExpr 
                    putStr "\n"
                    compileLlvmModule initModule (buildIRTree dimlExpr) fname 
     
processRepl :: String -> IO ()
processRepl expr = 
    case parseExpr expr of
        Left err -> print err
        Right dimlExpr -> 
            case inferExpr empty dimlExpr of
                Left err -> print err
                Right scheme -> putStrLn $ show dimlExpr ++ " :: " ++ show scheme

initModule :: AST.Module
initModule = emptyModule "dimlProgram"

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
    where loop mod' = do
              minput <- getInputLine "diML> "
              case minput of
                  Nothing -> outputStrLn "Goodbye."
                  Just input -> do 
                        modn <- liftIO $ (processRepl input >> procLlvmModule mod' input)
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
        [fname] -> processfile fname 
