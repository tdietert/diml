module Main where

import Parser
import Syntax
import IR
import TypeInfer
import Type
import Codegen
import EmitLLVM
import JIT

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Reader
import System.Console.Haskeline
import System.Environment

import Debug.Trace

import qualified LLVM.General.AST as AST

-- NOTE:
--
-- 1) SUM TYPES, CASE EXPRS! inl inr exprs


debugLlvmModule :: AST.Module -> String -> IO (Maybe AST.Module)
debugLlvmModule base source = do
    case parseExpr source of
        Left err -> print err >> return Nothing
        Right dimlExpr -> do
            case inferExpr empty dimlExpr of
                Left err -> print err >> return Nothing
                Right typeScheme -> do
                    let irExpr = buildFinalPrgm dimlExpr
                    putStrLn "\nDimlExpr AST:\n"
                    putStrLn $ "    " ++ show dimlExpr
                    putStrLn "\nDimlIR AST:\n"
                    putStrLn $ "    " ++ show irExpr
                    putStr "\n"
                    Just <$> codegen base irExpr

debugProcessFile :: String -> IO ()
debugProcessFile fname = do
    file <- readFile fname
    case parseExpr file of
        Left err -> print err
        Right dimlExpr -> do
            case constraintsExpr empty dimlExpr of
                Left err -> print err
                Right (cs,sub,typ,typSch) -> do
                    putStrLn $ "Inferred Type: " ++ show typ
                    putStrLn $ "Constraints: " ++ show cs
                    putStrLn $ "Substitution: " ++ show sub
                    putStrLn $ "Type Scheme: " ++ show typSch
                    let irExpr = buildFinalPrgm dimlExpr
                    putStrLn "\nDimlExpr AST:\n"
                    print dimlExpr
                    putStrLn "\nDimlIR AST:\n"
                    print irExpr
                    putStr "\n"
                    mod <- linkBuiltins initModule
                    -- CURRENTLY THIS COMPILES AND RUNS THE FILE (CHANGE)
                    codegen mod (buildFinalPrgm dimlExpr)
                    return ()


initModule :: AST.Module
initModule = emptyModule "dimlProgram"

-- for single file compilation
processfile :: String -> IO ()
processfile fname = do
    file <- readFile fname
    case parseExpr file of
        Left err -> print err
        Right dimlExpr -> do
            case inferExpr empty dimlExpr of
                Left err -> print err
                Right scheme -> do
                   mod <- linkBuiltins initModule
                   mod' <- codegen mod (buildFinalPrgm dimlExpr)
                   compileFile mod' fname

processRepl :: AST.Module -> String -> IO (Maybe AST.Module)
processRepl oldMod expr =
    case parseExpr expr of
        Left err -> print err >> return Nothing
        Right dimlExpr ->
            case inferExpr empty dimlExpr of
                Left err -> print err >> return Nothing
                Right scheme -> do
                   putStrLn $ show dimlExpr ++ " :: " ++ show scheme
                   let irExpr = buildFinalPrgm dimlExpr
                   newMod <- codegen oldMod irExpr
                   result <- runJIT newMod
                   case result of
                      Left err -> print err >> return Nothing
                      Right res -> return $ Just res

repl :: IO ()
repl = do
    mod <- linkBuiltins initModule
    runInputT defaultSettings (loop mod)
    where loop mod' = do
             minput <- getInputLine "diML> "
             case minput of
                 Nothing -> outputStrLn "Goodbye."
                 Just input -> do
                     modn <- liftIO $ processRepl mod' input
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
