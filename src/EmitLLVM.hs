{-# LANGUAGE OverloadedStrings #-}

module EmitLLVM where

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Analysis
import LLVM.General.Target

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.Target as TM
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as Inst
import qualified LLVM.General.ExecutionEngine as EE

import Data.Word
import Data.Int
import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import System.Directory
import qualified Data.Map as Map

-- JIT compilation
import Foreign.Ptr ( FunPtr, castFunPtr )

import Codegen
import IR

-- turn function arg into llvm arg
toFunArg :: [(Arg,IExpr)] -> [(AST.Type, AST.Name)]
toFunArg = map (\(name,expr) ->
      case expr of
          ITup e1 e2 -> (tuple, AST.Name name)
          otherwise -> (double, AST.Name name)
    )

------------------------
-- Toplevel Codegen
------------------------

stdlib :: LLVM ()
stdlib = do
    define tVoid "printInt" [(T.i64, AST.UnName 0)] [] L.External
    define double "fst" [(tuple, AST.Name "tuple")] [] L.External
    define double "snd" [(tuple, AST.Name "tuple")] [] L.External

codegenTop :: IR.IExpr -> LLVM ()
codegenTop (IR.ITopLevel e1 e2) = codegenTop e1 >> codegenTop e2
codegenTop (IR.IClosure name arg env body) = do
    define double name fnargs bls L.Internal
    where fnargs = (double, AST.Name arg) : toFunArg env -- arg is hard coded as double, might not be so
          bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              forM fnargs $ \(typ,(AST.Name arg)) -> do
                  var <- alloca typ
                  assign arg var
                  store var (local (AST.Name arg))
              cgen body >>= ret
codegenTop (IR.ILet decl body) = do
    define double "main" [] bls L.External
    where bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              case decl of
                  IR.IUnit -> return nullVal
                  _     -> cgen decl
              cgen body >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt a b = do
  test <- fcmp FP.UGT a b
  uitofp double test

eq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
eq a b = do
  eqtest <- fcmp FP.OEQ a b
  uitofp double eqtest

binops :: Map.Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
              ("+", fadd)
            , ("-", fsub)
            , ("*", fmul)
            , ("/", fdiv)
            , ("<", lt)
            , (">", gt)
            , ("==", eq)
          ]

-- code gen for code within blocks, creates sequence of instructions
cgen :: IR.IExpr -> Codegen AST.Operand
cgen (IR.ITrue) = return true
cgen (IR.IFalse) = return false
cgen (IR.IInt n) = return $ int n
cgen (IR.IVar x) = getvar x >>= load
cgen (IR.ITup e1 e2) = do
    e1' <- cgen e1
    e2' <- cgen e2
    tup <- alloca tuple >>= (\t -> load t)
    tup' <- insertElem e1' tup 0
    insertElem e2' tup' 1
cgen (IR.IPrintInt n) = do
    intArg <- cgen n
    argToIntType <- fptoui T.i64 intArg
    call (externf (AST.Name "printInt")) [argToIntType]
    return true
cgen (IR.IApp fun args) = do
    e <- get
    largs <- mapM cgen args
    call (externf (AST.Name fun)) largs
cgen (IR.IEq e1 e2) = do
    a <- cgen e1
    b <- cgen e2
    fcmp FP.OEQ a b
cgen (IR.IIf cond tru fals) = do
    -- construct three new blocks
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    -- eval condition
    condval <- cgen cond
    test <- fcmp FP.ONE false condval
    -- instruction to branch
    cbr test ifthen ifelse
    -- start ifthen block
    setBlock ifthen
    trueBrVal <- cgen tru
    br ifexit
    -- get new if then block since nested
    -- exprs in rec cgen call may set block
    -- llvm is emitting to
    ifthen <- getBlock
    setBlock ifelse
    falsBrVal <- cgen fals
    br ifexit
    ifelse <- getBlock
    setBlock ifexit
    phi double [(trueBrVal, ifthen), (falsBrVal, ifelse)]
cgen (IR.IBinOp op a b) = do
    case Map.lookup op binops of
      Just f  -> do
          ca <- cgen a
          cb <- cgen b
          f ca cb
      Nothing -> error "No such operator"
cgen (IR.IDec name e) = do
    asgn <- cgen e
    case e of
        ITup e1 e2 -> do
            var <- alloca tuple
            assign name var
            store var asgn
        otherwise -> do
            var <- alloca double
            assign name var
            store var asgn
cgen (IR.IInL e) = cgen e
cgen (IR.IInR e) = cgen e
cgen (IR.ICase (IR.IInL e1) [inl,_] [e2,_]) =
    case inl of
        (IR.IInL (IR.IVar x)) -> cgen (IR.IDec x e1) >> cgen e2
        (IR.IInL e) -> if e1 == e then cgen e2 
                       else error $ show e1 ++ " does not match " ++ show e ++ "."
cgen (IR.ICase (IR.IInR e1) [_,inr] [_,e3]) =
    case inr of
        (IR.IInR (IR.IVar x)) -> cgen (IR.IDec x e1) >> cgen e3
        (IR.IInR e) -> if e1 == e then cgen e3 
                       else error $ show e1 ++ " does not match " ++ show e ++ "."  
cgen (IR.ILet decl body) = cgen decl >> cgen body
cgen (IR.IUnit) = return nullVal
cgen (IR.IBuiltin e) =
    case e of
        ITupFst e' -> cgen $ IR.IApp "fst" [e']
        ITupSnd e' -> cgen $ IR.IApp "snd" [e']
cgen e = error $ "'cgen' function in Codegen.hs not defined for " ++ show e

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> IR.IExpr -> IO AST.Module
codegen mod fn = withContext $ \context ->
   liftError $ withModuleFromAST context newast $ \m -> return newast
   where modn = codegenTop fn
         newast = runLLVM mod modn

builtins :: IO File
builtins = do
    projectDir <- getCurrentDirectory
    return . File $ projectDir ++ "/builtins/builtins.ll"

linkBuiltins :: AST.Module -> IO AST.Module
linkBuiltins mod = withContext $ \context -> do
    builtinFuncs <- builtins
    err <- runExceptT $ withModuleFromLLVMAssembly context builtinFuncs moduleAST
    case err of
        Left errMsg -> putStrLn (show errMsg) >> return (emptyModule "dimlProgram")
        Right mod' -> return mod'

makeTargetFilePath :: FilePath -> IO File
makeTargetFilePath filename = do
    dir <- getCurrentDirectory
    return . File $ dir ++ "/" ++ filename ++ ".s"

compileFile :: AST.Module -> String -> IO ()
compileFile mod name = do
    let filename = takeWhile (/= '.') name
    withContext $ \context -> do
        err <- liftM join . runExceptT . withModuleFromAST context mod $ \m -> do
            runExceptT $ withHostTargetMachine $ \tm -> do
                withPassManager defaultCuratedPassSetSpec $ \pm -> do
                    filepath <- makeTargetFilePath filename
                    runExceptT $ verify m
                    runPassManager pm m
                    runExceptT $ writeTargetAssemblyToFile tm filepath m
        case err of
            Left s -> putStrLn $ show s
            Right _ -> return ()
