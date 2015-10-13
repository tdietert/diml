{-# LANGUAGE OverloadedStrings #-}

module EmitLLVM where

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Analysis

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Linkage as L 
import qualified LLVM.General.Target as TM
import qualified LLVM.General.AST.Type as T

import Data.Word
import Data.Int
import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import System.Directory
import qualified Data.Map as Map

import Codegen
import IR

------------------------
-- Constant Operands
------------------------

false :: AST.Operand
false = cons $ Const.Float (F.Double 0)

true :: AST.Operand
true = cons $ Const.Float (F.Double 1)

int :: Integer -> AST.Operand
int = cons . Const.Float . F.Double . fromIntegral

tup :: AST.Operand -> AST.Operand -> AST.Operand
tup (AST.ConstantOperand a) (AST.ConstantOperand b) = cons . Const.Vector $ [a,b]

one :: AST.Operand 
one = cons $ Const.Int (fromIntegral 1) 32

toFunArg :: [(Arg,IExpr)] -> [(AST.Type, AST.Name)]
toFunArg = map (\(name,expr) -> 
      case expr of 
          ITup e1 e2 -> (tuple, AST.Name name)
          otherwise -> (double, AST.Name name)
    ) 

------------------------
-- Toplevel Codegen
------------------------

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
    
codegenTop (IR.ILet decls body) = do
    define voidType "printInt" [(T.i64, AST.UnName 0)] [] L.External
    define double "main" [] bls L.External
    where bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry 
              forM decls (\decl -> cgen decl)
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
  return $ tup e1' e2'
cgen (IR.IPrintInt n) = do
  intArg <- cgen n
  argToIntType <- fptoui T.i64 intArg
  call (externf (AST.Name "printInt")) [argToIntType]
  return true
-- cgen (IR.Tuple x y) = 
cgen (IR.IApp fun args) = do 
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
cgen (IR.ILet decls body) = do
    mapM cgen decls
    cgen body
cgen e = error $ "Failed on lookup " ++ show e

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> IR.IExpr -> IO AST.Module
codegen mod fn = withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return newast
  where
    modn = codegenTop fn 
    newast = runLLVM mod modn

-----------------------------------
--  Compile to .ll file 
-----------------------------------

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec

compileLlvmModule :: AST.Module -> IExpr -> String -> IO ()
compileLlvmModule base irExpr source = do
    mod <- codegen base irExpr
    compile mod source

compile :: AST.Module -> String -> IO ()
compile mod name =  
    withContext $ \context -> do
        projectDir <- getCurrentDirectory
        err <- runExceptT $ withModuleFromLLVMAssembly context (File $ projectDir ++ "/builtins/builtins.ll") $ \builtins -> do
            err <- liftM join . runExceptT . withModuleFromAST context mod $ \m -> do
                      withPassManager passes $ \pm -> do
                          err <- runExceptT $ verify m
                          case err of
                              Left s -> putStr s
                              Right _ -> return ()
                          runExceptT $ linkModules False m builtins
                          runPassManager pm m
                          let filename = takeWhile (/= '.') name
                          runExceptT $ writeLLVMAssemblyToFile (File $ filename ++ ".ll") m
                          moduleLLVMAssembly m
                          liftM join $ runExceptT $ TM.withHostTargetMachine $ \target -> do
                              runExceptT $ writeTargetAssemblyToFile target (File $ filename ++ ".s") m
            case err of
                Left s -> putStr s
                Right _ -> return ()
        case err of
            Left s -> putStrLn $ show s
            Right _ -> return ()