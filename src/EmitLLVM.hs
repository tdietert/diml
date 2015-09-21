{-# LANGUAGE OverloadedStrings #-}

module EmitLLVM where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map

import Codegen
import qualified Syntax as Syn

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

-- top level code gen
codegenTop :: Syn.DimlExpr -> LLVM ()
codegenTop (Syn.Let decls body) = do
    define double "main" [] bls
    where bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry 
              forM decls (\decl -> cgen decl)
              cgen body >>= ret

codegenTop (Syn.Fun name arg _ _ body) = do
    define double name fnargs bls
    where fnargs = toSig [arg]
          bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              forM [arg] $ \a -> do    -- supports multiple args, remove for loop for 1 arg
                  var <- alloca double
                  assign a var
                  store var (local (AST.Name a))
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

binops :: Map.Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
              ("+", fadd)
            , ("-", fsub)
            , ("*", fmul)
            , ("/", fdiv)
            , ("<", lt)
            , (">", gt)
          ]

-- code gen for code within blocks, creates sequence of instructions
cgen :: Syn.DimlExpr -> Codegen AST.Operand
cgen (Syn.DTrue) = return . cons $ Const.Float (F.Double 1)
cgen (Syn.DFalse) = return . cons $ Const.Float (F.Double 0)
cgen (Syn.DInt n) = return . cons $ Const.Float (F.Double $ fromIntegral n)
cgen (Syn.Var x) = getvar x >>= load
-- cgen (Syn.Tuple x y) = 
cgen (Syn.Apply (Syn.Var fn) arg) = do 
    larg <- cgen arg
    call (externf (AST.Name fn)) [larg]
cgen (Syn.Eq e1 e2) = do 
    a <- cgen e1
    b <- cgen e2
    fcmp FP.OEQ a b 
cgen (Syn.If cond tru fals) = do
    -- construct three new blocks 
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    -- eval condition
    condval <- cgen cond
    -- instruction to branch
    cbr condval ifthen ifelse
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
cgen (Syn.BinOp op a b) = do
    case Map.lookup op binops of
      Just f  -> do
          ca <- cgen a
          cb <- cgen b
          f ca cb
      Nothing -> error "No such operator"
cgen (Syn.Decl name e) = do
    var <- alloca double
    asgn <- cgen e
    assign name var
    store var asgn
cgen (Syn.Let decls body) = do
    mapM cgen decls
    cgen body

cgen _ = error $ "Failed on lookup "

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- can be modified to take list of Exprs
codegen :: AST.Module -> Syn.DimlExpr -> IO AST.Module
codegen mod fn = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = codegenTop fn
    newast  = runLLVM mod modn