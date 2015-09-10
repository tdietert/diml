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
import qualified Data.Map as Map

import Codegen
import qualified Syntax as Syn

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: Syn.DimlExpr -> LLVM ()
codegenTop (Syn.Fun name arg _ _ body) = do
    define double name fnargs bls
    where fnargs = toSig [arg]
          bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry
              forM [arg] $ \a -> do
                  var <- alloca double
                  store var (local (AST.Name a))
                  assign a var
              cgen body >>= ret
codegenTop (Syn.Let decls body) = do
    define double "main" [] bls  
    where bls = createBlocks . execCodegen $ do
              entry <- addBlock entryBlockName
              setBlock entry 
              forM decls $ \(Syn.Decl name e) -> do
                  var <- alloca double
                  asgn <- cgen e
                  store var asgn
                  assign name var
              cgen body >>= ret
--codegenTop (Syn.Extern name args) = do
--  external double name fnargs
--  where fnargs = toSig args

--codegenTop exp = do
--  define double "main" [] blks
--  where
--    blks = createBlocks $ execCodegen $ do
--      entry <- addBlock entryBlockName
--      setBlock entry
--      cgen exp >>= ret

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

cgen :: Syn.DimlExpr -> Codegen AST.Operand
cgen (Syn.Var x) = getvar x >>= load
cgen (Syn.DInt n) = return $ cons $ Const.Float (F.Double $ fromIntegral n)
cgen (Syn.Apply (Syn.Var fn) arg) = do 
    larg <- cgen arg
    call (externf (AST.Name fn)) [larg]
cgen (Syn.BinOp op a b) = do
    let binops = Map.fromList [
              ("+", fadd)
            , ("-", fsub)
            , ("*", fmul)
            , ("/", fdiv)
            , ("<", lt)
            , (">", gt)
          ]
    case Map.lookup op binops of
      Just f  -> do
          ca <- cgen a
          cb <- cgen b
          f ca cb
      Nothing -> error "No such operator"
cgen _ = error $ "Failed on lookup "
--cgen (Syn.Call fn args) = do
--  largs <- mapM cgen args
--  call (externf (AST.Name fn)) largs
-- this could work for let exprs
--cgen (Syn.BinaryOp "=" (Syn.Var var) val) = do
--  a <- getvar var
--  cval <- cgen val
--  store a cval
--  return cval

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