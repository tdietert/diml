{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR where

import Syntax
import Data.Maybe

import Control.Monad.State
import Control.Applicative

type SymbolTable = [(Name,IExpr)]

data EnvState  
  = EnvState {
    symtab :: SymbolTable
  , names :: Int
  } deriving (Show)

-- for newtype, field type isomorphic to type of Env a
newtype Env a = Env { runEnv :: State EnvState a }
    deriving (Functor, Applicative, Monad, MonadState EnvState)

nextName :: Env Int
nextName = do
    i <- gets names
    modify $ \s -> s { names = 1 + i }
    return $ i + 1

-- defines another IR AST with lambda Lifts
data IExpr = IInt Integer
           | IBool Bool
           | ITupl (IExpr,IExpr)
           | IVar Name
           | IBinOp Name IExpr IExpr
           | IEq IExpr IExpr
           | IIf IExpr IExpr IExpr
           | IApp IExpr IExpr
           | IDec Name IExpr
           | ILet [IExpr] IExpr
           | ITup IExpr IExpr
           | IClosure Name Name SymbolTable IExpr -- functions and lambdas
           | ITopLevel IExpr IExpr
           | Empty
           deriving (Eq,Show)

lambdaLift :: Env IExpr -> DimlExpr -> Env IExpr
lambdaLift env expr = case expr of
    DTrue -> return $ IBool True
    DFalse -> return $ IBool False
    DInt n -> return $ IInt n
    Var x -> return $ IVar x

    Tuple e1 e2 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        return $ ITup e1' e2'

    BinOp s e1 e2 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        return $ IBinOp s e1' e2'

    Eq e1 e2 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        return $ IEq e1' e2'

    If e1 e2 e3 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        e3' <- lambdaLift env e3
        return $ IIf e1' e2' e3'

    Decl name e -> do
        e' <- lambdaLift env e
        case e' of 
          IDec _ decVal -> return $ IDec name decVal
          otherwise -> do
              env' <- gets symtab 
              modify $ \e -> e { symtab = (name,e'):env' }
              return $ IDec name e' 

    -- very ugly, try to change (need to figure out how to handle function decls
    -- inside let exprs. Want let exprs to introduce functions, but also push to top level)
    Let decls body -> do
        let llDeclRes = foldl collect [] decls
        newDecls <- sequence llDeclRes 
        newBody <- lambdaLift (last llDeclRes) body
        return $ ILet newDecls newBody
        -- collects list of declarations inside Env monad
        -- each new decl relies on the previous decls environment (e.g. "last decs")
        where collect :: [Env IExpr] -> DimlExpr -> [Env IExpr]
              collect [] decl@(Decl _ _) = [lambdaLift env decl]
              collect [] fun@(Fun name _ _ _ _) = [lambdaLift env fun]
              collect decs decl@(Decl _ _) = decs ++ [lambdaLift (last decs) decl]
              collect decs fun@(Fun name _ _ _ _) = decs ++ [lambdaLift (last decs) fun]          

    Lam arg typ body -> do
        num <- nextName
        env' <- gets symtab
        lBody <- lambdaLift env body
        let lName = "lambda" ++ show num
            lClos = IClosure lName arg env' lBody
        modify $ \s -> s { symtab = (lName,lClos):env' }
        return $ IVar lName

    Fun fName argName argType retType body -> do
        fBody <- lambdaLift env body
        fEnv <- gets symtab
        let fClos = IClosure fName argName fEnv fBody
        modify $ \s -> s { symtab = (fName,fClos):fEnv }
        return $ IDec fName (IVar fName) -- return declaration f = f

    Apply fun@(Var fname) arg -> do
        lArg <- lambdaLift env arg
        return $ IApp (IVar fname) lArg

    e -> error $ "Your eval func is broken: "++show e++" slipped through..."


emptyLLTree :: EnvState
emptyLLTree = EnvState [] 0

runLLTree :: Env IExpr -> (IExpr, EnvState)
runLLTree m =  runState (runEnv m) emptyLLTree

getClosures :: EnvState -> [IExpr]
getClosures es = foldr (\(name,val) acc-> 
      case val of
        iclos@(IClosure _ _ _ _) -> val:acc 
        _ -> acc
      ) [] env
    where env = symtab es 

buildIRTree :: DimlExpr -> IExpr
buildIRTree dimlExpr = 
    case topLevels of 
      (t:ts) -> ITopLevel (foldl ITopLevel t ts) iExpr
      []     -> iExpr
    where (iExpr, closures) = runLLTree $ lambdaLift (return Empty) dimlExpr
          topLevels = getClosures closures