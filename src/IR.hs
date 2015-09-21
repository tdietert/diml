{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR where

import Syntax
import Data.Maybe

import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map

type SymbolTable = [(Name,IExpr)]
type Arg = String
type Names = Map.Map String Int

data EnvState  
  = EnvState {
    symtab :: SymbolTable
  , names :: Names
  } deriving (Show)

-- for newtype, field type isomorphic to type of Env a
newtype Env a = Env { runEnv :: State EnvState a }
    deriving (Functor, Applicative, Monad, MonadState EnvState)

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of  
    Nothing -> (nm,  Map.insert nm 1 ns)  -- if name exists, add name to Names
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns) -- else add name "name#vars"

lookupVar :: Name -> Env IExpr
lookupVar var = do
    env <- gets symtab
    case lookup var env of
        Just x -> return x
        Nothing -> error (show var ++ " not in symbol table: " ++ show env)

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
           | IClosure Name Arg [Arg] IExpr -- functions and lambdas
           | ITopLevel IExpr IExpr
           | Empty
           deriving (Eq,Show)

lambdaLift :: Env IExpr -> DimlExpr -> Env IExpr
lambdaLift env expr = case expr of
    DTrue -> return $ IBool True
    DFalse -> return $ IBool False
    DInt n -> return $ IInt n
    Var x -> do
        nms <- gets names
        let (newName,supply) = uniqueName x nms
        modify $ \s ->  s { names = supply }
        return $ IVar newName

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
        nms <- gets names
        let (lamName, supply) = uniqueName "lambda" nms
        modify $ \s -> s { names = supply }
        ctxt <- gets symtab
        lBody <- lambdaLift env body
        let lCtxt = map (\(name,val) -> name) ctxt
            lClos = IClosure lamName arg lCtxt lBody
        return lClos

    Fun fName arg argType retType body -> do
        fBody <- lambdaLift env body
        ctxt <- gets symtab
        nms <- gets names
        let (newName, supply) = uniqueName fName nms
            fCtxt = map (\(name,val) -> name) ctxt
            fClos = IClosure fName arg fCtxt fBody
        modify $ \s -> s { symtab = (fName,fClos):ctxt, names = supply }
        return $ IDec newName (IVar newName) -- return declaration f = f

    Apply fun@(Var fname) arg -> do
        lArg <- lambdaLift env arg
        return $ IApp (IVar fname) lArg

    e -> error $ "Your eval func is broken: "++show e++" slipped through..."


emptyLLTree :: EnvState
emptyLLTree = EnvState [] Map.empty

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