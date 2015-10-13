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
  , nameMap :: [(Name,Name)]
  } deriving (Show)

-- for newtype, field type isomorphic to type of Env a
newtype Env a = Env { runEnv :: State EnvState a }
    deriving (Functor, Applicative, Monad, MonadState EnvState)


------------
-- Utility
------------
uniqueName :: String -> Env String
uniqueName name = do 
  (EnvState _ nms nmMap) <- get
  case Map.lookup name nms of  
    Nothing -> do
        modify $ \s -> s { names = Map.insert name 1 nms }  -- if name exists, add name to Names
        return name
    Just ix -> do
        modify $ \s -> s { names = Map.insert name (ix+1) nms } -- else add name "name#vars"
        return $ name ++ show ix

lookupVarName :: Name -> Env Name
lookupVarName var = do
    nmMap <- gets nameMap
    case lookup var nmMap of
        Just x -> return x
        Nothing -> return var

--------------------------------------------------
-- Lambda Lifting (probably easier than I make it)
--     defines another IR AST with lambda Lifts
--------------------------------------------------
data IExpr = IInt Integer
           | ITrue
           | IFalse
           | ITupl (IExpr,IExpr)
           | IVar Name
           | IBinOp Name IExpr IExpr
           | IEq IExpr IExpr
           | IIf IExpr IExpr IExpr
           | IApp Name [IExpr]
           | IDec Name IExpr
           | ILet [IExpr] IExpr
           | ITup IExpr IExpr
           | IClosure Name Arg SymbolTable IExpr -- functions and lambdas
           | ITopLevel IExpr IExpr     -- top level closures followed by first let expr
           | IPrintInt IExpr
           | Empty
           deriving (Eq,Show)

------------------------------------------
-- IR AST : Final Tree Construction
------------------------------------------
emptyLLTree :: EnvState
emptyLLTree = EnvState [] Map.empty []

runLLTree :: Env IExpr -> (IExpr, EnvState)
runLLTree m =  runState (runEnv m) emptyLLTree

getClosures :: EnvState -> [IExpr]
getClosures es = foldl collectClosures [] env
    where env = symtab es 
          collectClosures clsrs (name,f@(IClosure _ _ _ _)) = f:clsrs
          collectClosures clsrs _ = clsrs 

buildIRTree :: DimlExpr -> IExpr
buildIRTree dimlExpr =  
    case getClosures closures of 
        -- add toplevel let env to end of function(closure) declarations
        (c:cs) -> foldl ITopLevel c $ cs++[prgm]
        []     -> prgm
    where (prgm, closures) = runLLTree $ lambdaLift (return Empty) dimlExpr

-------------------------------------------

lambdaLift :: Env IExpr -> DimlExpr -> Env IExpr
lambdaLift env expr = case expr of
    Lit x -> case x of 
                 DTrue -> return $ ITrue
                 DFalse -> return $ IFalse
                 DInt n -> return $ IInt n
    Var x -> do
        newName <- lookupVarName x
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
        nmMap <- gets nameMap
        ctxt <- gets symtab 
        case e' of
          (IVar x) -> return Empty -- do
          --     modify $ \s -> s { nameMap = (name,x) : nmMap }
          --     return $ IDec x (IVar x)
          otherwise -> do
              newName <- uniqueName name
              modify $ \s -> s {
                    symtab = (newName,e') :  ctxt
                  , nameMap = (name,newName) : nmMap 
                }
              return $ IDec newName e' 

    Let decls body -> do
        newDecls <- foldM collect [] decls
        newBody <- lambdaLift (return $ last newDecls) body
        return $ ILet newDecls newBody
        -- collects list of declarations inside Env monad
        -- each new decl relies on the previous decls environment (e.g. "last decs")
        where collect :: [IExpr] -> DimlExpr -> Env [IExpr]
              -- if declaration is lambda, lift out of decl, 
              -- give declared var alias to lambda name
              collect decs (Decl name lam@(Lam arg typ body)) = do
                  (IVar lamName) <- lambdaLift (return $ last decs) lam
                  nmMap <- gets nameMap
                  modify $ \s -> s { nameMap = (name,lamName):nmMap }
                  return decs
              -- if declaration is function, lift out of decl
              collect decs fun@(Fun name _ _ _ _) = do
                  lambdaLift (return $ last decs) fun 
                  return decs   
              collect decs decl = do
                  currDec <- lambdaLift (return $ last decs) decl
                  return $ decs ++ [currDec]
      

    Lam arg typ body -> do
        nms <- gets names
        lamName <- uniqueName "lambda" 
        ctxt <- gets symtab
        lBody <- lambdaLift env body
        let lClos = IClosure lamName arg ctxt lBody
        modify $ \s -> s { symtab = (lamName,lClos) : ctxt }
        return (IVar lamName)

    Fun fName arg argType retType body -> do
        (EnvState ctxt nms nmMap) <- get 
        argName <- uniqueName arg 
        newFName <- uniqueName fName

        -- tranform fun body with temporary closure
        let fCtxt = filter cleanClosures ctxt
            tmpClos = IClosure newFName argName fCtxt Empty
        modify $ \s -> s { 
            symtab = (newFName, tmpClos) : ctxt
          , nameMap = (fName,newFName) : (arg,argName) : nmMap
        }
        fBody <- lambdaLift env body

        -- fix closure in symboltable with fully transformed closure
        sytb <- gets symtab
        let fClos = IClosure newFName argName fCtxt fBody
            (top,(s:syms)) = span (/= (newFName,tmpClos)) sytb
        modify $ \s -> s { 
            symtab = top ++ [(newFName,fClos)] ++ syms
        }

        return $ IVar newFName

    Apply f arg -> do
        (IVar fun) <- lambdaLift env f
        sytb <- gets symtab
        case lookup fun sytb of
            Just (IClosure _ _ args _) -> do
                lArg <- lambdaLift env arg
                sytb <- gets symtab
                -- makes sure necessary args are passed to func
                return . IApp fun $ lArg : (map (\(name,val) -> IVar name) args) 
            Nothing -> error $ "looking up " ++ show fun ++ " in " ++ show sytb

    PrintInt e -> do
      eIR <- lambdaLift env e
      return $ IPrintInt eIR

    where cleanClosures (name,expr) =
              case expr of
                (IClosure _ _ _ _ ) -> False
                anythingelse -> True

