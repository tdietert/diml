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
           | ILet IExpr IExpr
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

-- lambda lifting ignores type annotations because 
-- type inference is run before lambda lifting
lambdaLift :: Env IExpr -> DimlExpr -> Env IExpr
lambdaLift env expr = case expr of
    Lit x -> case x of 
                 DTrue -> return $ ITrue
                 DFalse -> return $ IFalse
                 DInt n -> return $ IInt n
    Var x -> do
        newName <- lookupVarName x
        return $ IVar newName

    Tuple e1 e2 _-> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        return $ ITup e1' e2'

    BinOp s e1 e2 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        return $ IBinOp s e1' e2'

    If e1 e2 e3 -> do
        e1' <- lambdaLift env e1
        e2' <- lambdaLift env e2
        e3' <- lambdaLift env e3
        return $ IIf e1' e2' e3'

    Decl name e -> do
        e' <- lambdaLift env e
        nmMap <- gets nameMap
        ctxt <- gets symtab 
        newName <- uniqueName name
        case e' of
            lam@(IClosure lamName _ _ _) -> do
                modify $ \s -> s {
                    nameMap = (name,lamName) : nmMap
                }
                return $ lam
            otherwise -> do
                modify $ \s -> s {
                   symtab = (newName,e') :  ctxt
                 , nameMap = (name,newName) : nmMap 
                }
                return $ IDec newName e' 

    Let decl body -> do
        decl' <- lambdaLift env decl
        body' <- lambdaLift (return decl') body
        case decl' of
            (IClosure name _ _ _) -> do 
                -- FIX:
                --    remove "placeHolder" var. 
                placeHold <- uniqueName "lamPlaceHold"
                return $ ILet Empty body'
            otherwise -> return $ ILet decl' body'      

    Lam arg _ body -> do
        nms <- gets names
        lamName <- uniqueName "lambda" 
        ctxt <- gets symtab
        lBody <- lambdaLift env body
        let lClos = IClosure lamName arg ctxt lBody
        modify $ \s -> s { symtab = (lamName,lClos) : ctxt }
        return lClos

    Fun fName arg _ _ body -> do
        (EnvState ctxt nms nmMap) <- get 
        newArgName <- uniqueName arg 
        newFName <- uniqueName fName
        -- all closures are top level, don't need to keep in env
        let fCtxt = filter cleanClosures ctxt
            tmpClos = IClosure newFName newArgName fCtxt Empty
        -- add func and arg to nameMap for lexical scoping
        modify $ \s -> s { 
            symtab = (newFName, tmpClos) : ctxt
          , nameMap = (fName,newFName) : (arg,newArgName) : nmMap
        }
        fBody <- lambdaLift env body
        -- fix closure in symboltable with fully transformed closure
        sytb <- gets symtab
        let fClos = IClosure newFName newArgName fCtxt fBody
            (top,(s:syms)) = span (/= (newFName,tmpClos)) sytb
        -- since Functions are closures, we must return the state to
        -- a point without all locally scoped vars that were added during
        -- lambda lifting the body of the function (e.g. nested let expr)
        modify $ \s -> s { 
            symtab = top ++ (newFName,fClos) : syms,
            nameMap = (fName,newFName) : nmMap
        }
      
        return fClos

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

    Parens e _ -> lambdaLift env e
   
    PrintInt e -> do
      e' <- lambdaLift env e
      return $ IPrintInt e'

    where cleanClosures :: (Name,IR.IExpr) -> Bool
          cleanClosures (name,expr) =
              case expr of
                (IClosure _ _ _ _ ) -> False
                anythingelse -> True

          removeVarFromState :: Name -> Env ()
          removeVarFromState name = do
              nmMap <- gets nameMap
              let (names,(var:rest)) = span (\(v,vnm) -> v /= name) nmMap
              modify $ \s -> s { nameMap = names ++ rest }
