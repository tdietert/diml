module Typecheck where
--------------------------------
-- Typechecker for minimal diML
--------------------------------
-- 
--         Typechecking using an ErrorT + Reader Monad transformer stack(?).
--      All this means is that ErrorT's base monad is a Reader Monad instead of the Identity Monad
--      This way we can encapsulate ill typed errors in programs while carrying around a mutable typing environment (context)
--
--------------------------------
import Syntax
import Control.Monad.Trans.Except
import Control.Monad.Reader

-- list of (variable name, type) as typing context
type Context = [(Name,Type)]
-- datatype to carry information about current 
-- typing context and/or typing errors during typechecking
type Check = ExceptT TypeError (Reader Context)

-- datatype to describe ill typed expr error
data TypeError = Mismatch Type Type String
               | NotFunction Type
               | NotInScope Name
               | MultipleDecls Name
               | InvalidExpr String
     deriving (Eq, Ord, Show)

-- "local" simply runs a function on the Reader Monad environment and
--  procduces the new Reader Environment resulting from the function execution
unionContext :: Context -> Check a -> Check a 
unionContext = local . (++)

lookupVar :: Name -> Check Type
lookupVar x = do
    context <- ask -- "ask" simply gets the Reader Monad environment
    case lookup x context of 
        Just var -> return var
        Nothing  -> throwE $ NotInScope x    

-- Main typechecking function
check :: DimlExpr -> Check Type
check expr = case expr of
    
    DTrue  -> return TBool
    DFalse -> return TBool
    DInt n -> return TInt

    Var x  -> lookupVar x 
    
    BinOp s e1 e2 
        | s `elem` ["==",">","<"] -> tcNumExpr e1 >> tcNumExpr e2 >> return TBool
        | otherwise               -> tcNumExpr e1 >> tcNumExpr e2

    Eq e1 e2 -> do 
        t1 <- check e1
        t2 <- check e2
        if t1 == t2 then return TBool
        else throwE $ Mismatch t1 t2 ("In equality comparison: " ++ show (Eq e1 e2))

    Lam name argType body -> do
        bodyType <- unionContext [(name,argType)] (check body)
        return $ TArr argType bodyType

    fun@(Fun funName argName argType retType body) -> do
        bodyType <- unionContext [(argName, argType),(funName,TArr argType retType)] (check body)
        let errMsg = "In function expr: " ++ show fun
        if bodyType == retType
            then return (TArr argType retType)
        else throwE (Mismatch bodyType retType errMsg)

    If e1 e2 e3 -> do       
        tcBoolExpr e1 -- type checks e1 and makes sure it's a bool, else fail
        t2 <- check e2
        t3 <- check e3
        if t2 == t3 then return t3
        else throwE $ Mismatch t2 t3 errMsg
        where errMsg = "In if expr: " ++ show (If e1 e2 e3) 

    Decl var e1 -> check e1

    Tuple e1 e2 -> do
        t1 <- check e1
        t2 <- check e2
        return $ TProd t1 t2

    Let (Decl v1 e1:rest) body ->
        check e1 >>= (\t -> unionContext [(v1,t)] . check $ Let rest body)
    Let (fun@(Fun name _ argty retty _):rest) body -> do
        funType <- check fun
        unionContext [(name,funType)] (check (Let rest body))
    Let [] body -> check body

    Apply e1 e2 -> do
        t1 <- check e1
        t2 <- check e2
        case t1 of 
            TArr a b | (a == t2) -> return b
                     | otherwise -> throwE $ Mismatch t2 a (show (Apply e1 e2))
            notArrType           -> throwE $ NotFunction t1

    where tcNumExpr e = do
              t <- check e
              if t == TInt then return TInt
              else throwE $ Mismatch t TInt ("numExpr: "++show e)
          
          tcBoolExpr e = do
              t <- check e
              if t == TBool then return TBool
              else throwE $ Mismatch t TBool (show e)

-- this function takes a context and returns a function
-- that reads a monad Transformer Check a and returns
-- either a type error or successful result
typeCheck :: Context -> DimlExpr -> Either TypeError Type
typeCheck context = flip runReader context . runExceptT . check

