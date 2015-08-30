module Eval where

import Syntax
import Data.Maybe

type Env = [(Name,Value)]

data Value = VInt Integer
           | VBool Bool
           | VClosure Env Name DimlExpr
           deriving (Eq,Show)

eval :: Env -> DimlExpr -> Value
eval env expr = case expr of
    DTrue -> VBool True
    DFalse -> VBool False
    DInt n -> VInt n
    Var x -> case lookup x env of
                 Just val -> val
                 Nothing  -> lookupFail
             where lookupFail = error $ "broke when looking up: "++show x++"in env "++show env
    Add e1 e2 -> arithOpHelper (+) e1 e2
    Sub e1 e2 -> arithOpHelper (-) e1 e2
    Mul e1 e2 -> arithOpHelper (*) e1 e2
    Div e1 e2 -> let (VInt denom) = eval env e2 in
                 if denom /= 0 then arithOpHelper div e1 e2
                 else error "divide by zero error!"

    Eq e1 e2 -> if v1 == v2 then VBool True
                else VBool False
        where v1 = eval env e1
              v2 = eval env e2

    Lam name typ body -> VClosure env name body
    Fun funName argName argType retType body -> 
        let fun = VClosure env argName body
        in VClosure ((funName,fun):env) argName body
    Less e1 e2 -> boolOpHelper (<) e1 e2
    Great e1 e2 -> boolOpHelper (>) e1 e2

    If e1 e2 e3 -> case eval env e1 of
        (VBool True) -> eval env e2
        (VBool False) -> eval env e3

    Let decls body -> eval (newVars ++ env) body 
        where newVars = foldl evalDecl env decls
              evalDecl :: Env -> DimlExpr -> Env
              evalDecl env' (Decl var e) = (var, eval env' e):env'
              evalDecl env' fun@(Fun name _ _ _ _) = 
                  let fval = eval env' fun 
                  in (name,fval):env' 

    Apply fun@(Var fname) arg -> eval ((fname,fval):(paramName,argResult):env') funBody
        where argResult = eval env arg
              fval@(VClosure env' paramName funBody) = eval env fun

    e -> error $ "Your functions are not written well: "++show e++" slipped through..."

    where arithOpHelper f e1 e2 = 
              let (VInt a) = eval env e1 in 
              let (VInt b) = eval env e2 
              in VInt(f a b)
           
          boolOpHelper :: (Integer -> Integer -> Bool) -> DimlExpr -> DimlExpr -> Value
          boolOpHelper f e1 e2 =
              let (VInt a) = eval env e1 in 
              let (VInt b) = eval env e2 
              in VBool (f a b)
      
