module Eval where

import Syntax
import Data.Maybe

import Prelude hiding ((/))

type Env = [(Name,Value)]

data Value = VInt Integer
           | VBool Bool
           | VTupl (Value,Value)
           | VClosure Env Name DimlExpr
           deriving (Eq,Show)

(/) :: (Integral a) => a -> a -> a
(/) = div

arithOpTable :: (Integral a) => [(String,a -> a -> a)]
arithOpTable = [
      ("+",(+))
    , ("-",(-))
    , ("*",(*))
    , ("/",(/))
  ]

boolOpTable :: (Ord a) => [(String,a -> a -> Bool)]
boolOpTable = [
      ("<",(<))
    , (">",(>))
  ]

eval :: Env -> DimlExpr -> Value
eval env expr = case expr of
    DTrue -> VBool True
    DFalse -> VBool False
    DInt n -> VInt n
    Var x -> case lookup x env of
                 Just val -> val
                 Nothing  -> lookupFail
             where lookupFail = error $ "broke when looking up: "++show x++"in env "++show env
    
    BinOp s e1 e2 ->
        case lookup s arithOpTable of
            Just (/) -> let (VInt denom) = eval env e2 in
                            if denom /= 0 
                                then arithOpHelper (/) e1 e2
                            else error "divide by zero error!"
            Just op  -> arithOpHelper op e1 e2
            Nothing  -> case lookup s boolOpTable of
                            Just op -> boolOpHelper op e1 e2
                            Nothing -> error $ "that binOp is not recognized!"

    Eq e1 e2 -> if v1 == v2 then VBool True
                else VBool False
        where v1 = eval env e1
              v2 = eval env e2

    Lam name typ body -> VClosure env name body
    Fun funName argName argType retType body -> 
        let fun = VClosure env argName body
        in VClosure ((funName,fun):env) argName body

    If e1 e2 e3 -> case eval env e1 of
        (VBool True) -> eval env e2
        (VBool False) -> eval env e3

    Let decls body -> eval (newVars ++ env) body 
        where newVars = foldl evalDecl env decls -- folds over list of declarations, evals each 
              evalDecl :: Env -> DimlExpr -> Env
              evalDecl env' (Decl var e) = (var, eval env' e):env'
              evalDecl env' fun@(Fun name _ _ _ _) = 
                  let fval = eval env' fun 
                  in (name,fval):env' 

    Apply fun@(Var fname) arg -> eval ((fname,fval):(paramName,argResult):env') funBody
        where argResult = eval env arg
              fval@(VClosure env' paramName funBody) = eval env fun

    Tuple e1 e2 -> VTupl (eval env e1, eval env e2)

    e -> error $ "Your eval func is broken: "++show e++" slipped through..."

    where arithOpHelper :: (Integer -> Integer -> Integer) -> DimlExpr -> DimlExpr -> Value
          arithOpHelper f e1 e2 = 
              let (VInt a) = eval env e1 in 
              let (VInt b) = eval env e2 
              in VInt(f a b)
           
          boolOpHelper :: (Integer -> Integer -> Bool) -> DimlExpr -> DimlExpr -> Value
          boolOpHelper f e1 e2 =
              let (VInt a) = eval env e1 in 
              let (VInt b) = eval env e2 
              in VBool (f a b)
      
