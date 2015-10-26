module Syntax where

type Name = String

data DLit 
    = DTrue
    | DFalse
    | DInt Integer
  deriving (Eq, Ord, Show)

data DimlExpr 
    = Lit DLit
    | Var Name
    | BinOp Name DimlExpr DimlExpr 
    | Lam Name DimlExpr
    | Fun Name Name DimlExpr        -- (name : T1) : T2 body-- Diml Expression Definition    
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Decl Name DimlExpr            -- helper expr for multi declaration letexprs
    | Let [DimlExpr] DimlExpr 
    | Tuple DimlExpr DimlExpr
    | PrintInt DimlExpr
  deriving (Eq, Ord, Show)

--Diml Patterns for pattern matching
--data Pattern 
--    = IntPat Int 
--    | TruePat 
--    | FalsePat 
--    | WildCardPat 
--    | VarPat String