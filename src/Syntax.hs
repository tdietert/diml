module Syntax where

type Name = String

data Type 
    = TBool 
    | TInt 
    | TArr Type Type
    deriving (Eq, Ord, Show)
    
data DimlExpr 
    = DTrue 
    | DFalse
    | DInt Integer
    | Var Name
    | Add DimlExpr DimlExpr
    | Sub DimlExpr DimlExpr 
    | Mul DimlExpr DimlExpr
    | Div DimlExpr DimlExpr
    | Eq DimlExpr DimlExpr
    | Lam Name Type DimlExpr
    | Fun Name Name Type Type DimlExpr  -- (name : T1) : T2 body-- Diml Expression Definition
    | Less DimlExpr DimlExpr            -- LessEq DimlExpr DimlExpr
    | Great DimlExpr DimlExpr           --  GreatEq DimlExpr DimlExpr
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Decl Name DimlExpr            -- helper expr for multi declaration letexprs
    | Let [DimlExpr] DimlExpr 
  deriving (Eq, Ord, Show)

--Diml Patterns for pattern matching
--data Pattern 
--    = IntPat Int 
--    | TruePat 
--    | FalsePat 
--    | WildCardPat 
--    | VarPat String