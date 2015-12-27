module Syntax where

import Type

type Name = String
type Annot = Maybe Type

data DLit
    = DUnit
    | DTrue
    | DFalse
    | DInt Integer
  deriving (Eq, Ord, Show)

data Builtins
    = TupFst DimlExpr
    | TupSnd DimlExpr
  deriving (Eq, Ord, Show)

data DimlExpr
    = Lit DLit
    | Var Name
    | BinOp Name DimlExpr DimlExpr
    | Lam Name Annot DimlExpr
    | Fun Name Name Annot Annot DimlExpr       -- (name : T1) : T2 body-- Diml Expression Definition
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Decl Name DimlExpr             -- helper expr for multi declaration letexprs
    | Let DimlExpr DimlExpr
    | Tuple DimlExpr DimlExpr Annot
    | Parens DimlExpr Annot
    | InL DimlExpr Annot
    | InR DimlExpr Annot
    | PrintInt DimlExpr
    | Builtins Builtins
   deriving (Eq, Ord, Show)

--Diml Patterns for pattern matching
--data Pattern
--    = IntPat Int
--    | TruePat
--    | FalsePat
--    | WildCardPat
--    | VarPat String
