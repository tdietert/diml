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
    | Fun Name Name Annot Annot DimlExpr     
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Decl Name DimlExpr             
    | Let DimlExpr DimlExpr
    | Tuple DimlExpr DimlExpr Annot
    | Parens DimlExpr Annot
    | InL DimlExpr Annot
    | InR DimlExpr Annot
    | Case DimlExpr [DimlExpr] [DimlExpr]
    | PrintInt DimlExpr
    | Builtins Builtins
   deriving (Eq, Ord, Show)

--Diml Patterns rn matching
data Pattern
    = IntPat Int
    | TruePat
    | FalsePat
    | UnitPat
    | WildCardPat
    | VarPat String
    | InLPat Pattern
    | InRPat Pattern
    | TupPat Pattern Pattern
