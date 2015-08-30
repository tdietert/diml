### diML Compiler (diminished ML)
---
##### Compiler in progress for my Honor's Thesis, Summer/Fall 2015
Currently, the lexing and parsing, type checking, and evalution functions are written. Code generation with LLVM (following Stephen Diehl's nice tutorial <http://dev.stephendiehl.com/fun/WYAH.pdf>) will be implemented during the Fall semester, starting in a week or two.

Since Haskell lends itself to expressing CFGs with algebraic data types, I'm just posting the Syntax.hs definition of diML as the CFG for the language:
```haskell
type Name = String

data Type 
    = TBool
    | TInt 
    | TArr Type Type
    
data DimlExpr 
    = DTrue 
    | DFalse
    | DInt Integer
    | Var Name
    | Add DimlExpr DimlExpr
    | Sub DimlExpr DimlExpr 
    | Mul DimlExpr DimlExpr
    | Div DimlExpr DimlExpr
    | Lam Name Type DimlExpr
    | Fun Name Name Type Type DimlExpr  
    | Less DimlExpr DimlExpr 
    | LessEq DimlExpr DimlExpr
    | Great DimlExpr DimlExpr
    | GreatEq DimlExpr DimlExpr
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Let [Name] [DimlExpr] DimlExpr
```


#####To Do List:
---
Code-Gen to LLVM

Type Inference (Hindley-Milner)

Pattern Matching


**New Exprs:**

>Case Expressions

>References (Arrays too?)

>Objects (sub-typing)


#####Examples of valid programs:
---
(soon)
