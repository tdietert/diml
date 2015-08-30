## diML Compiler (diminished ML)

##### Compiler in progress for my Honor's Thesis, Summer/Fall 2015
Currently, the lexing and parsing, type checking, and evalution functions are written. Code generation with LLVM (following Stephen Diehl's nice tutorial <http://dev.stephendiehl.com/fun/WYAH.pdf>) will be implemented during the Fall semester, starting in a week or two.


#####Build Instructions:

> $ git clone https://github.com/tdietert/dimlCompiler.git

> $ stack setup

> $ stack build

> $ stack exec dimlCompiler

This will run the repl for the language and you can write expressions that will be evaluated very similar to the way ghci runs code... though currently, variable and function declarations do not stay in scope when using the repl. Right now the repl functions mostly as a single expression parser,typechecker, and simple evaluator.


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

#####Note:

My grammar is a bit clunky and will get cleaned up soon. The IR I'm using needs some work so that it will be a bit easier to transform into LLVM IR. 


**To Do:**

>Code-Gen to LLVM

>Type Inference (Hindley-Milner)

>Pattern Matching


**New Exprs:**

>Case Expressions

>References (Arrays too?)

>Objects (sub-typing)


#####Examples of valid programs:
---
firstProgram.txt:

> $ evalProgram "..\\firstProgram.txt"

```
let (fun fib(x:Int):Int = 
    if (x < 2) then (1)
    else fib (x-1) + fib (x - 2))
in fib 6
```

AST:
```
Let [ Fun "fib" "x" TInt TInt 
         (If (Less (Var "x") (DInt 2)) 
             (DInt 1) 
             (Add (Apply (Var "fib") (Sub (Var "x") (DInt 1))) (Apply (Var "fib") (Sub (Var "x") (DInt 2)))))
    ] (Apply (Var "fib") (DInt 5))
```

Evaluated:

> $ it = VInt 13 : TInt
