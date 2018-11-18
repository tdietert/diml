# diML Compiler (diminished ML)

## Compiler in progress for my Honor's Thesis, Summer/Fall 2015
The project is in a semi-final state, where I have completed my thesis, but have much more to add/experiment with. The thesis covering the implementation of this Compiler is found in the file 'diML_Thesis.pdf.' This compiler uses Parsec for lexing, parsing, and AST generation. The two modules Typecheck.hs and TypeInfer.hs handle the typechecking (though TypeInfer.hs subsumes Typecheck.hs). Code generation targets LLVM IR using the llvm-general and llvm-general-pure libraries (Haskell bindings to call C++ LLVM functions) with the help of Stephen Diehl's tutorial <http://dev.stephendiehl.com/fun/WYAH.pdf>. 

## Build Instructions:

> $ git clone https://github.com/tdietert/dimlCompiler.git

> $ stack setup

> $ stack build

> $ stack exec dimlCompiler

> diML> *enter code here*

## File Compilation:
> $ stack exec dimlCompiler \<nameOfFile\>

> $ gcc \<nameOfFile\>.s -o \<outfile\>

> $ ./\<outfile\>

This will run the repl for the language and you can write expressions that will be evaluated very similar to the way ghci runs code. The repl takes single line exprs and displays the codegen result of the expression entered. The environment is preserved with the help of the function "procLlvmModule" and an InputT monad transformer in conjunction with the haskeline package, so that when a sequence of expressions is entered into the repl, the variables defined in an expression are in context when evaluating subsequent expressions.


Since Haskell lends itself to expressing CFGs with algebraic data types, here is the Haskell definition of the DimlExpr data type as the CFG for the language:
```haskell

type Name = String
type Annot = Maybe Type

-- | Types
data TVar = TV String
    deriving (Eq, Ord, Show)

data Type 
    = Unit
    | TVar TVar
    | TCon String
    | TArr Type Type
    | TProd Type Type
    deriving (Eq, Ord, Show)

data Scheme = Forall [TVar] Type
    deriving (Show)

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
    | InL DimlExpr Annot
    | InR DimlExpr Annot
    | Case DimlExpr [DimlExpr] [DimlExpr]
    | Parens DimlExpr Annot
    | PrintInt DimlExpr
    | Builtins Builtins
   deriving (Eq, Ord, Show)
```

## Note:

Type inference is fully implemented. Will do some minor tweaks to first IR and then attempt to implement pattern matching specifically to destruct tuples (perhaps I need to add case exprs to demonstrate the pattern matching. I want to add the list data type as well, and have some built in functions like map and filter. 


**To Do:**
- ~~Code-Gen to LLVM~~
- ~~Lambda Lift Trasformation~~
- ~~Type Inference (Hindley-Milner)~~ 
- ~~Change let exprs in DimlExpr definition~~
- ~~File compilation to host machine assembly~~
- Pretty Printing
- Pattern Matching


**New Exprs (after base llvm codegen is added):**
- ~~Sum Types (InL / InR exprs, case expressions)~~
- Recursive Types, Lists
- References (Arrays too?)
- Objects (sub-typing)

## Example Repl Usage:
---

**diML> fun add1(x:Int):Int = x + 1**

```
diML> let inc = (\n -> n + 1) in inc(inc(1))
Let [Decl "inc" (Lam "n" (BinOp "+" (Var "n") (Lit (DInt 1))))] (Apply (Var "inc") (Apply (Var "inc") (Lit (DInt 1)))) :: Forall [] (TCon "Int")

DimlExpr AST:
    Let [Decl "inc" (Lam "n" (BinOp "+" (Var "n") (Lit (DInt 1))))] (Apply (Var "inc") (Apply (Var "inc") (Lit (DInt 1))))

DimlIR AST:
    ITopLevel (IClosure "lambda" "n" [] (IBinOp "+" (IVar "n") (IInt 1))) (ILet [] (IApp "lambda" [IApp "lambda" [IInt 1]]))

; ModuleID = 'dimlProgram'

define internal double @lambda(double %n) {
entry:
  %0 = alloca double
  store double %n, double* %0
  %1 = load double* %0
  %2 = fadd double %1, 1.000000e+00
  ret double %2
}

declare void @printInt(i64)

define double @main() {
entry:
  %0 = call double @lambda(double 1.000000e+00)
  %1 = call double @lambda(double %0)
  ret double %1
}
```

## Example of valid program and compilation:
---

**Usage:**
> $ stack exec dimlCompiler firstProgram.txt

**firstProgram.txt:**
```
let y = 5,
    fun fib(x:Int):Int = 
       if (x < 2) then 1
       else fib (x-1) + fib(x-2),
    add1 = (\x:Int -> x + 1)
in fib(y) + add1(y)
```

**Resulting AST:**
```
Let [ Decl "y" (DInt 5)
    , Fun "fib" "x" TInt TInt 
        (If (BinOp "<" (Var "x") (DInt 2)) (DInt 1) 
        (BinOp "+" 
            (Apply (Var "fib") (BinOp "-" (Var "x") (DInt 1))) 
            (Apply (Var "fib") (BinOp "-" (Var "x") (DInt 2)))))
    , Decl "add1" (Lam "x" TInt (BinOp "+" (Var "x") (DInt 1)))] 
   (BinOp "+" 
       (Apply (Var "fib") (Var "y")) 
       (Apply (Var "add1") (Var "y")))
```

Then, we must "lambda lift" (aka closure conversion) all nested function declarations to the top level. LLVM IR does not support nested functions (as many assembly and assembly-like languages don't), so we must transform the AST into our own "IR". The transformation code is located in IR.hs:

**Resulting (lambda lifted) IR AST:**
``` 
ITopLevel 
    (ITopLevel 
        (IClosure "lambda" "x" ["fib","y"] (IBinOp "+" (IVar "x") (IInt 1))) 
        (IClosure "fib" "x" ["y"] 
            (IIf (IBinOp "<" (IVar "x") (IInt 2)) 
                (IInt 1) 
                (IBinOp "+" 
                    (IApp "fib" (IBinOp "-" (IVar "x") (IInt 1))) 
                    (IApp "fib" (IBinOp "-" (IVar "x") (IInt 2))))))) 
    (ILet [ IDec "y" (IInt 5),
          , IDec "add1" (IVar "lambda")] 
        (IBinOp "+" 
            (IApp "fib" (IVar "y")) 
            (IApp "lambda" (IVar "y"))))
```

**Compilation to LLVM IR:**
```
define double @lambda(double %x, double %fib, double %y) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = load double* %0
  %2 = fadd double %1, 1.000000e+00
  ret double %2
}

define double @fib(double %x, double %y) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = load double* %0
  %2 = fcmp ult double %1, 2.000000e+00
  %3 = uitofp i1 %2 to double
  br double %3, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  %4 = load double* %0
  %5 = fsub double %4, 1.000000e+00
  %6 = call double @fib(double %5)
  %7 = load double* %0
  %8 = fsub double %7, 2.000000e+00
  %9 = call double @fib(double %8)
  %10 = fadd double %6, %9
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %11 = phi double [ 1.000000e+00, %if.then ], [ %10, %if.else ]
  ret double %11
}

define double @main() {
entry:
  %0 = alloca double
  store double 5.000000e+00, double* %0
  %1 = alloca double
  %2 = load double (double, double, double)* @lambda
  store double (double, double, double) %2, double* %1
  %3 = load double* %0
  %4 = call double @fib(double %3)
  %5 = load double* %0
  %6 = call double @lambda(double %5)
  %7 = fadd double %4, %6
  ret double %7
}
```
