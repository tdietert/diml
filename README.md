## diML Compiler (diminished ML)

##### Compiler in progress for my Honor's Thesis, Summer/Fall 2015
Currently, the lexing and parsing, type checking, and evalution (to check parser) functions are written. Code generation with LLVM (using Stephen Diehl's nice tutorial <http://dev.stephendiehl.com/fun/WYAH.pdf> as a guide) is currently in progress.


#####Build Instructions:

> $ git clone https://github.com/tdietert/dimlCompiler.git

> $ stack setup

> $ stack build

> $ stack exec dimlCompiler

> diML> *enter code here*

This will run the repl for the language and you can write expressions that will be evaluated very similar to the way ghci runs code. The repl takes single line exprs and displays the codegen result of the expression entered. The environment is preserved with the help of the function "procLlvmModule" and an InputT monad transformer in conjunction with the haskeline package, so that when a sequence of expressions is entered into the repl, the variables defined in an expression are in context when evaluating subsequent expressions.


Since Haskell lends itself to expressing CFGs with algebraic data types, here is the Haskell definition of the DimlExpr data type as the CFG for the language:
```haskell
type Name = String

data Type 
    = TBool
    | TInt 
    | TArr Type Type
    | TProd Type Type
    
data DimlExpr 
    = DTrue 
    | DFalse
    | DInt Integer
    | Var Name
    | BinOp Name DimlExpr DimlExpr
    | Eq DimlExpr DimlExpr   
    | Lam Name Type DimlExpr
    | Fun Name Name Type Type DimlExpr      
    | If DimlExpr DimlExpr DimlExpr
    | Apply DimlExpr DimlExpr
    | Decl Name DimlExpr            -
    | Let [DimlExpr] DimlExpr 
    | Tuple DimlExpr DimlExpr
```

#####Note:

IR is much cleaner than before, simplifying Add, Sub, Mul, Div, Great, and Less exprs into a *BinOp* expr. Codegen for top level functions is done, however this will be replaced by top level let declarations. I may include a new expr soon for a "main" block and allow top level functions. These top level exprs will evaluate to llvm blocks (global vars and functions) and be evaluated in a single llvm module.


**To Do:**

- ~~Code-Gen to LLVM~~ (95%, needs testing)
- ~~Lambda Lift Trasformation~~
- Codegen to x86
- Type Inference (Hindley-Milner)
- Pattern Matching
- Garbage Collection (RTS)


**New Exprs (after base llvm codegen is added):**

- Case Expressions
- References (Arrays too?)
- Objects (sub-typing)

####Example Repl Usage:
---

**diML> fun add1(x:Int):Int = x + 1**

```
Fun "add1" "x" TInt TInt (BinOp "+" (Var "x") (DInt 1))

; ModuleID = 'diML Repl'

define double @add1(double %x) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = load double* %0
  %2 = fadd double %1, 1.000000e+00
  ret double %2
}
```

**diML> fun add2(x:Int):Int = add1(add1(x))**

```
Fun "add2" "x" TInt TInt (Apply (Var "add1") (Apply (Var "add1") (Var "x")))

; ModuleID = 'diML Repl'

define double @add1(double %x) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = load double* %0
  %2 = fadd double %1, 1.000000e+00
  ret double %2
}

define double @add2(double %x) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = load double* %0
  %2 = call double @add1(double %1)
  %3 = call double @add1(double %2)
  ret double %3
}
```

####Example of valid program and compilation:
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
}```
