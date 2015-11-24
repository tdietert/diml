; ModuleID = 'dimlProgram'

define internal double @f(double %x, [2 x double] %z) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = alloca [2 x double]
  store [2 x double] %z, [2 x double]* %1
  %2 = load double* %0
  %3 = load [2 x double]* %1
  %4 = call double @lambda(double %2, [2 x double] %3)
  ret double %4
}

define internal double @lambda(double %x, [2 x double] %z) {
entry:
  %0 = alloca double
  store double %x, double* %0
  %1 = alloca [2 x double]
  store [2 x double] %z, [2 x double]* %1
  %2 = load double* %0
  %3 = fadd double %2, 2.000000e+00
  ret double %3
}

define internal double @fib(double %x1, [2 x double] %z) {
entry:
  %0 = alloca double
  store double %x1, double* %0
  %1 = alloca [2 x double]
  store [2 x double] %z, [2 x double]* %1
  %2 = load double* %0
  %3 = fcmp ult double %2, 2.000000e+00
  %4 = uitofp i1 %3 to double
  %5 = fcmp one double 0.000000e+00, %4
  br i1 %5, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  %6 = load double* %0
  %7 = fsub double %6, 1.000000e+00
  %8 = load [2 x double]* %1
  %9 = call double @fib(double %7, [2 x double] %8)
  %10 = load double* %0
  %11 = fsub double %10, 2.000000e+00
  %12 = load [2 x double]* %1
  %13 = call double @fib(double %11, [2 x double] %12)
  %14 = fadd double %9, %13
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %15 = phi double [ 1.000000e+00, %if.then ], [ %14, %if.else ]
  ret double %15
}

@.fmti = private unnamed_addr constant [3 x i8] c "%d\00"

define double @fst([2 x double] %tuple) {
entry:
  %0 = alloca double
  %1 = extractvalue [2 x double] %tuple, 0
  store double %1, double* %0
  %2 = load double* %0
  ret double %2
}

declare i32 @printf(i8* noalias nocapture, ...)

define void @printInt(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64  %this)
  ret void
}

define double @main() {
entry:
  %0 = alloca [2 x double]
  %1 = load [2 x double]* %0
  %2 = insertvalue [2 x double] %1, double 1.000000e+00, 0
  %3 = insertvalue [2 x double] %2, double 2.000000e+00, 1
  %4 = alloca [2 x double]
  store [2 x double] %3, [2 x double]* %4
  %5 = load [2 x double]* %4
  %6 = call double @fib(double 1.000000e+01, [2 x double] %5)
  %7 = load [2 x double]* %4
  %8 = call double @f(double 1.100000e+01, [2 x double] %7)
  %9 = fadd double %6, %8
  %10 = fptosi double %9 to i64
  %11 = call double @fst([2 x double] %3)
  %12 = fptosi double %11 to i64
  call void @printInt(i64 %12)
  ret double 1.000000e+00
}

