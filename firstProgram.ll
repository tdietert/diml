; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc double @f(double %x, <2 x double> %z) #0 {
entry:
  %0 = tail call fastcc double @lambda(double %x, <2 x double> %z)
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @lambda(double %x, <2 x double> %z) #0 {
entry:
  %0 = fadd double %x, 2.000000e+00
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @fib(double %x1, double %y, <2 x double> %z) #0 {
entry:
  %0 = fcmp ult double %x1, 2.000000e+00
  br i1 %0, label %if.exit, label %if.else

if.else:                                          ; preds = %entry
  %1 = fadd double %x1, -1.000000e+00
  %2 = tail call fastcc double @fib(double %1, double %y, <2 x double> %z)
  %3 = fadd double %x1, -2.000000e+00
  %4 = tail call fastcc double @fib(double %3, double %y, <2 x double> %z)
  %5 = fadd double %2, %4
  ret double %5

if.exit:                                          ; preds = %entry
  ret double 1.000000e+00
}

; Function Attrs: nounwind readnone
define double @fst(<2 x double> %tuple) #0 {
entry:
  %0 = alloca double
  ret double 1.000000e+05
}

; Function Attrs: nounwind readnone
define double @main() #0 {
entry:
  %0 = tail call fastcc double @fib(double 1.000000e+01, double 2.342340e+05, <2 x double> undef)
  %1 = tail call fastcc double @f(double 2.342340e+05, <2 x double> undef)
  %2 = fadd double %0, %1
  ret double %2
}

declare i32 @printf(i8* noalias nocapture, ...)

define void @printInt(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
